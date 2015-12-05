{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

import Data.String (fromString)
import System.Environment (getArgs)
import System.IO (withFile,IOMode(ReadMode,WriteMode))
import System.Posix.Signals (installHandler,sigINT,sigTERM,Handler(Catch))
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.Wai.Application.Static as Static
import Data.FileEmbed (embedDir)
import WaiAppStatic.Types (toPieces)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.HTTP.Types as H
import qualified Network.Wai.Parse as Parse
import qualified Network.WebSockets as WS
import Control.Concurrent (ThreadId,forkIO,threadDelay)
import Data.Typeable (Typeable)
import Control.Exception (Exception,catch,throwIO,throwTo,finally)
import Control.Monad (forever,void)
import qualified Data.ByteString.Char8 as BS -- use for input 
import qualified Data.ByteString.Lazy.Char8 as LBS -- use for output
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Network.HTTP.Types.URI (parseSimpleQuery)
import Data.Aeson.Types (ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE

import Control.Applicative ((<$>),(<*>))
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async(race)
import qualified System.Random as R

import Server
import qualified Bingo as B
--import JSON




main :: IO ()
main = do
  host:port:_ <- getArgs
  server <- newServer
--  host:port:backupPath:_ <- getArgs
--  server <- restoreServer backupPath
--  threadBackup <- forkIO $ backupServer server backupPath -- fork backup thread
  Warp.runSettings (
    Warp.setHost (fromString host) $
    Warp.setPort (read port) $
--    Warp.setInstallShutdownHandler (installShutdownHandler threadBackup) $
    Warp.defaultSettings
    ) $ websocketsOr WS.defaultConnectionOptions (websocketApp server) (plainOldHttpApp server)

  
-- [TODO] change to UserInterrupt
{--
data StopException = StopException deriving (Show,Typeable)
instance Exception StopException


installShutdownHandler :: ThreadId -> IO () -> IO ()
installShutdownHandler threadBackup close = do
  void $ installHandler sigINT (Catch shutdown) Nothing
  void $ installHandler sigTERM (Catch shutdown) Nothing
  where
    shutdown = do
      -- [TODO] last backup before shutdown
      close
      throwTo threadBackup StopException
  

restoreServer :: String -> IO Server
restoreServer path = do
  putStrLn "restore: "
  json <- withFile path ReadMode BS.hGetContents
  case AE.decodeStrict json of
    Nothing -> newServer
    Just jo -> STM.atomically $ serverFromJO jo


backupServer ::  Server -> String -> IO ()
backupServer server path =
  -- [TODO] mask exception
  loop `catch` onException
  where
    loop = do 
      putStrLn "backup: "
      jo <- serverToJO server
      withFile path WriteMode ( \h -> LBS.hPutStrLn h $ AE.encode jo )
      threadDelay $ 30 * 1000 * 1000
      loop

    onException :: StopException -> IO ()
    onException e = return ()

--}


data BingoException = BingoException Error deriving (Show,Typeable)
instance Exception BingoException

throwErrorIO :: Error -> IO a
throwErrorIO error = throwIO $ BingoException error

throwErrorSTM :: Error -> STM.STM a
throwErrorSTM error = STM.throwSTM $ BingoException error




contentTypeJsonHeader :: H.Header
contentTypeJsonHeader = ("Content-Type","application/json")


plainOldHttpApp :: Server -> Wai.Application
plainOldHttpApp server req respond
  | (["draw_game"]   == path) = (drawGameProc   server req respond) `catch` onError
  | (["reset_game"]  == path) = (resetGameProc  server req respond) `catch` onError
  | (["add_game"]    == path) = (addGameProc    server req respond) `catch` onError
  | (["get_game"]    == path) = (getGameProc    server req respond) `catch` onError
  | otherwise = staticHttpApp req respond -- static html/js/css files
  where
    path = Wai.pathInfo req

    onError :: BingoException -> IO Wai.ResponseReceived
    onError (BingoException error) =
      -- [TODO] log
      respond $ Wai.responseLBS
      H.status200
      [contentTypeJsonHeader]
      (AE.encode error)
    

staticHttpApp :: Wai.Application
staticHttpApp = Static.staticApp $ settings { Static.ssIndices = indices }
  where
    settings = Static.embeddedSettings $(embedDir "static") -- embed contents as ByteString
    indices = fromJust $ toPieces ["admin.html"] -- default content


lookupParams :: BS.ByteString -> [(BS.ByteString,BS.ByteString)] -> Maybe String
lookupParams key params = do
  b <- Map.lookup key $ Map.fromList params
  return $ T.unpack $ decodeUtf8 b
  


data Response = ResponseGame GameSecretKey GamePublicKey GameCaption
              | ResponseDraw
              | ResponseReset
              deriving (Show)

instance ToJSON Response where
  toJSON (ResponseGame sk pk ca) =
    object ["success" .= True
           ,"type"    .= ("game" :: String)
           ,"content" .= object ["secret_key" .= sk
                                ,"public_key" .= pk
                                ,"caption"    .= ca
                                ]
           ]
  toJSON (ResponseDraw) =
    object ["success" .= True
           ,"type"    .= ("draw" :: String)
           ,"content" .= ("ok" :: String)
           ]
  toJSON (ResponseReset) =
    object ["success" .= True
           ,"type"    .= ("reset" :: String)
           ,"content" .= ("ok" :: String)
           ]



drawGameProc :: Server -> Wai.Application
drawGameProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case lookupParams "secret_key" params of
    Nothing -> throwErrorIO GameSecretKeyNotSpecified
    Just sk -> return sk

  g <- R.newStdGen
  STM.atomically $ do
    mgame <- getGameFromSecretKey server secretKey
    case mgame of
      Nothing -> throwErrorSTM GameFromSecretKeyNotFound
      Just game -> draw g game
  
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseDraw )



resetGameProc :: Server -> Wai.Application
resetGameProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case lookupParams "secret_key" params of
    Nothing -> throwErrorIO GameSecretKeyNotSpecified
    Just sk -> return sk

  g <- R.newStdGen
  STM.atomically $ do
    mgame <- getGameFromSecretKey server secretKey
    case mgame of
      Nothing -> throwErrorSTM GameFromSecretKeyNotFound
      Just game -> reset g game
  
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseReset )




addGameProc :: Server -> Wai.Application
addGameProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  publicKey <- case lookupParams "public_key" params of
    Nothing -> throwErrorIO GamePublicKeyNotSpecified
    Just pk -> return pk

  caption <- case lookupParams "caption" params of
    Nothing -> throwErrorIO GameCaptionNotSpecified
    Just ca -> return ca

  egame <- addGameIO server publicKey caption
  game <- case egame of
    Left error  -> throwErrorIO error
    Right game -> return game

  putStrLn $ "Server.addGameIO: secretKey: " ++ (gameSecretKey game) ++ " publicKey: " ++ publicKey ++ " caption: " ++ caption
  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    (AE.encode $ ResponseGame (gameSecretKey game) publicKey caption)



getGameProc :: Server -> Wai.Application
getGameProc server req respond = do
  (params, _) <- Parse.parseRequestBody Parse.lbsBackEnd req -- parse post parameters

  secretKey <- case lookupParams "secret_key" params of
    Nothing -> throwErrorIO GameSecretKeyNotSpecified
    Just sk -> return sk

  mgame <- STM.atomically $ getGameFromSecretKey server secretKey
  game <- case mgame of
    Nothing -> throwErrorIO GameFromSecretKeyNotFound
    Just game -> return game

  respond $ Wai.responseLBS
    H.status200
    [contentTypeJsonHeader]
    ( AE.encode $ ResponseGame secretKey (gamePublicKey game) (gameCaption game))



websocketApp :: Server -> WS.ServerApp
websocketApp server pconn
  | ("/viewer"      == path) = (viewerServer server pconn)      `catch` onError
  | ("/participant" == path) = (participantServer server pconn) `catch` onError
  | otherwise = WS.rejectRequest pconn "endpoint not found"
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    path = BS.takeWhile (/='?') requestPath

    onError :: BingoException -> IO()
    onError (BingoException error) = do
      -- [TODO] log
      WS.rejectRequest pconn (BS.pack $ show error)



onCloseError :: WS.Connection -> BingoException -> IO()
onCloseError conn (BingoException error) = do
  -- [TODO] log
  WS.sendClose conn (BS.pack $ show error)





data WSMessage = WSMessageGame GamePublicKey GameCaption [Int]
               | WSMessageParticipant ParticipantKey GamePublicKey GameCaption [Int] B.Card [Bool] B.CardStatus
               deriving (Show)
                      
instance ToJSON WSMessage where
  toJSON (WSMessageGame pk ca sd) =
    object ["type"    .= ("game" :: String)
           ,"content" .= object ["public_key" .= pk
                                ,"caption"    .= ca
                                ,"selected"   .= sd
                                ]
           ]
  toJSON (WSMessageParticipant pk gpk ca sd cd ev st) =
    object ["type"    .= ("participant" :: String)
           ,"content" .= object ["participant_key" .= pk
                                ,"game_public_key" .= gpk
                                ,"game_caption"    .= ca
                                ,"game_selected"   .= sd
                                ,"card"            .= cd
                                ,"eval"            .= ev
                                ,"state"           .= st
                                ]
           ]



viewerServer :: Server -> WS.ServerApp
viewerServer server pconn = do
  putStrLn $ "viewerServer: " ++ BS.unpack(requestPath) -- debug

  publicKey <- case lookupParams "public_key" query of
    Nothing -> throwErrorIO GamePublicKeyNotSpecified
    Just pk -> return pk

  mgame <- STM.atomically $ getGameFromPublicKey server publicKey
  game <- case mgame of
    Nothing -> throwErrorIO GameFromPublicKeyNotFound
    Just game -> return game


  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30


  st <- STM.atomically $ STM.readTVar (gameState game)

  let (_,sd) = st 

  WS.sendTextData conn $ AE.encode $ WSMessageGame publicKey (gameCaption game) sd

  chan <- STM.atomically $ STM.dupTChan $ gameChan game
  (loop conn chan) `catch` (onCloseError conn)

  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    loop :: WS.Connection -> STM.TChan Message -> IO()
    loop conn chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageResetGame     -> WS.sendTextData conn $ AE.encode msg
        MessageDrawGame _ _  -> WS.sendTextData conn $ AE.encode msg
        otherwise -> return ()
      loop conn chan

  


    


participantServer :: Server -> WS.ServerApp
participantServer server pconn = do
  putStrLn $ "participantServer: " ++ BS.unpack(requestPath) -- debug

  publicKey <- case lookupParams "game_public_key" query of
    Nothing -> throwErrorIO GamePublicKeyNotSpecified
    Just bpk -> return bpk


  mgame <- STM.atomically $ getGameFromPublicKey server publicKey
  game <- case mgame of
    Nothing -> throwErrorIO GameFromPublicKeyNotFound
    Just game -> return game


  participant <- case lookupParams "participant_key" query of
    Nothing -> addParticipant' game
    Just (pk) -> do
      mparticipant <- STM.atomically $ getParticipant game pk
      case mparticipant of
        Nothing -> addParticipant' game -- [TODO] must be an error ?
        Just participant -> return participant


  conn <- WS.acceptRequest pconn
  WS.forkPingThread conn 30

  (gst,cd) <- STM.atomically $
             (,) <$> STM.readTVar (gameState game) <*> STM.readTVar (participantCard participant)

  let (_,sd) = gst
  let ev = B.processCard cd sd
  let st = B.evalCard ev
      

  WS.sendTextData conn $ AE.encode $
    WSMessageParticipant (participantKey participant) (gamePublicKey game) (gameCaption game) sd cd ev st
  
  chan <- STM.atomically $ STM.dupTChan $ participantChan participant
  (loop conn chan) `catch` (onCloseError conn)
    
  where
    requestPath = WS.requestPath $ WS.pendingRequest pconn
    query = parseSimpleQuery $ BS.dropWhile (/='?') requestPath

    addParticipant' game = do
      mparticipant <- addParticipantIO game
      case mparticipant of
        Left error -> throwErrorIO error
        Right participant -> return participant


    loop :: WS.Connection -> STM.TChan Message -> IO()
    loop conn chan = do
      msg <- STM.atomically $ STM.readTChan chan
      case msg of
        MessageResetParticipant _       -> WS.sendTextData conn $ AE.encode msg
        MessageDrawParticipant _ _ _ _  -> WS.sendTextData conn $ AE.encode msg
        otherwise -> return ()
      loop conn chan
      
