{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

--module Server (
--   ReporterKey
--  ,Reporter (..)
--  ,newReporter
--  ,BoardSecretKey
--  ,BoardPublicKey
--  ,BoardCaption
--  ,Board (..)
--  ,newBoard
--  ,Server (..)
--  ,newServer
--  ,Response (..)
--  ,Message (..)
--  ,Error (..)
--  ,addBoardIO
--  ,getBoardFromPublicKey
--  ,getBoardFromSecretKey
--  ,resetBoard
--  ,addReporterIO
--  ,getReporter
--  ,aha
--  ) where


import Data.Data (Data,toConstr)
import Control.Applicative ((<$>),(<*>))
import Data.Typeable (Typeable)
import qualified Data.Map as Map
import Data.UUID.V1 (nextUUID)
import qualified Data.UUID as UUID
import Data.Aeson.Types (ToJSON,toJSON,object,(.=))
import qualified Data.Aeson as AE
import qualified Control.Concurrent.STM as STM
import Control.Concurrent.Async (race)

import qualified System.Random as R
import qualified Lottery as Lot
import qualified Bingo as B



import Control.Concurrent (forkIO,threadDelay)
import Control.Exception (throwIO,ErrorCall(..))

type ParticipantKey = String

data Participant = Participant
  { participantKey  :: ParticipantKey
  , participantCard :: STM.TVar B.Card
  , participantChan :: STM.TChan Message
  }

newParticipant :: ParticipantKey -> B.Card -> STM.STM Participant
newParticipant pk card = do
  card' <- STM.newTVar card
  chan  <- STM.newBroadcastTChan
  return Participant { participantKey  = pk
                     , participantCard = card'
                     , participantChan = chan
                     }


type GameSecretKey = String
type GamePublicKey = String
type GameCaption   = String

data Game = Game
  { gameSecretKey    :: GameSecretKey
  , gamePublicKey    :: GamePublicKey
  , gameCaption      :: GameCaption
  , gameCandidate    :: STM.TVar [Int]
  , gameState        :: STM.TVar B.State
  , gameParticipants :: STM.TVar (Map.Map ParticipantKey Participant)
  , gameChan         :: STM.TChan Message
  }

newGame :: GameSecretKey -> GamePublicKey -> GameCaption -> [Int] -> STM.STM Game
newGame gsk gpk caption cand = do
  cand'        <- STM.newTVar cand
  state        <- STM.newTVar (cand,[])
  participants <- STM.newTVar Map.empty
  chan         <- STM.newBroadcastTChan
  return Game { gameSecretKey    = gsk
              , gamePublicKey    = gpk
              , gameCaption      = caption
              , gameCandidate    = cand'
              , gameState        = state
              , gameParticipants = participants
              , gameChan         = chan
              }


data Server = Server
  { serverGames    :: STM.TVar (Map.Map GamePublicKey Game)
  , serverGameKeys :: STM.TVar (Map.Map GameSecretKey GamePublicKey)
  }

newServer :: IO Server
newServer = do
  games <- STM.newTVarIO Map.empty
  keys  <- STM.newTVarIO Map.empty
  return Server { serverGames = games, serverGameKeys = keys }


-- * 新規Game
-- ** Game -> MessageGame gsk, gpk, gcaption

-- * 新規Participant
-- ** Participant -> MessageParticipant psk, pcard, gpk, gcaption 

-- * draw
-- ** 全            -> MessageDraw (x,(_,ss) 今回何が選ばれて、今まで何が選ばれたか
-- ** 全Participant -> MessageEval CardStatus, [Bool]
  
-- * reset
-- ** 全             -> MessageReset
-- ** 全Participant  -> MessageCard pcard


-- * Game再接続
-- ** MessageGame + 今まで何が選ばれたか

-- * Participant再接続
-- ** Participant -> MessageParticipant psk, pcard, gpk, gcaption 
-- ** Participant -> MessageEval CardStatus, [Bool]



{--
data Message = MessageGame GamePublicKey GameCaption
             | MessageParticipant ParticipantKey B.Card GamePublicKey GameCaption
             | MessageDraw Int [Int]
             | MessageEval B.CardStatus [Bool]
             | MessageReset
             | MessageCard B.Card
             deriving (Show)

instance ToJSON B.CardStatus where
  toJSON (B.Bingo) =
    object ["status"  .= ("bingo" :: String)]
  toJSON (B.Lizhi x) =
    object ["status"  .= ("lizhi" :: String)
            ,"rem"    .= x
           ]
  toJSON (B.Blank) =
    object ["status"  .= ("blank" :: String)]


instance ToJSON Message where
  toJSON (MessageGame pk ca) =
    object ["type"    .= ("game" :: String)
           ,"content" .= object ["public_key" .= pk
                                ,"caption"    .= ca
                                ]
           ]
  toJSON (MessageParticipant pk cd gpk ca) =
    object ["type"    .= ("participant" :: String)
           ,"content" .= object ["key"             .= pk
                                ,"card"            .= cd
                                ,"game_public_key" .= gpk
                                ,"game_caption"    .= ca
                                ]
           ]
  toJSON (MessageDraw x ss) =
    object ["type"    .= ("card" :: String)
           ,"content" .= object ["item"     .= x
                                ,"selected" .= ss
                                ]
           ]
  toJSON (MessageEval state ev) =
    object ["type"    .= ("card" :: String)
           ,"content" .= object ["state" .= state
                                ,"eval"  .= ev
                                ]
           ]
  toJSON (MessageCard cd) =
    object ["type"    .= ("card" :: String)
           ,"content" .= object ["card" .= cd
                                ]
           ]
  toJSON (MessageReset) =
    object ["type" .= ("reset" :: String)]
--}

data Message = MessageCard B.Card
             | MessageDraw Int [Int]
             | MessageReset
             deriving (Show)
{--
instance ToJSON Message where
  toJSON (MessageCard cd) =
    object ["type"    .= ("card" :: String)
           ,"content" .= object ["card" .= cd
                                ]
           ]
  toJSON (MessageDraw x ss) =
    object ["type"    .= ("card" :: String)
           ,"content" .= object ["item"     .= x
                                ,"selected" .= ss
                                ]
           ]
  toJSON (MessageReset) =
    object ["type" .= ("reset" :: String)]
--}



data Error = GameSecretKeyInvalid
           | GameSecretKeyDuplicated
           | GameSecretKeyNotSpecified
           | GamePublicKeyInvalid
           | GamePublicKeyDuplicated
           | GamePublicKeyNotSpecified
           | GameCaptionInvalid
           | GameCaptionNotSpecified
           | GameFromSecretKeyNotFound
           | GameFromPublicKeyNotFound
           | ParticipantKeyInvalid
           | ParticipantKeyDuplicated
           | ParticipantKeyNotSpecified       -- unused
           | ParticipantFromSecretKeyNotFound -- unused
           deriving (Show,Typeable,Data)


instance ToJSON Error where
  toJSON (e) = simpleErrorJSON e

simpleErrorJSON :: Error -> AE.Value
simpleErrorJSON e = object ["success" .= False
                             ,"type"    .= (show $ toConstr e)
                             ]


n :: Int
n = 25


addGameIO :: Server -> GamePublicKey -> GameCaption -> IO (Either Error Game)
addGameIO server gpk caption
  | not $ isValidPublicKey gpk   = return $ Left GamePublicKeyInvalid
  | not $ isValidCaption caption = return $ Left GameCaptionInvalid
  | otherwise = do

    g <- R.newStdGen
    mugsk <- nextUUID
    case mugsk of
      Nothing   -> return $ Left GameSecretKeyInvalid
      Just ugsk -> STM.atomically $
                   addGame server (UUID.toString ugsk) gpk caption (B.newCandidate g n)
  where
    isValidPublicKey cand = (all (\c -> elem c ("abcdefghijklmnopqrstuvwxyz0123456789" :: String) ) cand)
                            && (0 < length cand && length cand <= 20)
    isValidCaption cand = 0 < length cand && length cand <= 20




addGame :: Server -> GameSecretKey -> GamePublicKey -> GameCaption -> [Int] -> STM.STM (Either Error Game)
addGame Server{..} gsk gpk caption ini = do

  keys   <- STM.readTVar serverGameKeys
  games <- STM.readTVar serverGames

  case () of
    _ | Map.member gsk keys  -> return $ Left GameSecretKeyDuplicated
      | Map.member gpk games -> return $ Left GamePublicKeyDuplicated
      | otherwise -> do
        game <- newGame gsk gpk caption ini
        STM.writeTVar serverGameKeys $ Map.insert gsk gpk keys
        STM.writeTVar serverGames   $ Map.insert gpk game games
        return $ Right game


getGameFromPublicKey :: Server -> GamePublicKey -> STM.STM (Maybe Game)
getGameFromPublicKey Server{..} gpk = 
  (Map.lookup gpk) <$> STM.readTVar serverGames
  

getGameFromSecretKey :: Server -> GameSecretKey -> STM.STM (Maybe Game)
getGameFromSecretKey server@Server{..} gsk = do
  mgpk <- (Map.lookup gsk) <$> STM.readTVar serverGameKeys
  case mgpk of
    Nothing  -> return $ Nothing
    Just gpk -> getGameFromPublicKey server gpk




addParticipantIO :: Game -> IO (Either Error Participant)
addParticipantIO game@Game{..} = do
  -- カードのクジを引く

  g <- R.newStdGen
  mupk <- nextUUID
  case mupk of
    Nothing  -> return $ Left ParticipantKeyInvalid
    Just upk -> STM.atomically $ do
      cand <- STM.readTVar gameCandidate
      addParticipant game (UUID.toString upk) (B.newCard g n cand)


addParticipant :: Game -> ParticipantKey -> B.Card -> STM.STM (Either Error Participant)
addParticipant Game{..} pk ini = do
  participants <- STM.readTVar gameParticipants
  if Map.member pk participants
    then return $ Left ParticipantKeyDuplicated
    else do
      participant <- newParticipant pk ini
      STM.writeTVar gameParticipants $ Map.insert pk participant participants
      return $ Right participant
              

getParticipant :: Game -> ParticipantKey -> STM.STM (Maybe Participant)
getParticipant Game{..} pk =
  (Map.lookup pk) <$> STM.readTVar gameParticipants






draw :: Game -> IO ()
draw Game{..} = do
  g <- R.newStdGen
  STM.atomically $ do
    st <- STM.readTVar gameState
    let r@(x,st'@(_,ss)) = Lot.draw g st
    STM.writeTVar gameState st'
    STM.writeTChan gameChan (MessageDraw x ss)


reset :: Game -> IO ()
reset Game{..} = do
  g <- R.newStdGen
  let cand = B.newCandidate g n
  STM.atomically $ do
    STM.writeTVar gameCandidate cand
    STM.writeTVar gameState (cand,[])

  parts <- STM.atomically $ STM.readTVar gameParticipants
  mapM_ (\p -> do
            g <- R.newStdGen
            STM.atomically $ do
              let card = B.newCard g n cand
              STM.writeTVar (participantCard p) card
              STM.writeTChan (participantChan p) (MessageCard card)
        ) $ Map.elems parts
    
  STM.atomically $ STM.writeTChan gameChan MessageReset





test :: IO ()
test = do

  rchan <- STM.newTChanIO
  
  server <- newServer

  egame1 <- addGameIO server "abcd1234" "テスト"
  game1 <- case egame1 of
    Left err -> throwIO $ ErrorCall "error: 1"
    Right game -> do
      forkIO $ gameLoop rchan game
      return game
      
  epart1 <- addParticipantIO game1
  part1 <- case epart1 of
    Left err -> throwIO $ ErrorCall "error: 2"
    Right part -> do
      forkIO $ partLoop rchan game1 part
      return part

  epart2 <- addParticipantIO game1
  part2 <- case epart2 of
    Left err -> throwIO $ ErrorCall "error: 3"
    Right part -> do
      forkIO $ partLoop rchan game1 part
      return part

  epart3 <- addParticipantIO game1
  part3 <- case epart3 of
    Left err -> throwIO $ ErrorCall "error: 4"
    Right part -> do
      forkIO $ partLoop rchan game1 part
      return part




  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  threadDelay $ 1000 * 1000

  reset game1
  threadDelay 100

  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1
  draw game1

  report rchan
  
  return ()



  where
    gameLoop :: STM.TChan String -> Game -> IO ()
    gameLoop rchan game@Game{..} = do
      chan <- STM.atomically $ STM.dupTChan gameChan
      loop chan
      where
        loop chan = do
--          msg <- STM.atomically $ STM.readTChan chan
--          STM.atomically $ STM.writeTChan rchan $ show msg
          STM.atomically $ do
            msg <- STM.readTChan chan
            STM.writeTChan rchan $ show msg
          threadDelay 100
          loop chan


    partLoop :: STM.TChan String -> Game -> Participant -> IO ()
    partLoop rchan game@Game{..} part@Participant{..} = do
      chan <- STM.atomically $ STM.dupTChan gameChan
      loop chan
      where
        loop chan = do
--          msg <- STM.atomically $ STM.readTChan chan
--          case msg of
--            MessageDraw x ss -> do
--              card <- STM.atomically $ STM.readTVar participantCard
--              let r = B.processCard card ss
--              STM.atomically $ do
--                STM.writeTChan rchan $ show $ sbl r
--                STM.writeTChan rchan $ show $ B.evalCard r
          STM.atomically $ do
            msg <-STM.readTChan chan
            case msg of
              MessageDraw x ss -> do
                card <- STM.readTVar participantCard
                let r = B.processCard card ss
                STM.writeTChan rchan $ show $ sbl r
                STM.writeTChan rchan $ show $ B.evalCard r

              MessageReset -> STM.writeTChan rchan $ show msg

          threadDelay 100
          loop chan

      
    report :: STM.TChan String -> IO()
    report c = loop
      where
        loop = do
          msg <- STM.atomically $ STM.readTChan c
          putStrLn msg
          threadDelay 100
          loop
          
    sbl = map (\b -> case b of
                  True -> 1
                  False -> 0)
