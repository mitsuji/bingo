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


type ParticipantKey = String

data Participant = Participant
  { participantKey  :: ParticipantKey
  , participantCard :: STM.TVar B.Card
  , participantChan :: STM.TChan Message
  }

newParticipant :: ParticipantKey -> B.Card -> STM.STM Participant
newParticipant pk ini = do
  card  <- STM.newTVar ini
  chan  <- STM.newBroadcastTChan 
  return Participant { participantKey  = pk
                     , participantCard = card
                     , participantChan = chan
                     }


type GameSecretKey = String
type GamePublicKey = String
type GameCaption   = String

data Game = Game
  { gameSecretKey    :: GameSecretKey
  , gamePublicKey    :: GamePublicKey
  , gameCaption      :: GameCaption
  , gameState        :: STM.TVar B.State
  , gameParticipants :: STM.TVar (Map.Map ParticipantKey Participant)
  , gameChan         :: STM.TChan Message
  }

newGame :: GameSecretKey -> GamePublicKey -> GameCaption -> [Int] -> STM.STM Game
newGame gsk gpk caption ini = do
  state        <- STM.newTVar (ini,[])
  participants <- STM.newTVar Map.empty
  chan         <- STM.newBroadcastTChan
  return Game { gameSecretKey    = gsk
              , gamePublicKey    = gpk
              , gameCaption      = caption
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




{--
data Response = ResponseBoard BoardSecretKey BoardPublicKey BoardCaption
              | ResponseReset
              deriving (Show)

instance ToJSON Response where
  toJSON (ResponseBoard sk pk ca) =
    object ["success" .= True
           ,"type"    .= ("board" :: String)
           ,"content" .= object ["secret_key" .= sk
                                ,"public_key" .= pk
                                ,"caption"    .= ca
                                ]
           ]
  toJSON (ResponseReset) =
    object ["success" .= True
           ,"type"    .= ("reset" :: String)
           ,"content" .= ("ok" :: String)
           ]
--}


data Message = MessageGame GamePublicKey GameCaption
             | MessageParticipant ParticipantKey GamePublicKey GameCaption
             | MessageReset
             deriving (Show)
                      
instance ToJSON Message where
  toJSON (MessageGame pk ca) =
    object ["type"    .= ("game" :: String)
           ,"content" .= object ["public_key" .= pk
                                ,"caption"    .= ca
                                ]
           ]
  toJSON (MessageParticipant rk bpk ca) =
    object ["type"    .= ("participant" :: String)
           ,"content" .= object ["reporter_key"     .= rk
                                ,"game_public_key" .= bpk
                                ,"game_caption"    .= ca
                                ]
           ]
  toJSON (MessageReset) =
    object ["type" .= ("reset" :: String)]




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





addGameIO :: Server -> GamePublicKey -> GameCaption -> IO (Either Error Game)
addGameIO server gpk caption
  | not $ isValidPublicKey gpk   = return $ Left GamePublicKeyInvalid
  | not $ isValidCaption caption = return $ Left GameCaptionInvalid
  | otherwise = do
    -- 分母のクジを引く
    mugsk <- nextUUID
    case mugsk of
      Nothing   -> return $ Left GameSecretKeyInvalid
      Just ugsk -> STM.atomically $
                   addGame server (UUID.toString ugsk) gpk caption []
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
addParticipantIO game = do
  -- カードのクジを引く
  mupk <- nextUUID
  case mupk of
    Nothing  -> return $ Left ParticipantKeyInvalid
    Just upk -> STM.atomically $
                addParticipant game (UUID.toString upk) ([],[])


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




{--
resetBoard :: Board -> STM.STM ()
resetBoard Board{..} = do
  reporters <- STM.readTVar boardReporters

  -- reset all
  STM.writeTVar boardAha 0
  mapM_ (\r -> STM.writeTVar (reporterAha r) 0) (Map.elems reporters)

  -- reset message
  STM.writeTChan boardChan MessageReset
  



aha :: Board -> ReporterKey -> STM.STM ()
aha board rk = do
  mreporter <- getReporter board rk
  case mreporter of
    Nothing -> return ()
    Just reporter -> do
      ahaBoard board
      ahaReporter reporter

ahaReporter :: Reporter -> STM.STM ()
ahaReporter Reporter{..} = do
  aha <- STM.readTVar reporterAha
  let aha' = aha +1
  STM.writeTVar reporterAha aha'
  STM.writeTChan reporterChan $ MessageAha aha'
  
ahaBoard :: Board -> STM.STM ()
ahaBoard Board{..} = do
  aha <- STM.readTVar boardAha
  let aha' = aha +1
  STM.writeTVar boardAha aha'
  STM.writeTChan boardChan $ MessageTotalAha aha'
  
--}

