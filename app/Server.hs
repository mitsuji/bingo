{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

module Server (
   ParticipantKey
  ,Participant (..)
  ,newParticipant
  ,GameSecretKey
  ,GamePublicKey
  ,GameCaption
  ,Game (..)
  ,newGame
  ,Server (..)
  ,newServer
  ,Message (..)
  ,Error (..)
  ,addGameIO
  ,getGameFromPublicKey
  ,getGameFromSecretKey
  ,addParticipantIO
  ,getParticipant
  ,draw
  ,reset
) where


import Data.Data (Data,toConstr)
import Control.Applicative ((<$>),(<*>))
import Control.Monad (foldM_,when)
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


data Message = MessageResetGame
             | MessageResetParticipant B.Card
             | MessageDrawGame Int [Int]
             | MessageDrawParticipant Int [Int]  [Bool] B.CardStatus
             deriving (Show)

instance ToJSON Message where
  toJSON (MessageResetGame) =
    object ["type" .= ("reset" :: String)]

  toJSON (MessageResetParticipant cd) =
    object ["type"    .= ("reset" :: String)
           ,"content" .= object ["card" .= cd]
           ]

  toJSON (MessageDrawGame x ss) =
    object ["type"    .= ("draw" :: String)
           ,"content" .= object ["item"     .= x
                                ,"selected" .= ss
                                ]
           ]

  toJSON (MessageDrawParticipant x ss ev st) =
    object ["type"    .= ("draw" :: String)
           ,"content" .= object ["game_item"     .= x
                                ,"game_selected" .= ss
                                ,"eval"          .= ev
                                ,"state"         .= st
                                ]
           ]


instance ToJSON B.CardStatus where
  toJSON (B.Bingo) =
    object ["type" .= ("bingo" :: String)]
    
  toJSON (B.Blank) =
    object ["type" .= ("blank" :: String)]

  toJSON (B.Lizhi i) =
    object ["type"    .= ("lizhi" :: String)
           ,"content" .= object ["num" .= i]
           ]




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
                   addGame server (UUID.toString ugsk) gpk caption (fst (B.newCandidate g n))
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
      addParticipant game (UUID.toString upk) (fst (B.newCard g n cand))


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




draw :: R.RandomGen g => g -> Game -> STM.STM ()
draw g Game{..} = do
    st@(c,_) <- STM.readTVar gameState
    when (c/=[]) $ do
      let r@(x,st'@(_,ss)) = Lot.draw g st
      STM.writeTVar gameState st'
      STM.writeTChan gameChan (MessageDrawGame x ss)
      parts <- STM.readTVar gameParticipants
      mapM_ (\p -> do
                card <- STM.readTVar (participantCard p)
                let r = B.processCard card ss
                let s = B.evalCard r
                STM.writeTChan (participantChan p) (MessageDrawParticipant x ss r s)
            ) $ Map.elems parts


reset :: R.RandomGen g => g -> Game -> STM.STM ()
reset g Game{..} = do
    let (cand,g') = B.newCandidate g n
    STM.writeTVar gameCandidate cand
    STM.writeTVar gameState (cand,[])
    STM.writeTChan gameChan MessageResetGame
    parts <- STM.readTVar gameParticipants
    foldM_ (\g'' p -> do
               let (card,g''') = B.newCard g'' n cand
               STM.writeTVar (participantCard p) card
               STM.writeTChan (participantChan p) (MessageResetParticipant card)
               return g'''
           ) g' $ Map.elems parts

