{-# LANGUAGE OverloadedStrings #-}
module Game (
  ConnectionKey,
  Caption,
  ParticipantKey,
  Game,
  Error(..),
  new,
  caption,
  dealerConnections,
  addDealerConnection,
  delDealerConnection,
  reset,
  draw,
  hasParticipant,
  addParticipant,
  allParticipantConnections,
  participantConnections,
  addParticipantConnection,
  delParticipantConnection,
  ) where

import qualified Network.WebSockets as WS
import qualified Data.Map.Strict as Map
import Data.Maybe(isJust,catMaybes)

import Data.Aeson.Types(ToJSON,FromJSON,Value(Object),toJSON,parseJSON,object,(.=),(.:))
import Data.Functor ((<$>))
import Control.Applicative (pure,(<*>))
import Control.Monad(mzero)

import Data.List(concatMap)

import qualified System.Random as R
import qualified Lottery as Lot
import qualified Bingo as B




type ConnectionKey = Int
type Caption = String
type ParticipantKey = String


data ParticipantImp = ParticipantImp {
  dParticipantConnections :: [(ConnectionKey,WS.Connection)],
  dParticipantCard :: B.Card
  }
                
data GameImp = GameImp {
  dGameCaption :: Caption,
  dGameState :: B.State,
  dGameCurrentConnectionKey :: ConnectionKey,
  dGameDealerConnections :: [(ConnectionKey,WS.Connection)],
  dGameParticipants :: Map.Map ParticipantKey ParticipantImp
  }

newtype Game = Game GameImp

data Error = CaptionInvalid
           | ParticipantKeyDuplicated
           | ParticipantNotFound
           deriving(Show)




new :: R.RandomGen g => g -> Caption -> Either Error Game
new g c
  | not $ isValidCaption c = Left CaptionInvalid
  | otherwise              = Right $ Game $ GameImp c (cs,[]) 0 [] Map.empty
  where
    isValidCaption cand = 0 < length cand && length cand <= 20
    cs = B.newCandidate g 25

caption :: Game -> Caption
caption (Game gi) = dGameCaption gi


dealerConnections :: Game -> [WS.Connection]
dealerConnections (Game gi) = map snd $ dGameDealerConnections gi


addDealerConnection :: Game -> WS.Connection -> (Game,ConnectionKey)
addDealerConnection (Game gi) conn =
  (Game $ gi { dGameDealerConnections = m, dGameCurrentConnectionKey = k  }, k) 
  where
    k = (dGameCurrentConnectionKey gi) + 1
    m = (k,conn) : (dGameDealerConnections gi)


delDealerConnection :: Game -> ConnectionKey -> Game
delDealerConnection (Game gi) k =
  Game $ gi { dGameDealerConnections = m } 
  where
    m = filter ((/=k) . fst) (dGameDealerConnections gi)




reset :: R.RandomGen g => g -> Game -> Game
reset g (Game gi) = Game $ gi { dGameState = (cs,[]), dGameParticipants = ps }
  where
    ps = Map.map (\r -> r { dParticipantCard = ([],[]) }) (dGameParticipants gi)
    cs = B.newCandidate g 25
         

draw :: R.RandomGen g => g -> Game -> (Game,Int,[Int])
draw g (Game gi) = (Game $ gi { dGameState = st }, x, ss)
  where
    (x,st@(_,ss)) = Lot.draw g $ dGameState gi

    


participant' :: Game -> ParticipantKey -> Either Error ParticipantImp
participant' (Game gi) k = case Map.lookup k (dGameParticipants gi) of
  Nothing -> Left ParticipantNotFound
  Just r -> Right r


insertParticipant' :: Game -> ParticipantKey -> ParticipantImp -> Game
insertParticipant' (Game gi) k p = Game $ gi { dGameParticipants = ps }
  where
    ps = Map.insert k p (dGameParticipants gi)


updateParticipant' :: Game -> ParticipantKey -> (ParticipantImp -> ParticipantImp) -> Either Error Game
updateParticipant' g k f = 
  (\r -> insertParticipant' g k (f r)) <$> participant' g k 




hasParticipant :: Game -> ParticipantKey -> Bool
hasParticipant (Game gi) k = Map.member k (dGameParticipants gi)


addParticipant :: Game -> ParticipantKey -> Either Error Game
addParticipant g k = case hasParticipant g k of
  True  -> Left ParticipantKeyDuplicated
  False -> Right $ insertParticipant' g k (ParticipantImp [] ([],[]))




allParticipantConnections :: Game -> [WS.Connection]
allParticipantConnections (Game gi) = concatMap
                           ((map snd) . dParticipantConnections) $ Map.elems (dGameParticipants gi)

participantConnections :: Game -> ParticipantKey -> Either Error [WS.Connection]
participantConnections g k = ((map snd) . dParticipantConnections) <$> participant' g k


addParticipantConnection :: Game -> ParticipantKey -> WS.Connection -> Either Error (Game, ConnectionKey)
addParticipantConnection g@(Game gi) k conn = do
  p <- participant' g k
  let ck = (dGameCurrentConnectionKey gi) + 1
  let m' = (ck,conn) : (dParticipantConnections p)
  let g' = insertParticipant' g k (p { dParticipantConnections = m' })
  return (g',ck)



delParticipantConnection :: Game -> ParticipantKey -> ConnectionKey -> Either Error Game
delParticipantConnection g k ck = 
  updateParticipant' g k (\p -> p { dParticipantConnections = filter ((/=ck) . fst) (dParticipantConnections p) })




participantCard :: Game -> ParticipantKey -> Either Error B.Card
participantCard g k = dParticipantCard <$> participant' g k






--instance ToJSON ReporterImp where
--  toJSON (ReporterImp _ aha) =
--    object ["aha" .= aha]
--
--instance FromJSON ReporterImp where
--  parseJSON (Object v) = ReporterImp <$> pure AutoMap.empty <*> v .: "aha"
--  parseJSON _ = mzero
--
--
--instance ToJSON BoardImp where
--  toJSON (BoardImp caption _ reporters) =
--    object ["caption"   .= caption
--           ,"reporters" .= reporters
--           ]
--    
--instance FromJSON BoardImp where
--  parseJSON (Object v) = BoardImp <$> v .: "caption" <*> pure AutoMap.empty <*> v .: "reporters"
--  parseJSON _ = mzero
--    
--
--instance ToJSON Board where
--  toJSON (Board bi) = toJSON bi
--
--instance FromJSON Board where
--  parseJSON o = Board <$> parseJSON o



