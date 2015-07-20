{-# LANGUAGE OverloadedStrings #-}
module ServerState (
  GameSecretKey,
  GamePublicKey,
  ServerState,
  Error(..),
  new,
  addGame,
  gameFromPublicKey,
  publicKeyFromSecretKey,
--  Item,
--  dump,
--  restore,
  ) where

import qualified Data.Map.Strict as Map
import Control.Concurrent (MVar,newMVar,readMVar)

import Data.Aeson.Types(ToJSON,FromJSON,Value(Object),toJSON,parseJSON,object,(.=),(.:))
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Control.Monad(mzero)
import Data.Maybe(fromJust)

import Control.Monad(forM,foldM)
import Control.Exception(throwIO)
import System.IO.Error(userError)

import qualified Game as Game




type GameSecretKey = String
type GamePublicKey = String

data ServerStateImp = ServerStateImp {
  secrets :: Map.Map GameSecretKey GamePublicKey,
  boards :: Map.Map GamePublicKey (MVar Game.Game)
  }

newtype ServerState = ServerState ServerStateImp

data Error = GameSecretKeyDuplicated
           | GamePublicKeyDuplicated
           | GameSecretKeyInvalid
           | GamePublicKeyInvalid
           deriving(Show)




new :: ServerState
new = ServerState $ ServerStateImp Map.empty Map.empty


addGame :: ServerState -> GameSecretKey -> GamePublicKey -> MVar Game.Game -> Either Error ServerState
addGame (ServerState ssi) bsk bpk vboard
  | not $ isValidSecretKey bsk = Left GameSecretKeyInvalid
  | not $ isValidPublicKey bpk = Left GamePublicKeyInvalid
  | Map.member bsk srs = Left GameSecretKeyDuplicated
  | Map.member bpk bds = Left GamePublicKeyDuplicated
  | otherwise = Right $ ServerState $ ServerStateImp srs' bds'
  where
    isValidSecretKey cand = True -- must be UUID
    isValidPublicKey cand = (all (\c -> elem c "abcdefghijklmnopqrstuvwxyz0123456789") cand)
                            && (0 < length cand && length cand <= 20)
    srs' = Map.insert bsk bpk srs
    bds' = Map.insert bpk vboard bds
    srs = secrets ssi
    bds = boards ssi


gameFromPublicKey :: ServerState -> GamePublicKey -> Maybe (MVar Game.Game)
gameFromPublicKey (ServerState ssi) bpk = Map.lookup bpk (boards ssi)


publicKeyFromSecretKey :: ServerState -> GameSecretKey -> Maybe GamePublicKey
publicKeyFromSecretKey (ServerState ssi) bsk = Map.lookup bsk (secrets ssi)




--data Item = Item BoardSecretKey BoardPublicKey Board.Board
--
--instance ToJSON Item where
--  toJSON (Item sk pk b) =
--    object ["secret_key" .= sk
--           ,"public_key" .= pk
--           ,"board"      .= b
--           ]
--
--instance FromJSON Item where
--  parseJSON (Object v) = Item <$> v .: "secret_key" <*> v .: "public_key" <*> v.: "board"
--  parseJSON _ = mzero
--
--
--dump :: ServerState -> IO [Item]
--dump (ServerState ssi) = forM srs $ \(sk, pk) -> do
--  board <- readMVar $ fromJust $ Map.lookup pk bds
--  return $ Item sk pk board
--  where
--    srs = Map.toList $ secrets ssi
--    bds = boards ssi
--
--restore :: [Item] -> IO ServerState
--restore items = foldM restore' ServerState.new items
--  where
--    restore' :: ServerState -> Item -> IO ServerState
--    restore' state (Item bsk bpk board) = do
--      vboard <- newMVar board
--      case addBoard state bsk bpk vboard of
--        Left err -> throwIO $ userError $ show err
--        Right state' -> return state'
--
--
