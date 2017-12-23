{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DataStructure where
  import Data.Eq
  import GHC.Generics
  import Data.Map (Map)
  import qualified Data.Map as Map

  data Color = Yellow | Red deriving(Show, Eq, Generic)
  data Game = Game {
                     board::Map Int [Color],
                     turn::Int,
                     player::Color,
                     sizecolumn::Int,
                     sizeline::Int,
                     maxchain::Int,
                     lastpawn::(Color, Int, Int)
                     }deriving(Show, Eq, Generic)
  initBoard :: Map Int [Color]
  initBoard =
    (Map.fromList [(x, []) | x<-[0..6]])

  oppositePlayer color
    | color == Red = Yellow
    | otherwise = Red

  oppositeColor color
    | color == Red = Yellow
    | otherwise = Red

  getBoard :: Map Int [Color] -> Int -> [Color]
  getBoard m k =
    case Map.lookup k m of
      Nothing -> []
      Just n -> n

  updateBoard :: Map Int [Color] -> Int -> Color -> Map Int [Color]
  updateBoard m k v =
    Map.insert k (v:(getBoard m k)) m



