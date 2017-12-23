module MonteCarlo where
  import Board
  import DataStructure
  import System.Random
  import Data.Map (Map)
  import qualified Data.Map as Map
  import Data.Ord
  import Data.List

  argmin f l
      | null l = error "Given list is empty"
      | otherwise = minimumBy (comparing f) l

  argmax f l
      | null l = error "Given list is empty"
      | otherwise = maximumBy (comparing f) l

  getMap :: Map Int Int -> Int -> Int
  getMap m k =
    case Map.lookup k m of
      Nothing -> 0
      Just n -> n

  updateMap :: Map Int Int -> Int -> Int -> Map Int Int
  updateMap m k v
    | k == -1 = m
    | otherwise = Map.insert k ((getMap m k) + v) m

  randomMove :: [Int] -> IO Int
  randomMove moves = do
    col <- randomIO :: IO Int
    return (moves!!(col `mod` (length moves)))

  randomGame :: Game -> IO Game
  randomGame game = do
    column <- randomMove (legalMoves game)
    let new_game = addPawn game column
    if (gameComplete new_game) /= 2
      then
        return (new_game)
      else
        randomGame new_game
--
--  _QFunction :: Game -> Float
--  _QFunction game candidateGame =
--    coef*(100*)
--
  monteCarlo :: Game -> IO Int
  monteCarlo game = do
    let moves = legalMoves game
    -- generate 1000 game for each legal move
    games <- mapM (\y -> mapM (\x -> randomGame (addPawn game x)) [0..1000]) moves
    -- (putStrLn . show . maximum . concat)  (map (\x -> map (\y -> turn y) x) games)
    -- (putStrLn . concat . (map (\x -> drawBoard x ++ "\n")) . (filter (\x -> (gameComplete x) == 0))) $concat games
    let score = Map.fromList $zip moves $map(\x->sum(map(\y->(100*gameComplete y)-(turn y)) x)) games
    putStrLn $show score
    let res = (fst .((if (player game) == Yellow then argmax else argmin) (\x -> (snd x)))) $Map.toList score
    return (res)


