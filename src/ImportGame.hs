module ImportGame where
  import Board
  import DataStructure

  playGame :: Game -> String -> Game
  playGame game [] = game
  playGame game (x:xs)
    | x `elem` ['0'..'6'] = playGame (addPawn game $read [x]) xs
    | otherwise = game

  importGameFromTXT :: String -> IO Game
  importGameFromTXT filename = do
    moves <- readFile filename
    return (playGame initGame moves)

