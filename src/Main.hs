module Main where
  import Board
  import ImportGame
  import Game
  import Test

  main :: IO ()
  main = do
    -- g <- importGameFromTXT "data/dummy.txt"
    -- putStrLn $show $gameComplete $ g
    run
    -- x <- gameLoop initGame
    putStrLn "end"

