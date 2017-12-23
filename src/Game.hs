module Game where
  import MonteCarlo
  import Board
  import DataStructure
  import Board

  gameLoop :: Game -> IO Game
  gameLoop game = do
    column <- monteCarlo game
    let new_game = addPawn game column
    putStrLn $drawBoard new_game
    if (gameComplete new_game) /= 2
      then
        return (new_game)
      else
        gameLoop new_game


