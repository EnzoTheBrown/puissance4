module Board where
  import DataStructure
  import qualified Data.Map as Map

  initGame =
    Game initBoard
         0 Yellow
         6 7 4
         (Red, -1, -1)

  addPawn :: Game -> Int -> Game
  addPawn game column =
    Game (updateBoard (board game) column (player game))
         (turn game + 1)
         (oppositeColor $player game)
         (sizecolumn game)
         (sizeline game)
         (maxchain game)
         (player game, column, length (getBoard (board game), column) - 1)

  gameComplete :: Game -> Int
  gameComplete game
    | m == 4 && pp == Red = -1
    | m == 4 && pp == Yellow = 1
    | legalMoves game == [] = 0
    | otherwise = 2
    where b = board game
          p = lastpawn game
          c = [chain b p x y | x<-[-1..1], y<-[-1..1], (x, y)/=(0,0)]
          m = maximum [(c!!0 + c!!7 - 1),(c!!1 + c!!6 - 1),(c!!2 + c!!5 - 1),(c!!3 + c!!4 - 1)]
          pp = (oppositePlayer $ player game)

  chain bd (a, b, c) x y
    | (length column - 1) >= c && c >= 0 && a == column!!c = 1 + (chain bd (a, b + x, c + y) x y)
    | otherwise = 0
    where column = getBoard bd b



  legalMoves :: Game -> [Int]
  legalMoves game =
    filter (\x-> x< sizeline game) $map (\(x, y) -> x) (filter (\(x, y) -> length y < (sizecolumn game)) (Map.toList $board game))

  drawBoard :: Game -> String
  drawBoard game =
    concat [[if y==Yellow then 'X' else '0' | y <- reverse (snd x) ]++"\n" | x<-b]
    where b = Map.toList $board game


  -- the pawns of each columns and the a | to separate the columns
--  resumeBoard :: Game -> String
--  resumeBoard game =
--    concat [map (\x -> if x == Red then '0' else 'X') $getBoard b x ++ '|' |x <- [0..(sizecolumn game)]]
--    where b = board game
--
