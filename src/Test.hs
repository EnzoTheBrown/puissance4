module Test where
  import Test.HUnit
  import Board
  import MonteCarlo
  import ImportGame

  testGameComplete filename result =
    TestCase ( do g <- importGameFromTXT filename
                  assertEqual "test game complete" (gameComplete g) result)

  testMonteCarlo filename column =
    TestCase ( do g <- importGameFromTXT filename
                  c <- monteCarlo g
                  assertEqual "test montecarlo" c column)


  test0 = testGameComplete "data/testDiagonalTop.txt" 1
  test1 = testGameComplete "data/testColumn.txt" 1
  test2 = testGameComplete "data/testDiagonalDown.txt" 1
  test3 = testGameComplete "data/ObviousLose.txt" (-1)
  test8 = testGameComplete "data/testObviousWin.txt" 1

  test4 = testMonteCarlo "data/testObviousLose.txt"  0
  test5 = testMonteCarlo "data/testObviousWin.txt"   1
  test6 = testMonteCarlo "data/testObviousWin2.txt"  2
  test7 = testMonteCarlo "data/testObviousLose2.txt" 1

  tests = TestList [
    TestLabel "test"  test0,
    TestLabel "test"  test1,
    TestLabel "test"  test2,
    TestLabel "test"  test3,
    TestLabel "test"  test8,
    TestLabel "test mc 1"  test4,
    TestLabel "test mc 2"  test5,
    TestLabel "test mc 3"  test6,
    TestLabel "test mc 4"  test7
                       ]

  run = do
    runTestTT tests


