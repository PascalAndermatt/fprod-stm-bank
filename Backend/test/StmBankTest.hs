module Main (main) where
import StmBank ( unlines' )
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Test.Tasty.HUnit ( testCase, (@?=) )


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "unlines' empty list" $
      unlines' [] @?= "",

    testCase "unlines' singleton list" $
      unlines' ["hello"] @?= "hello",

    testCase "unlines' larger list" $
      unlines' ["hello","world"] @?= "hello\nworld"  
  ]

