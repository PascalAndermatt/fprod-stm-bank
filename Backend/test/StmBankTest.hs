{-# LANGUAGE OverloadedStrings #-}

{-|
    This module contains the test suite for the bank application 
-}

module Main (main) where

import Test.Tasty ( defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.Providers
import Test.Tasty.QuickCheck
import qualified StmBank (StmResult (..), maybeResult, isOwnerInvalid, createIban)


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ isOwnerInvalidUnitTests, 
                            createIbanUnitTests, 
                            createIbanPropertyUnitTests, 
                            maybeResultUnitTests]

isOwnerInvalidUnitTests :: TestTree
isOwnerInvalidUnitTests = testGroup "is owner valid - unit tests"
  [ testCase "owner with non-alpha chars" $
      StmBank.isOwnerInvalid "34dfgdos" @?= True,

    testCase "owner with alpha chars" $
      StmBank.isOwnerInvalid "peter" @?= False,

    testCase "owner with arbitrary alpha chars" $
      StmBank.isOwnerInvalid "sjdfsdsjdgsighsgurig" @?= False,

    testCase "owner with arbitrary non-alpha chars" $
      StmBank.isOwnerInvalid ".sgdg-?" @?= True
  ]

createIbanUnitTests :: TestTree
createIbanUnitTests = testGroup "create iban - unit tests"
  [ testCase "simple owner with salt" $
      StmBank.createIban 1234 "Peter" @?= "CH4756589931287272597PET"
  ]

createIbanPropertyUnitTests :: TestTree
createIbanPropertyUnitTests = testGroup "create iban property-test - unit tests"
  [
    testProperty "random" $ 
      \randomSalt owner ->
        length owner >= 3 ==>
          length (StmBank.createIban randomSalt owner)  == length (StmBank.createIban randomSalt owner)
  ]

maybeResultUnitTests :: TestTree
maybeResultUnitTests = testGroup "maybe result - unit tests"
  [ testCase "good case" $ 
      StmBank.maybeResult [StmBank.Result 12, StmBank.Result 24] @?= Just [12, 24],

    testCase "bad case" $ 
      StmBank.maybeResult [StmBank.Result 12, StmBank.Error "error"] @?= Nothing
  ]
