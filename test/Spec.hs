{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Control.Concurrent
import Data.ByteString (ByteString)
import Data.ByteString.Base64.Type
import Data.Foldable
import Data.Text.Arbitrary
import Polysemy hiding (run)
import Polysemy.Resource
import TDLib.Effect
import TDLib.EventLoop
import TDLib.Generated.Functions
import TDLib.Generated.Types hiding (Text)
import TDLib.Types.Common
import Test.QuickCheck
import Test.QuickCheck.Instances.ByteString ()
import Test.QuickCheck.Monadic

-- ** Helper functions

mm ::
  Testable a =>
  PropertyM (Sem _) a ->
  Property
mm = monadic rr

rr ::
  Testable a =>
  Sem _ a ->
  Property
rr m = (monadicIO . run . runFinal . runTDLibEventLoop 1 (const (pure ()))) (setVerbosity Fatal >> m)

-- ** Main

main :: IO ()
main = traverse_ check checkedProperties

data Test = forall a. Testable a => Test a

check :: Test -> IO ()
check (Test t) = quickCheck t

checkedProperties :: [Test]
checkedProperties =
  [ Test squareInt,
    Test callEmpty,
    Test callString,
    Test callVecInt,
    Test callVecIntObject,
    Test callVecString,
    Test callBytes
  ]

-- ** Properties

squareInt :: I32 -> Property
squareInt i = mm $ do
  r <- run $ testSquareInt i
  assert $ Inr (TestInt $ (i :: Int) ^ (2 :: Int)) == r

callEmpty :: Property
callEmpty = mm $ do
  ok <- run $ testCallEmpty
  assert $ Inr Ok == ok

callString :: Text -> Property
callString t = mm $ do
  r <- run $ testCallString t
  assert $ Inr (TestString t) == r

callVecInt :: [I32] -> Property
callVecInt vec = mm $ do
  r <- run $ testCallVectorInt vec
  assert $ Inr (TestVectorInt vec) == r

callVecIntObject :: [I32] -> Property
callVecIntObject vec = mm $ do
  let v' = fmap TestInt vec
  r <- run $ testCallVectorIntObject v'
  assert $ Inr (TestVectorIntObject v') == r

callVecString :: [Text] -> Property
callVecString vec = mm $ do
  r <- run $ testCallVectorString vec
  assert $ Inr (TestVectorString vec) == r

callVecStringObject :: [Text] -> Property
callVecStringObject vec = mm $ do
  let v' = fmap TestString vec
  r <- run $ testCallVectorStringObject v'
  assert $ Inr (TestVectorStringObject v') == r

callBytes :: ByteString -> Property
callBytes bs = mm $ do
  let bs' = makeByteString64 bs
  r <- run $ testCallBytes bs'
  assert $ Inr (TestBytes bs') == r
