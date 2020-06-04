module Main where

import Data.ByteString (ByteString)
import Data.ByteString.Base64.Type
import Data.Text.Arbitrary
import Polysemy hiding (run)
import TDLib.Effect
import TDLib.EventLoop
import TDLib.Generated.Functions
import TDLib.Generated.Types hiding (Text)
import TDLib.Types.Common
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- ** Helper functions

mm ::
  Testable a =>
  PropertyM (Sem '[TDLib, Embed IO]) a ->
  Property
mm = monadic rr

rr ::
  Testable a =>
  Sem '[TDLib, Embed IO] a ->
  Property
rr m = (monadicIO . run . runM . runTDLibEventLoop 0.1 (const (pure ()))) (setVerbosity Fatal >> m)

-- ** Main

main :: IO ()
main = do
  quickCheck $ squareInt
  quickCheck $ callEmpty
  quickCheck $ callString
  quickCheck $ callVecInt
  quickCheck $ callVecIntObject
  quickCheck $ callVecString
  quickCheck $ callVecStringObject

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
