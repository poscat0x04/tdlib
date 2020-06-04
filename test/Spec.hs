module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import Data.ByteString (ByteString)
import Data.ByteString.Base64.Type
import Data.Text.Arbitrary
import Polysemy hiding (run)
import TDLib.Effect
import TDLib.EventLoop
import TDLib.Generated.FunArgs
import TDLib.Generated.Functions
import TDLib.Generated.Types hiding (Text)
import TDLib.Types.Common
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- ** Helper functions

mm ::
  Testable a =>
  InChan Update ->
  PropertyM (Sem '[TDLib, Embed IO]) a ->
  Property
mm ichan = monadic (rr ichan)

rr ::
  Testable a =>
  InChan Update ->
  Sem '[TDLib, Embed IO] a ->
  Property
rr ichan m = (monadicIO . run . runM . runTDLibEventLoop 0.1 ichan) (setVerbosity Fatal >> m)

updateConsumer :: OutChan Update -> IO ()
updateConsumer chan = go
  where
    go = do
      readChan chan
      go

-- ** Main

main :: IO ()
main = do
  (ichan, ochan) <- newChan
  forkIO $ updateConsumer ochan
  quickCheck $ squareInt ichan
  quickCheck $ callEmpty ichan
  quickCheck $ callString ichan
  quickCheck $ callVecInt ichan
  quickCheck $ callVecIntObject ichan
  quickCheck $ callVecString ichan
  quickCheck $ callVecStringObject ichan

-- ** Properties

squareInt :: InChan Update -> I32 -> Property
squareInt ichan i = mm ichan $ do
  r <- run $ testSquareInt i
  assert $ Inr (TestInt $ (i :: Int) ^ (2 :: Int)) == r

callEmpty :: InChan Update -> Property
callEmpty ichan = mm ichan $ do
  ok <- run $ testCallEmpty
  assert $ Inr Ok == ok

callString :: InChan Update -> Text -> Property
callString ichan t = mm ichan $ do
  r <- run $ testCallString t
  assert $ Inr (TestString t) == r

callVecInt :: InChan Update -> [I32] -> Property
callVecInt ichan vec = mm ichan $ do
  r <- run $ testCallVectorInt vec
  assert $ Inr (TestVectorInt vec) == r

callVecIntObject :: InChan Update -> [I32] -> Property
callVecIntObject ichan vec = mm ichan $ do
  let v' = fmap TestInt vec
  r <- run $ testCallVectorIntObject v'
  assert $ Inr (TestVectorIntObject v') == r

callVecString :: InChan Update -> [Text] -> Property
callVecString ichan vec = mm ichan $ do
  r <- run $ testCallVectorString vec
  assert $ Inr (TestVectorString vec) == r

callVecStringObject :: InChan Update -> [Text] -> Property
callVecStringObject ichan vec = mm ichan $ do
  let v' = fmap TestString vec
  r <- run $ testCallVectorStringObject v'
  assert $ Inr (TestVectorStringObject v') == r

callBytes :: InChan Update -> ByteString -> Property
callBytes ichan bs = mm ichan $ do
  let bs' = makeByteString64 bs
  r <- run $ testCallBytes bs'
  assert $ Inr (TestBytes bs') == r
