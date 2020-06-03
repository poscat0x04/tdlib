module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan.Unagi
import Data.Text (Text)
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
import Text.Pretty.Simple

-- ** Helper functions

mm ::
  Testable a =>
  InChan Update ->
  PropertyM (Sem '[TDLib, Embed IO]) a ->
  Property
mm ichan = monadic (r ichan)

r ichan m = (monadicIO . run . runM . runTDLibEventLoop 0.1 ichan) (setVerbosity Fatal >> m)

updateConsumer :: OutChan Update -> IO ()
updateConsumer chan = go
  where
    go = do
      u <- readChan chan
      go

-- ** Main

main :: IO ()
main = do
  (ichan, ochan) <- newChan
  forkIO $ updateConsumer ochan
  quickCheck $ squareInt ichan
  quickCheck $ callEmpty ichan
  quickCheck $ callString ichan

-- ** Properties

squareInt :: InChan Update -> I32 -> Property
squareInt ichan i = mm ichan $ do
  r <- run $ testSquareInt (TestSquareInt i)
  assert $ Inr (TestInt $ i ^ 2) == r

callEmpty :: InChan Update -> Property
callEmpty ichan = mm ichan $ do
  ok <- run $ testCallEmpty TestCallEmpty
  assert $ Inr Ok == ok

callString :: InChan Update -> Text -> Property
callString ichan t = mm ichan $ do
  r <- run $ testCallString (TestCallString t)
  assert $ Inr (TestString t) == r
