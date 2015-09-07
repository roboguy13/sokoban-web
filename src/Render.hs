module Render
  (blankRender)
  where

import           Prelude hiding (lines)

import           Control.Monad.State.Strict
import           Data.Text

import           Graphics.Blank

blankRender :: DeviceContext -> Double -> (Double, Double) -> Text -> IO ()
blankRender context fontSize startPoint t = do
  send context $ do
    font . pack $ show (ceiling fontSize) ++ "pt Courier"
    render fontSize startPoint t

render :: Double -> (Double, Double) -> Text -> Canvas ()
render fontSize (startingX, startingY) t
  = evalStateT (mapM_ renderLine $ lines t) startingY
  where
    renderLine :: Text -> StateT Double Canvas ()
    renderLine line = do
      y <- get
      lift $ fillText (line, startingX, y)
      modify (+fontSize)

