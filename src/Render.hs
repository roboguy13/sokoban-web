module Render
  (renderGame
  ,blankRender
  ,wonMessage
  )
  where

import           Prelude hiding (lines)

import           Board

import           Control.Monad.State.Strict
import           Data.Text (Text, pack)
import qualified Data.Text as Text

import           Data.List

import           Graphics.Blank

movementInstructions :: String
movementInstructions
  = concat
  $ intersperse "\n"
      ["Controls:"
      ,""
      ,"   k  "
      ," h   l"
      ,"   j  "
      ,""
      ,"Move the boxes onto the goals:"
      ,"  o -> ."
      ]

wonMessage :: Text
wonMessage
  = pack
  . concat
  $ intersperse "\n"
      ["____    ____  ______    __    __     ____    __    ____  ______   .__   __.  __  "
      ,"\\   \\  /   / /  __  \\  |  |  |  |    \\   \\  /  \\  /   / /  __  \\  |  \\ |  | |  | "
      ," \\   \\/   / |  |  |  | |  |  |  |     \\   \\/    \\/   / |  |  |  | |   \\|  | |  | "
      ,"  \\_    _/  |  |  |  | |  |  |  |      \\            /  |  |  |  | |  . `  | |  | "
      ,"    |  |    |  `--'  | |  `--'  |       \\    /\\    /   |  `--'  | |  |\\   | |__| "
      ,"    |__|     \\______/   \\______/         \\__/  \\__/     \\______/  |__| \\__| (__) "
      ]

renderGame :: DeviceContext -> Double -> (Int, Int) -> (Double, Double) -> Board -> IO ()
renderGame context fontSize (width, height) startPoint@(x, y) board = do
  blankRender context fontSize startPoint . pack $ renderBoard width height board

  let moveInstrWidth = fromIntegral . maximum . map length $ lines movementInstructions
  blankRender context fontSize (((fromIntegral width * fontSize) - x - moveInstrWidth) / 2
                               , y + (fromIntegral height * fontSize) + (fontSize * 2))
            $ pack movementInstructions

blankRender :: DeviceContext -> Double -> (Double, Double) -> Text -> IO ()
blankRender context fontSize startPoint t = do
  send context $ do
    font . pack $ show (ceiling fontSize) ++ "pt Courier"
    render fontSize startPoint t

render :: Double -> (Double, Double) -> Text -> Canvas ()
render fontSize (startingX, startingY) t
  = evalStateT (mapM_ renderLine $ Text.lines t) startingY
  where
    renderLine :: Text -> StateT Double Canvas ()
    renderLine line = do
      y <- get
      lift $ fillText (line, startingX, y)
      modify (+fontSize)

