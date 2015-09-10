module Render
  (renderGame
  ,renderInstructions
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

import           Graphics.Blank hiding (fillStyle, strokeStyle)
import           Graphics.Blank.Style

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
renderGame context fontSize (width, height) startPoint board = do
  blankRender True white context fontSize startPoint . pack $ renderBoard width height board

renderInstructions :: DeviceContext -> Double -> (Int, Int) -> (Double, Double) -> IO ()
renderInstructions context fontSize (width, height) (x, y) =
  blankRender False
              white
              context
              fontSize
              (((fromIntegral width * fontSize) - x - moveInstrWidth) / 2
              , y + (fromIntegral height * fontSize) + (fontSize * 2))
            $ pack movementInstructions
  where
    moveInstrWidth = fromIntegral . maximum . map length $ lines movementInstructions

blankRender :: Style style => Bool -> style -> DeviceContext -> Double -> (Double, Double) -> Text -> IO ()
blankRender clearScreen style context fontSize startPoint t = do
  send context $ do
    when clearScreen $ do
      fillStyle black
      fillRect (0, 0, w, h)

    fillStyle style

    font . pack $ show (ceiling fontSize) ++ "pt Courier"
    render fontSize startPoint t
  where
    (w, h) = textBlockSize t fontSize

textBlockSize :: Text -> Double -> (Double, Double)
textBlockSize text fontSize =
  -- XXX: Why is the '+ 2' necessary here?
  (fromIntegral (Text.length text) * fontSize, fromIntegral (length (Text.lines text) + 2) * fontSize)

render :: Double -> (Double, Double) -> Text -> Canvas ()
render fontSize (startingX, startingY) t
  = evalStateT (mapM_ renderLine $ Text.lines t) startingY
  where
    renderLine :: Text -> StateT Double Canvas ()
    renderLine line = do
      y <- get
      lift $ fillText (line, startingX, y)
      modify (+fontSize)

