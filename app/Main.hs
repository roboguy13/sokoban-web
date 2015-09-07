{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding (unlines)

import           Render
import           Board
import           Game

import           Graphics.Blank
import           Web.KeyCode

import           Data.Text (pack, unpack)
import qualified Data.Text as Text

import           Control.Monad.State

import           Data.List
import           Data.Ord

main :: IO ()
main = do
  putStrLn "Starting server..."

  let boardStr = testBoard2
      parsedBoard = parseBoard boardStr

      fontSize = 20
      startPoint@(startX, startY) = (50, 50)

      width, height :: Int
      width  = succ . fst . fst $ maximumBy (comparing $ fst . fst) parsedBoard
      height = succ . snd . fst $ maximumBy (comparing $ snd . fst) parsedBoard

  blankCanvas 3000 { events = ["keydown", "keypress"] } $ \context -> do
    flip evalStateT (parseBoard boardStr) $ do
      let redraw = do
            liftIO $ send context clearCanvas
            currBoard <- get
            liftIO $ blankRender context fontSize startPoint (pack (renderBoard width height currBoard))

      redraw

      forever $ do
        event <- liftIO $ wait context

        if eType event `elem` ["keydown", "keypress"]
          then do
            case keyCodeLookup <$> eWhich event of
              Just KeyH -> movePlayer L >> redraw
              Just KeyJ -> movePlayer D >> redraw
              Just KeyK -> movePlayer U >> redraw
              Just KeyL -> movePlayer R >> redraw
              _         -> return ()
          else return ()

