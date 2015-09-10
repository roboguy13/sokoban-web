{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude hiding (unlines)

import           Render
import           Board
import           Game

import           Graphics.Blank hiding (fillStyle, strokeStyle)
import qualified Graphics.Blank as Blank
import           Graphics.Blank.Style
import           Web.KeyCode

import           Data.Text (pack, unpack)
import qualified Data.Text as Text

import           Control.Monad.State

import qualified Data.Map as Map

import           Lens.Micro

import           Data.List
import           Data.Ord

import           System.Exit

main :: IO ()
main = do
  putStrLn "Starting server..."

  let boardStr = testBoard2
      parsedBoard = parseBoard boardStr

      fontSize = 20
      startPoint@(startX, startY) = (50, 50)

      width, height :: Int
      width  = succ . fst . fst $ maximumBy (comparing $ fst . fst) . Map.assocs $ parsedBoard ^. boardData
      height = succ . snd . fst $ maximumBy (comparing $ snd . fst) . Map.assocs $ parsedBoard ^. boardData

  blankCanvas 3000 { events = ["keydown", "keypress"] } $ \context -> do

    let w = Blank.width context
        h = Blank.height context

    send context $ do
      fillStyle black
      fillRect (0, 0, w, h)

      fillStyle white

    renderInstructions context fontSize (width, height) startPoint

    flip evalStateT (parseBoard boardStr) $ do
      let redraw = do
            currBoard <- get
            liftIO $ renderGame context fontSize (width, height) startPoint currBoard

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

        won <- isWon
        if won
          then do
            liftIO $ putStrLn "You won!"
            lift . send context $ do
              fillStyle black
              fillRect (0, 0, w, h)
            liftIO $ blankRender False red context 15 (70, 150) wonMessage
            liftIO exitSuccess
          else return ()
