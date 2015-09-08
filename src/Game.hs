{-# LANGUAGE FlexibleContexts #-}
module Game
  where

import           Board

import           Control.Monad.State

import qualified Data.Map as Map

import           Lens.Micro
import           Lens.Micro.Mtl

import Debug.Trace

data Direction = U | D | L | R

type Game = StateT Board IO

isWon :: MonadState Board m => m Bool
isWon = gets (all isTaken . traceShowId . Map.assocs . _boardData)
  where
    isTaken (_, Goal FreeGoal) = False
    isTaken _                  = True

movePlayer :: MonadState Board m => Direction -> m ()
movePlayer dir = do
  currPlayerLoc <- getPlayerLoc
  let newLoc = moveInDir dir currPlayerLoc

  newLocObjectM <- getObject newLoc
  case newLocObjectM of
    Nothing -> do
      playerState .= OffGoal
      playerLoc .= newLoc

    Just newLocObject ->
      case newLocObject of
        Wall -> return ()
        Goal FreeGoal -> do
          playerState .= OnGoal
          playerLoc .= newLoc

        Box -> do
          wasMoved <- moveBox dir newLoc
          if wasMoved
            then movePlayer dir
            else return ()

        Goal TakenGoal -> do
          wasMoved <- moveBox dir newLoc
          if wasMoved
            then do
              placeObject newLoc (Goal FreeGoal)
              movePlayer dir
            else return ()

moveBox :: MonadState Board m => Direction -> Loc -> m Bool
moveBox dir loc = do
  let newLoc = moveInDir dir loc

  if loc == newLoc
    then return False
    else do
      newLocObjectM <- getObject newLoc

      case newLocObjectM of
        Nothing -> do
          removeObject loc
          placeObject newLoc Box
          return True

        Just newLocObject ->
          case newLocObject of
            Wall           -> return False
            Box            -> return False
            Goal TakenGoal -> return False
            Goal FreeGoal -> do
              removeObject loc
              placeObject newLoc (Goal TakenGoal)
              return True


moveInDir :: Direction -> Loc -> Loc
moveInDir U (x, y) = (x, y-1)
moveInDir D (x, y) = (x, y+1)
moveInDir L (x, y) = (x-1, y)
moveInDir R (x, y) = (x+1, y)

