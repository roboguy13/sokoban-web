{-# LANGUAGE FlexibleContexts #-}
module Game
  where

import           Board

import           Control.Monad.State

data Direction = U | D | L | R

type Game = StateT Board IO

isWon :: MonadState Board m => m Bool
isWon = gets (not . any isFreeGoal)
  where
    isFreeGoal (_, Goal FreeGoal) = True
    isFreeGoal _                  = False

movePlayer :: MonadState Board m => Direction -> m ()
movePlayer dir = do
  playerLoc <- getPlayerLoc
  let newLoc = moveInDir dir playerLoc

  newLocObjectM <- getObject newLoc
  case newLocObjectM of
    Nothing -> do
      removePlayer playerLoc
      placeObject newLoc (Player OffGoal)

    Just newLocObject ->
      case newLocObject of
        Wall -> return ()
        Goal FreeGoal -> do
          removePlayer playerLoc
          placeObject newLoc (Player OnGoal)

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

        Player _ -> error "Internal error: Player ran into themselves"

removePlayer :: MonadState Board m => Loc -> m ()
removePlayer loc = do
  obj <- getObject loc
  case obj of
    Just (Player OnGoal) -> do
      removeObject loc
      placeObject loc $ Goal FreeGoal
    _ -> removeObject loc

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
            Player _       -> error "Internal error: Box collided with player moving box"
            Goal FreeGoal -> do
              removeObject loc
              placeObject newLoc (Goal TakenGoal)
              return True


moveInDir :: Direction -> Loc -> Loc
moveInDir U (x, y) = (x, y-1)
moveInDir D (x, y) = (x, y+1)
moveInDir L (x, y) = (x-1, y)
moveInDir R (x, y) = (x+1, y)

