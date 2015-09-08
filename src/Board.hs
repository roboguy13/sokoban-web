{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Board
  where

import           Control.Monad.State

import           Data.List

import           Data.Text (Text)
import qualified Data.Text as Text

import           Data.Map (Map)
import qualified Data.Map as Map

import           Lens.Micro
import           Lens.Micro.Mtl
import           Lens.Micro.TH
import           Lens.Micro.GHC

testBoard :: String
testBoard
  = unlines
      [ "#####"
      , "#.o@#"
      , "#####"
      ]

testBoard2 :: String
testBoard2
  = unlines
      ["    #####"
      ,"    #   #"
      ,"    #o  #"
      ,"  ###  o##"
      ,"  #  o o #"
      ,"### # ## #   ######"
      ,"#   # ## #####  ..#"
      ,"# o  o          ..#"
      ,"##### ### #@##  ..#"
      ,"    #     #########"
      ,"    #######"
      ]

data Object
  = Wall
  | Goal GoalState
  | Box
  deriving (Show) -- NOTE: Show is for debugging purposes

data PlayerState = OffGoal | OnGoal
  deriving (Show)
data GoalState   = FreeGoal | TakenGoal
  deriving (Show)

type Loc = (Int, Int)

data Board
  = Board
    { _boardData   :: Map Loc Object
    , _playerLoc   :: Loc
    , _playerState :: PlayerState
    }
makeLenses ''Board

parseBoard :: String -> Board
parseBoard = go (0, 0)
  where
    go     (_, y) ('\n':as) =                        go (0, y+1) as
    go     (x, y) (' ' :as) =                        go (x+1, y) as
    go loc@(x, y) (a   :as) = parseObject loc a $ go (x+1, y) as
    go _          []        = Board { _boardData = Map.empty }

    parseObject loc '#' = boardData %~ (Map.insert loc Wall)
    parseObject loc '@' =
      (playerLoc .~ loc)
      . (playerState .~ OffGoal)
    parseObject loc '+' =
      (playerLoc .~ loc)
      . (playerState .~ OnGoal)
    parseObject loc 'o' = boardData %~ (Map.insert loc Box)
    parseObject loc '.' = boardData %~ (Map.insert loc (Goal FreeGoal))
    parseObject loc '*' = boardData %~ (Map.insert loc (Goal TakenGoal))
    parseObject _   c   = error $ "Unrecognized game object: " ++ [c]

renderBoard :: Int -> Int -> Board -> String
renderBoard width height board = fillIn (0, 0) . Map.assocs $ _boardData board
  where
    renderObject Wall             = '#'
    renderObject Box              = 'o'
    renderObject (Goal FreeGoal)  = '.'
    renderObject (Goal TakenGoal) = '*'

    renderPlayer OffGoal = '@'
    renderPlayer OnGoal  = '+'

    nextLoc (x, y)
      | x == width = (0, y+1)
      | otherwise  = (x+1, y)

    fillIn (currX, currY) [] =
      replicate (width-currY) ' ' ++ unlines (replicate (height-currY) (replicate width ' '))
    fillIn loc@(currX, currY) as
      | currX >= width   = '\n' : fillIn (nextLoc loc) as
      | currY >= height  = []
      | loc == _playerLoc board = renderPlayer (_playerState board) : fillIn (nextLoc loc) (removeAssoc loc as)
      | otherwise        =
          case lookup loc as of
            Just obj -> renderObject obj : fillIn (nextLoc loc) (removeAssoc loc as)
            _        -> ' '              : fillIn (nextLoc loc) as

moveObject :: MonadState Board m => Loc -> Loc -> m ()
moveObject from to = do
  objM <- getObject from
  case objM of
    Just obj -> do
      removeObject from
      placeObject to obj
    _ -> error $ "No object at " ++ show from

boardCompare :: (Loc, Object) -> (Loc, Object) -> Ordering
boardCompare (a, _) (b, _) = compare (swap a) (swap b)
  where
    swap (x, y) = (y, x)

removeObject :: MonadState Board m => Loc -> m ()
removeObject loc = boardData %= Map.delete loc

placeObject :: MonadState Board m => Loc -> Object -> m ()
placeObject loc obj = do
  removeObject loc
  boardData %= Map.insert loc obj

getPlayerLoc :: MonadState Board m => m Loc
getPlayerLoc = use playerLoc

getObject :: MonadState Board m => Loc -> m (Maybe Object)
getObject loc = gets $ Map.lookup loc . _boardData

removeAssoc :: Eq a => a -> [(a, b)] -> [(a, b)]
removeAssoc target ((x, y):rest)
  | x == target = rest
  | otherwise   = (x, y) : removeAssoc target rest
removeAssoc _ [] = []

