{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Board
  where

import           Control.Monad.State
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as Text

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
  | Player PlayerState
  | Goal   GoalState
  | Box
  deriving (Show) -- NOTE: Show is for debugging purposes

data PlayerState = OffGoal | OnGoal
  deriving (Show)
data GoalState   = FreeGoal | TakenGoal
  deriving (Show)

type Loc = (Int, Int)

type Board = [(Loc, Object)]

parseBoard :: String -> Board
parseBoard = go (0, 0)
  where
    go     (_, y) ('\n':as) =                        go (0, y+1) as
    go     (x, y) (' ' :as) =                        go (x+1, y) as
    go loc@(x, y) (a   :as) = (loc, parseObject a) : go (x+1, y) as
    go _          []        = []

    parseObject '#' = Wall
    parseObject '@' = Player OffGoal
    parseObject '+' = Player OnGoal
    parseObject 'o' = Box
    parseObject '.' = Goal FreeGoal
    parseObject '*' = Goal TakenGoal
    parseObject c   = error $ "Unrecognized game object: " ++ [c]

renderBoard :: Int -> Int -> Board -> String
renderBoard width height = fillIn (0, 0)
  where
    renderObject Wall             = '#'
    renderObject (Player OffGoal) = '@'
    renderObject (Player OnGoal ) = '+'
    renderObject Box              = 'o'
    renderObject (Goal FreeGoal)  = '.'
    renderObject (Goal TakenGoal) = '*'

    nextLoc (x, y)
      | x == width = (0, y+1)
      | otherwise  = (x+1, y)

    fillIn (currX, currY) [] =
      replicate (width-currY) ' ' ++ unlines (replicate (height-currY) (replicate width ' '))
    fillIn loc@(currX, currY) as
      | currX >= width   = '\n' : fillIn (nextLoc loc) as
      | currY >= height  = []
      | otherwise        =
          case lookup loc as of
            Just obj -> renderObject obj : fillIn (nextLoc loc) (removeAssoc loc as)
            _        -> ' '              : fillIn (nextLoc loc) as

    -- fillIn loc@(currX, currY) (((x, y), obj):as)
    --   | currX >= width-1         = '\n' : fillIn (nextLoc loc) as
    --   | currY >= height          = []
    --   | (currX, currY) == (x, y) = renderObject obj : fillIn (nextLoc loc) as
    --   | otherwise                = ' '              : fillIn (nextLoc loc) as
    -- fillIn (currX, currY) [] =
    --   replicate (width-currY) ' ' ++ unlines (replicate (height-currY) (replicate width ' '))

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
removeObject loc = modify $ removeAssoc loc

placeObject :: MonadState Board m => Loc -> Object -> m ()
placeObject loc obj = do
  removeObject loc
  modify $ (insertBy boardCompare (loc, obj))

getPlayerLoc :: MonadState Board m => m Loc
getPlayerLoc = do
  elements <- gets $ filter (isPlayer . snd)
  case elements of
    [(loc, _)] -> return loc
    [] -> do b <- get ; error $ "Player not found on game board: " ++ show b
    _  -> error "Multiple player locations on game board."
  where
    isPlayer (Player _) = True
    isPlayer _          = False

getObject :: MonadState Board m => Loc -> m (Maybe Object)
getObject = gets . lookup

removeAssoc :: Eq a => a -> [(a, b)] -> [(a, b)]
removeAssoc target ((x, y):rest)
  | x == target = rest
  | otherwise   = (x, y) : removeAssoc target rest
removeAssoc _ [] = []

