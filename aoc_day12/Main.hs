module Main where

import           Text.Read                      ( readMaybe )
import           Data.Maybe                     ( mapMaybe )

-- Main
main :: IO ()
main = do
  contents <- readFile "input.txt"
  let lines_input  = lines contents
  let parsed_input = mapMaybe genNewCommand lines_input
  let initState = State East 0 0
  let final_state = foldl (flip foldFunction) initState parsed_input
  let part1 = genPart1 final_state
  print part1
---------------------------------------------------
genPart1::State -> Int
genPart1 (State _  n e) = abs n + abs e

foldFunction::Command -> State -> State
foldFunction currCommand currState = genNextState currState currCommand

prop::Int -> Bool
prop x = rotate x North == East


-- Typeclass declarations

data HeavenDirs = North | South | East | West 
    deriving (Show,Eq, Enum)

data Direction = TD HeavenDirs | ArrowAction Dir
    deriving (Show,Eq)


data Dir = Left' | Right' | Forward
    deriving (Show,Eq)

data Command = Command Direction Int
  deriving (Show, Eq)

type NorthIndex = Int
type EastIndex = Int

data State = State HeavenDirs NorthIndex EastIndex
    deriving(Show,Eq)
-----------------------------------------------------

genNextState:: State -> Command -> State
genNextState state@(State dir nor east) command =
    case command of 
      Command (TD North) x -> State dir (nor+x) east
      Command (TD South) x -> State dir (nor-x) east
      Command (TD East) x -> State dir nor (east+x)
      Command (TD West) x -> State dir nor (east-x)
      Command (ArrowAction z) x -> handleArrowAction state z x


type HandleHeavenDirs = State -> Int -> State

handleArrowAction:: State -> Dir -> Int -> State
handleArrowAction curState dir len = 
    case dir of
      Forward -> handleForward curState len
      Right' -> handleRight curState len
      Left' -> handleLeft curState len

handleForward:: HandleHeavenDirs
handleForward (State dir nor east) len =
    case dir of
      North -> State dir (nor+len) east
      South -> State dir (nor-len) east
      East -> State dir nor (east+len)
      West -> State dir nor (east-len)

handleRight::HandleHeavenDirs
handleRight (State dir nor east) len = State (rotate len dir) nor east

handleLeft::HandleHeavenDirs
handleLeft (State dir nor east) len = State (rotate (-1*len) dir) nor east

rotate::Int -> HeavenDirs -> HeavenDirs
rotate 90 North = East
rotate 90 East = South
rotate 90 South = West
rotate 90 West = North
rotate 180 x = rotate 90 $ rotate 90 x
rotate 270 x = rotate 90 $ rotate 90 $ rotate 90 x

rotate (-90) North = West
rotate (-90) East = North
rotate (-90) South = East
rotate (-90) West = South
rotate (-180) x = rotate (-90) $ rotate (-90) x
rotate (-270) x = rotate (-90) $ rotate (-90) $ rotate (-90) x

rotate _ x = x


-- Parsing
genNewCommand :: String -> Maybe Command
genNewCommand [] = Nothing
genNewCommand (x : xs) = do
  len    <- readMaybe xs :: Maybe Int
  direction <- getDirection x
  return $ Command direction len


getDirection:: Char -> Maybe Direction
getDirection str | str == 'N' = Just $ TD North
                 | str == 'S' = Just $ TD South
                 | str == 'E' = Just $ TD East
                 | str == 'W' = Just $ TD West
                 | str == 'F' = Just $ ArrowAction Forward
                 | str == 'L' = Just $ ArrowAction Left'
                 | str == 'R' = Just $ ArrowAction Right'
                 | otherwise  = Nothing
-----------------------------------------------------

