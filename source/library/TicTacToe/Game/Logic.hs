module TicTacToe.Game.Logic where

import Data.Maybe
import TicTacToe.Game

emptyBoard :: Board
emptyBoard = Fields $ const Empty

allPositions :: [Position]
allPositions = [minBound .. maxBound]

emptyPositions :: Board -> [Position]
emptyPositions board = filter ((== Empty) . readField board) allPositions

strikes :: [(Position, Position, Position)]
strikes =
  -- horizontal
  [ (TopLeft, TopCenter, TopRight)
  , (CenterLeft, CenterCenter, CenterRight)
  , (BottomLeft, BottomCenter, BottomRight)
  , -- vertical
    (TopLeft, CenterLeft, BottomLeft)
  , (TopCenter, CenterCenter, BottomCenter)
  , (TopRight, CenterRight, BottomRight)
  , -- diagonal
    (TopLeft, CenterCenter, BottomRight)
  , (TopRight, CenterCenter, BottomLeft)
  ]

play :: Player -> Position -> Board -> Board
play player position board = Fields $ \pos -> if pos == position then Written player else readField board pos

gameOver :: Board -> Maybe Result
gameOver board =
  if isBoardFull
    then Just Draw
    else case winningStrikes of
      [] -> Nothing
      ss@((_, player) : _) ->
        if any (/= player) $ snd <$> ss
          then error "Multiple players won at the same time"
          else Just $ Winner player (fst <$> ss)
 where
  isBoardFull :: Bool
  isBoardFull = all (== Empty) $ readField board <$> allPositions
  markedStrike :: (Position, Position, Position) -> Maybe ((Position, Position, Position), (Player, Player, Player))
  markedStrike (x, y, z) = case (readField board x, readField board y, readField board z) of
    (Written a, Written b, Written c) -> Just ((x, y, z), (a, b, c))
    _ -> Nothing
  markedStrikes :: [((Position, Position, Position), (Player, Player, Player))]
  markedStrikes = mapMaybe markedStrike strikes
  winningStrike :: ((Position, Position, Position), (Player, Player, Player)) -> Maybe ((Position, Position, Position), Player)
  winningStrike ((x, y, z), (a, b, c)) = if a == b && b == c then Just ((x, y, z), a) else Nothing
  winningStrikes :: [((Position, Position, Position), Player)]
  winningStrikes = mapMaybe winningStrike markedStrikes
