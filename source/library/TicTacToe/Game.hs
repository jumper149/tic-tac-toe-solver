module TicTacToe.Game where

import Data.Kind
import Data.Maybe

type Player :: Type
data Player
  = X
  | Circle
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

type Mark :: Type
data Mark
  = Empty
  | Written Player
  deriving stock (Eq, Ord, Read, Show)

type Position :: Type
data Position
  = TopLeft
  | TopCenter
  | TopRight
  | CenterLeft
  | CenterCenter
  | CenterRight
  | BottomLeft
  | BottomCenter
  | BottomRight
  deriving stock (Bounded, Enum, Eq, Ord, Read, Show)

positions :: [Position]
positions = [minBound .. maxBound]

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

type Board :: Type
newtype Board = Fields {readField :: Position -> Mark}

type Move :: Type
newtype Move = MkMove {unMove :: Position}
  deriving stock (Eq, Ord, Read, Show)
  deriving newtype (Bounded, Enum)

type Result :: Type
data Result
  = Draw
  | Winner Player [(Position, Position, Position)]
  deriving stock (Eq, Ord, Read, Show)

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
  isBoardFull = all (== Empty) $ readField board <$> positions
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

{- FOURMOLU_DISABLE -}
drawBoard :: Board -> String
drawBoard board =
  [ '╔', '═', '╤', '═', '╤', '═', '╗', '\n'
  , '║', xtl, '│', xtc, '│', xtr, '║', '\n'
  , '╟', '─', '┼', '─', '┼', '─', '╢', '\n'
  , '║', xcl, '│', xcc, '│', xcr, '║', '\n'
  , '╟', '─', '┼', '─', '┼', '─', '╢', '\n'
  , '║', xbl, '│', xbc, '│', xbr, '║', '\n'
  , '╚', '═', '╧', '═', '╧', '═', '╝', '\n'
  ]
 where
  xtl = drawMark $ readField board TopLeft
  xtc = drawMark $ readField board TopCenter
  xtr = drawMark $ readField board TopRight
  xcl = drawMark $ readField board CenterLeft
  xcc = drawMark $ readField board CenterCenter
  xcr = drawMark $ readField board CenterRight
  xbl = drawMark $ readField board BottomLeft
  xbc = drawMark $ readField board BottomCenter
  xbr = drawMark $ readField board BottomRight
{- FOURMOLU_ENABLE -}

drawMark :: Mark -> Char
drawMark = \case
  Empty -> ' '
  Written X -> 'x'
  Written Circle -> '∘'
