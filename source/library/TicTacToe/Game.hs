module TicTacToe.Game where

import Data.Kind

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
