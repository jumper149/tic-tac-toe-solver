module TicTacToe.Run where

import Control.Monad.Logic
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Infix
import Control.Monad.Trans.Compose.Transparent
import Control.Monad.Trans.Select
import Control.Monad.Trans.State
import Data.Kind
import TicTacToe.Game
import TicTacToe.Game.Logic
import TicTacToe.Solver

type AppT :: (Type -> Type) -> Type -> Type
type AppT = TransparentT .|> SelectT () .|> LogicT .|> StateT Board

main :: IO ()
main = run f
 where
  f :: AppT IO ()
  f = game (X, strategy depth breadth) (Circle, strategy depth breadth)
  run :: AppT IO a -> IO a
  run =
    runTransparentT
      . (`runSelectT` undefined)
      . deComposeT
      . observeT
      . deComposeT
      . (`evalStateT` emptyBoard)
      . deComposeT

depth :: Nat
depth = intToNat 6

breadth :: Nat
breadth = intToNat 9
