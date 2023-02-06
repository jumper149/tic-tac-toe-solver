module TicTacToe.Run where

import Control.Monad.Logic
import Control.Monad.Trans.State
import TicTacToe.Game
import TicTacToe.Game.Logic
import TicTacToe.Solver

main :: IO ()
main = observeT (evalStateT (game (X, strategy depth breadth) (Circle, strategy depth breadth)) emptyBoard)

depth :: Nat
depth = intToNat 6

breadth :: Nat
breadth = intToNat 9
