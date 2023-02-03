module TicTacToe.Run where

import Control.Monad.Logic
import Control.Monad.Trans.State
import TicTacToe.Game
import TicTacToe.Game.Logic
import TicTacToe.Solver

main :: IO ()
main = evalStateT (observeT (game (X, strategy) (Circle, strategy))) emptyBoard
