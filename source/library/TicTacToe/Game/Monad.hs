{-# LANGUAGE UndecidableInstances #-}

module TicTacToe.Game.Monad where

import Control.Monad.Error.Class qualified as C
import Control.Monad.State.Class qualified as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Compose
import Control.Monad.Trans.Compose.Infix
import Control.Monad.Trans.Compose.Transparent
import Control.Monad.Trans.Control
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Kind
import TicTacToe.Game
import TicTacToe.Game.Logic qualified

type TicTacToeT :: (Type -> Type) -> Type -> Type
newtype TicTacToeT m a = MkTicTacToeT {unTicTacToeT :: (TransparentT .|> StateT Board .|> ExceptT (Maybe Result) .|> StateT Player) m a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadTrans, MonadTransControl)

runTicTacToeT :: TicTacToeT m a -> m (StT TicTacToeT a)
runTicTacToeT =
  runTransparentT
    . (`runStateT` TicTacToe.Game.Logic.emptyBoard)
    . deComposeT
    . runExceptT
    . deComposeT
    . (`runStateT` X)
    . deComposeT
    . unTicTacToeT

-- TODO: Catch exceptions with illegal moves.
play :: Monad m => Position -> TicTacToeT m ()
play position = do
  player <- nextPlayer
  MkTicTacToeT $ C.modify TicTacToe.Game.Logic.nextPlayer
  board <- currentBoard
  let newBoard = TicTacToe.Game.Logic.play player position board
  MkTicTacToeT . ComposeT . lift $ C.put newBoard
  case TicTacToe.Game.Logic.gameOver newBoard of
    Nothing -> pure ()
    Just result -> MkTicTacToeT . C.throwError $ Just result

currentBoard :: Monad m => TicTacToeT m Board
currentBoard = MkTicTacToeT . ComposeT . lift $ C.get

nextPlayer :: Monad m => TicTacToeT m Player
nextPlayer = MkTicTacToeT C.get
