module TicTacToe.Run where

import Control.Monad.IO.Class
import Control.Monad.Logic.Class
import Control.Monad.State.Class
import Data.Kind
import TicTacToe.Game
import TicTacToe.Game.Draw
import TicTacToe.Game.Logic

type Score :: Type
type Score = Int

type Strategy :: (Type -> Type) -> Type
type Strategy m = Player -> Board -> m (Board, Score)

game ::
  ( MonadState Board m
  , MonadLogic m
  , MonadIO m
  ) =>
  Strategy m ->
  Strategy m ->
  m ()
game strategyX strategyCircle = game' (X, strategyX) (Circle, strategyCircle) emptyBoard
 where
  game' currentPlayer@(currentMark, currentStrategy) otherPlayer board =
    case gameOver board of
      Just result -> do
        liftIO $ putStrLn $ "Game Over: " <> show result
      Nothing -> do
        (board', _) <- once (currentStrategy currentMark board)
        liftIO $ putStr $ drawBoard board'
        game' otherPlayer currentPlayer board'
