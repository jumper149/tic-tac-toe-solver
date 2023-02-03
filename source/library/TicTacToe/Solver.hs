module TicTacToe.Solver where

import Control.Applicative
import Control.Monad
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
type Strategy m = Player -> m Score

game ::
  forall m.
  ( MonadState Board m
  , MonadLogic m
  , MonadPlus m
  , MonadIO m
  ) =>
  (Player, Strategy m) ->
  (Player, Strategy m) ->
  m ()
game currentPlayer@(currentMark, currentStrategy) otherPlayer = do
  board <- get
  case gameOver board of
    Just result -> do
      liftIO $ putStrLn $ "Game Over: " <> show result
    Nothing -> do
      board' <- once $ currentStrategy currentMark >> get
      liftIO $ putStr $ drawBoard board'
      game otherPlayer currentPlayer

strategy ::
  forall m.
  ( MonadState Board m
  , MonadLogic m
  , MonadPlus m
  , MonadIO m
  ) =>
  Strategy m
strategy player = do
  board <- get
  case evaluateBoard player board of
    Just score -> pure score
    Nothing -> do
      position <- asum $ pure <$> emptyPositions board
      modify $ play player position
      board' <- get
      case evaluateBoard player board' of
        Just score -> pure score
        Nothing -> pure 0

evaluateBoard :: Player -> Board -> Maybe Score
evaluateBoard player board =
  case gameOver board of
    Just result ->
      case result of
        Draw -> Just minBound
        Winner p _ ->
          if p == player
            then Just maxBound
            else Just minBound
    Nothing -> Nothing
