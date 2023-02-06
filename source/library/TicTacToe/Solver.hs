module TicTacToe.Solver where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logic.Class
import Control.Monad.State.Class
import Data.Kind
import Data.List qualified
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
  , MonadIO m
  ) =>
  Nat ->
  Nat ->
  Strategy m
strategy depth breadth player = do
  board <- get
  case evaluateBoard player board of
    Just score -> pure score
    Nothing -> minmax strategy depth breadth player

minmax ::
  forall m.
  ( MonadState Board m
  , MonadLogic m
  ) =>
  (Nat -> Nat -> Strategy m) ->
  (Nat -> Nat -> Strategy m)
minmax self depth breadth player = do
  scores <-
    bagOfN (Just breadth) $ do
      board <- get
      position <- choose $ emptyPositions board
      let newBoard = play player position board
      case depth of
        Z -> do
          case evaluateBoard player newBoard of
            Nothing -> pure (0, newBoard)
            Just x -> pure (x, newBoard)
        S n -> do
          put newBoard
          score <- self n breadth (nextPlayer player)
          pure (-score, newBoard)
  let (bestScore, bestBoard) = Data.List.maximumBy (\(s1, _) (s2, _) -> compare s1 s2) scores
  put bestBoard
  pure bestScore

evaluateBoard :: Player -> Board -> Maybe Score
evaluateBoard player board =
  case gameOver board of
    Just result ->
      case result of
        Draw -> Just -1
        Winner p _ ->
          if p == player
            then Just 100
            else Just -100
    Nothing -> Nothing

bagOfN ::
  MonadLogic m =>
  Maybe Nat ->
  m a ->
  m [a]
bagOfN n ma =
  case n of
    Just Z -> pure []
    _ ->
      msplit ma >>= \case
        Nothing -> pure []
        Just (a, ma') ->
          let n' = case n of
                Just (S m) -> Just m
                Nothing -> Nothing
           in (a :) <$> bagOfN n' ma'

choose :: MonadLogic m => [a] -> m a
choose = foldr ((<|>) . pure) empty

type Nat :: Type
data Nat = Z | S Nat

natToInt :: Nat -> Int
natToInt = \case
  Z -> 0
  S n -> succ $ natToInt n

intToNat :: Int -> Nat
intToNat n =
  if n <= 0
    then Z
    else S $ intToNat $ pred n
