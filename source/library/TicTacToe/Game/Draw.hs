module TicTacToe.Game.Draw where

import TicTacToe.Game

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
