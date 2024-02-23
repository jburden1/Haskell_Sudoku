module Sudoku where

-- a Sudoku Board is a 9x9 matrix of values
type Board = [[Value]]

-- a value is a Char
type Value = Char

-- a position (row, col) on the board
type Position = (Int, Int)



