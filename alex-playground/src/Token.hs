module Token where


-- https://mail.haskell.org/pipermail/haskell-cafe/2006-August/017630.html

data Token
  = Let
  | In
  | Sym Char
  | Var String
  | Int Int
  | EOF
  deriving (Eq, Show)
