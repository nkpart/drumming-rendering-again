module Note where

newtype Note
  = Note Bool
  deriving (Eq, Show)

note :: Bool -> Note
note = Note
