{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Note where
import Data.String

newtype Note
  = Note String
  deriving (Eq, Show, IsString)

note :: Bool -> Note
note True = Note "P"
note False = Note "p"


