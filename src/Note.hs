module Note where

newtype Note
  = Note Bool
  deriving (Eq, Show)

note :: Bool -> NoteTree
note = Leaf . Note

fromList :: [Note] -> NoteTree
fromList [] = error "beep boop"
fromList [x] = Leaf x
fromList (x:xs) = Branch (Leaf x) (fromList xs)

data NoteTree
  = Leaf Note
  | Branch NoteTree NoteTree
  deriving (Eq, Show)

toList :: NoteTree -> [Note]
toList = toListFromLeft []

toListFromLeft :: [Note] -> NoteTree -> [Note]
toListFromLeft acc (Leaf n) = n : acc
toListFromLeft acc (Branch l r) =
  toListFromLeft (toListFromLeft acc r) l

instance Semigroup NoteTree where
  n <> m = Branch n m

modify :: (Ord a, Eq a, Num a, Applicative f) => a -> NoteTree -> (Note -> f NoteTree) -> f NoteTree
modify 0 (Leaf n) f = f n
modify _ (Leaf _) _ = error "yeah?"
modify ix (Branch l r) f =
  let leftCount = countNotes l
   in if ix < leftCount
       then Branch <$> modify ix l f <*> pure r
       else Branch l <$> modify (ix - leftCount) r f


delete :: (Ord a, Eq a, Num a) => a -> NoteTree -> NoteTree
delete _ (Leaf _) = error "can't delete a leaf"
delete 0 (Branch (Leaf _) r) = r 
delete ix (Branch l r) = 
  let leftCount = countNotes l
   in if ix < leftCount
       then Branch (delete ix l) r
       else Branch l (delete (ix - leftCount) r)

modifyNote :: (Ord a, Eq a, Num a, Applicative f) => a -> NoteTree -> (Note -> f Note) -> f NoteTree
modifyNote n ns f = modify n ns (fmap Leaf . f)

countNotes :: Num p => NoteTree -> p
countNotes (Leaf _) = 1
countNotes (Branch l r) = countNotes l + countNotes r
