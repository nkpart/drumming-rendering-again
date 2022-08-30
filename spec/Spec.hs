
import Test.Tasty
import Test.Tasty.HUnit
-- import Data.Functor
import qualified Control.Monad.State.Strict as SM

import Note

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "NoteTree"
  [ 
    testCase "toList" $ do
      [Note False, Note True, Note False] @=? toList (note False <> note True <> note False)
      [Note False, Note True, Note False] @=? (toList . fromList) [Note False, Note True, Note False]
    ,
    testCase "modify can expand" $ do
      let initial = fromList [Note False, Note True, Note False]
      modified <- modify (1::Int) initial (\n -> pure $ Branch (Leaf n) (Leaf n))
      [Note False, Note True, Note True, Note False] @=? toList modified
    ,
    testCase "delete" $ do
      let initial = fromList [Note False, Note True, Note False]
      let modified = delete (1::Int) initial
      [Note False, Note False] @=? toList modified
    ,
    testCase "example" $ do
     print . toList $ flip SM.execState (note False) $ do
       SM.modify (\n -> Branch n (note True))
       SM.modify (\n -> Branch n (note True))
       pure ()
      
     pure ()
  ]


-- returning :: Functor f => (b -> f a) -> b -> f b
-- returning f a = f a $> a
