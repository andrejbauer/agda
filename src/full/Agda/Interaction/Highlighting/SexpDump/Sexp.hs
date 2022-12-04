-- | A very simple implementation of S-expressions that can be dumped to Text easily

module Agda.Interaction.Highlighting.SexpDump.Sexp where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

data Sexp = Atom Text | Cons [Sexp]

toText :: Sexp -> T.Text
toText (Atom x)   = x
toText (Cons lst) = '(' `T.cons` (T.intercalate (T.singleton ' ') (map toText lst)) `T.snoc` ')'

class Sexpable a where
  toSexp :: a -> Sexp

instance Sexpable Bool where
  toSexp False = Atom (T.pack "False")
  toSexp True = Atom (T.pack "True")

instance Sexpable a => Sexpable [a] where
  toSexp lst = Cons (map toSexp lst)
