-- | A very simple implementation of S-expressions that can be dumped to Text easily

module Agda.Interaction.Highlighting.SexpDump.Sexp where

import Data.Word
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

data Sexp = Atom Text | String String | Integer Integer | Cons [Sexp]

constr :: String -> [Sexp] -> Sexp
constr head lst = Cons (Atom (':' `T.cons` (T.pack head)) : lst)

toText :: Sexp -> T.Text
toText (Atom x)   = x
toText (Integer k) = T.pack $ show k
toText (String s) = T.pack s
toText (Cons lst) = '(' `T.cons` (T.intercalate (T.singleton ' ') (map toText lst)) `T.snoc` ')'

class Sexpable a where
    toSexp :: a -> Sexp

instance Sexpable Bool where
    toSexp False = Atom (T.pack "False")
    toSexp True = Atom (T.pack "True")

instance Sexpable Integer where
    toSexp k = Integer k

instance Sexpable String where
    toSexp = String

instance Sexpable Word64 where
    toSexp w = Integer (toInteger w)
