-- | A very simple implementation of S-expressions that can be dumped to Text easily

module Agda.Interaction.Highlighting.Sexp.Sexp where

import Data.Word
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text as DT

data Sexp = Atom Text | String String | Integer Integer | Double Double | Cons [Sexp]

constr :: String -> [Sexp] -> Sexp
constr head lst = Cons (Atom (':' `T.cons` (T.pack head)) : lst)

toText :: Sexp -> T.Text
toText (Atom x)   = x
toText (Integer k) = T.pack $ show k
toText (Double x) = T.pack $ show x
toText (String s) = '"' `T.cons` (T.pack $ escape s) `T.snoc` '"' --
  where
    -- We escape characters with bare hands to keep UTF8 intact
    escape :: String -> String
    escape [] = []
    escape ('"' : s) = '\\' : '"' : escape s
    escape ('\\' : s) = '\\' : '\\' : escape s
    escape (c : s) = c : escape s

toText (Cons lst) = '(' `T.cons` (T.intercalate (T.singleton ' ') (map toText lst)) `T.snoc` ')'


class Sexpable a where
    toSexp :: a -> Sexp

instance Sexpable Bool where
    toSexp False = constr "false" []
    toSexp True = constr "true" []

instance Sexpable Integer where
    toSexp k = Integer k

instance Sexpable Int where
    toSexp k = Integer (toInteger k)

instance Sexpable String where
    toSexp = String

instance Sexpable T.Text where
    toSexp t = String $ T.unpack t

instance Sexpable DT.Text where
    toSexp t = String $ DT.unpack t

instance Sexpable Double where
    toSexp = Double

instance Sexpable Word64 where
    toSexp w = Integer (toInteger w)
