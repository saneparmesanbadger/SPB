{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
module SPB.Base where
import Data.Monoid ( (<>) )
import Data.Hashable (Hashable (..), hash)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
-- Adding this now so we don't end up in sweetJS hell

data Context = Context {
  lineNumber :: Int
  } deriving (Show, Eq)

type CSExp = (Context, SExp)

data Atom = 
  ASymbol T.Text
  | T
  | AChar Char
  | AInt Int
  | AFloating Double
  | AHashMap (HM.HashMap SExp CSExp)
  | ALambda T.Text CSExp (HM.HashMap T.Text CSExp)
  deriving (Eq, Show)

data SExp = 
  SAtom Atom
  | SE [CSExp]
  deriving (Eq, Show)


instance Hashable Context where             
  hashWithSalt i (Context int) = i + int
           
instance Hashable Atom where           
  hashWithSalt i (ASymbol t) = hashWithSalt i t
  hashWithSalt i T = 1 + i
  hashWithSalt i (AChar c) = hashWithSalt i c
  hashWithSalt i (AInt int) = hashWithSalt i int
  hashWithSalt i (AFloating d) = hashWithSalt i d 
  hashWithSalt i (AHashMap hm) = 0 + i
  hashWithSalt i (ALambda t l c) = 0 + i
           
instance Hashable SExp where           
  hashWithSalt i (SAtom a) = hashWithSalt i a
  hashWithSalt i (SE l) = hashWithSalt i l

c0 :: a -> (Context, a)
c0 a = (Context 0, a)

prettyPrint :: CSExp -> T.Text
prettyPrint (c, (SE [])) =  "NIL"
prettyPrint (c, (SE exps)) =  "(" <> T.intercalate " " (map prettyPrint exps) <> ")"
prettyPrint (c, (SAtom a)) = prettyPrintAtom a

prettyPrintAtom :: Atom -> T.Text
prettyPrintAtom (ASymbol t) =  t
prettyPrintAtom T =  "T"
prettyPrintAtom (AChar c) = T.singleton c
prettyPrintAtom (AInt i) = T.pack $ show i
prettyPrintAtom (AFloating f) = T.pack $ show f
prettyPrintAtom (AHashMap f) = (HM.foldlWithKey' 
                               (\a k v -> a <> "(" <> prettyPrint (c0 k)
                                          <> ", " <> prettyPrint v <> ") ")
                               "[" f) <> "]"
prettyPrintAtom _  = "<function>"

string2sexp (SE l) [] = c0 $ (SE $ reverse l)
string2sexp (SE l) (s:ss) = string2sexp (SE $ (c0 $ (SAtom (AChar s))):l) ss
string2sexp (SE []) (s:ss) =  string2sexp (SE $ [c0 $ SAtom (AChar s)]) ss