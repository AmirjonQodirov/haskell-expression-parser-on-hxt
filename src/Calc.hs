module Calc
( Expr(..), eval, someFunc, expp
) where
import GHC.Read (list)
import Text.XML.HXT.Arrow.Pickle
import Data.Maybe (listToMaybe)
import Text.XML.HXT.Parser.XmlParsec (xread)

data Expr = Add [Expr]
          | Sub [Expr]
          | Mul [Expr]
          | Div [Expr]
          | Sin Expr
          | Cos Expr
          | Lit Int deriving ( Eq, Show )

instance XmlPickler Expr where
    xpickle = xpAlt tag ps
	where
	tag (Add _    ) = 0
	tag (Sub _    ) = 1
	tag (Mul _    ) = 2
	tag (Div _    ) = 3
	tag (Sin _    ) = 4
	tag (Cos _    ) = 5
	tag (Lit _    ) = 6
	ps = [ xpWrap ( Add
		      , \ (Add list ) -> list
                      ) $
               ( xpElem "Add" $
		 xpList xpickle
               )

	     , xpWrap ( Sub
		      , \ (Sub list ) -> list
                      ) $
               ( xpElem "Sub" $
		 xpList xpickle
               )

	     , xpWrap ( Mul
		      , \ (Mul list ) -> list
                      ) $
               ( xpElem "Mul" $
		 xpList xpickle
               )

	     , xpWrap ( Div
		      , \ (Div list ) -> list
                      ) $
               ( xpElem "Div" $
		 xpList xpickle
               )

	     , xpWrap ( Sin
		      , \ (Sin i ) -> i
                      ) $
               ( xpElem "Sin"   $
		 xpickle
               )

         , xpWrap ( Cos
		      , \ (Cos i ) -> i
                      ) $
               ( xpElem "Cos"   $
		 xpickle
               )

         , xpWrap ( Lit
		      , \ (Lit i ) -> i
                      ) $
               ( xpElem "Lit"   $
		 xpickle
               )
	     ]

addExpr :: [Expr] -> Double
addExpr [] = 0
addExpr (x:xs) = eval x + addExpr xs

subExpr :: [Expr] -> Double
subExpr [] = 0
subExpr (x:xs) = eval x - addExpr xs

mulExpr :: [Expr] -> Double
mulExpr [] = 1
mulExpr (x:xs) = eval x * mulExpr xs

divExpr :: [Expr] -> Double
divExpr [] = 1
divExpr (x:xs) = eval x / mulExpr xs

eval :: Expr -> Double
eval (Lit a) = fromIntegral a
eval (Add list) = addExpr list
eval (Sub list) = subExpr list
eval (Mul list) = mulExpr list
eval (Div list) = divExpr list
eval (Sin a) = sin $ (eval a) * (pi / 180)
eval (Cos a) = cos $ (eval a) * (pi / 180)


expp :: String inp -> Maybe Expr
expp = unpickleDoc xpickle =<< listToMaybe (xread inp)

eee = listToMaybe $ xread "<Mul><Lit>5</Lit><Lit>5</Lit><Lit>2</Lit><Add><Lit>2</Lit><Lit>3</Lit></Add><Sin><Lit>90</Lit></Sin></Mul>"

calculate :: Maybe Expr -> Double 
calculate (Just a) = eval a
calculate Nothing = 0

calc :: String xml -> Maybe Double
calc = eval $ 