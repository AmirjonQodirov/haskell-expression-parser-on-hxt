module Calc( Expr(..), xpExpr, toExpr, calc) where
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
    xpickle = xpExpr

xpExpr :: PU Expr
xpExpr = xpAlt tag ps
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
addExpr (x:xs) = calc x + addExpr xs

subExpr :: [Expr] -> Double
subExpr [] = 0
subExpr (x:xs) = calc x - addExpr xs

mulExpr :: [Expr] -> Double
mulExpr [] = 1
mulExpr (x:xs) = calc x * mulExpr xs

divExpr :: [Expr] -> Double
divExpr [] = 1
divExpr (x:xs) = calc x / mulExpr xs

calc :: Expr -> Double
calc (Lit a) = fromIntegral a
calc (Add list) = addExpr list
calc (Sub list) = subExpr list
calc (Mul list) = mulExpr list
calc (Div list) = divExpr list
calc (Sin a) = sin $ (calc a) * (pi / 180)
calc (Cos a) = cos $ (calc a) * (pi / 180)

toExpr :: String -> Maybe Expr
toExpr inp = unpickleDoc xpickle =<< listToMaybe (xread inp)
