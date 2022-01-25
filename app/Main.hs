module Main where
import Text.XML.HXT.Core
import Text.XML.HXT.Arrow.Pickle

import Calc

exp :: Maybe Expr
exp = unpickleDoc xpickle =<< listToMaybe (xread "<Add><Lit v=1><Lit v=2></Add>")


