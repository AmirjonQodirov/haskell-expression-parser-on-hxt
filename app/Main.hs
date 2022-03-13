module Main where
import System.IO
import Text.XML.HXT.Core
import Data.Maybe (listToMaybe)
import Text.XML.HXT.Arrow.Pickle

import Calc ( xpExpr, calc, toExpr, Expr )
import Text.Read (Lexeme(String))

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    programm
    
programm :: IO()
programm = do
    putStrLn "-----WELCOME!-----"
    putStrLn "Please choose how get xml "
    putStrLn "Enter 1 for  ---  get XML from std in"
    putStrLn "Enter 2 for  ---  get XML from file"
    method <- getLine
    let opt = (read method :: Int)
    if opt == 1 then do
        putStrLn "Please enter xml: "
        xml <- getLine
        let res = toExpr xml
        case res of
            Just x -> putStrLn $ "Result: " ++ show (calc x)
            Nothing -> putStrLn "Error wrong input" 
    else do
        putStrLn "Please enter file path: "
        xml_path <- getLine
        res <- loadProgram xml_path        
        putStrLn $ "Result: " ++ show (calc res)


loadProgram :: String -> IO Expr
loadProgram filePath = do
    [a] <- runX
            ( xunpickleDocument xpExpr
                  [ withRemoveWS yes   -- remove redundant whitespace
                  , withValidate no    -- don't validate source
                  ] filePath
            )
    return a