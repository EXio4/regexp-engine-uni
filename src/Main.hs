{-# LANGUAGE RankNTypes, BangPatterns, GADTs, FlexibleContexts #-}

module Main (main) where

import System.IO
import Automata
import RegExp

data IdSigma = Letter
             | Digit
             | Other
    deriving (Eq, Show)

identificador :: RegExpS IdSigma
identificador = R_Sequ (R_Lit Letter) (R_Closure (R_Or (R_Lit Letter) (R_Lit Digit)))

parser_id x
     | (x >= 'A' && x <= 'Z') ||
       (x >= 'a' && x <= 'z') = Letter
     | x >= '0' && x <= '9'   = Digit
     | otherwise              = Other

identificador_str = "#c(#d|#c)*"

main :: IO ()
main = hSetBuffering stdout NoBuffering >> go where
    Just regex = compileRegExp <$> parseRegExp identificador_str
    go = do putStr "Inserte cadena: "
            str <- getLine
            print (run' regex id str)
            go

main_repl  :: IO ()
main_repl = do hSetBuffering stdout NoBuffering
               regex <- getRegex
               print regex
               loop (compileRegExp regex)
    where loop regex = do
              putStr "> "
              x <- getLine
              print (run' regex id x)
              loop regex
          getRegex = do
                putStrLn "Regex to test:"
                x <- getLine
                case parseRegExp x of
                     Nothing -> putStrLn "Invalid regex, try again." >> getRegex
                     Just v  -> return v

