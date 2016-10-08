{-# LANGUAGE RankNTypes, BangPatterns, GADTs, FlexibleContexts #-}

module RegExp (RegExpS(..), parseRegExp, compileRegExp) where

import           Automata
import qualified Text.Parsec      as P
import           Text.Parsec ((<|>), (<?>))
import qualified Text.Parsec.Expr as P

{- extended&simplified non-standard regexp [no epsilon, can't be arsed to implement today] -}
data RegExpS sigma = R_Lit    sigma
                   | R_LitSub [sigma]
                   | R_Sequ (RegExpS sigma) (RegExpS sigma)
                   | R_Or   (RegExpS sigma) (RegExpS sigma)
                   | R_Closure (RegExpS sigma)
    deriving (Show)


{-
-}
parseRegExp :: String -> Maybe (RegExpS Char)
parseRegExp = either (const Nothing) Just . P.parse expr "<user-input>" where
        expr    = P.buildExpressionParser table term
                <?> "expression"

        term    =  P.between (P.char '(') (P.char ')') expr
                <|> p_lit
                <?> "simple expression"

        table   = [ [postfix "*" R_Closure]
                  , [seque       R_Sequ     P.AssocRight]
                  , [binary "|"  R_Or       P.AssocRight]
                  ]

        binary  name fun assoc = P.Infix (do{ P.string name; return fun }) assoc
        seque        fun assoc = P.Infix (do{ return fun }) assoc
        prefix  name fun       = P.Prefix (do{ P.string name; return fun })
        postfix name fun       = P.Postfix (do{ P.string name; return fun })
        p_lit = do x <- P.noneOf "*()|"
                   if x /= '#'
                   then return (R_Lit x)
                   else do category <- P.oneOf "dc"
                           return . R_LitSub $
                                case category of
                                    'd' -> ['0'..'9']
                                    'c' -> ['a'..'z'] ++ ['A'..'Z']
                                    _   -> [] {- impossible case -}

{- needs more testing, I kinda wrote this without thinking much -}
compileRegExp :: Ord sigma => RegExpS sigma -> DetAutomataE sigma
compileRegExp r = case r of
                    R_Lit z      -> AutomataE (det_lit_once z)
                    R_LitSub zs  -> AutomataE (det_litsub zs)
                    R_Or   r1 r2 -> combineAuto (\f1 f2 -> AutomataE (det_or   f1 f2)) r1 r2
                    R_Sequ r1 r2 -> combineAuto (\f1 f2 -> AutomataE (det_sequ f1 f2)) r1 r2
                    R_Closure r  -> case compileRegExp r of AutomataE f -> AutomataE (det_clos f)

combineAuto :: Ord sigma =>
                (forall s1 s2. (Ord s1, Ord s2) => DetAutomata s1 sigma -> DetAutomata s2 sigma -> DetAutomataE sigma) ->
                RegExpS sigma -> RegExpS sigma -> DetAutomataE sigma
combineAuto fun r1 r2 =
            case (compileRegExp r1 , compileRegExp r2) of
                (AutomataE r1_fsm, AutomataE r2_fsm) ->
                    fun r1_fsm r2_fsm




