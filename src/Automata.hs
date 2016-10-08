{-# LANGUAGE RankNTypes, BangPatterns, GADTs, FlexibleContexts #-}

module Automata (AutomataE(..), DetAutomata(..), DetAutomataE, NonDetAutomata(..), NonDetAutomataE, det_lit_once, det_litsub, det_clos, det_and, det_or, det_sequ, det_not, run, run', compileNonDet) where

import qualified Data.Set as S
import           Data.Set (Set)

data AutomataE typ sigma = forall state. Ord state => AutomataE (typ state sigma)

{- let's assume state and sigma are finite types
   [and let's assume strict semantics too!] -}
data DetAutomata state sigma = DetAutomata {
         fsm_initialState :: state
        ,fsm_is_final     :: state -> Bool
        ,fsm_trans        :: (state, sigma) -> state
}
type DetAutomataE = AutomataE DetAutomata

data NonDetAutomata state sigma = NonDetAutomata {
         nfsm_initialState :: S.Set state
        ,nfsm_is_final     :: state -> Bool
        ,nfsm_trans        :: (state, sigma) -> S.Set state
}
type NonDetAutomataE = AutomataE NonDetAutomata

data LiteralState = Lit_SInitial
                  | Lit_SFinal
                  | Lit_SFailed
    deriving (Eq, Ord)

det_lit_once :: Eq sigma => sigma -> DetAutomata LiteralState sigma
det_lit_once c = DetAutomata Lit_SInitial (== Lit_SFinal) f
    where f (Lit_SInitial, c') | c' == c   = Lit_SFinal
                               | otherwise = Lit_SFailed
          f (_ , _) = Lit_SFailed


det_litsub :: Ord sigma => [sigma] -> DetAutomata LiteralState sigma
det_litsub cs = DetAutomata Lit_SInitial (== Lit_SFinal) f
    where f (Lit_SInitial, c) | c `S.member` zs = Lit_SFinal
                              | otherwise       = Lit_SFailed
          f (_ , _) = Lit_SFailed
          zs = S.fromList cs


det_comb :: (Bool -> Bool -> Bool) ->
            DetAutomata s1 sigma ->
            DetAutomata s2 sigma ->
            DetAutomata (s1,s2) sigma

det_comb op (DetAutomata s1_initial s1_isfinal s1_trans)
            (DetAutomata s2_initial s2_isfinal s2_trans)
    = DetAutomata (s1_initial, s2_initial) (\(s1, s2) -> op (s1_isfinal s1) (s2_isfinal s2)) f
   where f ((s1,s2),sigma) = let s1' = s1_trans (s1,sigma)
                                 s2' = s2_trans (s2,sigma)
                             in (s1',s2')


det_clos :: Ord s => DetAutomata s sigma -> DetAutomata (Set (Maybe s)) sigma
det_clos x = compileNonDet (det_clos' x)

det_clos' :: Ord s => DetAutomata s sigma -> NonDetAutomata (Maybe s) sigma
det_clos' (DetAutomata initial isfinal trans) =
        NonDetAutomata (S.singleton Nothing) isfinal' trans'
    where isfinal' Nothing  = True
          isfinal' (Just s) = isfinal s

          trans' (Just s, sigma)  = S.singleton (Just (trans (s, sigma))) `S.union`
                                        if isfinal s
                                        then S.singleton (Just (trans (initial, sigma)))
                                        else S.empty
          trans' (Nothing, sigma) = S.singleton (Just (trans (initial, sigma)))


det_and,det_or :: DetAutomata s1 sigma -> DetAutomata s2 sigma -> DetAutomata (s1,s2) sigma

det_and = det_comb (&&)
det_or  = det_comb (||)

det_sequ :: (Ord s1, Ord s2) => DetAutomata s1 sigma -> DetAutomata s2 sigma -> DetAutomata (S.Set (Either s1 s2)) sigma
det_sequ x y = compileNonDet (det_sequ' x y)

det_sequ' :: (Ord s1, Ord s2) => DetAutomata s1 sigma -> DetAutomata s2 sigma -> NonDetAutomata (Either s1 s2) sigma
det_sequ' (DetAutomata s1_initial s1_final s1_trans) (DetAutomata s2_initial s2_final s2_trans) =
    NonDetAutomata (S.singleton (Left s1_initial)) final trans
    where final (Left{}) = False
          final (Right s2) = s2_final s2

          trans (Left  s1, sigma) = let s1' = s1_trans (s1, sigma)
                                    in S.singleton (Left s1') `S.union` 
                                            if s1_final s1'
                                            then S.singleton (Right s2_initial)
                                            else S.empty
          trans (Right s2, sigma) = S.singleton (Right (s2_trans (s2, sigma)))



det_not :: DetAutomata s sigma -> DetAutomata s sigma
det_not (DetAutomata initial isfinal trans) =
            DetAutomata initial (not . isfinal) trans


run :: DetAutomata s sigma -> (a -> sigma) -> [a] -> Bool
run (DetAutomata initialState isfinal trans) = \f ->  go initialState . map f where
        go !s [] = isfinal s
        go !s (x : xs) = go (trans (s, x)) xs

run' :: DetAutomataE sigma -> (a -> sigma) -> [a] -> Bool
run' (AutomataE f) = run f

compileNonDet :: Ord state => NonDetAutomata state sigma -> DetAutomata (S.Set state) sigma
compileNonDet (NonDetAutomata initial final trans) =
        DetAutomata initial final' trans'
    where trans' (s, sigma) = S.foldr (\currS rest -> trans (currS, sigma) `S.union` rest) S.empty s
          final' s = S.foldr (\c r -> final c || r) False s
