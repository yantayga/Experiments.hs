module CDDB.Tree.Syntax where

import GHC.Generics

-- Visualizer  https://mshang.ca/syntree/?i=...
data XType = U -- Unknown
    | C -- 
    | T -- tense
    | N -- noun
    | V -- verb
    | P -- preposition
    | A -- adjective
    | Adv -- Adverb
    | Det
    deriving (Eq, Ord, Enum, Show)

data XPhrase = -- phrase
      XPhrase XType X'
    | XPhraseS XType X X' -- with specifier
data X' = X'X XType X | X'XP XType X XPhrase | X'X'P XType X' XPhrase
data X = X XType String

instance Show XPhrase where
    show (XPhrase t x') = "[" ++ show t ++ "P " ++ show x' ++ "]"
    show (XPhraseS t x x') = "[" ++ show t ++ "P " ++ show x ++ " " ++ show x' ++ "]"

instance Show X' where
    show (X'X t x) = "[" ++ show t ++ "' " ++ show x ++ "]"
    show (X'XP t x xp) = "[" ++ show t ++ "' " ++ show x ++ " " ++ show xp ++ "]"
    show (X'X'P t x' xp) = "[" ++ show t ++ "' " ++ show x' ++ " " ++ show xp ++ "]"

instance Show X where
    show (X t s) = "[" ++ show t ++ " " ++ show s ++ "]"

-- buildTP nv vp = XPhraseS N 

buildAdjA' adj = X'X A (X A adj)

buildAP adj' = XPhrase A adj'

buildAdjNounN' adjP n' = X'X'P N n' adjP

buildAdjsNounN' [adj] n = X'XP N (X N n) (buildAP (buildAdjA' adj))
buildAdjsNounN' (adj:adjs) n = X'X'P N (buildAdjsNounN' adjs n) (buildAP (buildAdjA' adj))
buildAdjsNounN' [] n = X'X N (X N n)

buildPP p det adjs n = buildPP' p (buildNP det (buildAdjsNounN' adjs n))
        
buildPP' prep np = XPhrase P (X'XP P (X P prep) np)

buildNP Nothing n' = XPhrase N n'
buildNP (Just det) n' = XPhraseS N (X Det det) n'

buildV' verb [] [] = X'X V (X V verb)
buildV' verb [obj] [] = X'XP V (X V verb) (np obj)
    where
        np (det, adjs, obj) = buildNP det (buildAdjsNounN' adjs obj)
buildV' verb (obj:objs) [] = X'X'P V (buildV' verb objs []) (np obj)
    where
        np (det, adjs, obj) = buildNP det (buildAdjsNounN' adjs obj)
buildV' verb [] [p] = X'XP V (X V verb) (pp p)
    where
        pp (prep, det, adjs, obj) = buildPP prep det adjs obj
buildV' verb objs (p:ps) = X'X'P V (buildV' verb objs ps) (pp p)
    where
        pp (prep, det, adjs, obj) = buildPP prep det adjs obj

buildVP verb objs ps = XPhrase V (buildV' verb objs ps)

testObj = [(Nothing, ["fresh"], "lobsters")]
testPrep = [("at", Just "the", [], "pier")]

