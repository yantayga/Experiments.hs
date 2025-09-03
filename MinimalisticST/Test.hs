{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Test where

data MSTag = U -- Unknown
           | C -- 
           | T -- tense
           | N -- noun
           | V -- verb
           | P -- preposition
           | A -- adjective
           | Adv -- adverb
           | Det -- determonative
           deriving (Eq, Ord, Enum, Show)

data MSTree = MSPhrase MSTag MSTree MSTree
            | MSWord MSTag String
             deriving (Eq, Ord)

instance Show MSTree where
    show (MSPhrase t t1 t2) = "[" ++ show t ++ "P " ++ show t1 ++ " " ++ show t2 ++ "]"
    show (MSWord t s) = "[" ++ show t ++ " " ++ show s ++ "]"
