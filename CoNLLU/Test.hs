{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module Main where

import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.Vector.Strict as V
import Data.Maybe
import Data.Tree
import Control.DeepSeq

import CoNLLU.Types
import CoNLLU.Parse
import CoNLLU.Load

drawDepTree m ss = drawTree $ drawDepTree' m (V.toList (items ss)) Nothing

drawDepTree' m ws w = Node {rootLabel = T.unpack $ fromJust $ M.lookup wid m, subForest = map (drawDepTree' m ws . Just) ws'}
    where
        !root = case w of
            Nothing -> fst $ wordId $ head $ filter ((== 0). depHead) ws
            Just w' -> fst $ wordId w'
        !wid = word (ws !! root)
        !ws' = filter ((== root). depHead) ws

printSentences db ss = do
    mapM_ (\(k, v) -> putStr (show k ++ ":") >> putStrLn (T.unpack v)) $ M.toList m
    --res <- stats
    --print res
    where
        m = invertBijection $ dictWords db
        stats = mapM_ (print . V.foldl' sumAll 0 . items) ss
        sumAll accN w = force $ accN + word w + initialWord w + uposTag w + xposTag w + depHead w + depRel w


main = do
    !db <- loadDirectory "../conllu/"
    print "Loaded..."
    printSentences db $ sentences db
    print db
