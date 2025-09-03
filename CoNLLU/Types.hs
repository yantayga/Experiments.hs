{-# LANGUAGE DeriveAnyClass, NoGeneralizedNewtypeDeriving, DerivingStrategies #-}

module CoNLLU.Types where

import GHC.Generics
import qualified Data.Map.Strict as M
import qualified Data.Vector.Strict as V

import Data.Text

import Control.DeepSeq

type Word2Index = M.Map Text WordIndex
type Index2Word = M.Map WordIndex Text
type WordIndex = Int

type CoNLLUSentences = V.Vector CoNLLUSentence

data CoNLLUData = CoNLLUData {
    fileName :: Text,
    sentences :: CoNLLUSentences,
    dictWords :: Word2Index,
    dictWordIxs :: Index2Word,
    tags :: Word2Index,
    tagIxs :: Index2Word,
    startWord :: WordIndex,
    endWord :: WordIndex,
    unkWord :: WordIndex
    } deriving (Show, Generic, NFData)

type CoNLLUWords = V.Vector CoNLLUWord

data CoNLLUSentence = CoNLLUSentence {
        text :: Text,
        items :: CoNLLUWords
    } deriving (Show, Generic, NFData)

type UPOSTagIndex = WordIndex -- https://universaldependencies.org/u/pos/index.html
type XPOSTagIndex = WordIndex

type Features = [(FeatureIndex, FeatureValue)] -- https://universaldependencies.org/u/feat/index.html
type FeatureIndex = WordIndex
type FeatureValue = WordIndex

type DepRelIndex = WordIndex -- https://universaldependencies.org/u/dep/index.html

data CoNLLUWord = CoNLLUWord {
        wordId :: (WordIndex, WordIndex),
        word :: WordIndex,
        initialWord :: WordIndex,
        uposTag :: UPOSTagIndex,
        xposTag :: XPOSTagIndex,
        features :: Features,
        depHead :: WordIndex,
        depRel :: DepRelIndex,
        misc :: Text
    } deriving (Show, Generic, NFData)
