{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}

module CoNLLU.Merge where

import qualified Data.Map.Strict as M
import qualified Data.Vector.Strict as V
import qualified Data.Text as T
import Data.Maybe (fromMaybe)

import CoNLLU.Types
import CoNLLU.Parse

mergeData :: [CoNLLUData] -> CoNLLUData
mergeData [db] = db
mergeData dbs = CoNLLUData {
            fileName = "merged",
            dictWords = mergedDictWords,
            dictWordIxs = mergedWordIxs,
            tags = mergedTags,
            tagIxs = mergedTagIxs,
            sentences = reindexSentences unkWordIdx mergedDictWords mergedTags $ zip3 (map sentences dbs) (map dictWordIxs dbs) (map tagIxs dbs),
            startWord = mergedDictWords M.! fst startTag,
            endWord = mergedDictWords M.! fst endTag,
            unkWord = unkWordIdx
        }
    where
        mergedDictWords = mergeKeys $ map dictWords dbs
        mergedTags = mergeKeys $ map tags dbs
        mergedWordIxs = invertBijection mergedDictWords
        mergedTagIxs = invertBijection mergedTags
        unkWordIdx = mergedDictWords M.! fst unkTag

mergeKeys :: [Word2Index] -> Word2Index
mergeKeys wis = snd $ M.mapAccum (\acc _ -> (acc + 1, acc + 1)) 0 (M.unions wis)

reindexSentences :: WordIndex -> Word2Index -> Word2Index -> [(CoNLLUSentences, Index2Word, Index2Word)] -> CoNLLUSentences
reindexSentences unkIx w2ix t2ix = foldl' (reindexSentences' unkIx w2ix t2ix) V.empty
    where
        reindexSentences' :: WordIndex -> Word2Index -> Word2Index -> CoNLLUSentences -> (CoNLLUSentences, Index2Word, Index2Word) -> CoNLLUSentences
        reindexSentences' unkIx w2ix t2ix acc (ss, i2words, i2tags) = acc V.++ V.map (reindexSentence unkIx w2ix t2ix i2words i2tags) ss

reindexSentence :: WordIndex -> Word2Index -> Word2Index -> Index2Word -> Index2Word -> CoNLLUSentence -> CoNLLUSentence
reindexSentence unkIx w2ix t2ix i2words i2tags s = s {items = reindexWords unkIx w2ix t2ix i2words i2tags (items s)}

reindexWords :: WordIndex -> Word2Index -> Word2Index -> Index2Word -> Index2Word -> CoNLLUWords -> CoNLLUWords
reindexWords unkIx w2ix t2ix i2words i2tags = V.map (reindexWord unkIx w2ix t2ix i2words i2tags)

reindexWord :: WordIndex -> Word2Index -> Word2Index -> Index2Word -> Index2Word -> CoNLLUWord -> CoNLLUWord
reindexWord unkIx w2ix t2ix i2words i2tags w = w {
        wordId = (reindexWs $ fst $ wordId w, 0),
        word = reindexWs $ word w,
        initialWord = reindexWs $ initialWord w,
        uposTag = reindexTs $ uposTag w,
        xposTag = reindexTs $ xposTag w,
        features = map (\(a, b) -> (reindexTs a, reindexTs b)) $ features w,
        depRel = reindexTs $ depRel w
    }
    where
        reindexWs = reindex unkIx w2ix i2words
        reindexTs = reindex unkIx t2ix i2tags

reindex :: WordIndex -> Word2Index -> Index2Word -> WordIndex -> WordIndex
reindex unkIx w2i i2w ix = case M.lookup ix i2w of
    Nothing -> unkIx
    Just w -> fromMaybe unkIx (M.lookup w w2i)
