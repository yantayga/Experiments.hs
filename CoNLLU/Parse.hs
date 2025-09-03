{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}
-- Reading colluu data
-- https://universaldependencies.org/format.html
module CoNLLU.Parse where

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Map.Strict as M
import qualified Data.Vector.Strict as V
import Data.List (group)
import Data.List.Extra (split, replace)
import Data.Maybe (fromMaybe)
import Data.Either (fromRight)
import Data.Char (isDigit)

import Control.Monad (foldM)
import Control.DeepSeq

import CoNLLU.Types

emptyDB :: CoNLLUData
emptyDB = CoNLLUData {
                fileName = "",
                sentences = V.empty,
                dictWords = initialIndex,
                dictWordIxs = initialIndexRev,
                tags = initialIndex,
                tagIxs = initialIndexRev,
                startWord = snd startTag,
                endWord = snd endTag,
                unkWord = snd unkTag
            }
    where
        initialIndex = M.fromList tagsPredefined
        initialIndexRev = invertBijection initialIndex

startTag = ("<-start->", 0)
endTag = ("<-end->", 1)
unkTag = ("<-end->", 2)
tagsPredefined = [startTag, endTag, unkTag]

parseCoNLLU :: CoNLLUData -> T.Text -> Maybe CoNLLUData
parseCoNLLU d ss = do
    db <- parseCoNLLUSentences d $ T.lines ss
    return db {
        dictWordIxs = invertBijection $ dictWords db,
        tagIxs = invertBijection $ tags db
        }

parseCoNLLUSentences :: CoNLLUData -> [T.Text] -> Maybe CoNLLUData
parseCoNLLUSentences d ss = foldM parseCoNLLUSentence d $ filter (not . null) $ split T.null ss

parseCoNLLUSentence :: CoNLLUData -> [T.Text] -> Maybe CoNLLUData
parseCoNLLUSentence d ss = force $ case parseWords d wordsLines of
    Just (d', !is) -> return d' {
        sentences = V.cons (CoNLLUSentence {
            text = T.strip $ dropTextPrefix "=" $ fromMaybe "" (lookup "text" params),
            items = V.fromList $ reverse is
            }) (sentences d')
        }
    Nothing -> return d
    where
        (header, wordsLines) = span ("#" `T.isPrefixOf`) ss
        params = map (mapTuple2 T.strip . T.span (/= '=') . dropTextPrefix "#") header

mapTuple2 f (a,b) = (f a, f b)

parseWords :: CoNLLUData -> [T.Text] -> Maybe (CoNLLUData, [CoNLLUWord])
parseWords d = foldM parseWord (d, [])

parseWord :: (CoNLLUData, [CoNLLUWord]) -> T.Text -> Maybe (CoNLLUData, [CoNLLUWord])
parseWord (d, ws) s = if tabsCount < 9 then Just (d, ws) else do
    return (
        d {dictWords = fws', tags = tis'''}, newWord: ws
        )
    where
        tabsCount = length $ filter (== '\t') $ T.unpack s
     --  (n1-n2) idx POS  XPOS features parent role
        (wid: w: iw: opt: xpt: fs:      dp:    drole: _: misc) = T.split (== '\t') s
        (wid1:wid2) = T.split (== '-') wid
        (wIdx, fws) = updateWord2IndexWith (dictWords d) (filterNums w) id
        (iwIdx, fws') = updateWord2IndexWith fws (filterNums iw) id
        (optIdx, tis) = updateWord2IndexWith (tags d) opt T.toUpper
        (xptIdx, tis')  = updateWord2IndexWith tis xpt T.toUpper
        (ifs, tis'') = parseFeatures tis' fs
        (dnIdx, tis''')  = updateWord2IndexWith tis'' drole T.toLower
        newWord = CoNLLUWord {
                wordId = (fst $ fromRight (0,"") $ TR.decimal wid1, -1),
                word = wIdx,
                initialWord = iwIdx,
                uposTag = optIdx,
                xposTag = xptIdx,
                features = ifs,
                depHead = (fst $ fromRight (0,"") $ TR.decimal dp),
                depRel = dnIdx,
                misc = (T.unwords misc)
            }
        filterNums w = T.concat $ replace [""] ["<N>"] $ map head $ group $ T.split isDigit w

parseFeatures :: Word2Index -> T.Text -> (Features, Word2Index)
parseFeatures tis s = foldl' update ([], tis) pairs
    where
        pairs = map (T.span (/= '=')) $ T.split (== '|') s
        update :: (Features, Word2Index) -> (T.Text, T.Text) -> (Features, Word2Index)
        update (fs, tis) (n, v) =
            let (ni, tis') = updateWord2IndexWith tis n T.toLower
                (vi, tis'') = updateWord2IndexWith tis' (dropTextPrefix "=" v) T.toLower
            in ((ni, vi):fs, tis'')

updateWord2IndexWith :: Word2Index -> T.Text -> (T.Text -> T.Text) -> (WordIndex, Word2Index)
updateWord2IndexWith w2i s f = case M.insertLookupWithKey (\_ _ a -> a) (f s) nextIx w2i of
        (Nothing, newMap)  -> (nextIx, newMap)
        (Just oldval, newMap) -> (oldval, newMap)
    where
        nextIx = M.size w2i

dropTextPrefix p s = fromMaybe s $ T.stripPrefix p s

invertBijection :: Word2Index -> Index2Word
invertBijection = M.foldrWithKey (flip M.insert) M.empty
