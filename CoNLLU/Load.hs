{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, OverloadedStrings #-}

module CoNLLU.Load where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Text.IO as TIO

import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath (combine)

import Control.Monad (foldM)
import Control.DeepSeq
import Control.Parallel.Strategies

import CoNLLU.Types
import CoNLLU.Parse
import CoNLLU.Merge

loadCoNLLU :: CoNLLUData -> FilePath -> IO (Maybe CoNLLUData)
loadCoNLLU db fn = do
    content <- TIO.readFile fn
    return $ parseCoNLLU (db {fileName = T.pack fn}) content

loadDirectory :: FilePath -> IO CoNLLUData
loadDirectory fp = do
    fs <- getFiles fp
    res <- loadParallel fs
    res2 <- sequenceA res
    return $ mergeData res2
    where
        loadParallel fs = map (loadCoNLLUWithLog emptyDB) fs `usingIO` parList rseq
        loadCoNLLUWithLog db fn = do
            maybeDB <- loadCoNLLU db fn
            return $ case maybeDB of
                Nothing -> db
                Just db' -> db'
        getFiles fp  = do
            fp' <- canonicalizePath fp
            ed <- doesDirectoryExist fp'
            ef <- doesFileExist fp'
            if ed
            then do
                fs <- listDirectory fp'
                sfs <- mapM (getFiles . combine fp') fs
                return $ concat sfs
            else if ef then return [fp'] else return []

