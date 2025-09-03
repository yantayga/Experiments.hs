{-# LANGUAGE NoGeneralizedNewtypeDeriving, DerivingStrategies, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module CoNLLU.Training where

import qualified Data.Map as M

import POS.HMM.Types
import POS.HMM.Matrix
import POS.HMM.Training

import CoNLLU.Types

collectHiddensCoNLLU :: (Fractional p, Matrix m Double) => CoNLLUData -> m
collectHiddensCoNLLU d = foldl collectHidden (empty hiddenSize hiddenSize 0.0) hiddenStates
    where
        hiddenSize = M.size $ uPOSTags d
        hiddenStates = map (map uposTag . items) $ sentences d

collectEmissionsCoNLLU :: (Fractional p, Matrix m Double) => CoNLLUData -> m
collectEmissionsCoNLLU d = foldl collectEmissions (empty hiddenSize emissionsSize 0.0) states
    where
        hiddenSize = M.size $ uPOSTags d
        emissionsSize = M.size $ fullWords d
        states = map ((\x -> zip (map uposTag x) (map word x)). items) $ sentences d

