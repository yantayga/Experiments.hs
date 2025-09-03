module DepTreeBuilder where

import qualified Data.Map as M
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Tree

type FeatureName = String
type FeatureValue = String
type FeatureDef = (FeatureName, FeatureValue)

data Word = Word {
        word :: String,
        features :: [FeatureDef]
    } deriving (Eq, Ord, Show)

type Rules = [Rule]

data RuleSearch = RuleSearch String [Restriction] deriving (Eq, Ord, Show)

data Rule = Rule {
        level :: Int,
        searches :: [RuleSearch]
    } deriving (Eq, Ord, Show)

data Restriction = ExactFeature FeatureName FeatureValue
    | Agreement FeatureName
    deriving (Eq, Ord, Show)

data DepTree = DepTree String [DepTree]
    | Terminal String
    deriving (Eq, Ord, Show)

drawDepTree dt = drawTree $ treeFromDepTree dt
    where
        treeFromDepTree (DepTree s sdt) = Node {rootLabel = s, subForest = map treeFromDepTree sdt}
        treeFromDepTree (Terminal s) = Node {rootLabel = s, subForest = []}

topRules :: Rules
topRules = [
        Rule 8 [RuleSearch "headSConj" [ExactFeature "pos" "sconj"]],
        Rule 5 [RuleSearch "headV" [ExactFeature "pos" "V"]]
    ]
 
wordRules = [
        (
            [("pos", "V"), ("type", "transitive")], 
            [
                RuleSearch "subj" [ExactFeature "pos" "N", ExactFeature "case" "nom", Agreement "num"], 
                RuleSearch "obj"  [ExactFeature "pos" "N", ExactFeature "case" "acc", Agreement "num"]
            ]
        )
    ]

filterFowWord w ws = [map (filterFowWord' w ws) r | r <- rulesForWord w]

filterFowWord' w ws (RuleSearch role restr) = [(role, w') | w' <- ws, all (isRestrictionOk w w') restr]

rulesForWord w = map snd $ filter (checkRule w) wordRules

checkRule w (conds, _) = conds `isSubsequenceOf` (features w)

isRestrictionOk _       wChild (ExactFeature n v) = (n, v) `elem` (features wChild)
isRestrictionOk wParent wChild (Agreement n) = lookup n (features wParent) == lookup n (features wChild)

w_puer = Word "puer" [("pos", "N"), ("num", "sing"), ("case", "nom")]
w_puellam = Word "puellam" [("pos", "N"), ("num", "sing"), ("case", "acc")]
w_amat = Word "amat" [("pos", "V"), ("num", "sing"), ("type", "transitive")]

testWS = [w_puellam, w_puer, w_amat]