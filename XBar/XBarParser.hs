module XBarParser where

import Data.List (isInfixOf, findIndex)
import Data.Maybe (fromJust)

-- Token: (word, POS, case, valency)
type Token = (String, String, String, String)

-- XBar Tree: head, specifier, complement
data XBar = XBar {
    headToken :: Token,
    specifier :: Maybe XBar,
    complement :: Maybe XBar
} deriving (Show)

-- Find index of a token by word
findIndexByWord :: String -> [Token] -> Maybe Int
findIndexByWord word = findIndex (\(w, _, _, _) -> w == word)

-- Check if a token is a conjunction (e.g., ""et"")
isConjunction :: Token -> Bool
isConjunction (_, ""CC"", _, _) = True -- ""CC"" = conjunction
isConjunction _ = False

-- Check if a token is a relative pronoun (e.g., ""qui"", ""quae"", ""quod"")
isRelativePronoun :: Token -> Bool
isRelativePronoun (_, ""WP"", _, _) = True -- ""WP"" = wh-pronoun (relative)
isRelativePronoun _ = False

-- Check if a verb is transitive (valency 2)
isTransitive :: Token -> Bool
isTransitive (_, ""VB"", _, val) = val == ""2""
isTransitive _ = False

-- Parse a list of tokens into an XBar tree
parseXBar :: [Token] -> XBar
parseXBar tokens = case findHeads tokens of
    [] -> error ""No head found""
    (headToken:_) -> XBar headToken specifier complement
    where
        (relBar, remaining) = handleRelative tokens
        specifier = handleSpecifier remaining
        complement = handleComplement remaining

-- Find potential heads (nouns, verbs)
findHeads :: [Token] -> [Token]
findHeads = filter (\(_, pos, _, _) -> pos `elem` [""NN"", ""NNS"", ""VB"", ""VBD"", ""VBN""])

-- Handle relative clauses (e.g., ""qui videt"")
handleRelative :: [Token] -> (XBar, [Token])
handleRelative tokens = case findIndex isRelativePronoun tokens of
    Just idx -> 
        let (clause, rest) = splitAt idx tokens
            relBar = parseXBar clause
        in (relBar, rest)
    Nothing -> (XBar (head tokens) Nothing Nothing, tokens)

-- Find specifiers (e.g., determiners with case ""NOM"")
handleSpecifier :: [Token] -> Maybe XBar
handleSpecifier tokens = case [token | token@(word, pos, case', _) <- tokens, 
                                    pos == ""DT"", case' == ""NOM""] of
    [] -> Nothing
    [dt] -> Just $ XBar dt Nothing Nothing
    _ -> error ""Multiple specifiers found""

-- Find complements (e.g., accusative nouns or prepositional phrases)
handleComplement :: [Token] -> Maybe XBar
handleComplement tokens = 
    case [token | token@(w, pos, case', val) <- tokens, 
                (pos == ""NN"" || pos == ""NNS"") && case' == ""ACC""] of
        [] -> handlePPComplement tokens
        [accNoun] -> Just $ XBar accNoun Nothing Nothing
        _ -> error ""Multiple accusative nouns found""

-- Handle prepositional phrases (PPs) as complements
handlePPComplement :: [Token] -> Maybe XBar
handlePPComplement tokens = case findIndexByWord ""cum"" tokens of
    Just idx -> 
        let (pp, rest) = splitAt (idx + 1) tokens
            ppBar = parseXBar pp
        in Just ppBar
    Nothing -> Nothing
    
- Example input: Latin sentence with tokens
exampleTokens :: [Token]
exampleTokens = [
    (""Puer"", ""NN"", ""NOM"", ""1""),
    (""qui"", ""WP"", ""NOM"", ""1""),
    (""videt"", ""VB"", ""NOM"", ""2""),
    (""puellam"", ""NNS"", ""ACC"", ""1""),
    (""et"", ""CC"", ""NOM"", ""1""),
    (""puellam"", ""NNS"", ""ACC"", ""1"")
]

-- Parse the example
main :: IO ()
main = print $ parseXBar exampleTokens