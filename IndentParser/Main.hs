module Main where

import Text.Parsec hiding (State)
import Text.Parsec.Indent
import Control.Monad.State
import Text.Parsec (optional, many)
import qualified Data.Text as T
import Data.Time
import Data.Maybe

import Types

-----------------------------------------------------------------
-- Run parser
-----------------------------------------------------------------

iParse :: IParser a -> SourceName -> String -> Either ParseError a
iParse aParser source_name input =
    runIndentParser aParser () source_name input

-----------------------------------------------------------------
-- PrimitiveTemplate parsers
-----------------------------------------------------------------

-- Parser for quoted strings
noncommentStringParser :: IParser String
noncommentStringParser = many (noneOf "\n#")

-- Parser for dates
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%d.%m.%Y"

dateParser :: IParser (Maybe UTCTime)
dateParser = do
    strDate <- noncommentStringParser
    spaces
    optional commentParser
    return $ parseBasicDate strDate

-- Parser for integers
integerParser :: IParser Integer
integerParser = do
    sign <- option "" (string "-" <|> string "+")
    integerPart <- many1 digit
    return $ read (sign ++ integerPart)

-- Parser for doubles
doubleParser :: IParser Double
doubleParser = do
    sign <- option "" (string "-" <|> string "+")
    integerPart <- many1 digit
    fractionalPart <- option "" (char '.' >> many1 digit)
    scientificPart <- option "" scientific
    return $ read (sign ++ integerPart ++ ('.':fractionalPart))
      where
        scientific = do
            expChar <- oneOf "eE"
            expSign <- option '+' (char '-' <|> char '+')
            expDigits <- many1 digit
            return (expChar:expSign:expDigits)

-- Parser for comments (lines starting with #)
commentParser :: IParser ()
commentParser = do
    char '#'
    content <- many (noneOf "\n")
    spaces
    return ()

manyCommentsParser :: IParser [()]
manyCommentsParser = many commentParser

commaSeparatedparser :: IParser a -> IParser [a]
commaSeparatedparser parser = sepBy parser (char ',')

-- Parser for variable names
variableName :: IParser VariableName
variableName = do
    qm <- option "" (string "?")
    name <- many (alphaNum <|> char '_')
    return $ VariableName (qm /= "") name

-----------------------------------------------------------------
-- More complicated parsers
-----------------------------------------------------------------

fieldNameParser :: Name -> IParser String
fieldNameParser name = string name <* char ':' <* spaces <* optional commentParser

fieldDataParser :: Name -> IParser a -> IParser a
fieldDataParser name parser = do
    _ <- string name
    char ':'
    spaces
    value <- parser
    spaces
    optional commentParser
    return value

fieldDataValueParser :: IParser a -> IParser b -> IParser (a, b)
fieldDataValueParser nameParser valueParser = do
    name <- nameParser
    char ':'
    spaces
    value <- valueParser
    spaces
    optional commentParser
    return (name, value)

fieldParser :: Name -> IParser a -> IParser [a]
fieldParser name parser = withPos $ do
    term <- fieldNameParser name
    subs <- many $ indented *> parser
    return subs

indentedfieldDataParser :: Name -> IParser a -> IParser a
indentedfieldDataParser name parser = indented *> fieldDataParser name parser

indentedFieldOptionalDataParser :: Name -> IParser a -> a -> IParser a
indentedFieldOptionalDataParser name parser defaultvalue = option defaultvalue $ indented *> fieldDataParser name parser

indentedfieldDataValueParser :: IParser a -> IParser b -> IParser (a, b)
indentedfieldDataValueParser nameParser valueParser = indented *> fieldDataValueParser nameParser valueParser

indentedfieldOptionalDataValueParser :: IParser a -> IParser b -> (a, b) -> IParser (a, b)
indentedfieldOptionalDataValueParser nameParser valueParser defaultvalue = option defaultvalue $ indented *> fieldDataValueParser nameParser valueParser

indentedFieldParser:: Name -> IParser a -> IParser [a]
indentedFieldParser name parser = indented *> fieldParser name parser

indentedFieldOptionalParser:: Name -> IParser a -> IParser [a]
indentedFieldOptionalParser name parser = option [] $ indented *> fieldParser name parser

-----------------------------------------------------------------
-- Top parsers
-----------------------------------------------------------------

localVariableParser :: IParser LocalVariable
localVariableParser  = withPos $ do
    (name, value) <- fieldDataValueParser variableName noncommentStringParser
    return $ LocalVariable name value

conditionParser :: IParser Condition
conditionParser = withPos $ noncommentStringParser <* spaces <* optional commentParser

ruleParser :: PrimitiveTemplates -> IParser Rule
ruleParser primitives = withPos $ do
    _ <- fieldNameParser "rule"
    comment <- indentedFieldOptionalDataParser "comment" noncommentStringParser ""
    match <- indentedfieldDataParser "match" noncommentStringParser
    score <- indentedFieldOptionalDataParser "score" doubleParser 1.0
    locals <- indentedFieldOptionalParser "locals" localVariableParser
    conditions <- indentedFieldOptionalParser "conditions" conditionParser
    further <- indentedFieldOptionalDataParser "further" noncommentStringParser ""
    primitives <- indentedFieldOptionalParser "primitives" pTaxonomy
    actions <- indentedFieldOptionalDataParser "actions" noncommentStringParser ""
    return $ Rule comment score match further (LocalVariables locals) conditions primitives actions

rulesParser :: PrimitiveTemplates -> IParser Rules
rulesParser primitives = do
    rules <- fieldParser "rules" $ ruleParser primitives
    return $ Rules rules

primitiveTemplateParser :: IParser PrimitiveTemplate
primitiveTemplateParser = withPos $ do
    _ <- fieldNameParser "primitive"
    name <- indentedfieldDataParser "name" noncommentStringParser
    fields <- indentedfieldDataParser "fields" $ commaSeparatedparser noncommentStringParser
    return $ PrimitiveTemplate name fields

primitivesParsers :: IParser PrimitiveTemplates
primitivesParsers = withPos $ do
    primitives <- fieldParser "primitives" primitiveTemplateParser
    return $ PrimitiveTemplates primitives

aCDDBParsers :: IParser CDDB
aCDDBParsers = do
    many commentParser
    name <- fieldDataParser "name" noncommentStringParser
    version <- fieldDataParser "version" integerParser
    date <- fieldDataParser "date" dateParser
    primitives <- primitivesParsers
    rules <- rulesParser primitives
    return $ CDDB name version date primitives rules

-----------------------------------------------------------------
-- Top top parser
-----------------------------------------------------------------

parseCDDB :: FilePath -> String -> Either ParseError CDDB
parseCDDB path content = iParse aCDDBParsers path content


-------------------
pTerm :: IParser String
pTerm = noncommentStringParser <* spaces

pTaxonomy :: IParser Taxonomy
pTaxonomy = withPos $ do
    term <- pTerm
    subs <- many $ indented *> pTaxonomy
    return $ Taxonomy term subs