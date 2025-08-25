module Types where

import Text.Parsec.Indent
import Data.Time

type IParser a = IndentParser String () a

-------------------
type Term = String
data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)
-------------------

type Name = String
type Version = Integer
type Date = Maybe UTCTime
type Comment = String
type Score = Double
type Match = String
type Actions = String
type Further = String
type Nullable = Bool
type Condition = String

type Primitives = [Taxonomy]
type Conditions = [Condition]

data VariableName = VariableName Nullable Name deriving (Eq, Show)

data LocalVariable = LocalVariable VariableName Name deriving (Eq, Show)

data LocalVariables = LocalVariables [LocalVariable] deriving (Eq, Show)

data Rule = Rule Comment Score Match Further LocalVariables Conditions Primitives Actions deriving (Eq, Show)

data Rules = Rules [Rule] deriving (Eq, Show)

data PrimitiveTemplate = PrimitiveTemplate Name [Name] deriving (Eq, Show)

data PrimitiveTemplates = PrimitiveTemplates [PrimitiveTemplate] deriving (Eq, Show)

data CDDB = CDDB Name Version Date PrimitiveTemplates Rules deriving (Eq, Show)