{-# LANGUAGE GADTs #-}
module MyGrep.Common.Types where

import qualified Data.Text as T

newtype ErrorMessage  = ErrorMessage { getError :: T.Text } deriving (Eq,Show)

data LocationType = File | Directory | STDIN deriving (Eq,Show)
data Flags        = Recursive | LineNumber | ShowFile | CaseInSensitive deriving (Eq,Show)

data Args = Args {
    searchTerm         :: T.Text
  , searchLocation     :: Maybe FilePath
  , searchLocationType :: LocationType
  , flags              :: [Flags]
} deriving (Eq,Show)

data ResultLine = ResultLine {
    file :: Maybe FilePath
  , lineNumber :: Maybe Int
  , line :: T.Text
} deriving (Eq)

instance Show ResultLine where
  show (ResultLine (Just f) (Just ln) l) = f <> " " <> show ln <> " : " <> T.unpack l
  show (ResultLine Nothing (Just ln) l) = show ln <> " " <> T.unpack l
  show (ResultLine Nothing Nothing l) = T.unpack l
  show (ResultLine (Just f) Nothing l) = f <> " " <> T.unpack l