{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module MyGrep.Core where

import           MyGrep.Common.Types
import           MyGrep.Common.Utils
import           MyGrep.Search
import qualified System.Environment as Env
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory

getArguments :: IO (Either ErrorMessage [String])
getArguments = do
  args <- Env.getArgs
  if null args then pure (Left $ ErrorMessage "Please provide arguments :(") else pure (Right args)

mkArgs :: [String] -> IO (Either ErrorMessage Args)
mkArgs args = case splitFlagsTerms of
    (_,[]) -> pure $ Left (ErrorMessage "Not Enough Arguments") -- TODO: If no 'FILE' is given, take input from STDIN
    (flags,[term]) -> pure $ Right $ Args {searchTerm =T.pack term,searchLocation=Nothing,searchLocationType=STDIN,flags=getFlags flags}
    (flags, searchTerm':searchLocation':_) -> go searchTerm' searchLocation' flags
  where
    splitFlagsTerms = (,) (filter ((=='-').head) args) (filter ((/='-').head) args)
    getFlags = map (\str -> case str of
      "-r" -> Recursive
      "-n" -> LineNumber
      "-f" -> ShowFile
      "-i" -> CaseInSensitive
      _    -> Recursive
      )

    
    go searchTerm searchLocation flags = do
      isDirectory <- doesDirectoryExist searchLocation
      isFile <- doesFileExist searchLocation
      if isDirectory then pure $ Right $ Args {searchTerm =T.pack searchTerm,searchLocation=Just searchLocation,searchLocationType=Directory,flags=getFlags flags}
        else if isFile then pure $ Right $ Args {searchTerm =T.pack searchTerm,searchLocation=Just searchLocation,searchLocationType=File,flags =getFlags flags}
          else pure $ Left (ErrorMessage $ "Search Location not valid: " <> T.pack searchLocation)

parseArguments :: IO (Either ErrorMessage Args)
parseArguments = do
  args <- getArguments
  case args of
    Left e -> pure $ Left e
    Right r -> do
      arg <- mkArgs r
      case arg of
        Left e -> pure $ Left e
        Right res -> pure $ Right res

main' :: IO ()
main' = do
  eArg <- parseArguments
  case eArg of
    Left err -> T.putStrLn $ "Error: " <> getError err
    Right arg -> do
      res <- search arg
      mapM_ fancyPrint res
