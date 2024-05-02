 {-# LANGUAGE RecordWildCards #-}
module MyGrep.Search (search) where

import           MyGrep.Common.Types
import           MyGrep.Common.Utils (trim)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           System.Directory
import           Data.Text.Internal.Search
import           Data.Maybe (fromMaybe)
import           Control.Exception
import           System.FilePath

search :: Args -> IO [ResultLine]
search Args{..} = do
  case searchLocationType of
        File      -> searchInFile $ fromMaybe "" searchLocation
        Directory -> searchInDirectory $ fromMaybe "" searchLocation
        STDIN     -> searchString 0 "" . T.lines <$> T.getContents
        _         -> pure [] -- impossible case
  where
      searchInDirectory :: FilePath -> IO [ResultLine]
      searchInDirectory searchLocation' = do
        files_ <- listDirectory searchLocation'
        let files = filter (\x -> head x /= '.') files_   -- filtering out hidden directories
        mconcat $ map (\file -> do
          res <- doesDirectoryExist (searchLocation' </> file)
          (if res then searchInDirectory (searchLocation' </> file) else searchInFile (searchLocation' </> file))
          ) files

      searchInFile :: FilePath -> IO [ResultLine]
      searchInFile fileName = do
        eContent <- try $ T.lines <$> T.readFile fileName :: IO (Either IOError [T.Text])
        case eContent of
          Left _        -> pure [] -- print (e :: IOError)
          Right content -> pure $ searchString 0  fileName content

      searchString :: Int -> FilePath -> [T.Text] -> [ResultLine]
      searchString _ _ [] = []
      searchString n fileName (hayStack:hayStacks) = do
        let res = if CaseInSensitive `elem` flags then indices (T.toLower searchTerm) (T.toLower hayStack) else indices searchTerm hayStack
        case res of
            [] -> searchString (n+1) fileName hayStacks
            _  -> ResultLine {
                    file=Just fileName
                  , lineNumber=if LineNumber `elem` flags then Just n else Nothing
                  , line=trim hayStack
                } : searchString (n+1) fileName hayStacks
