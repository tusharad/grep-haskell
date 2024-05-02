 {-# LANGUAGE RecordWildCards #-}
module MyGrep.Search (search) where

import           MyGrep.Common.Types
import           MyGrep.Common.Utils (trim,fancyPrint)
import qualified Data.Text.IO as T
import qualified Data.Text as T
import           System.Directory
import           Data.Text.Internal.Search
import           Data.Maybe (fromMaybe)
import           Control.Exception
import           System.FilePath

search :: Args -> IO ()
search Args{..} = do
  case searchLocationType of
        File      -> searchInFile $ fromMaybe "" searchLocation
        Directory -> searchInDirectory $ fromMaybe "" searchLocation
        STDIN     -> T.getContents >>= (\content -> searchString 0 "" (T.lines content))
  where
      searchInDirectory :: FilePath -> IO ()
      searchInDirectory searchLocation' = do
        files_ <- listDirectory searchLocation'
        let files = filter (\x -> head x /= '.') files_   -- filtering out hidden directories
        mapM_ go files
        where 
          go :: FilePath -> IO ()
          go fp = do
            let fullPath = searchLocation' </> fp
            doesDirectoryExist fullPath >>= (\isDir -> if isDir then searchInDirectory fullPath 
              else searchInFile fullPath)

      searchInFile :: FilePath -> IO ()
      searchInFile fileName = do
        eContent <- try $ T.lines <$> T.readFile fileName 
          :: IO (Either IOError [T.Text])
        case eContent of
          Left _        -> pure () -- print (e :: IOError)
          Right content -> searchString 0 fileName content

      searchString :: Int -> FilePath -> [T.Text] -> IO ()
      searchString _ _ [] = pure ()
      searchString n fileName (hayStack:hayStacks) = do
        let res = if CaseInSensitive `elem` flags 
            then indices (T.toLower searchTerm) (T.toLower hayStack) 
            else indices searchTerm hayStack
        case res of
            [] -> searchString (n+1) fileName hayStacks
            _  -> do
              let 
                resultLine = ResultLine {
                                file=Just fileName
                              , lineNumber= if LineNumber `elem` flags 
                                then Just n else Nothing
                              , line=trim hayStack
                            }
              fancyPrint resultLine
              searchString (n+1) fileName hayStacks
