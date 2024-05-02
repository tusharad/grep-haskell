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
import           Control.Concurrent


addInMVar :: MVar () -> IO ()
addInMVar mVar1 = putMVar mVar1 ()

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
        isSearchCompleteVar <- newEmptyMVar
        mapM_ (\file -> forkIO $ go isSearchCompleteVar file) files
        mapM_ (\_ -> takeMVar isSearchCompleteVar) files -- wait for all threads to finish
        where 
          go :: MVar () -> FilePath -> IO ()
          go mVar1 fp = do
            let fullPath = searchLocation' </> fp
            doesDirectoryExist fullPath >>= (\isDir -> if isDir then searchInDirectory fullPath >> addInMVar mVar1
              else searchInFile fullPath >> addInMVar mVar1)

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
