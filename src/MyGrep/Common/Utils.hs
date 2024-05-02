{-# LANGUAGE OverloadedStrings #-}
module MyGrep.Common.Utils where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Char
import           MyGrep.Common.Types
import           Text.Colour
import           Data.Maybe

trim :: T.Text -> T.Text
trim = T.dropWhile isSpace . T.dropWhileEnd isSpace

fancyPrint :: ResultLine -> IO ()
fancyPrint rLine = T.putStrLn $ fileName <> "\t:: " <> lineNumber' <> "\t" <> line'
    where
        fileName = renderChunkText With8Colours $ fore brightMagenta $ chunk $ T.pack fName
        lineNumber' = renderChunkText With8Colours $ fore brightGreen $ chunk $ T.pack lName
        line' = renderChunkText With8Colours $ fore brightYellow $ chunk $ getL rLine
        fName = fromMaybe "" (file rLine)
        lName = maybe "" show (lineNumber rLine)
        getL ResultLine{line=ll} = ll
