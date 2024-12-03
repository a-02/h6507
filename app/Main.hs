{-# LANGUAGE BinaryLiterals #-} 
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Lens
import Data.List (intersperse)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Maybe
import System.Environment
import System.IO
import Numeric.Lens

main :: IO ()
main = do
  arg <- listToMaybe <$> getArgs
  let (a :: Int) = read $ fromJust arg
  putChar '\n'
  TIO.hPutStrLn stdout (displayConversions a)

displayConversions :: Int -> T.Text
displayConversions int =
  renderTable (
    MarkdownTable 
      { headers = conversionHeaders
      , rows = [baseRow int]
      } 
  )

data MarkdownTable = MarkdownTable
  { headers :: [T.Text]
  , rows :: [[T.Text]]
  }

conversionHeaders :: [T.Text]
conversionHeaders = 
  [" binary "," octal "," decimal "," hexadecimal "]

baseRow :: Int -> [T.Text]
baseRow int = T.pack <$>
  [int ^.re binary, int ^.re octal, int ^.re decimal, int ^.re hex]

fitTextSpaces :: Int -> T.Text -> T.Text
fitTextSpaces maxLength txt = 
  let remaining = maxLength - T.length txt
   in case compare remaining 0 of -- whens the last time ive used the Ord type?
        GT -> T.center maxLength ' ' txt
        LT -> T.take maxLength txt
        EQ -> txt

horizontalDivider :: Int -> T.Text
horizontalDivider len = T.replicate len "-"

renderTable :: MarkdownTable -> T.Text
renderTable markdown = 
  let maxLength = maximum $ T.length <$> markdown.headers 
      spacedHeaders = fitTextSpaces maxLength <$> markdown.headers
      renderHeader = T.intercalate "|" spacedHeaders
      (renderRows :: [T.Text]) = fmap (T.intercalate "|" . fmap (fitTextSpaces maxLength)) markdown.rows
      divider = T.intercalate "|" $ replicate (length markdown.headers) (horizontalDivider maxLength)
  in T.unlines . intersperse divider $ renderHeader : renderRows
