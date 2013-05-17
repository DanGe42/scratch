{-# OPTIONS_GHC -Wall #-}

module Main where

import System.Environment (getArgs, getProgName)
import System.FilePath (takeFileName)

import Data.Maybe (fromMaybe)

import ArgParser (parseArgs)
import Util (stringSplit, maybeRead)
import DateTuple (DateTuple(..), currentLocalTime)
import Types (Title)


main :: IO ()
main = do
  args        <- getArgs
  progName    <- getProgName
  currentDate <- currentLocalTime

  (mDateString, title, fp) <- parseArgs args progName
  let date = fromMaybe currentDate (mDateString >>= parseDateString)
      outputFn = outputFileName date fp

  contents <- readFile fp
  writeFile outputFn $ processContents title contents


parseDateString :: String -> Maybe DateTuple
parseDateString s =
  case stringSplit s ',' of
       [yearStr, monthStr, dayStr] -> do
         year  <- maybeRead yearStr
         month <- maybeRead monthStr
         day   <- maybeRead dayStr
         return $ DateTuple (year, month, day)
       _ -> Nothing

{- Takes the contents of the article and prepends:

   ---
   title: <title>
   ---
 -}
processContents :: Title -> String -> String
processContents title contents =
  "---\n" ++
  "title: " ++ title ++ "\n" ++
  "---\n" ++
  contents

-- Generates the output file name as thus: yyyy-mm-dd-<original filename>
outputFileName :: DateTuple -> FilePath -> FilePath
outputFileName dt fn = show dt ++ "-" ++ takeFileName fn
