{-# OPTIONS_GHC -Wall #-}

-- |A single-purpose library for parsing arguments provided to the
-- publish-markdown program.
module ArgParser (parseArgs) where

import System.Console.GetOpt
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure, exitSuccess)

import Data.List (find)

import Types (Title)

-- Valid argument flags
data Flag = PubDate String
          | Help
          deriving (Eq, Ord, Show)

-- Flag representation for the getopt library
flags :: [OptDescr Flag]
flags =
  [ Option ['d'] ["date"]   (ReqArg PubDate "DATE") (
      "Sets the publication date with a comma-separated yyyy,mm,dd string.\n" ++
      "If this parameter is not provided, the current local date will be used."
      )
  , Option []    ["help"]   (NoArg Help)
      "Print this help message"
  ]

-- |Takes a list of string arguments and parses it into a tuple in which:
--
-- * The first value is the date string provided. It is 'Nothing' if the 
--   argument was not provided.
-- * The second value is the title of the article
-- * The third value is the path to the article file
--
-- The second argument to this function is the name of this program, which is
-- useful for the usage hint message.
--
-- Structure of this function adapted from:
-- <http://www.haskell.org/haskellwiki/Tutorials/Programming_Haskell/Argument_handling#Parsing_the_flags>
parseArgs :: [String] -> String -> IO (Maybe String, Title, FilePath)
parseArgs argv progName =
  case getOpt Permute flags argv of
       (args, [title, file], []) ->
         if Help `elem` args
            then do hPutStrLn stderr (usageInfo shortHelp flags)
                    exitSuccess
         else
            return (getDateFromArgs args, title, file)
       (_, _, errs) -> do
         hPutStrLn stderr (concat errs ++ usageInfo shortHelp flags)
         exitFailure

  where shortHelp = "Usage: " ++ progName ++ " [-d yyyy,mm,dd] title file"
        getDateFromArgs args =
          case find datePred args of
               Just (PubDate s) -> Just s
               _ -> Nothing
          where
            datePred (PubDate _) = True
            datePred _           = False

