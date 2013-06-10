{-# OPTIONS_GHC -Wall #-}

-- |Collection of utility functions.
module Util (stringSplit,
             maybeRead,
             padLeft
            ) where

import Data.Text (pack, unpack, split)
import Data.Maybe (listToMaybe)

-- |This function works exactly how 'Prelude.read' works, except that it returns
-- a 'Maybe'.
--
-- Stolen from "Network.CGI.Protocol" source. Don't know why this isn't in a
-- more general-use utility package
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- |Dead-simple string-splitting using 'Text.split'. Not efficient.
stringSplit :: String -> Char -> [String]
stringSplit s c = map unpack $ split (== c) $ pack s

-- |Pad the left side of a string up to a specified length with the specified
-- character.
padLeft :: Char -> Int -> String -> String
padLeft c len str = replicate (len - length str) c ++ str
