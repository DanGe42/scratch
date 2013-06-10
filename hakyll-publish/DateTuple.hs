{-# OPTIONS_GHC -Wall #-}

module DateTuple (
                  -- * Types
                  DateTuple(..),
                  -- * Functions
                  currentLocalTime
                 ) where

import Data.Time.Calendar (toGregorian)
import Data.Time.LocalTime (localDay, zonedTimeToLocalTime, getZonedTime)
import Control.Monad (liftM)
import Util (padLeft)

-- |A type class wrapping a date tuple. The date tuple is formatted as
-- (year, month, day).
newtype DateTuple = DateTuple (Integer, Int, Int)
instance Show DateTuple where
  show (DateTuple (year,month,day)) =
    show year ++ "-" ++ pad (show month) ++ "-" ++ pad (show day)
    where pad = padLeft '0' 2

-- |Retrieves the current local time (determined by the system clock) and
-- returns it as a 'DateTuple'.
currentLocalTime :: IO DateTuple
currentLocalTime =
  -- The $ expression returns a IO (Integer, Int, Int), so we use liftM to wrap
  -- it into a IO (DateTuple ...)
  liftM DateTuple $
    fmap (toGregorian . localDay . zonedTimeToLocalTime) getZonedTime

