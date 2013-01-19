-- | NOTE: This exception should only be used to indicate an Hquery bug.
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Hquery.Internal.Error where

import Control.Exception
import Data.Typeable

data HqueryInternalException = HqueryInternalException String
  deriving (Show, Typeable)
instance Exception HqueryInternalException

raise :: String -> a
raise = throw . HqueryInternalException
