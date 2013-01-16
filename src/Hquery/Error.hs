{-# LANGUAGE DeriveDataTypeable #-}
module Hquery.Error where

import Control.Exception
import Data.Typeable

data HqueryInternalException = HqueryInternalException String
  deriving (Show, Typeable)
instance Exception HqueryInternalException

raise :: String -> a
raise = throw . HqueryInternalException
