-- | NOTE: This exception should only be used to indicate an Hquery bug.
{-# LANGUAGE DeriveDataTypeable #-}
module Text.Hquery.Internal.Error where

import Control.Exception
import Data.Typeable

data HqueryInternalException = HqueryInternalException String
  deriving (Show, Typeable)
instance Exception HqueryInternalException

-- | Unconditionally throw an HqueryInternalException with the specified error
-- message. This should not be used for user errors, just internal hquery
-- errors.
raise :: String -> a
raise = throw . HqueryInternalException
