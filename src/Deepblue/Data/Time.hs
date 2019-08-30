{-# LANGUAGE DeriveGeneric, OverloadedStrings, OverloadedLabels, FlexibleInstances #-}
module Deepblue.Data.Time ( UTC
                          , parseISOtime
                          , timeDiffSeconds
                          , formatUTC
                          ) where

import qualified Data.Text as T

import Data.Time.Clock
import Data.Time.Format
import Data.Time.Format.ISO8601
import Data.Fixed

type UTC = UTCTime

-- | Parse ISO format timestamp to UTCTime
{- INLINE -}
parseISOtime :: T.Text -> Maybe UTC
parseISOtime = iso8601ParseM . T.unpack

timeDiffSeconds :: UTC -> UTC -> Pico
timeDiffSeconds a b = nominalDiffTimeToSeconds $ diffUTCTime a b

--formatUTC
formatUTC :: UTC -> String
formatUTC = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%SZ")
