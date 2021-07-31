module Gnucash.Utils where

import Control.Arrow (second, (***))
import Data.Decimal (roundTo)
import Data.Time
  ( Day,
    ParseTime,
    UTCTime,
    defaultTimeLocale,
    formatTime,
    parseTimeOrError, FormatTime
  )
import Gnucash.Types (Quantity (..))

-- TODO: handle wrong dates correctly
readTimestamp :: ParseTime t => String -> String -> t
readTimestamp = parseTimeOrError True defaultTimeLocale
{-# INLINE readTimestamp #-}

formatTimestamp :: FormatTime t => String -> t -> String
formatTimestamp = formatTime defaultTimeLocale
{-# INLINE formatTimestamp #-}

gnucashDatetimeFormat :: String
gnucashDatetimeFormat = "%Y-%m-%d %H:%M:%S %z"

gnuCashDateFormat :: String
gnuCashDateFormat = "%Y-%m-%d"

readGnuCashTimestampFormat :: String -> UTCTime
readGnuCashTimestampFormat = readTimestamp gnucashDatetimeFormat
{-# INLINE readGnuCashTimestampFormat #-}

readGnuCashDateFormat :: String -> Day
readGnuCashDateFormat = readTimestamp gnuCashDateFormat
{-# INLINE readGnuCashDateFormat #-}

formatDatetimeForGnucash :: UTCTime -> String
formatDatetimeForGnucash = formatTimestamp gnucashDatetimeFormat
{-# INLINE formatDatetimeForGnucash #-}

formatDateForGnuCash :: Day -> String
formatDateForGnuCash = formatTimestamp gnuCashDateFormat
{-# INLINE formatDateForGnuCash #-}

-- | GnuCash quantities are expressed in multiples of an SCU (
-- Smallest Commodity Unit). For instance, "2.34" with an SCU of
-- 100 would be "234/100"
formatQuantityForGnucash :: Quantity -> String
formatQuantityForGnucash Quantity {..} =
  show (roundTo 0 $ quantityAmount * fromInteger quantitySCU)
    <> "/"
    <> show quantitySCU
{-# INLINE formatQuantityForGnucash #-}

readGnuCashQuantity :: String -> Quantity
readGnuCashQuantity quantity =
  let (fractionalQuantityAmount, quantitySCU) = (read *** read) $ splitOn '/' quantity
      quantityAmount = fractionalQuantityAmount / fromInteger quantitySCU
   in Quantity {..}
  where
    splitOn :: Eq a => a -> [a] -> ([a], [a])
    splitOn sep = second tail . span (/= sep)

{-# INLINE readGnuCashQuantity #-}