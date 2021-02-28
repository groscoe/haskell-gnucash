module Gnucash.Utils where

import Control.Arrow (second, (***))
import Data.Decimal (roundTo)
import Data.Time
  ( Day,
    ParseTime,
    UTCTime,
    defaultTimeLocale,
    formatTime,
    parseTimeOrError,
  )
import Gnucash.Types (Quantity (..))

-- TODO: handle wrong dates correctly
readTimestamp :: ParseTime t => String -> String -> t
readTimestamp = parseTimeOrError True defaultTimeLocale

gnucashDatetimeFormat :: String
gnucashDatetimeFormat = "%Y-%m-%d %H:%M:%S %z"

readGnuCashTimestampFormat :: String -> UTCTime
readGnuCashTimestampFormat = readTimestamp "%Y-%m-%d %H:%M:%S %z"

readGnuCashDateFormat :: String -> Day
readGnuCashDateFormat = readTimestamp "%Y-%m-%d"

formatDatetimeForGnucash :: UTCTime -> String
formatDatetimeForGnucash = formatTime defaultTimeLocale gnucashDatetimeFormat

-- | GnuCash quantities are expressed in multiples of an SCU (
-- Smallest Commodity Unit). For instance, "2.34" with an SCU of
-- 100 would be "234/100"
formatQuantityForGnucash :: Quantity -> String
formatQuantityForGnucash Quantity {..} =
  show (roundTo 0 $ quantityAmount * fromInteger quantitySCU)
    <> "/"
    <> show quantitySCU

readGnuCashQuantity :: String -> Quantity
readGnuCashQuantity quantity =
  let (fractionalQuantityAmount, quantitySCU) = (read *** read) $ splitOn '/' quantity
      quantityAmount = fractionalQuantityAmount / fromInteger quantitySCU
   in Quantity {..}
  where
    splitOn :: Eq a => a -> [a] -> ([a], [a])
    splitOn sep = second tail . span (/= sep)
