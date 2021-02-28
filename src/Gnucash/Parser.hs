{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Gnucash.Parser where

import Control.Arrow
  ( Arrow (arr, second, (***)),
    returnA,
    (<<<),
    (>>>),
  )
import Control.Arrow.ArrowIf
  ( ArrowIf (choiceA, orElse),
    IfThen ((:->)),
  )
import Control.Arrow.ArrowList (ArrowList (constA, listA))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text, pack)
import Data.Time
  ( Day,
    ParseTime,
    UTCTime,
    defaultTimeLocale,
    parseTimeOrError,
  )
import Gnucash.Types
import Text.XML.HXT.Core
  ( ArrowTree (deep, getChildren),
    ArrowXml (getText, hasAttrValue, hasName, isElem),
    XmlTree,
  )

-- utilitiess
type Parser b = forall a. ArrowXml a => a XmlTree b

atNamedChild :: String -> Parser XmlTree
atNamedChild tag = getChildren >>> isElem >>> hasName tag

stringAtTag :: String -> Parser String
stringAtTag tag = atNamedChild tag >>> getChildren >>> getText

textAtTag :: String -> Parser Text
textAtTag tag = stringAtTag tag >>> arr pack

timestampAtTag :: String -> Parser UTCTime
timestampAtTag tag = atNamedChild tag >>> stringAtTag "ts:date" >>> arr readGnuCashTimestampFormat

-- TODO: handle wrong dates correctly
readTimestamp :: ParseTime t => String -> String -> t
readTimestamp = parseTimeOrError True defaultTimeLocale

readGnuCashTimestampFormat :: String -> UTCTime
readGnuCashTimestampFormat = readTimestamp "%Y-%m-%d %H:%M:%S %z"

readGnuCashDateFormat :: String -> Day
readGnuCashDateFormat = readTimestamp "%Y-%m-%d"

maybeA :: ArrowIf a => a b c -> a b (Maybe c)
maybeA a = (a >>> arr Just) `orElse` constA Nothing

-- HXT definitions
parseBook :: Parser Book
parseBook =
  deep (atNamedChild "gnc:book") >>> proc el -> do
    bookId <- textAtTag "book:id" -< el
    bookCommodities <- listA (parseCommodity <<< atNamedChild "gnc:commodity") -< el
    bookAccounts <- listA parseAccount -< el
    bookTransactions <- listA parseTransaction -< el
    bookPrices <- listA (parsePrice <<< atNamedChild "gnc:pricedb") -< el
    bookSlots <- parseSlotsIfPresent "book:slots" -< el
    returnA -< Book {..}

parseSlotsIfPresent :: String -> Parser Slots
parseSlotsIfPresent tagName = (atNamedChild tagName >>> parseSlots) `orElse` constA mempty

parseSlots :: Parser Slots
parseSlots = listA parseSlotPair >>> arr HM.fromList

parseSlotPair :: Parser (Text, KvpValue)
parseSlotPair =
  atNamedChild "slot" >>> proc el -> do
    slotKey <- textAtTag "slot:key" -< el
    slotValue <- parseSlotValue -< el
    returnA -< (slotKey, slotValue)

parseSlotValue :: Parser KvpValue
parseSlotValue =
  let tagName = "slot:value"
   in atNamedChild tagName
        >>> choiceA
          [ hasAttrValue "type" (== "integer") :-> (getChildren >>> getText >>> arr (KvpInteger . read)),
            hasAttrValue "type" (== "double") :-> (getChildren >>> getText >>> arr (KvpDouble . read)),
            hasAttrValue "type" (== "numeric") :-> (getChildren >>> getText >>> arr (KvpNumeric . parseGnuCashQuantity)),
            hasAttrValue "type" (== "string") :-> (getChildren >>> getText >>> arr (KvpString . pack)),
            hasAttrValue "type" (== "guid") :-> (getChildren >>> getText >>> arr (KvpGUID . pack)),
            hasAttrValue "type" (== "timespec") :-> (timestampAtTag tagName >>> arr KvpTimespec),
            hasAttrValue "type" (== "gdate") :-> (stringAtTag "gdate" >>> arr (KvpDate . readGnuCashDateFormat)),
            hasAttrValue "type" (== "binary") :-> (getChildren >>> getText >>> arr (KvpBinary . pack)),
            hasAttrValue "type" (== "list") :-> (listA parseSlotValue >>> arr KvpList),
            hasAttrValue "type" (== "frame") :-> (parseSlots >>> arr KvpFrame)
          ]

parseCommodity :: Parser Commodity
parseCommodity = parseComplexCommodity `orElse` parseSimpleCommodity

parseComplexCommodity :: Parser Commodity
parseComplexCommodity = proc el -> do
  complexCmdtySpace <- textAtTag "cmdty:space" -< el
  complexCmdtyId <- textAtTag "cmdty:id" -< el
  complexCmdtyName <- maybeA (textAtTag "cmdty:name") -< el
  complexCmdtyXCode <- maybeA (textAtTag "cmdty:xcode") -< el
  complexCmdtyFraction <- arr read <<< stringAtTag "cmdty:fraction" -< el
  complexCmdtySlots <- parseSlotsIfPresent "cmdty:slots" -< el
  returnA -< ComplexCommodity (ComplexCommodityAttrs {..})

parseSimpleCommodity :: Parser Commodity
parseSimpleCommodity = proc el -> do
  simpleCmdtySpace <- textAtTag "cmdty:space" -< el
  simpleCmdtyId <- textAtTag "cmdty:id" -< el
  returnA -< SimpleCommodity (SimpleCommodityAttrs {..})

parseAccount :: Parser Account
parseAccount =
  atNamedChild "gnc:account"
    >>> proc el -> do
      actName <- textAtTag "act:name" -< el
      actId <- textAtTag "act:id" -< el
      actCode <- maybeA (textAtTag "act:code") -< el
      actDescription <- maybeA (textAtTag "act:description") -< el
      actType <- arr readAccountType <<< textAtTag "act:type" -< el
      actCommodity <- parseCommodity <<< atNamedChild "act:commodity" -< el
      actCommoditySCU <- arr read <<< stringAtTag "act:commodity-scu" -< el
      actParentId <- maybeA (textAtTag "act:parent") -< el
      actSlots <- parseSlotsIfPresent "act:slots" -< el
      returnA -< Account {..}

-- TODO: handle wrong values
readAccountType :: Text -> AccountType
readAccountType = \case
  "NONE" -> None
  "BANK" -> Bank
  "CASH" -> Cash
  "CREDIT" -> Credit
  "ASSET" -> Asset
  "LIABILITY" -> Liability
  "STOCK" -> Stock
  "MUTUAL" -> Mutual
  "CURRENCY" -> Currency
  "INCOME" -> Income
  "EXPENSE" -> Expense
  "EQUITY" -> Equity
  "RECEIVABLE" -> Receivable
  "PAYABLE" -> Payable
  "ROOT" -> Root
  "TRADING" -> Trading
  "CHECKING" -> Checking
  "SAVINGS" -> Savings
  "MONEYMRKT" -> MoneyMarket
  "CREDITLINE" -> Creditline

parseTransaction :: Parser Transaction
parseTransaction =
  atNamedChild "gnc:transaction"
    >>> proc el -> do
      trnId <- textAtTag "trn:id" -< el
      trnNum <- maybeA (textAtTag "trn:num") -< el
      trnCurrency <- parseCommodity <<< atNamedChild "trn:currency" -< el
      trnDatePosted <- timestampAtTag "trn:date-posted" -< el
      trnDateEntered <- timestampAtTag "trn:date-entered" -< el
      trnDescription <- textAtTag "trn:description" -< el
      trnSplits <- listA parseTransactionSplit <<< atNamedChild "trn:splits" -< el
      trnSlots <- parseSlotsIfPresent "trn:slots" -< el
      returnA -< Transaction {..}

parseTransactionSplit :: Parser TransactionSplit
parseTransactionSplit =
  atNamedChild "trn:split"
    >>> proc el -> do
      splitId <- textAtTag "split:id" -< el
      splitMemo <- maybeA (textAtTag "split:memo") -< el
      splitAction <- maybeA (textAtTag "split:action") -< el
      splitReconcileDate <- maybeA (timestampAtTag "split:reconcile-date") -< el
      splitReconciledState <- arr readSplitReconciledState <<< textAtTag "split:reconciled-state" -< el
      splitValue <- arr parseGnuCashQuantity <<< stringAtTag "split:value" -< el
      splitQuantity <- arr parseGnuCashQuantity <<< stringAtTag "split:quantity" -< el
      splitAccountId <- textAtTag "split:account" -< el
      returnA -< TransactionSplit {..}

-- TODO: handle wrong values
readSplitReconciledState :: Text -> SplitReconciledState
readSplitReconciledState = \case
  "y" -> Y
  "n" -> N
  "c" -> C
  "f" -> F
  "v" -> V

parseGnuCashQuantity :: String -> Quantity
parseGnuCashQuantity quantity =
  let (fractionalQuantityAmount, quantitySCU) = (read *** read) $ splitOn '/' quantity
      quantityAmount = fractionalQuantityAmount / fromInteger quantitySCU
   in Quantity {..}
  where
    splitOn :: Eq a => a -> [a] -> ([a], [a])
    splitOn sep = second tail . span (/= sep)

parsePrice :: Parser CommodityPrice
parsePrice =
  atNamedChild "price"
    >>> proc el -> do
      priceId <- textAtTag "price:id" -< el
      priceCommodity <- parseCommodity <<< atNamedChild "price:commodity" -< el
      priceCurrency <- parseCommodity <<< atNamedChild "price:currency" -< el
      priceTime <- timestampAtTag "price:time" -< el
      priceSource <- maybeA (textAtTag "price:source") -< el
      priceType <- maybeA (arr readPriceType <<< textAtTag "price:type") -< el
      priceValue <- arr parseGnuCashQuantity <<< stringAtTag "price:value" -< el
      returnA -< CommodityPrice {..}

-- TODO: handle wrong values
readPriceType :: Text -> PriceType
readPriceType = \case
  "bid" -> BidPrice
  "ask" -> AskPrice
  "last" -> LastPrice
  "nav" -> NavPrice
  "transaction" -> TransactionPrice
  "unknown" -> UnknownPrice
