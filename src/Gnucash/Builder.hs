{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Gnucash.Builder where

import Data.Decimal (roundTo)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text, unpack)
import Data.Time
import Gnucash.Types
import Text.XML.HXT.Core

buildDocument :: ArrowXml a => [Book] -> a n XmlTree
buildDocument books =
  root
    []
    [ mkelem
        "gnc-v2"
        [ sattr "xmlns:gnc" "http://www.gnucash.org/XML/gnc",
          sattr "xmlns:act" "http://www.gnucash.org/XML/act",
          sattr "xmlns:book" "http://www.gnucash.org/XML/book",
          sattr "xmlns:cd" "http://www.gnucash.org/XML/cd",
          sattr "xmlns:cmdty" "http://www.gnucash.org/XML/cmdty",
          sattr "xmlns:price" "http://www.gnucash.org/XML/price",
          sattr "xmlns:slot" "http://www.gnucash.org/XML/slot",
          sattr "xmlns:split" "http://www.gnucash.org/XML/split",
          sattr "xmlns:sx" "http://www.gnucash.org/XML/sx",
          sattr "xmlns:trn" "http://www.gnucash.org/XML/trn",
          sattr "xmlns:ts" "http://www.gnucash.org/XML/ts",
          sattr "xmlns:fs" "http://www.gnucash.org/XML/fs",
          sattr "xmlns:bgt" "http://www.gnucash.org/XML/bgt",
          sattr "xmlns:recurrence" "http://www.gnucash.org/XML/recurrence",
          sattr "xmlns:lot" "http://www.gnucash.org/XML/lot",
          sattr "xmlns:addr" "http://www.gnucash.org/XML/addr",
          sattr "xmlns:billterm" "http://www.gnucash.org/XML/billterm",
          sattr "xmlns:bt-days" "http://www.gnucash.org/XML/bt-days",
          sattr "xmlns:bt-prox" "http://www.gnucash.org/XML/bt-prox",
          sattr "xmlns:cust" "http://www.gnucash.org/XML/cust",
          sattr "xmlns:employee" "http://www.gnucash.org/XML/employee",
          sattr "xmlns:entry" "http://www.gnucash.org/XML/entry",
          sattr "xmlns:invoice" "http://www.gnucash.org/XML/invoice",
          sattr "xmlns:job" "http://www.gnucash.org/XML/job",
          sattr "xmlns:order" "http://www.gnucash.org/XML/order",
          sattr "xmlns:owner" "http://www.gnucash.org/XML/owner",
          sattr "xmlns:taxtable" "http://www.gnucash.org/XML/taxtable",
          sattr "xmlns:tte" "http://www.gnucash.org/XML/tte",
          sattr "xmlns:vendor" "http://www.gnucash.org/XML/vendor"
        ]
        ( mkelem
            "gnc:count-data"
            [sattr "cd:type" "book"]
            [txt (show (length books))] :
          fmap buildBook books
        )
    ]

buildBook :: ArrowXml a => Book -> a n XmlTree
buildBook book@Book {..} =
  mkelem "gnc:book" [sattr "version" "2.0.0"] $
    concat
      [ [buildGUID "book:id" bookId],
        [selem "book:slots" $ buildSlots bookSlots | not (null bookSlots)],
        [buildCountDataDirectives book],
        buildCommodity "gnc:commodity" [sattr "version" "2.0.0"] <$> bookCommodities,
        [mkelem "gnc:pricedb" [sattr "version" "1"] $ buildPrice <$> bookPrices],
        buildAccount <$> bookAccounts,
        buildTransaction <$> bookTransactions
      ]

buildSlots :: ArrowXml a => Slots -> [a n XmlTree]
buildSlots =
  fmap buildSlot
    . HM.toList
  where
    buildSlot (key, kvpValue) =
      selem
        "slot"
        [ buildText "slot:key" key,
          buildKvpValue kvpValue
        ]

getKvpValueType :: KvpValue -> String
getKvpValueType = \case
  KvpInteger _ -> "integer"
  KvpDouble _ -> "double"
  KvpNumeric _ -> "numeric"
  KvpString _ -> "string"
  KvpGUID _ -> "guid"
  KvpTimespec _ -> "timespec"
  KvpDate _ -> "gdate"
  KvpBinary _ -> "binary"
  KvpList _ -> "list"
  KvpFrame _ -> "frame"

buildKvpValue :: ArrowXml a => KvpValue -> a n XmlTree
buildKvpValue kvpValue = mkelem "slot:value" [sattr "type" (getKvpValueType kvpValue)] $
  case kvpValue of
    KvpInteger i -> [txt (show i)]
    KvpDouble d -> [txt (show d)]
    KvpNumeric q -> [txt (formatQuantityForGnucash q)]
    KvpString s -> [txt (unpack s)]
    KvpGUID guid -> [txt (unpack guid)]
    KvpTimespec t -> [txt (formatDatetimeForGnucash t)]
    KvpDate d -> [buildGDate d]
    KvpBinary b -> [txt (unpack b)]
    KvpList vs -> buildKvpValue <$> vs
    KvpFrame f -> buildSlots f

buildCountDataDirectives :: ArrowXml a => Book -> a n XmlTree
buildCountDataDirectives Book {..} =
  buildCountData "commodity" (length bookCommodities)
    <+> buildCountData "account" (length bookAccounts)
    <+> buildCountData "transaction" (length bookTransactions)
    <+> buildCountData "price" (length bookPrices)

buildCountData :: ArrowXml a => String -> Int -> a n XmlTree
buildCountData countType count =
  mkelem "gnc:count-data" [sattr "cd:type" countType] [txt (show count)]

buildGUID :: ArrowXml a => String -> Text -> a n XmlTree
buildGUID tagName guid = mkelem tagName [sattr "type" "guid"] [txt (unpack guid)]

buildCommodity :: ArrowXml a => String -> [a n XmlTree] -> Commodity -> a n XmlTree
buildCommodity tagName attrs (SimpleCommodity SimpleCommodityAttrs {..}) =
  mkelem
    tagName
    attrs
    [ buildText "cmdty:space" simpleCmdtySpace,
      buildText "cmdty:id" simpleCmdtyId
    ]
buildCommodity tagName attrs (ComplexCommodity ComplexCommodityAttrs {..}) =
  mkelem tagName attrs $
    catMaybes
      [ Just $ buildText "cmdty:space" complexCmdtySpace,
        Just $ buildText "cmdty:id" complexCmdtyId,
        buildText "cmdty:name" <$> complexCmdtyName,
        buildText "cmdty:xcode" <$> complexCmdtyXCode,
        Just $ eelem "cmdty:fraction" += txt (show complexCmdtyFraction),
        listToMaybe [selem "cmdty:slots" (buildSlots complexCmdtySlots) | not (null complexCmdtySlots)]
      ]

buildAccount :: ArrowXml a => Account -> a n XmlTree
buildAccount Account {..} =
  mkelem "gnc:account" [sattr "version" "2.0.0"] $
    catMaybes
      [ Just $ buildText "act:name" actName,
        Just $ buildGUID "act:id" actId,
        Just $ buildAccountType actType,
        Just $ buildCommodity "act:commodity" [] actCommodity,
        Just $ selem "act:commodity-scu" [txt (show actCommoditySCU)],
        buildText "act:code" <$> actCode,
        buildText "act:description" <$> actDescription,
        listToMaybe [selem "act:slots" (buildSlots actSlots) | not (null actSlots)],
        buildGUID "act:parent" <$> actParentId
      ]

--
buildAccountType :: ArrowXml a => AccountType -> a n XmlTree
buildAccountType actType =
  buildText "act:type" $ case actType of
    None -> "NONE"
    Bank -> "BANK"
    Cash -> "CASH"
    Credit -> "CREDIT"
    Asset -> "ASSET"
    Liability -> "LIABILITY"
    Stock -> "STOCK"
    Mutual -> "MUTUAL"
    Currency -> "CURRENCY"
    Income -> "INCOME"
    Expense -> "EXPENSE"
    Equity -> "EQUITY"
    Receivable -> "RECEIVABLE"
    Payable -> "PAYABLE"
    Root -> "ROOT"
    Trading -> "TRADING"
    Checking -> "CHECKING"
    Savings -> "SAVINGS"
    MoneyMarket -> "MONEYMRKT"
    Creditline -> "CREDITLINE"

buildText :: ArrowXml a => String -> Text -> a n XmlTree
buildText tagName text =
  eelem tagName += txt (unpack text)

buildTransaction :: ArrowXml a => Transaction -> a n XmlTree
buildTransaction Transaction {..} =
  mkelem "gnc:transaction" [sattr "version" "2.0.0"] $
    catMaybes
      [ Just $ buildGUID "trn:id" trnId,
        Just $ buildCommodity "trn:currency" [] trnCurrency,
        buildText "trn:num" <$> trnNum,
        Just $ buildDatetime "trn:date-posted" trnDatePosted,
        Just $ buildDatetime "trn:date-entered" trnDateEntered,
        Just $ buildText "trn:description" trnDescription,
        listToMaybe [selem "trn:slots" (buildSlots trnSlots) | not (null trnSlots)],
        Just $ selem "trn:splits" (buildTransactionSplit <$> trnSplits)
      ]

buildDatetime :: ArrowXml a => String -> UTCTime -> a n XmlTree
buildDatetime tagName datetime =
  selem tagName [selem "ts:date" [txt (formatDatetimeForGnucash datetime)]]

formatDatetimeForGnucash :: UTCTime -> String
formatDatetimeForGnucash =
  let gnucashDatetimeFormat = "%Y-%m-%d %H:%M:%S %z"
   in formatTime defaultTimeLocale gnucashDatetimeFormat

buildGDate :: ArrowXml a => Day -> a n XmlTree
buildGDate date =
  selem "gdate" [txt (formatTime defaultTimeLocale "%Y-%m-%d" date)]

buildTransactionSplit :: ArrowXml a => TransactionSplit -> a n XmlTree
buildTransactionSplit TransactionSplit {..} =
  selem "trn:split" $
    catMaybes
      [ Just $ buildGUID "split:id" splitId,
        buildText "split:memo" <$> splitMemo,
        buildText "split:action" <$> splitAction,
        Just $ buildSplitReconciledState splitReconciledState,
        buildDatetime "split:reconcile-date" <$> splitReconcileDate,
        Just $ buildQuantity "split:value" splitValue,
        Just $ buildQuantity "split:quantity" splitQuantity,
        Just $ buildGUID "split:account" splitAccountId
      ]

buildSplitReconciledState :: ArrowXml a => SplitReconciledState -> a n XmlTree
buildSplitReconciledState state =
  buildText "split:reconciled-state" $ case state of
    Y -> "y"
    N -> "n"
    C -> "c"
    F -> "f"
    V -> "v"

buildQuantity :: ArrowXml a => String -> Quantity -> a n XmlTree
buildQuantity tagName quantity =
  selem tagName [txt (formatQuantityForGnucash quantity)]

-- | GnuCash quantities are expressed in multiples of an SCU (
-- Smallest Commodity Unit). For instance, "2.34" with an SCU of
-- 100 would be "234/100"
formatQuantityForGnucash :: Quantity -> String
formatQuantityForGnucash Quantity {..} =
  show (roundTo 0 $ quantityAmount * fromInteger quantitySCU)
    <> "/"
    <> show quantitySCU

buildPrice :: ArrowXml a => CommodityPrice -> a n XmlTree
buildPrice CommodityPrice {..} =
  selem "price" $
    catMaybes
      [ Just $ buildGUID "price:id" priceId,
        Just $ buildCommodity "price:commodity" [] priceCommodity,
        Just $ buildCommodity "price:currency" [] priceCurrency,
        Just $ buildDatetime "price:time" priceTime,
        buildText "price:source" <$> priceSource,
        buildPriceType <$> priceType,
        Just $ buildQuantity "price:value" priceValue
      ]

-- TODO: handle missing/wrong types
buildPriceType :: ArrowXml a => PriceType -> a n XmlTree
buildPriceType priceType = buildText "price:type" $ case priceType of
  BidPrice -> "bid"
  AskPrice -> "ask"
  LastPrice -> "last"
  NavPrice -> "nav"
  TransactionPrice -> "transaction"
  UnknownPrice -> "unknown"
