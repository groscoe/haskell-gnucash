module Gnucash.Types where

import Data.Decimal (Decimal)
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time (Day, UTCTime)

data Book = Book
  { bookId :: !Text,
    bookCommodities :: ![Commodity],
    bookPrices :: ![CommodityPrice],
    bookAccounts :: ![Account],
    bookTransactions :: ![Transaction],
    bookSlots :: !Slots
  }
  deriving (Show, Eq)

data Commodity
  = SimpleCommodity SimpleCommodityAttrs
  | ComplexCommodity ComplexCommodityAttrs
  deriving (Show, Eq)

data ComplexCommodityAttrs = ComplexCommodityAttrs
  { complexCmdtySpace :: !Text,
    complexCmdtyId :: !Text,
    complexCmdtyName :: !(Maybe Text),
    complexCmdtyXCode :: !(Maybe Text),
    complexCmdtyFraction :: !Int,
    complexCmdtySlots :: !Slots
  }
  deriving (Show, Eq)

data SimpleCommodityAttrs = SimpleCommodityAttrs
  { simpleCmdtySpace :: !Text,
    simpleCmdtyId :: !Text
  }
  deriving (Show, Eq)

data CommodityPrice = CommodityPrice
  { priceId :: !Text,
    priceCommodity :: !Commodity,
    priceCurrency :: !Commodity,
    priceTime :: !UTCTime,
    priceSource :: !(Maybe Text),
    priceType :: !(Maybe PriceType),
    priceValue :: !Quantity
  }
  deriving (Show, Eq)

data PriceType
  = BidPrice
  | AskPrice
  | LastPrice
  | NavPrice
  | TransactionPrice
  | UnknownPrice
  deriving (Ord, Eq, Show)

data Account = Account
  { actName :: !Text,
    actId :: !Text,
    actType :: !AccountType,
    actCode :: !(Maybe Text),
    actDescription :: !(Maybe Text),
    actCommodity :: !Commodity,
    actCommoditySCU :: !Integer,
    actParentId :: !(Maybe Text),
    actSlots :: !Slots
  }
  deriving (Show, Eq)

data AccountType
  = None
  | Bank
  | Cash
  | Credit
  | Asset
  | Liability
  | Stock
  | Mutual
  | Currency
  | Income
  | Expense
  | Equity
  | Receivable
  | Payable
  | Root
  | Trading
  | Checking
  | Savings
  | MoneyMarket
  | Creditline
  deriving (Eq, Ord, Show)

data Transaction = Transaction
  { trnId :: !Text,
    trnCurrency :: !Commodity,
    trnNum :: !(Maybe Text),
    trnDatePosted :: !UTCTime,
    trnDateEntered :: !UTCTime,
    trnDescription :: !Text,
    trnSplits :: ![TransactionSplit],
    trnSlots :: !Slots
  }
  deriving (Show, Eq)

data TransactionSplit = TransactionSplit
  { splitId :: !Text,
    splitMemo :: !(Maybe Text),
    splitAction :: !(Maybe Text),
    splitReconciledState :: !SplitReconciledState,
    splitReconcileDate :: !(Maybe UTCTime),
    splitValue :: !Quantity,
    splitQuantity :: !Quantity,
    splitAccountId :: !Text
  }
  deriving (Show, Eq)

data SplitReconciledState
  = Y
  | N
  | C
  | F
  | V
  deriving (Eq, Ord, Show)

data Quantity = Quantity
  { quantityAmount :: !Decimal,
    quantitySCU :: !Integer
  }
  deriving (Show, Eq)

data KvpValue
  = KvpInteger !Int
  | KvpDouble !Double
  | KvpNumeric !Quantity
  | KvpString !Text
  | KvpGUID !Text
  | KvpTimespec !UTCTime
  | KvpDate !Day
  | KvpBinary !Text
  | KvpList ![KvpValue]
  | KvpFrame !Slots
  deriving (Show, Eq)

type Slots = HashMap Text KvpValue
