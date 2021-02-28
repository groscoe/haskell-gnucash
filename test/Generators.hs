{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Generators where

import Control.Monad (replicateM)
import Data.Decimal (Decimal)
import qualified Data.HashSet as HS
import Data.Text (Text)
import qualified Data.Text as T
import Generic.Random
import Gnucash.Types
import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- Simple generators
arbitraryGUID :: Gen Text
arbitraryGUID =
  T.pack
    <$> replicateM
      33
      (oneof (pure <$> "abcdef0123456789"))

arbitraryDecimal :: Gen Decimal
arbitraryDecimal = fromRational <$> arbitrary

arbitraryPowerOf10 :: Integral a => Gen a
arbitraryPowerOf10 = elements $ (10 ^) <$> [0 .. 6]

arbitrarySlots :: Gen Slots
arbitrarySlots = resize 5 arbitrary

arbitraryCurrency :: Gen SimpleCommodityAttrs
arbitraryCurrency =
  SimpleCommodityAttrs "CURRENCY"
    <$> elements ["BRL", "USD", "EUR", "JPY"]

instance Arbitrary Quantity where
  arbitrary = do
    quantitySCU <- arbitraryPowerOf10
    quantityAmount <- (fromInteger quantitySCU *) <$> arbitraryDecimal
    pure $ Quantity {..}

instance Arbitrary KvpValue where
  arbitrary = sized $ \n ->
    frequency
      [ (6, KvpInteger <$> arbitrary),
        (6, KvpDouble <$> arbitrary),
        (6, KvpNumeric <$> arbitrary),
        (6, KvpString <$> arbitrary),
        (6, KvpGUID <$> arbitraryGUID),
        (6, KvpTimespec <$> arbitrary),
        (6, KvpDate <$> arbitrary),
        (1, KvpBinary <$> arbitrary),
        (2, KvpList <$> resize (n `div` 2) arbitrary),
        (2, KvpFrame <$> resize 3 arbitrary)
      ]

instance Arbitrary SimpleCommodityAttrs where
  arbitrary =
    oneof
      [ arbitraryCurrency,
        genericArbitrarySingle
      ]

instance Arbitrary ComplexCommodityAttrs where
  arbitrary = genericArbitrarySingleG customG
    where
      customG ::
        FieldGen "complexCmdtySpace" Text
          :+ FieldGen "complexCmdtyId" Text
          :+ FieldGen "complexCmdtyFraction" Int
          :+ FieldGen "complexCmdtySlots" Slots
      customG =
        FieldGen (arbitrary `suchThat` (not . T.null))
          :+ FieldGen (arbitrary `suchThat` (not . T.null))
          :+ FieldGen (getNonNegative <$> arbitrary)
          :+ FieldGen arbitrarySlots

instance Arbitrary Commodity where
  arbitrary =
    frequency
      [ (3, SimpleCommodity <$> arbitraryCurrency),
        (1, genericArbitrary uniform)
      ]

instance Arbitrary AccountType where
  arbitrary = genericArbitrary uniform

instance Arbitrary Account where
  arbitrary = genericArbitrarySingleG customG
    where
      customG ::
        FieldGen "actId" Text
          :+ FieldGen "actCommoditySCU" Integer
          :+ FieldGen "actSlots" Slots
      customG =
        FieldGen arbitraryGUID
          :+ FieldGen arbitraryPowerOf10
          :+ FieldGen arbitrarySlots

instance Arbitrary PriceType where
  arbitrary = genericArbitrary uniform

instance Arbitrary CommodityPrice where
  arbitrary = genericArbitrarySingleG customG
    where
      customG ::
        FieldGen "priceId" Text
          :+ FieldGen "priceCommodity" Commodity
          :+ FieldGen "priceCurrency" Commodity
      customG =
        FieldGen arbitraryGUID
          :+ FieldGen (SimpleCommodity <$> genericArbitrarySingle)
          :+ FieldGen (SimpleCommodity <$> arbitraryCurrency)

instance Arbitrary SplitReconciledState where
  arbitrary = genericArbitrary uniform

instance Arbitrary TransactionSplit where
  arbitrary = genericArbitrarySingleG customG
    where
      customG ::
        FieldGen "splitId" Text
          :+ FieldGen "splitAccountId" Text
      customG =
        FieldGen arbitraryGUID
          :+ FieldGen arbitraryGUID

arbitrarySplitPairFromAccounts :: [Account] -> Gen [TransactionSplit]
arbitrarySplitPairFromAccounts accounts = do
  act1 <- elements accounts
  act2 <- elements accounts `suchThat` ((/= actId act1) . actId)
  split1 <- arbitrary
  split2 <- arbitrary
  let split1Amount = quantityAmount (splitQuantity split1)
  let split2Quantity = (splitQuantity split1) {quantityAmount = - split1Amount}
  pure
    [ split1 {splitAccountId = actId act2},
      split2 {splitAccountId = actId act2, splitQuantity = split2Quantity}
    ]

instance Arbitrary Transaction where
  arbitrary = genericArbitrarySingleG customTransactionG

customTransactionG ::
  FieldGen "trnId" Text
    :+ FieldGen "trnSlots" Slots
customTransactionG =
  FieldGen arbitraryGUID
    :+ FieldGen arbitrarySlots

arbitraryTransactionFromAccounts :: [Account] -> Gen Transaction
arbitraryTransactionFromAccounts acts =
  genericArbitrarySingleG (customFromAccounts :+ customTransactionG)
  where
    customFromAccounts :: FieldGen "trnSplits" [TransactionSplit]
    customFromAccounts = FieldGen $ concat <$> listOf1 (arbitrarySplitPairFromAccounts acts)

instance Arbitrary Book where
  arbitrary = sized $ \n -> do
    bookId <- arbitraryGUID
    someCurrency <- SimpleCommodity <$> arbitraryCurrency
    bookCommodities <- (someCurrency :) <$> arbitrary
    bookPrices <- arbitrary
    bookAccounts <- resize (abs n `div` 10) $ listOf1 arbitrary
    bookTransactions <- listOf1 $ arbitraryTransactionFromAccounts bookAccounts
    bookSlots <- arbitrarySlots
    pure $ Book {..}
