{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}

-- | Common seed sale functions and values.
module ADADAO.Contracts.Common where

import ADADAO.Contracts.Types
import Ledger (
  ChainIndexTxOut,
  PaymentPubKeyHash (PaymentPubKeyHash),
  PubKeyHash,
  TxOutRef,
  interval,
 )
import Ledger.Ada (lovelaceValueOf)
import Ledger.Time (POSIXTime (getPOSIXTime))
import Ledger.Value (Value, assetClassValue)
import Plutus.Contract
import PlutusTx.AssocMap qualified as PtMap
import PlutusTx.Prelude

-- | The minimum amount of Lovelace required at a seed sale output.
--
-- It is 2_000_000 (2 ADA) at the moment.
minLovelaceInSeedSaleOutput :: Integer
minLovelaceInSeedSaleOutput = 2_000_000

-- | The minimum amount of ADA required at a seed sale output.
--
-- It is 2 ADA at the moment.
minADAInSeedSaleOutput :: Value
minADAInSeedSaleOutput = lovelaceValueOf minLovelaceInSeedSaleOutput

{-# INLINEABLE updateWithdraw #-}
updateWithdraw ::
  PubKeyHash ->
  Integer ->
  PtMap.Map PubKeyHash (Integer, Integer) ->
  PtMap.Map PubKeyHash (Integer, Integer)
updateWithdraw
  key
  amount
  oldList =
    PtMap.fromList
      ((\(k, (rewardAmount, withdrawAmount)) -> (k, (rewardAmount, if k == key then amount + withdrawAmount else withdrawAmount))) <$> PtMap.toList oldList)

{-# INLINEABLE updateReward #-}
updateReward ::
  PubKeyHash ->
  Integer ->
  PtMap.Map PubKeyHash (Integer, Integer) ->
  PtMap.Map PubKeyHash (Integer, Integer)
updateReward
  key
  amount
  oldList =
    PtMap.fromList
      ((\(k, (rewardAmount, withdrawAmount)) -> (k, (if k == key then amount + rewardAmount else rewardAmount, withdrawAmount))) <$> PtMap.toList oldList)

{-# INLINEABLE checkAmountWithdraw #-}
checkAmountWithdraw ::
  PubKeyHash ->
  Integer ->
  PtMap.Map PubKeyHash (Integer, Integer) ->
  Bool
checkAmountWithdraw
  key
  amount
  oldList =
    let (rewardAmount, withdrawAmount) = fromMaybe (-1, -1) (PtMap.lookup key oldList)
     in rewardAmount >= withdrawAmount + amount

{-# INLINEABLE checkAmountDelegate #-}
checkAmountDelegate ::
  PubKeyHash ->
  Integer ->
  Integer ->
  PtMap.Map PubKeyHash (Integer, Integer) ->
  Bool
checkAmountDelegate pkh amount max oldList =
  let curReward = fst $ fromMaybe (0, 0) (PtMap.lookup pkh oldList)
   in curReward + amount <= max
