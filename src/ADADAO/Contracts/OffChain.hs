{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Core off-chain seed sale types and functions.
module ADADAO.Contracts.OffChain where

import Control.Monad
import Data.Aeson (FromJSON)
import Data.Map
import Data.Maybe
import Ledger (
  ChainIndexTxOut,
  PaymentPubKeyHash (PaymentPubKeyHash),
  PubKeyHash,
  TxOutRef,
  interval,
 )
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints

import Ledger.Value (
  assetClassValue,
  assetClassValueOf,
 )
import Plutus.Contract
import PlutusTx qualified
import PlutusTx.AssocMap qualified as PtMap
import Prelude

import ADADAO.Contracts.Common
import ADADAO.Contracts.OnChain
import ADADAO.Contracts.Types
import ADADAO.Contracts.Utils

-- | `findSeedSalePosition` @params pkh@ finds an authentic seed sale position with
-- the passed in `pkh` in white list.
findSeedSalePositionByPKH ::
  SeedSaleParams ->
  PubKeyHash ->
  Contract w s ContractError (TxOutRef, ChainIndexTxOut, SeedSaleDatum)
findSeedSalePositionByPKH params@SeedSaleParams {pAuthToken} pkh =
  maybe (throwError "Seed sale position not found") pure
    =<< findFirstOutput
      (seedSaleAddress params)
      ( \(_, o) -> case getDatum o of
          Just d ->
            PtMap.member pkh (dListSale d)
              && assetClassValueOf (getValue o) pAuthToken == 1
          _ -> False
      )

findSeedSalePositionByNum ::
  SeedSaleParams ->
  Integer ->
  Contract w s ContractError (TxOutRef, ChainIndexTxOut, SeedSaleDatum)
findSeedSalePositionByNum params@SeedSaleParams {pAuthToken} numContract =
  maybe (throwError "Seed sale position not found") pure
    =<< findFirstOutput
      (seedSaleAddress params)
      ( \(_, o) -> case getDatum o of
          Just d ->
            dNumContract d == numContract
              && assetClassValueOf (getValue o) pAuthToken == 1
          _ -> False
      )

-- | Schema of the seedSale endpoints with create, update, and withdraw.
type SeedSaleSchema =
  Endpoint "CreateSeedSale" CreateSeedSaleParams
    .\/ Endpoint "UpdateSeedSale" UpdateSeedSaleParams
    .\/ Endpoint "WithdrawSeedSale" WithdrawSeedSaleParams
    .\/ Endpoint "BuySeedSale" BuySeedSaleParams

seedSaleEndpoints :: SeedSaleParams -> Promise () SeedSaleSchema ContractError ()
seedSaleEndpoints seedSaleParams =
  ( handle @"CreateSeedSale" createSeedSale
      `select` handle @"UpdateSeedSale" updateSeedSale
      `select` handle @"WithdrawSeedSale" withdrawSeedSale
      `select` handle @"BuySeedSale" buySeedSale
  )
    <> seedSaleEndpoints seedSaleParams
  where
    handle ::
      forall l p.
      (HasEndpoint l p SeedSaleSchema, FromJSON p) =>
      (SeedSaleParams -> p -> Contract () SeedSaleSchema ContractError ()) ->
      Promise () SeedSaleSchema ContractError ()
    handle = endpoint @l . (handleError logError .) . ($ seedSaleParams)

-- | `createSeedSale` @seedSaleParams createParams@ submits a transaction that creates a seedSale position.
createSeedSale ::
  SeedSaleParams ->
  CreateSeedSaleParams ->
  Contract w s ContractError ()
createSeedSale
  seedSaleParams@SeedSaleParams {pAuthToken, pADADAOAsset}
  createSqeedSaleParams@CreateSeedSaleParams {cpOperatorPKH, cpInitialDatum, cpAmountADADAO} =
    submitTxPairs
      [ mustMintValue -- Mint an auth token
          [seedSaleAuthTokenPolicy (SeedSaleAuthTokenParams cpOperatorPKH)]
          (assetClassValue pAuthToken 1)
      , mustPayToScripts -- Create a seedSale output with the initial datum and vested amount
          (seedSaleScript seedSaleParams)
          [(cpInitialDatum, minADAOutput <> assetClassValue pAuthToken 1 <> assetClassValue pADADAOAsset cpAmountADADAO)]
      ]

updateSeedSale ::
  SeedSaleParams -> UpdateSeedSaleParams -> Contract w s ContractError ()
updateSeedSale
  seedSaleParams@SeedSaleParams {pADADAOAsset, pAuthToken}
  updateSeedSaleParams@UpdateSeedSaleParams {upNumContract, upNewDatum, upNewAmount} =
    do
      (oref, o, _) <- findSeedSalePositionByNum seedSaleParams upNumContract
      let inst = seedSaleScript seedSaleParams
          curValue = fromMaybe (assetClassValueOf (getValue o) pADADAOAsset) upNewAmount
          addValue = curValue - assetClassValueOf (getValue o) pADADAOAsset
          newValue = getValue o <> assetClassValue pADADAOAsset addValue
      submitTxPairs
        [ -- Spend the previous output
          mustSpendScriptOutputs inst [(oref, o, Update)]
        , -- Create a new seedSale output with the new datum and vested amount
          mustPayToScripts inst [(upNewDatum, newValue)]
        ]

buySeedSale :: SeedSaleParams -> BuySeedSaleParams -> Contract w s ContractError ()
buySeedSale
  seedSaleParams@SeedSaleParams {pADADAOAsset, pAuthToken}
  BuySeedSaleParams {bpNewAmount, bpSubmitTime} = do
    unless (bpNewAmount > 0) (throwError "Must withdraw a positive amount")

    PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
    (oref, o, prevDatum@SeedSaleDatum {dListSale, dRate, dMaxAmount}) <- findSeedSalePositionByPKH seedSaleParams pkh

    submitTime <- case bpSubmitTime of
      Just t -> pure t
      _ -> currentTime
    let check = checkAmountDelegate pkh (bpNewAmount * dRate) dMaxAmount dListSale
    unless check (throwError "Invalid buy amount")

    let newDatum = prevDatum {dListSale = updateReward pkh (bpNewAmount * dRate) dListSale}
        newValue = getValue o <> Ada.lovelaceValueOf bpNewAmount
        inst = seedSaleScript seedSaleParams
    submitTxPairs
      [ -- Spend the previous output
        mustSpendScriptOutputs inst [(oref, o, Buy pkh bpNewAmount)]
      , -- Pay to the seedSale person
        (mempty, Constraints.mustPayToPubKey (PaymentPubKeyHash pkh) (Ada.lovelaceValueOf (-bpNewAmount)))
      , -- Create a new seedSale output that reflects the buy
        mustPayToScripts inst [(newDatum, newValue)]
      , mustValidateIn (interval submitTime (submitTime + 100000))
      ]

withdrawSeedSale ::
  SeedSaleParams -> WithdrawSeedSaleParams -> Contract w s ContractError ()
withdrawSeedSale
  seedSaleParams@SeedSaleParams {pADADAOAsset}
  WithdrawSeedSaleParams {wpWithdrawAmount, wpSubmitTime} = do
    unless (wpWithdrawAmount > 0) (throwError "Must withdraw a positive amount")

    PaymentPubKeyHash pkh <- ownPaymentPubKeyHash
    (oref, o, prevDatum@SeedSaleDatum {dListSale}) <- findSeedSalePositionByPKH seedSaleParams pkh

    submitTime <- case wpSubmitTime of
      Just t -> pure t
      _ -> currentTime
    let check = checkAmountWithdraw pkh wpWithdrawAmount dListSale
    unless check (throwError "Invalid withdrawal amount")

    let newDatum = prevDatum {dListSale = updateWithdraw pkh wpWithdrawAmount dListSale}
        newValue = getValue o <> assetClassValue pADADAOAsset (-wpWithdrawAmount)
        inst = seedSaleScript seedSaleParams
    submitTxPairs
      [ mustSpendScriptOutputs inst [(oref, o, Withdraw pkh wpWithdrawAmount)]
      , (mempty, Constraints.mustPayToPubKey (PaymentPubKeyHash pkh) (assetClassValue pADADAOAsset wpWithdrawAmount))
      , mustPayToScripts inst [(newDatum, newValue)]
      , mustValidateIn (interval submitTime (submitTime + 888_888))
      ]
