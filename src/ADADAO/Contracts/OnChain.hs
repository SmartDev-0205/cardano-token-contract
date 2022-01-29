{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | Core on-chain seedSale functions.
module ADADAO.Contracts.OnChain where

import ADADAO.Contracts.Common
import ADADAO.Contracts.Types

import Ledger (
  Address,
  Extended (Finite),
  LowerBound (LowerBound),
  POSIXTime,
  PubKeyHash,
  ScriptContext (ScriptContext),
  TxInfo (TxInfo),
  ivFrom,
  mintingPolicyHash,
  mkMintingPolicyScript,
  scriptAddress,
  scriptContextTxInfo,
  txInfoValidRange,
  txSignedBy,
 )
import Ledger.Constraints.OnChain (checkOwnOutputConstraint)
import Ledger.Constraints.TxConstraints (OutputConstraint (OutputConstraint))
import Ledger.Typed.Scripts (
  MintingPolicy,
  TypedValidator,
  mkTypedValidatorParam,
  validatorScript,
  wrapMintingPolicy,
  wrapValidator,
 )
import Ledger.Value (CurrencySymbol, Value, mpsSymbol)
import PlutusTx (applyCode, compile, liftCode)
import PlutusTx.Prelude qualified as P

validateSeedSale :: SeedSaleParams -> SeedSaleDatum -> SeedSaleRedeemer -> Ledger.ScriptContext -> P.Bool
validateSeedSale
  params@SeedSaleParams {pOperatorPKH}
  datum
  redeemer
  ctx@ScriptContext {scriptContextTxInfo} =
    case redeemer of
      Withdraw pkh rAmount -> validateWithdraw params datum ctx rAmount pkh
      Update -> P.traceIfFalse "Invalid operator" (txSignedBy scriptContextTxInfo pOperatorPKH)
      Buy pkh rAmount -> validateBuyADADAO params datum ctx rAmount pkh

validateWithdraw :: SeedSaleParams -> SeedSaleDatum -> ScriptContext -> P.Integer -> PubKeyHash -> P.Bool
validateWithdraw
  params
  datum@SeedSaleDatum {}
  ctx@ScriptContext {scriptContextTxInfo = info@TxInfo {txInfoValidRange}}
  rAmount
  pkh =
    P.traceIfFalse "Invalid person" (txSignedBy info pkh)
      P.&& P.traceIfFalse "Invalid amount" (rAmount P.> 0)

validateBuyADADAO :: SeedSaleParams -> SeedSaleDatum -> ScriptContext -> P.Integer -> PubKeyHash -> P.Bool
validateBuyADADAO
  params
  datum@SeedSaleDatum {}
  ctx@ScriptContext {scriptContextTxInfo = info@TxInfo {txInfoValidRange}}
  rAmount
  pkh =
    P.traceIfFalse "Invalid person" (txSignedBy info pkh)
      P.&& P.traceIfFalse "Invalid amount" (rAmount P.> 0)

seedSaleScript :: SeedSaleParams -> TypedValidator SeedSalePosition
seedSaleScript =
  mkTypedValidatorParam @SeedSalePosition
    $$(compile [||validateSeedSale||])
    $$(compile [||wrapValidator||])

seedSaleAddress :: SeedSaleParams -> Address
seedSaleAddress = scriptAddress P.. validatorScript P.. seedSaleScript

validateSeedSaleAuthToken :: SeedSaleAuthTokenParams -> () -> ScriptContext -> P.Bool
validateSeedSaleAuthToken SeedSaleAuthTokenParams {tpOperatorPKH} _ ScriptContext {scriptContextTxInfo} =
  P.traceIfFalse "Invalid operator" (txSignedBy scriptContextTxInfo tpOperatorPKH)

seedSaleAuthTokenPolicy :: SeedSaleAuthTokenParams -> MintingPolicy
seedSaleAuthTokenPolicy params =
  mkMintingPolicyScript P.$
    $$(compile [||wrapMintingPolicy P.. validateSeedSaleAuthToken||]) `applyCode` liftCode params

seedSaleAuthTokenCurrencySymbol :: SeedSaleAuthTokenParams -> CurrencySymbol
seedSaleAuthTokenCurrencySymbol = mpsSymbol P.. mintingPolicyHash P.. seedSaleAuthTokenPolicy
