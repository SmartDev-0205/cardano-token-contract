{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ADADAO.Contracts.Token where

import Control.Monad (void)
import Data.Void (Void)
import Ledger qualified
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as TScripts
import Ledger.Value qualified as Value
import Plutus.Contract (Contract, ContractError, mkTxConstraints, submitTxConfirmed, waitNSlots)
import PlutusTx qualified
import PlutusTx.Prelude (Bool (True), Integer, ($), (.), (>>=))

adaoTokenName :: Value.TokenName
adaoTokenName = "ADADAO"

ADADAOAsset :: Value.AssetClass
ADADAOAsset = Value.assetClass fakeCurrencySymbol adaoTokenName

{-# INLINEABLE fakeValidator #-}
fakeValidator :: () -> Ledger.ScriptContext -> Bool
fakeValidator _ _ = True

fakeMintingPolicy :: TScripts.MintingPolicy
fakeMintingPolicy =
  Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [||TScripts.wrapMintingPolicy fakeValidator||])

fakeMintingPolicyHash :: Scripts.MintingPolicyHash
fakeMintingPolicyHash = Scripts.mintingPolicyHash fakeMintingPolicy

fakeCurrencySymbol :: Value.CurrencySymbol
fakeCurrencySymbol = Value.mpsSymbol fakeMintingPolicyHash

initADADAO :: forall s. Integer -> Contract () s ContractError ()
initADADAO amount = do
  let fakeAssetClass = Value.assetClass fakeCurrencySymbol adaoTokenName
      lookups = Constraints.mintingPolicy fakeMintingPolicy
      constraints = Constraints.mustMintValue (Value.assetClassValue fakeAssetClass amount)
  mkTxConstraints @Void lookups constraints >>= submitTxConfirmed . Constraints.adjustUnbalancedTx
  void $ waitNSlots 1
