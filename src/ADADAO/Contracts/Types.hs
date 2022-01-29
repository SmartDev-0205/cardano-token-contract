{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core Seed Sale types.
module ADADAO.Contracts.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (AssetClass, POSIXTime, PubKeyHash)
import Ledger.Typed.Scripts (ValidatorTypes (DatumType, RedeemerType))
import PlutusTx (makeLift, unstableMakeIsData)
import PlutusTx.AssocMap (Map)
import Prelude (Eq, Integer, Maybe, Show)

data SeedSaleParams = SeedSaleParams
  { pADADAOAsset :: AssetClass
  , pAuthToken :: AssetClass
  , pOperatorPKH :: PubKeyHash
  }
  deriving stock (Generic, Show)

data SeedSaleDatum = SeedSaleDatum
  { dListSale :: Map PubKeyHash (Integer, Integer)
  , dRate :: Integer
  , dAmountPerMonth :: Integer
  , dMaxAmount :: Integer
  , dNumContract :: Integer
  , dStart :: POSIXTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SeedSaleRedeemer
  = Update
  | Withdraw PubKeyHash Integer
  | Buy PubKeyHash Integer
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data SeedSalePosition

instance ValidatorTypes SeedSalePosition where
  type DatumType SeedSalePosition = SeedSaleDatum
  type RedeemerType SeedSalePosition = SeedSaleRedeemer

newtype SeedSaleAuthTokenParams = SeedSaleAuthTokenParams
  { tpOperatorPKH :: PubKeyHash
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data CreateSeedSaleParams = CreateSeedSaleParams
  { cpOperatorPKH :: PubKeyHash
  , cpAmountADADAO :: Integer
  , cpInitialDatum :: SeedSaleDatum
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data UpdateSeedSaleParams = UpdateSeedSaleParams
  { upNumContract :: Integer
  , upNewDatum :: SeedSaleDatum
  , upNewAmount :: Maybe Integer
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data BuySeedSaleParams = BuySeedSaleParams
  { bpNewAmount :: Integer
  , bpSubmitTime :: Maybe POSIXTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data WithdrawSeedSaleParams = WithdrawSeedSaleParams
  { wpWithdrawAmount :: Integer
  , wpSubmitTime :: Maybe POSIXTime
  }
  deriving stock (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

unstableMakeIsData ''SeedSaleDatum
unstableMakeIsData ''SeedSaleRedeemer
unstableMakeIsData ''SeedSaleParams
unstableMakeIsData ''SeedSaleAuthTokenParams

-- Make Lift
makeLift ''SeedSaleParams
makeLift ''SeedSaleAuthTokenParams
makeLift ''SeedSaleDatum
makeLift ''SeedSaleRedeemer
