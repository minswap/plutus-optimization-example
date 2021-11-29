{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-worker-wrapper #-}
{-# OPTIONS_GHC -fobject-code #-}

module OnChain
  (
    MyDatum(..),
    MyRedeemer(..),
    myScriptSBS,
    myScriptSerialised,
    myScriptSBS',
    myScriptSerialised',
  )
where

import PlutusTx.Prelude
import qualified Prelude as Haskell
import qualified PlutusTx
import qualified Ledger.Typed.Scripts as Scripts
import Plutus.V1.Ledger.Contexts (ScriptContext, TxOut, TxInfo (txInfoOutputs))
import Plutus.V1.Ledger.Api (Validator, ScriptContext (scriptContextTxInfo), unValidatorScript, TxOut (txOutDatumHash, txOutValue), mkValidatorScript)
import qualified CustomContexts as CC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import Cardano.Api.Shelley (PlutusScript (PlutusScriptSerialised), PlutusScriptV1)
import Codec.Serialise (serialise)
import Plutus.V1.Ledger.Value (AssetClass, assetClassValueOf)

data MyDatum = MyDatum {
  mdCoinA :: AssetClass,
  mdCoinB :: AssetClass
} deriving (Haskell.Show)

PlutusTx.makeIsDataIndexed ''MyDatum [('MyDatum, 0)]
PlutusTx.makeLift ''MyDatum

data MyRedeemer = Deposit 

PlutusTx.makeIsDataIndexed ''MyRedeemer [('Deposit, 0)]
PlutusTx.makeLift ''MyRedeemer

data MyInstance

instance Scripts.ValidatorTypes MyInstance where
  type RedeemerType MyInstance = MyRedeemer
  type DatumType MyInstance = MyDatum

{-# INLINEABLE mkMyScript #-}
mkMyScript :: Validator
mkMyScript = Scripts.validatorScript $ 
  Scripts.mkTypedValidator @MyInstance
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @MyDatum @MyRedeemer

{-# INLINEABLE mkValidator #-}
mkValidator :: MyDatum -> MyRedeemer -> ScriptContext -> Bool 
mkValidator MyDatum{..} redeemer context = case redeemer of 
  Deposit -> 
    mdCoinA /= mdCoinB
    && amountA > 0
    && amountB > 0
  where 
    info :: TxInfo
    info = scriptContextTxInfo context

    txOutputs :: [TxOut]
    txOutputs = txInfoOutputs info

    ownOutput :: TxOut
    ownOutput = case [ o
                        | o <- txOutputs,
                          isJust (txOutDatumHash o)
                      ] of
      [o] -> o
      _ -> error ()

    outVal = txOutValue ownOutput
    amountA = assetClassValueOf outVal mdCoinA
    amountB = assetClassValueOf outVal mdCoinB

{-# INLINEABLE mkMyScript' #-}
mkMyScript' :: Validator
mkMyScript' = mkValidatorScript $$(PlutusTx.compile [||mkValidator'||])

{-# INLINEABLE mkValidator' #-}
mkValidator' :: BuiltinData -> BuiltinData -> BuiltinData -> () 
mkValidator' rawDatum rawRedeemer rawContext = 
  case redeemer of 
    Deposit -> if mdCoinA /= mdCoinB
                  && amountA > 0
                  && amountB > 0
              then () else error ()
  where
    MyDatum{..} = PlutusTx.unsafeFromBuiltinData @MyDatum rawDatum
    redeemer = PlutusTx.unsafeFromBuiltinData @MyRedeemer rawRedeemer
    context = PlutusTx.unsafeFromBuiltinData @CC.CustomScriptContext rawContext

    info :: CC.TxInfo
    info = CC.scriptContextTxInfo context

    txOutputs :: [CC.TxOut]
    !txOutputs = CC.txInfoOutputs info

    ownOutput :: CC.TxOut
    !ownOutput = case [ o
                        | o <- txOutputs,
                          isJust (CC.txOutDatumHash o)
                      ] of
      [o] -> o

    !outVal = CC.txOutValue ownOutput
    !amountA = assetClassValueOf outVal mdCoinA
    !amountB = assetClassValueOf outVal mdCoinB

{-# INLINEABLE myScriptSBS #-}
myScriptSBS :: SBS.ShortByteString
myScriptSBS = SBS.toShort . LBS.toStrict $ serialise (unValidatorScript mkMyScript)

{-# INLINEABLE myScriptSerialised #-}
myScriptSerialised :: PlutusScript PlutusScriptV1
myScriptSerialised = PlutusScriptSerialised myScriptSBS

{-# INLINEABLE myScriptSBS' #-}
myScriptSBS' :: SBS.ShortByteString
myScriptSBS' = SBS.toShort . LBS.toStrict $ serialise (unValidatorScript mkMyScript')

{-# INLINEABLE myScriptSerialised' #-}
myScriptSerialised' :: PlutusScript PlutusScriptV1
myScriptSerialised' = PlutusScriptSerialised myScriptSBS'
