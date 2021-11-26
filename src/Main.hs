{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Cardano.Api
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V1.Ledger.Api as Plutus
import System.Environment (getArgs)
import Prelude
import OnChain
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import Plutus.V1.Ledger.Value (tokenName, currencySymbol, assetClass)
import Text.Hex (decodeHex)
import Data.Maybe (fromJust)
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Cardano.Api.Shelley (fromAlonzoData)
import Data.Aeson (encode)

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "compile" -> case args !! 1 of
      "script" -> writePlutusScript "plutus-script/normal_script.plutus" myScriptSerialised myScriptSBS
      "script-optimization" -> writePlutusScript "plutus-script/optimized_script.plutus" myScriptSerialised' myScriptSBS'
      _ -> error "Script not supported"
    "print" -> case args !! 1 of
      "datum" -> printDatum 
      "redeemer" -> printRedeemer 
      _ -> error "Script not supported"
    _ -> error "Command not supported"

printDatum :: IO ()
printDatum = do
  let coinA = assetClass adaSymbol adaToken
      coinB = assetClass (currencySymbol $ fromJust $ decodeHex "d797b57f7b27a1db66f792eb077044b13ae49df38df9f3ef816aa5f9") (tokenName "tMIN")
      datum = MyDatum coinA coinB
      scriptDatum = encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromAlonzoData $ Alonzo.Data $ Plutus.toData datum)
  print scriptDatum

printRedeemer :: IO ()
printRedeemer = do
  let redeemer = Deposit
      scriptDatum = encode (scriptDataToJson ScriptDataJsonDetailedSchema $ fromAlonzoData $ Alonzo.Data $ Plutus.toData redeemer)
  print scriptDatum

writePlutusScript :: FilePath -> PlutusScript PlutusScriptV1 -> SBS.ShortByteString -> IO ()
writePlutusScript filename scriptSerial scriptSBS =
  do
    case Plutus.defaultCostModelParams of
      Just m ->
        let (logout, e) = Plutus.evaluateScriptCounting Plutus.Verbose m scriptSBS []
         in do
              print ("Log output" :: String) >> print logout
              case e of
                Left evalErr -> print ("Eval Error" :: String) >> print evalErr
                Right exbudget -> print ("Ex Budget" :: String) >> print exbudget
      Nothing -> error "defaultCostModelParams failed"
    result <- writeFileTextEnvelope filename Nothing scriptSerial
    case result of
      Left err -> print $ displayError err
      Right () -> return ()
