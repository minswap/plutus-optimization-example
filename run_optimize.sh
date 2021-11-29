#!/usr/bin/env bash

set -euo pipefail

export PATH=$PWD/private-testnet/bin:$PATH
export CARDANO_NODE_SOCKET_PATH=$PWD/private-testnet/sockets/node-pool1.sock
faucetaddr=$(cardano-cli address build --payment-verification-key-file private-testnet/shelley/utxo-keys/utxo1.vkey --testnet-magic 42)
currencySymbol=$(cardano-cli transaction policyid --script-file private-testnet/shelley/utxo-keys/minting-policy.json)
scriptaddr=$(cardano-cli address build --payment-script-file plutus-script/optimized_script.plutus --testnet-magic 42)
datumhash=$(cardano-cli transaction hash-script-data --script-data-file plutus/datum.json)

cardano-cli transaction build \
  --alonzo-era \
  --change-address "$faucetaddr" \
  --tx-in "6035f29d428e1376611d658450c0905443907200205bbd48fb3898674c2ad5c3#0" \
  --tx-in "6035f29d428e1376611d658450c0905443907200205bbd48fb3898674c2ad5c3#1" \
  --tx-out "$faucetaddr"+10000000+"45000000000000000 $currencySymbol + 45000000000000000 $currencySymbol.tBTC + 45000000000000000 $currencySymbol.tETH + $((45000000000000000-100000000000)) $currencySymbol.tMIN" \
  --tx-out "$scriptaddr"+100000000000+"100000000000 $currencySymbol.tMIN" \
  --tx-out-datum-hash $datumhash \
  --testnet-magic 42 \
  --out-file tx_optimized.raw

cardano-cli transaction sign \
  --tx-body-file tx_optimized.raw \
  --signing-key-file private-testnet/shelley/utxo-keys/utxo1.skey \
  --testnet-magic 42 \
  --out-file tx_optimized.signed

cardano-cli transaction submit --testnet-magic 42 --tx-file tx_optimized.signed

sleep 30

txId=813e23afd33cc4f5a3db500ab871170e66b804f91343b5098358186d2324df5b
cardano-cli transaction build \
  --alonzo-era \
  --change-address "$faucetaddr" \
  --tx-in "$txId#0" \
  --tx-in "$txId#1" \
  --tx-in "$txId#2" \
  --tx-in-collateral "$txId#0" \
  --tx-in-script-file plutus-script/optimized_script.plutus \
  --tx-in-datum-file plutus/datum.json \
  --tx-in-redeemer-file plutus/redeemer.json \
  --tx-out "$faucetaddr"+10000000+"45000000000000000 $currencySymbol + 45000000000000000 $currencySymbol.tBTC + 45000000000000000 $currencySymbol.tETH + $((45000000000000000-200000000000)) $currencySymbol.tMIN" \
  --tx-out "$scriptaddr"+200000000000+"200000000000 $currencySymbol.tMIN" \
  --tx-out-datum-hash $datumhash \
  --testnet-magic 42 \
  --protocol-params-file protocol-parameters.json \
  --out-file tx_optimized.raw

cardano-cli transaction sign \
  --tx-body-file tx_optimized.raw \
  --signing-key-file private-testnet/shelley/utxo-keys/utxo1.skey \
  --testnet-magic 42 \
  --out-file tx_optimized.signed

cardano-cli transaction submit --testnet-magic 42 --tx-file tx_optimized.signed
