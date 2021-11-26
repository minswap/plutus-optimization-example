# Plutus Optimization Example
## Installation
- Checkout submodules `private-alonzo-testnet` and `exunits-calculator` by running `git submodule update --init`
- Download cardano-node binaries by running `./private-testnet/scripts/install.sh`
- Install `exunits-calculator` dependencies by running `cd exunits-calculator && npm install && npm install -g ts-node`

## Execution
- You need to run private testnet first by `./private-testnet/scripts/reset.sh`
- There are 2 bash scripts `run.sh` and `run_optimize.sh`, each script will trigger a mock transaction to plutus script defined on `OnChain.hs` file. The transaction data will be stored on `tx.raw` and `tx_optimized.raw` which will be analyzed later
- Basic steps: 
    - Run the private testnet node
    - Run the `run.sh` script
    - Stop the node and re-run
    - Run the `run_optimized.sh` script
    - Open `main.ts` then append two lines:
        - `printExUnitsFromTxBody("../tx.raw");`
        - `printExUnitsFromTxBody("../tx_optimized.raw");`
    - Run `cd exunits-calculator && ts-node main.ts`, you can see the difference of execution unit and the script size between two implementation methods.