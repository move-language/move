# Hardhat Project for Move-on-Evm

This project contains running examples for the hardhat integration with Move.

The `contracts` directory contains Move contracts (e.g., `FortyTwo.move`) and the equivalent Solidity contracts (e.g., `FortyTwo.sol`). The `test` directory contains test files (e.g., `FortyTwo.test.js`) which are shared by both Move contracts and Solidity contracts.

There are scripts to automate testing. `compile_sol_and_test.sh` tests the Solidity contracts while `compile_move_and_test.sh` tests the Move contracts against the same test files. Before running these scripts, set up the hardhat environment (https://hardhat.org/tutorial/setting-up-the-environment.html).
