# Changelog

## [0.1.1](https://github.com/paolino/cardano-mpfs-offchain/compare/v0.1.0...v0.1.1) (2026-02-20)


### Features

* add aiken to nix dev shell for E2E script simulation ([8e4c2d7](https://github.com/paolino/cardano-mpfs-offchain/commit/8e4c2d7831eeef0365b8fdb3a9540c9286bad898))
* add Aiken-compatible proof serialization ([#13](https://github.com/paolino/cardano-mpfs-offchain/issues/13)) ([71f639c](https://github.com/paolino/cardano-mpfs-offchain/commit/71f639cc21970a9e2339a3b274ee88b5059bd8bc))
* add Application wiring and Skeleton Indexer ([990b1c0](https://github.com/paolino/cardano-mpfs-offchain/commit/990b1c00998d62e8a5f36611e5847ccc9eb2d3bd))
* add cage persistent state — Columns, Codecs, Persistent + round-trip tests ([ea2a577](https://github.com/paolino/cardano-mpfs-offchain/commit/ea2a5775429312039555f4204946aed89ca565a0))
* add cardano-node 10.5.4 to nix dev shell ([8839dc9](https://github.com/paolino/cardano-mpfs-offchain/commit/8839dc9e351b97e600f6712b6a63d24ad6fe85aa))
* add cardano-utxo-csmt dependency and CageEvent module ([f671f88](https://github.com/paolino/cardano-mpfs-offchain/commit/f671f88527ee11e8d9795f1747403ce595a6776d))
* add CIP-57 blueprint validation for on-chain types ([42fb046](https://github.com/paolino/cardano-mpfs-offchain/commit/42fb046a3aa4bd0909616f8dac6c22dc7f304e41))
* add Haskell singleton interfaces for offchain service ([d23f8b1](https://github.com/paolino/cardano-mpfs-offchain/commit/d23f8b153e8af57dd3de31f1480a542f565a2674))
* add Lean 4 testable properties for Phase 4 ([f2838ee](https://github.com/paolino/cardano-mpfs-offchain/commit/f2838ee56e83f44a75c1a258c361f7ab68e6cb49))
* add Lean theorems, codec+inverse QC properties, fix inversesOf bug ([0e803a4](https://github.com/paolino/cardano-mpfs-offchain/commit/0e803a4482cb435bc357bb7ed0869d6fdaf92914))
* add MkDocs architecture documentation with GitHub Pages deployment ([ed7da8a](https://github.com/paolino/cardano-mpfs-offchain/commit/ed7da8ab8ddc680c15ea2f8e223d31f195566f5a))
* add mock implementations for all offchain interfaces ([d16fb60](https://github.com/paolino/cardano-mpfs-offchain/commit/d16fb60b10748ffa1e498cc1a995d942a97885be))
* add N2C LocalStateQuery + LocalTxSubmission clients ([179c10e](https://github.com/paolino/cardano-mpfs-offchain/commit/179c10e5897fe3509e5ad5f8de42625aa118d575))
* add N2C Submitter and LSQ Provider ([b2f3d9e](https://github.com/paolino/cardano-mpfs-offchain/commit/b2f3d9e86009b7fb2d3bf5811b4fb557166b1edf))
* add onchain blueprint integration and bootToken tests ([4d9667c](https://github.com/paolino/cardano-mpfs-offchain/commit/4d9667c7c696a193278fc4069fc15e45761a3fb9))
* add QuickCheck property tests for proof serialization ([8668e40](https://github.com/paolino/cardano-mpfs-offchain/commit/8668e40a40ad824cf971cf5557a740417a7d8685))
* add simple balanceTx for fee-paying UTxO ([f43d6aa](https://github.com/paolino/cardano-mpfs-offchain/commit/f43d6aa2c3af721ffdcbfbf35cab7df66543feaf))
* E2E cage flow with aiken simulate, fix UTxO selection and ExUnits ([f751d30](https://github.com/paolino/cardano-mpfs-offchain/commit/f751d30eb3c6920960d2c0130a1fd223c796c6cb))
* implement update/retract operations and E2E cage flow test ([28e5302](https://github.com/paolino/cardano-mpfs-offchain/commit/28e5302128c557fcf89701bb9ddfb35e35ce8306))
* initial MPF library extracted from haskell-csmt ([0bd61d4](https://github.com/paolino/cardano-mpfs-offchain/commit/0bd61d4bc22568d39eee985669038a6728cb277f))
* move time params from validator params to State datum ([2481cd9](https://github.com/paolino/cardano-mpfs-offchain/commit/2481cd9c113b9c706627e8697ef6df01f4f9fc6e))
* populate offchain interfaces with real types ([8a9547a](https://github.com/paolino/cardano-mpfs-offchain/commit/8a9547a9dbd5b7a703d04b26ecffb17df2f6a542))
* rename package to haskell-mpfs and add architecture plan ([c21f485](https://github.com/paolino/cardano-mpfs-offchain/commit/c21f4856dd4e8562a1caeaaf57eec9aa286bed92))
* sync on-chain types and add real transaction builders ([553a43a](https://github.com/paolino/cardano-mpfs-offchain/commit/553a43a0dc9ab829ea91b2f4e6644141258424e7))
* use real cardano-ledger types and add State property tests ([5ff7c1a](https://github.com/paolino/cardano-mpfs-offchain/commit/5ff7c1ace18d14efa6b82989edf72a252bb89fd5))


### Bug Fixes

* actually run unit tests in CI ([60846b6](https://github.com/paolino/cardano-mpfs-offchain/commit/60846b64a84cf877bc1fba6d4b33b4d15bcf15ca))
* align systemStart between genesis and CageConfig ([1fc5959](https://github.com/paolino/cardano-mpfs-offchain/commit/1fc5959f4936b438f5790cc5d052be7d479c3d8d))
* apply fourmolu and cabal-fmt formatting ([60dd1e4](https://github.com/paolino/cardano-mpfs-offchain/commit/60dd1e4744a1d2bcc6eb4fa9eeea02fb22c54525))
* apply fourmolu formatting to all source files ([01783ff](https://github.com/paolino/cardano-mpfs-offchain/commit/01783ff528451cee33c25e371daa73348066dfe9))
* correct cardano-utxo-csmt tag to actual main HEAD ([17330c5](https://github.com/paolino/cardano-mpfs-offchain/commit/17330c5f25a2e58719bc7b4507889c11df9f5c2f))
* correct double-s typo in cabal package references ([7194869](https://github.com/paolino/cardano-mpfs-offchain/commit/7194869b8020347bc9198ae579a976dff0dda145))
* correct offchain-tests binary name in CI ([4350b7c](https://github.com/paolino/cardano-mpfs-offchain/commit/4350b7ca6c1d511447fea0be4b77f3f32c0c3bda))
* format bench/mpf-bench-rocksdb.hs ([b200903](https://github.com/paolino/cardano-mpfs-offchain/commit/b200903439627dc0a37613cc2367b1bfc316e07e))
* generate Leaf/Fork proof steps matching Aiken format ([b28a011](https://github.com/paolino/cardano-mpfs-offchain/commit/b28a011a2bc0d335dc9d9f25c2f9feca212603aa))
* hlint — remove unused pragmas and eta-reduce lambda ([a4f01d3](https://github.com/paolino/cardano-mpfs-offchain/commit/a4f01d328982a9de81a1a67748574ca1653efafb))
* merge duplicate Data.Serialize imports ([79acb4c](https://github.com/paolino/cardano-mpfs-offchain/commit/79acb4c5748ab7d13f6b4e2218229037d45f2daf))
* replace block-beta mermaid diagrams with flowchart TD ([8dd079e](https://github.com/paolino/cardano-mpfs-offchain/commit/8dd079edf5ef604b31ead0929a3d81df0177ff81))
* resolve all hlint warnings ([6dec2d9](https://github.com/paolino/cardano-mpfs-offchain/commit/6dec2d9099c1e2581cdb56a10e18b386ea429b2e))
* restore when import and update ci recipe ([6800221](https://github.com/paolino/cardano-mpfs-offchain/commit/6800221280816f715a590b9e97dacc26b6b9a8de))
* run offchain tests in CI ([3e553ad](https://github.com/paolino/cardano-mpfs-offchain/commit/3e553ad31cb0d7b0e3e1c4b1764f723c8ae2ae35))
* use estimateMinFeeTx fixpoint in balanceTx ([db75938](https://github.com/paolino/cardano-mpfs-offchain/commit/db759384b10a3c06689c0d3b47f4b734f439ff31))
* use nix build for CI instead of nix develop + cabal ([55e6eb9](https://github.com/paolino/cardano-mpfs-offchain/commit/55e6eb9003d142ccb6156ebae65722db7e0b8eb8))

## 0.1.0.0

- Initial release extracted from haskell-csmt
- 16-ary Merkle Patricia Forestry implementation
- Blake2b-256 hashing compatible with Aiken on-chain validators
- Insertion (single, batch, chunked, streaming)
- Deletion with branch collapsing
- Inclusion proof generation and verification
- Pure in-memory and RocksDB backends
