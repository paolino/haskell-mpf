# cardano-mpfs-offchain

Off-chain companion to
[cardano-mpfs-onchain](https://github.com/cardano-foundation/cardano-mpfs-onchain)
— indexing, transaction building, and submission for Cardano Merkle
Patricia Forestry.

## Packages

| Package | Description |
|---|---|
| `merkle-patricia-forestry` | 16-ary hex Patricia trie with Blake2b-256, compatible with the [Aiken on-chain implementation](https://github.com/aiken-lang/merkle-patricia-forestry) |
| `cardano-mpfs-offchain` | Off-chain service layer: N2C node client, UTxO provider, transaction balancing, submission, and skeleton indexer |

## Building

```bash
nix develop
just build
just unit            # MPF unit tests
just unit-offchain   # offchain unit tests
just e2e             # E2E tests (requires cardano-node in PATH)
```

## Documentation

https://paolino.github.io/cardano-mpfs-offchain/
