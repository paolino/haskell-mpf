# Cardano MPFS Offchain

Haskell implementation of the offchain component for
[Merkle Patricia Forestry](https://github.com/aiken-lang/merkle-patricia-forestry)
on Cardano.

This service manages MPF tries, builds and submits transactions,
and exposes an HTTP API for oracle operations.

## Documentation

- [Architecture Overview](architecture/overview.md) — system diagram, phases, design principles
- [Data Sources](architecture/data-sources.md) — connections, protocols, data flow
- [Singletons](architecture/singletons.md) — record-of-functions interfaces
- [Database Layer](architecture/database.md) — storage design, RocksDB abstraction
