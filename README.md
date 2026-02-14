# haskell-mpfs

Merkle Patricia Forestry (MPF) implementation in Haskell.

A 16-ary hex-based Patricia trie with Blake2b-256 hashing, providing
insertion, deletion, and cryptographic inclusion proofs compatible with
the [Aiken on-chain implementation](https://github.com/aiken-lang/merkle-patricia-forestry).

## Features

- **Hex-nibble Patricia trie** with path compression
- **Blake2b-256 hashing** verified against Aiken reference
- **Inclusion proofs** — generation and pure verification
- **Multiple insertion strategies** — single, batch, chunked, streaming
- **Pluggable backends** — in-memory (pure) and RocksDB

## Building

```bash
nix develop
just build
just unit
```
