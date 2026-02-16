# Architecture Overview

## System Stack

```mermaid
flowchart TD
    http["HTTP Service (Servant)"]
    mpf["MPF Trie (haskell-mpf)\nProofs, insertion, deletion"]
    txb["Transaction Building Interface\nCoin selection, fee estimation\n(record of functions)"]
    csmt["cardano-utxo-csmt (embedded)\nUTxO queries via address prefix\nMithril bootstrap, ChainSync"]
    node["Node Client (node-to-client)\nLocal state query + tx submission"]

    http --> mpf --> txb --> csmt --> node
```

The system is a vertical stack. The HTTP layer receives oracle
requests, delegates to the MPF trie logic, which in turn uses
the transaction builder to construct Cardano transactions.
The transaction builder relies on the UTxO index (provided by
`cardano-utxo-csmt`) and the node client for protocol parameters,
transaction evaluation, and submission.

## Singleton Dependency Graph

Every major component is a **record of functions** (no typeclasses).
Records are created bottom-up and torn down top-down using bracket
patterns.

```mermaid
graph TD
    HTTP["HTTP API<br/>(Servant)"]
    CTX["Context<br/>(facade record)"]
    PRV["Provider<br/>(CSMT + node queries)"]
    TM["TrieManager<br/>(per-token MPF tries)"]
    ST["State<br/>(tokens, requests, rollbacks)"]
    IDX["Indexer<br/>(ChainSync follower)"]
    SUB["Submitter<br/>(LocalTxSubmission)"]
    DB["RocksDB"]
    NODE["Cardano Node<br/>(node-to-client socket)"]

    HTTP --> CTX
    CTX --> PRV
    CTX --> TM
    CTX --> ST
    CTX --> IDX
    CTX --> SUB
    PRV --> DB
    PRV --> NODE
    TM --> DB
    ST --> DB
    IDX --> NODE
    IDX --> ST
    IDX --> TM
    SUB --> NODE
```

## Creation Order

```mermaid
graph LR
    DB["RocksDB"] --> TM["TrieManager"]
    TM --> ST["State"]
    ST --> PR["Process"]
    PR --> IDX["Indexer"]
    IDX --> CTX["Context"]
    CTX --> API["HTTP API"]
```

Each layer uses a `withX` bracket for resource cleanup guarantees.

## External Dependencies

```mermaid
graph TD
    OFFCHAIN["cardano-mpfs-offchain\nService interfaces"]
    MPF["merkle-patricia-forestry\nMPF trie library"]
    RKVT["rocksdb-kv-transactions\npaolino/"]
    KVT["kv-transactions\nsublibrary"]
    ROCKSDB["rocksdb-haskell-jprupp\nHackage"]
    CRYPTON["crypton\nBlake2b-256"]
    CEREAL["cereal\nSerialization"]
    LENS["lens"]
    CSMT["cardano-utxo-csmt\nPlanned"]
    LEDGER["cardano-ledger\nPlanned"]

    OFFCHAIN --> MPF
    MPF --> RKVT
    MPF --> KVT
    MPF --> CRYPTON
    MPF --> CEREAL
    MPF --> LENS
    RKVT --> ROCKSDB
    RKVT --> KVT
    OFFCHAIN -.-> CSMT
    OFFCHAIN -.-> LEDGER

    style OFFCHAIN fill:#e1f5fe
    style MPF fill:#e8f5e9
    style RKVT fill:#fff3e0
    style KVT fill:#fff3e0
    style CSMT fill:#f3e5f5,stroke-dasharray:5
    style LEDGER fill:#f3e5f5,stroke-dasharray:5
```

| Color | Meaning |
|-------|---------|
| Blue | Offchain service (interfaces only) |
| Green | Merkle Patricia Forestry trie library |
| Orange | Custom repos (paolino/) |
| Purple dashed | Planned, not yet wired |
| White | Hackage dependencies |

## Design Principles

- **No typeclasses** — closed world with explicit records of functions.
- **Monad polymorphism** — all interfaces are polymorphic in `m`.
- **Visible dependency graph** — no implicit resolution surprises.
- **Trivial testing** — swap the record for a mock backend.
- **No orphan instances**.

## Implementation Phases

```mermaid
graph LR
    P0["Phase 0<br/>MPF Library ✓"]
    P1["Phase 1<br/>TxBuilder Interface"]
    P2["Phase 2<br/>UTxO Index +<br/>Node Client"]
    P3["Phase 3<br/>Transaction<br/>Builders"]
    P4["Phase 4<br/>Service"]
    P5["Phase 5<br/>Deployment"]

    P0 --> P1 --> P2 --> P3 --> P4 --> P5

    style P0 fill:#2d6,color:#fff
```

| Phase | Description | Status |
|-------|-------------|--------|
| 0 | MPF library — 16-ary Merkle Patricia Forestry, Blake2b-256 hashing, insertion/deletion/proofs, pure and RocksDB backends | Done |
| 1 | Transaction building interface — `TxBuilder` record, domain types, mock backend | Planned |
| 2 | UTxO index + node client — embed `cardano-utxo-csmt`, address-prefixed keys, LocalStateQuery, LocalTxSubmission | Planned |
| 3 | Transaction builders — boot, update, retract, end transactions with balancing and signing | Planned |
| 4 | Service — HTTP API, indexer, RocksDB-backed state, submitter | Planned |
| 5 | Deployment — Docker via Nix, deploy to plutimus.com | Planned |
