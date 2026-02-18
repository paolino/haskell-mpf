# Architecture Overview

## System Stack

```mermaid
flowchart TD
    app["Application<br/>(wiring + lifecycle)"]
    mpf["MPF Trie<br/>(merkle-patricia-forestry)<br/>Proofs, insertion, deletion"]
    txb["TxBuilder<br/>(MPFS protocol operations)<br/>boot, update, retract, end"]
    bal["Balance<br/>(fee estimation fixpoint)"]
    n2c["Node Client<br/>(node-to-client)<br/>LocalStateQuery + LocalTxSubmission"]

    app --> mpf --> txb --> bal --> n2c
```

The service connects to a Cardano node via a Unix socket carrying
multiplexed N2C mini-protocols. The `Provider` queries UTxOs and
protocol parameters via `LocalStateQuery`; the `Submitter` sends
signed transactions via `LocalTxSubmission`. The `TxBuilder`
constructs MPFS protocol transactions (boot, update, retract, end)
and `balanceTx` handles fee estimation through a fixpoint loop.

## Singleton Dependency Graph

Every major component is a **record of functions** (no typeclasses).
Records are created bottom-up and torn down top-down using bracket
patterns.

```mermaid
graph TD
    APP["Application<br/>(withApplication)"]
    CTX["Context<br/>(facade record)"]
    PRV["Provider<br/>(N2C LocalStateQuery)"]
    TM["TrieManager<br/>(per-token MPF tries)"]
    ST["State<br/>(tokens, requests, checkpoints)"]
    IDX["Indexer<br/>(skeleton / chain sync)"]
    SUB["Submitter<br/>(N2C LocalTxSubmission)"]
    TXB["TxBuilder<br/>(MPFS protocol ops)"]
    NODE["Cardano Node<br/>(Unix socket)"]

    APP --> CTX
    CTX --> PRV
    CTX --> TM
    CTX --> ST
    CTX --> IDX
    CTX --> SUB
    CTX --> TXB
    PRV --> NODE
    SUB --> NODE
```

## Application Wiring

`withApplication` creates and wires all components:

```mermaid
graph LR
    N2C["N2C Connection"] --> PRV["Provider"]
    N2C --> SUB["Submitter"]
    MOCK_ST["Mock State"] --> ST["State"]
    PURE_TM["Pure TrieManager"] --> TM["TrieManager"]
    SKEL["Skeleton Indexer"] --> IDX["Indexer"]
    MOCK_TXB["Mock TxBuilder"] --> TXB["TxBuilder"]
    PRV & SUB & ST & TM & IDX & TXB --> CTX["Context"]
```

The `Provider` and `Submitter` use real N2C connections. State,
TrieManager, and Indexer currently use mock/pure/skeleton
implementations.

## External Dependencies

```mermaid
graph TD
    OFFCHAIN["cardano-mpfs-offchain<br/>Service interfaces + N2C client"]
    MPF["merkle-patricia-forestry<br/>MPF trie library"]
    ONCHAIN["cardano-mpfs-onchain<br/>Aiken validators"]
    LEDGER["cardano-ledger<br/>Conway era types"]
    OUROBOROS["ouroboros-network<br/>N2C protocols"]
    PLUTUS["plutus-core / plutus-tx<br/>PlutusData encoding"]

    OFFCHAIN --> MPF
    OFFCHAIN --> LEDGER
    OFFCHAIN --> OUROBOROS
    OFFCHAIN --> PLUTUS
    OFFCHAIN -.->|"on-chain types<br/>& script hash"| ONCHAIN

    style OFFCHAIN fill:#e1f5fe
    style MPF fill:#e8f5e9
    style ONCHAIN fill:#fff3e0
    style LEDGER fill:#f3e5f5
    style OUROBOROS fill:#f3e5f5
    style PLUTUS fill:#f3e5f5
```

| Color | Meaning |
|-------|---------|
| Blue | This project |
| Green | MPF trie library (same repo) |
| Orange | On-chain Aiken validators (separate repo) |
| Purple | Cardano ecosystem dependencies |

## Design Principles

- **No typeclasses** — closed world with explicit records of functions.
- **All types from cardano-ledger** — `Tx ConwayEra`, `PParams ConwayEra`, `Addr`, `TxIn`, etc.
- **Visible dependency graph** — no implicit resolution surprises.
- **Trivial testing** — swap the record for a mock backend.
- **No orphan instances**.

## Implementation Phases

```mermaid
graph LR
    P0["Phase 0<br/>MPF Library ✓"]
    P1["Phase 1<br/>Service Interfaces ✓"]
    P2["Phase 2<br/>N2C Client +<br/>Provider ✓"]
    P3["Phase 3<br/>Transaction<br/>Builders"]
    P4["Phase 4<br/>ChainSync Indexer +<br/>Persistent State"]
    P5["Phase 5<br/>HTTP API +<br/>Deployment"]

    P0 --> P1 --> P2 --> P3 --> P4 --> P5

    style P0 fill:#2d6,color:#fff
    style P1 fill:#2d6,color:#fff
    style P2 fill:#2d6,color:#fff
```

| Phase | Description | Status |
|-------|-------------|--------|
| 0 | MPF library — 16-ary Merkle Patricia Forestry, Blake2b-256 hashing, insertion/deletion/proofs, pure and RocksDB backends | Done |
| 1 | Service interfaces — `Provider`, `Submitter`, `TxBuilder`, `State`, `Indexer`, `TrieManager`, `Context` records; mock implementations; `balanceTx` with fixpoint fee estimation; on-chain type encodings; CIP-57 blueprint validation; Aiken-compatible proof serialization | Done |
| 2 | N2C client + Provider — `ouroboros-network` LocalStateQuery and LocalTxSubmission clients; `mkNodeClientProvider` for UTxO and PParams queries; `mkN2CSubmitter` for transaction submission; E2E tests with cardano-node subprocess | Done |
| 3 | Transaction builders — real `TxBuilder` implementations for boot, update, retract, end operations with Plutus script witnesses, proof embedding, and on-chain datum construction | Planned |
| 4 | ChainSync indexer + persistent state — replace skeleton indexer with real ChainSync follower; RocksDB-backed State and TrieManager; block processing with rollback support | Planned |
| 5 | HTTP API + deployment — Servant HTTP layer, Docker via Nix, deploy to plutimus.com | Planned |
