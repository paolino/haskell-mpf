# PLAN: Haskell MPFS Service

## Goal

Rewrite the MPFS (Merkle Patricia Forestry Service) in Haskell,
replacing the TypeScript off-chain implementation while keeping the
Aiken on-chain validators unchanged.

## Architecture

```
┌─────────────────────────────────────┐
│         haskell-mpfs                │
│  HTTP service (Servant)             │
├─────────────────────────────────────┤
│  MPF trie (haskell-mpf)             │  ← DONE
│  Proofs, insertion, deletion        │
├─────────────────────────────────────┤
│  Transaction building & balancing   │  ← extracted from cardano-wallet
│  Coin selection, fee estimation     │
├─────────────────────────────────────┤
│  Yaci Store client (HTTP)           │  ← thin REST client
│  UTxO queries, protocol params      │
├─────────────────────────────────────┤
│  Signing & submission               │
│  via Yaci / Ogmios                  │
└─────────────────────────────────────┘
```

## Components

### 1. MPF Library ✓ DONE

Repository: `paolino/haskell-mpf` (this repo, to be renamed haskell-mpfs)

- 16-ary Merkle Patricia Forestry
- Blake2b-256, Aiken-compatible
- Proofs, insertion, deletion
- Pure + RocksDB backends

### 2. Transaction Balancing — Extract from cardano-wallet

**Goal:** Standalone `cardano-balance-tx` with zero cardano-wallet
dependency.

**Source:** `cardano-wallet/lib/balance-tx/` + `lib/coin-selection/`

**What exists:**
- `balanceTx` — takes PartialTx + UTxOs + protocol params → balanced Tx
- `cardano-coin-selection` — coin selection algorithms
- Both already semi-separated in the wallet monorepo

**Wallet coupling (to remove):**
- 11 thin wrapper types from `cardano-wallet-primitive`
  (Coin, Address, TxIn, TxOut, TokenBundle, TokenMap, etc.)
- `Convert` module — ~26 wallet↔ledger conversion functions
- These are all mechanical, no business logic

**Extraction strategy:**
1. Create minimal types package (~480 LOC) replacing wallet-primitive
2. Inline or vendor the Convert module (~200 LOC)
3. Copy balance-tx (~5,200 LOC) and coin-selection (~5,500 LOC)
4. Update imports, no logic changes needed
5. Migrate test suite (~800 LOC)

**Dependencies after extraction:**
- cardano-ledger-* (api, babbage, conway, alonzo, core)
- cardano-api
- cardano-addresses
- cardano-slotting
- standard Haskell (base, containers, etc.)
- NO cardano-wallet

**Effort estimate:** 7-10 days

**Open question:** Worth doing now or start with cardano-wallet-primitive
as dependency and extract later? Starting with the dependency is faster
but pulls in ouroboros-network transitively.

### 3. Yaci Store Client

**What we need from Yaci:**
- `GET /api/v1/utxos?address=<script_address>` — UTxOs at our address
- Protocol parameters (cached, updated per epoch)
- Transaction submission

**Implementation:** Simple Servant client or http-conduit, ~200 LOC.

No Ouroboros/ChainSync needed — Yaci handles node communication.

### 4. Service Layer

**Mirrors the TypeScript service:**
- HTTP API (GET/POST) for facts, proofs
- Swagger/OpenAPI docs
- Chain indexer watches for MPF-related transactions
- Submitter with retry logic

**From the TypeScript codebase (`mpfs/off_chain/`):**
- `service/signingless/` — HTTP endpoints
- `indexer/` — chain event watching via Ogmios
- `transactions/` — boot, request, update, retract tx builders
- `submitter.ts` — submission + retry

## Phases

### Phase 0 — MPF Library ✓
Extract MPF from haskell-csmt. Done.

### Phase 1 — Balance TX Extraction
- Extract cardano-balance-tx as standalone package
- Create minimal types replacing wallet-primitive
- Vendor coin-selection
- Full test suite

### Phase 2 — Yaci Store Client
- HTTP client for UTxO queries
- Protocol parameter fetching
- Transaction submission endpoint

### Phase 3 — Transaction Builders
- Boot transaction (create new MPF instance)
- Update transaction (insert/delete facts with proofs)
- Retract transaction
- End transaction
- Wire up balanceTx + signing

### Phase 4 — Service
- HTTP API matching TypeScript interface
- Indexer for chain events
- RocksDB-backed MPF trie state
- Submitter with retry

### Phase 5 — Deployment
- Docker image via Nix
- Deploy to plutimus.com alongside existing TypeScript service
- Integration tests with Yaci devkit

## Open Questions

1. **balanceTx extraction timing** — extract now or depend on
   cardano-wallet-primitive initially?
2. **Signing** — use cardano-api signing or keep keys external
   (signingless mode)?
3. **Indexer** — reuse Yaci store events or implement minimal
   ChainSync for our script address only?
4. **Multi-oracle** — the TypeScript version supports multiple
   oracles. Same for Haskell?
