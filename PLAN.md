# PLAN: Haskell MPFS Service

## Goal

Rewrite the MPFS (Merkle Patricia Forestry Service) in Haskell,
replacing the TypeScript off-chain implementation while keeping the
Aiken on-chain validators unchanged.

## Design Principles

**No typeclasses.** This is a closed world — we use explicit records
of functions (dictionaries), polymorphic in the monad. Pass them
around explicitly. Never use typeclasses for abstraction.

This keeps the dependency graph visible, makes testing trivial
(swap the record), and avoids orphan instances and implicit
resolution surprises.

## Architecture

```
┌─────────────────────────────────────┐
│         haskell-mpfs                │
│  HTTP service (Servant)             │
├─────────────────────────────────────┤
│  MPF trie (haskell-mpf)             │  ← DONE
│  Proofs, insertion, deletion        │
├─────────────────────────────────────┤
│  Transaction building interface     │  ← record of functions
│  Coin selection, fee estimation     │
├─────────────────────────────────────┤
│  cardano-utxo-csmt (embedded)      │  ← replaces Yaci Store
│  UTxO queries via address prefix    │
│  Mithril bootstrap, ChainSync      │
├─────────────────────────────────────┤
│  Ogmios client (thin)              │
│  Protocol params + tx submission    │
└─────────────────────────────────────┘
```

**Decision: No Yaci Store.** We embed `cardano-utxo-csmt` directly.
It already does ChainSync + Mithril bootstrap + full UTxO set in
RocksDB. To support UTxO-by-address queries we change the CSMT key
encoding to `address ++ txId ++ txIx` so prefix scans give us all
UTxOs at an address. Ogmios is only needed for protocol parameters
and transaction submission (2 endpoints).

**Runtime stack:** Cardano node + Ogmios (no Yaci Store).

## TypeScript Singleton Map

The existing TypeScript service (`mpfs/off_chain/`) is built around
6 singletons — records of async functions, created once at startup
and passed around explicitly. This is the architecture we replicate
in Haskell (replacing `Promise<T>` with `m a`).

### Dependency graph

```
         HTTP API (express, port N)
              |
              v
     +--------+--------+
     |     Context      |  facade — bundles all singletons
     +--+--+--+--+--+--+  into one record for tx builders
        |  |  |  |  |  |  and HTTP handlers
        |  |  |  |  |  |
        v  |  v  |  v  |
  Provider |  State  |  Indexer
  (Yaci/   |  |      |  |
  Blockf.) |  |      |  v
        |  |  |  Ogmios WS
        |  v  v  (ChainSync)
        | TrieManager  |
        | (per-token   |
        |  MPF tries)  |
        |     |        |
        +--+--+--------+
           |
           v
        LevelDB
```

### The 6 singletons

**1. Provider** — blockchain queries (read-only)

UTxO lookups via cardano-utxo-csmt (address prefix scan on
the embedded CSMT). Protocol parameters and tx evaluation
via Ogmios local-state-query.

```
Provider m = Provider
  { fetchUtxos         :: Address -> m [UTxO]  -- CSMT prefix scan
  , fetchProtocolParams :: m ProtocolParams     -- Ogmios
  , evaluateTx         :: TxCBOR -> m ExUnits   -- Ogmios
  }
```

**2. TrieManager** — per-token MPF trie storage

Manages a map of token-id to MPF trie, backed by LevelDB
sublevels. Mutex-locked access. Supports hide/unhide for
rollback handling.

```
TrieManager m = TrieManager
  { withTrie :: TokenId -> (SafeTrie m -> m a) -> m a
  , trieIds  :: m [TokenId]
  , hide     :: TokenId -> m ()
  , unhide   :: TokenId -> m ()
  , delete   :: TokenId -> m ()
  }
```

**3. State** — chain-derived state (tokens, requests, rollbacks)

Tracks known tokens, pending requests, checkpoints, and
rollback journal. All in LevelDB sublevels. Mutex-locked.

```
State m = State
  { addToken      :: Slotted Token -> m ()
  , removeToken   :: Slotted TokenId -> m ()
  , updateToken   :: Slotted TokenChange -> m ()
  , addRequest    :: Slotted Request -> m ()
  , removeRequest :: Slotted OutputRef -> m ()
  , rollback      :: WithOrigin Slot -> m ()
  , tokens        :: Tokens m
  , requests      :: Requests m
  , checkpoints   :: Checkpoints m
  }
```

**4. Indexer** — Ogmios ChainSync follower

WebSocket connection to Ogmios. Follows the chain block by
block, calling a Process function for each transaction.
Pausable via mutex (paused during tx building to avoid
state races).

```
Indexer m = Indexer
  { tips       :: m (NetworkTip, IndexerTip)
  , waitBlocks :: Int -> m BlockHeight
  , pause      :: m (m ())   -- returns release action
  }
```

**5. Submitter** — Ogmios tx submission

Separate Ogmios WebSocket connection for submitting signed
transactions. Stateless, opens a connection per submission.

```
Submitter m = Submitter
  { submitTx :: TxCBOR -> m TxHash
  }
```

**6. Context** — facade record

Bundles all singletons into one record passed to transaction
builders and HTTP handlers. This is the main "environment"
threaded through the application.

```
Context m = Context
  { cagingScript  :: CagingScript
  , signingWallet :: Maybe (SigningWallet m)
  , addressWallet :: Address -> m WalletInfo
  , newTxBuilder  :: m (TxBuilder m)
  , fetchTokens   :: m [Token]
  , fetchToken    :: TokenId -> m (Maybe Token)
  , fetchRequests :: Maybe TokenId -> m [Request]
  , evaluateTx    :: TxCBOR -> m ExUnits
  , withTrie      :: TokenId -> (SafeTrie m -> m a) -> m a
  , waitBlocks    :: Int -> m BlockHeight
  , tips          :: m (NetworkTip, IndexerTip)
  , waitSettlement :: TxHash -> m BlockHash
  , facts         :: TokenId -> m (Map Key Value)
  , pauseIndexer  :: m (m ())
  , submitTx      :: TxCBOR -> m TxHash
  }
```

### Creation order

Singletons are created bottom-up and torn down top-down
(bracket pattern / `withX` nesting):

```
LevelDB
  -> TrieManager
    -> State
      -> Process (pure function: State + TrieManager -> Tx -> m ())
        -> Indexer (takes State, Process, Ogmios URL)
          -> Context (bundles everything)
            -> HTTP API (takes Context)
```

Each layer is created with a `withX` bracket that guarantees
cleanup (close connections, flush DB) on exit.

## Components

### 1. MPF Library ✓ DONE

Repository: `paolino/haskell-mpf` (this repo, to be renamed haskell-mpfs)

- 16-ary Merkle Patricia Forestry
- Blake2b-256, Aiken-compatible
- Proofs, insertion, deletion
- Pure + RocksDB backends

### 2. Transaction Building Interface

**Goal:** Define a pluggable interface (record of functions) for
transaction building. The backend implementation is deferred —
could be extracted from cardano-wallet, or a lightweight custom
implementation.

**Interface covers:**
- Coin selection: pick inputs to cover outputs + fees
- Fee estimation: compute fees for a given transaction body
- Balancing: adjust a partial transaction to be valid
  (inputs ≥ outputs + fees, correct change outputs)

**Shape** (sketch):

```haskell
data TxBuilder m = TxBuilder
  { balanceTx
      :: PartialTx -> UTxOSet -> ProtocolParams -> m BalancedTx
  , estimateFee
      :: TxBody -> ProtocolParams -> m Coin
  , selectCoins
      :: CoinSelectionGoal -> UTxOSet -> m CoinSelection
  }
```

No typeclasses — pass `TxBuilder m` explicitly.

**Possible backends (decided later):**
- Extracted `cardano-balance-tx` from cardano-wallet
- Lightweight custom implementation (our transactions are simple)
- Mock backend for testing

### 3. UTxO Index (cardano-utxo-csmt)

**Replaces Yaci Store entirely.** We embed cardano-utxo-csmt as a
library dependency. It provides:

- Full UTxO set via Mithril bootstrap + ChainSync
- RocksDB-backed CSMT with rollback support
- Merkle inclusion proofs for any UTxO

**Required change:** re-key the CSMT to `address ++ txId ++ txIx`
so that `seekKey(addressPrefix)` + cursor iteration gives all
UTxOs at an address. This is a change in `UTxOs.hs` key encoding,
not in the CSMT library itself.

**What we still need from Ogmios** (thin client, ~100 LOC):
- `queryLedgerState/protocolParameters` — cached per epoch
- `submitTransaction` — tx submission
- `evaluateTransaction` — execution unit estimation

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

### Phase 1 — Transaction Building Interface
- Define `TxBuilder m` record of functions
- Define domain types (PartialTx, UTxOSet, ProtocolParams, etc.)
- Mock backend for testing
- Actual backend implementation deferred to Phase 3

### Phase 2 — UTxO Index + Ogmios Client
- Embed cardano-utxo-csmt as library (Mithril + ChainSync)
- Add address-prefixed key encoding for UTxO-by-address queries
- Thin Ogmios client for protocol params + tx submission

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
- Integration tests with Yaci devkit or local testnet

## Open Questions

1. **TxBuilder backend** — extract cardano-balance-tx from wallet
   (proven but heavyweight) or write a lightweight custom
   implementation (our transactions are simple)?
2. **Signing** — use cardano-api signing or keep keys external
   (signingless mode)?
3. ~~**Indexer**~~ DECIDED — embed cardano-utxo-csmt, no Yaci.
   ChainSync via csmt, protocol params + submit via Ogmios.
4. **Multi-oracle** — the TypeScript version supports multiple
   oracles. Same for Haskell?
