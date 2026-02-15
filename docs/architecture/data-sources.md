# Data Sources

All external data enters the system through a single Cardano node
connection. There is **no Ogmios, no Yaci Store** — only a
node-to-client Unix socket carrying multiplexed mini-protocols.

## Connections

```mermaid
graph LR
    NODE["Cardano Node<br/>(Unix socket)"]

    CS["ChainSync"]
    LSQ["LocalStateQuery"]
    LTS["LocalTxSubmission"]

    NODE --- CS
    NODE --- LSQ
    NODE --- LTS

    IDX["Indexer"]
    PRV["Provider"]
    SUB["Submitter"]

    CS --> IDX
    LSQ --> PRV
    LTS --> SUB
```

All three mini-protocols share the same multiplexed socket.

## Data Sources Table

| Data | Source | Protocol | Consumer | Why |
|------|--------|----------|----------|-----|
| Blocks / transactions | Cardano Node | ChainSync | Indexer | Follow the chain to detect token creation, updates, requests, and rollbacks |
| UTxO set by address | `cardano-utxo-csmt` | Mithril bootstrap + ChainSync | Provider | Look up UTxOs for coin selection and script inputs. Key format: `address ++ txId ++ txIx` enables prefix scan by address |
| Protocol parameters | Cardano Node | LocalStateQuery | Provider | Fee coefficients, max tx size, min UTxO, collateral percentage, execution prices. Cached per epoch (~5 days) |
| Transaction evaluation | Cardano Node | LocalStateQuery | Provider | Estimate execution units for Plutus scripts before submission |
| Chain tip | `cardano-utxo-csmt` | ChainSync | Indexer | Track sync progress and settlement |
| Transaction submission | Cardano Node | LocalTxSubmission | Submitter | Submit signed CBOR transactions to the node mempool |

## Data Flow: Source to Singleton

```mermaid
graph TD
    subgraph "Cardano Node"
        CS["ChainSync<br/>(blocks)"]
        LSQ["LocalStateQuery<br/>(params, eval)"]
        LTS["LocalTxSubmission"]
    end

    subgraph "Singletons"
        IDX["Indexer"]
        PRV["Provider"]
        SUB["Submitter"]
        ST["State"]
        TM["TrieManager"]
    end

    subgraph "Database"
        DB_ST["State tables<br/>(tokens, requests,<br/>checkpoints)"]
        DB_TM["Trie storage<br/>(per-token MPF<br/>nodes)"]
        DB_UTXO["UTxO index<br/>(address-prefixed<br/>CSMT)"]
    end

    CS -->|"blocks & txs"| IDX
    IDX -->|"token events"| ST
    IDX -->|"trie updates"| TM
    ST -->|"read/write"| DB_ST
    TM -->|"read/write"| DB_TM

    LSQ -->|"protocol params"| PRV
    LSQ -->|"execution units"| PRV
    PRV -->|"UTxO queries"| DB_UTXO

    LTS --> SUB
```

## Node-to-Client Mini-Protocols

### ChainSync

Follows the chain block by block. The Indexer processes each
block's transactions through a pure `Process` function that
identifies relevant events:

- New token creation
- Token updates (fact insertion/deletion)
- New requests
- Rollbacks (rewind state to a previous slot)

`cardano-utxo-csmt` already implements ChainSync — the Indexer
reuses this connection.

### LocalStateQuery

Queries the current ledger state. Used for two purposes:

1. **Protocol parameters** — fee coefficients, max transaction size,
   minimum UTxO value, collateral percentage, Plutus execution prices.
   Cached per epoch (~5 days), so queries are infrequent.

2. **Transaction evaluation** — estimate execution units for Plutus
   scripts. Called during transaction building to compute fees
   accurately before signing.

### LocalTxSubmission

Submits a signed transaction (CBOR) to the node's mempool.
The Submitter singleton wraps this protocol.
