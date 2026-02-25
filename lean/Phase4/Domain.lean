/-
  Phase 4 Domain Model

  Minimal model for stating testable properties. Each
  theorem here becomes a QuickCheck monadic property
  running against real RocksDB.
-/

namespace Phase4

-- Abstract types (opaque in Lean, concrete in QC)
variable (SlotNo TokenId TxIn Root K V : Type)

-- Trie visibility (models TrieStatus)
inductive TrieVisibility where
  | visible
  | hidden
  deriving Repr, DecidableEq

-- Cage state: tokens + requests + trie visibility + trie entries
structure CageState (TokenId TxIn Root K V : Type) where
  tokens : List (TokenId × Root)
  requests : List TxIn
  trieVisibility : TokenId → TrieVisibility
  trieEntries : TokenId → K → Option V

-- A cage event from a block
inductive CageEvent (TokenId TxIn Root : Type) where
  | boot (tid : TokenId) (root : Root)
  | request (txIn : TxIn)
  | update (tid : TokenId) (newRoot : Root)
      (consumed : List TxIn)
  | retract (txIn : TxIn)
  | burn (tid : TokenId)

-- An inverse operation (stored for rollback)
inductive InverseOp (TokenId TxIn Root K V : Type) where
  | restoreToken (tid : TokenId) (root : Root)
  | removeToken (tid : TokenId)
  | restoreRequest (txIn : TxIn)
  | removeRequest (txIn : TxIn)
  -- Trie-level inverse ops
  | restoreTrieEntry (tid : TokenId) (k : K) (v : V)
  | removeTrieEntry (tid : TokenId) (k : K)

end Phase4
