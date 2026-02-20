/-
  Phase 4 Domain Model

  Minimal model for stating testable properties. Each
  theorem here becomes a QuickCheck monadic property
  running against real RocksDB.
-/

namespace Phase4

-- Abstract types (opaque in Lean, concrete in QC)
variable (SlotNo TokenId TxIn Root : Type)

-- Cage state: tokens + requests
structure CageState (TokenId TxIn Root : Type) where
  tokens : List (TokenId × Root)
  requests : List TxIn

-- A cage event from a block
inductive CageEvent (TokenId TxIn Root : Type) where
  | boot (tid : TokenId) (root : Root)
  | request (txIn : TxIn)
  | update (tid : TokenId) (newRoot : Root)
      (consumed : List TxIn)
  | retract (txIn : TxIn)
  | burn (tid : TokenId)

-- An inverse operation (stored for rollback)
inductive InverseOp (TokenId TxIn Root : Type) where
  | restoreToken (tid : TokenId) (root : Root)
  | removeToken (tid : TokenId)
  | restoreRequest (txIn : TxIn)
  | removeRequest (txIn : TxIn)

end Phase4
