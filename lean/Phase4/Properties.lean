/-
  Phase 4 Testable Properties

  Each def here is a predicate that becomes a QC monadic
  property. The Lean type-checks the statement; QC tests
  it against real RocksDB.
-/

import Phase4.Domain

namespace Phase4

variable {SlotNo TokenId TxIn Root : Type}
  [DecidableEq TokenId] [DecidableEq TxIn]
  [DecidableEq Root]

-- Apply events to cage state (reference implementation)
def applyEvent (cs : CageState TokenId TxIn Root)
    : CageEvent TokenId TxIn Root
    → CageState TokenId TxIn Root
  | .boot tid root =>
    { cs with tokens := (tid, root) :: cs.tokens }
  | .request txIn =>
    { cs with requests := txIn :: cs.requests }
  | .update tid newRoot consumed =>
    { tokens := cs.tokens.map fun (t, r) =>
        if t == tid then (t, newRoot) else (t, r)
      requests := cs.requests.filter
        fun tx => consumed.all fun c => !(tx == c) }
  | .retract txIn =>
    { cs with requests :=
        cs.requests.filter fun tx => !(tx == txIn) }
  | .burn tid =>
    { cs with tokens :=
        cs.tokens.filter fun (t, _) => !(t == tid) }

def applyEvents (cs : CageState TokenId TxIn Root)
    (events : List (CageEvent TokenId TxIn Root))
    : CageState TokenId TxIn Root :=
  events.foldl applyEvent cs

-- Compute inverse ops for an event given current state
def inverseOf (cs : CageState TokenId TxIn Root)
    : CageEvent TokenId TxIn Root
    → List (InverseOp TokenId TxIn Root)
  | .boot tid _ => [.removeToken tid]
  | .request txIn => [.removeRequest txIn]
  | .update tid _ consumed =>
    let oldRoot := cs.tokens.find?
      fun (t, _) => t == tid
    let restoreRoot := match oldRoot with
      | some (_, r) => [.restoreToken tid r]
      | none => []
    let restoreReqs := consumed.map .restoreRequest
    restoreRoot ++ restoreReqs
  | .retract txIn => [.restoreRequest txIn]
  | .burn tid =>
    let old := cs.tokens.find? fun (t, _) => t == tid
    match old with
    | some (_, r) => [.restoreToken tid r]
    | none => []

-- Apply an inverse op
def applyInv (cs : CageState TokenId TxIn Root)
    : InverseOp TokenId TxIn Root
    → CageState TokenId TxIn Root
  | .restoreToken tid root =>
    { cs with tokens := (tid, root) :: cs.tokens }
  | .removeToken tid =>
    { cs with tokens :=
        cs.tokens.filter fun (t, _) => !(t == tid) }
  | .restoreRequest txIn =>
    { cs with requests := txIn :: cs.requests }
  | .removeRequest txIn =>
    { cs with requests :=
        cs.requests.filter fun tx => !(tx == txIn) }

def applyInvs (cs : CageState TokenId TxIn Root)
    (invs : List (InverseOp TokenId TxIn Root))
    : CageState TokenId TxIn Root :=
  invs.foldl applyInv cs

-- -------------------------------------------------------
-- PROPERTY 1: Atomic block processing
--
-- QC: process a block in a single RocksDB transaction,
-- read back all three domains, verify they match the
-- reference model.
-- -------------------------------------------------------

/-- After processing events, the DB state matches the
    reference model applied to the pre-block state. -/
def prop_atomicBlockProcessing
    (pre : CageState TokenId TxIn Root)
    (events : List (CageEvent TokenId TxIn Root))
    (dbPost : CageState TokenId TxIn Root)
    : Prop :=
  dbPost = applyEvents pre events

-- -------------------------------------------------------
-- PROPERTY 2: Rollback correctness
--
-- QC: process N blocks, rollback to block K, verify
-- state matches state-at-K.
-- -------------------------------------------------------

/-- Processing blocks then rolling back via inverse ops
    restores the original state. -/
def prop_rollbackCorrectness
    (pre : CageState TokenId TxIn Root)
    (event : CageEvent TokenId TxIn Root)
    (post : CageState TokenId TxIn Root)
    (rolled : CageState TokenId TxIn Root)
    : Prop :=
  let invs := inverseOf pre event
  post = applyEvent pre event
  → rolled = applyInvs post invs
  → rolled = pre

-- -------------------------------------------------------
-- PROPERTY 3: Trie-state consistency
--
-- QC: after processing a block with an Update event,
-- the trie root in the DB matches the root stored in
-- the token's state.
-- -------------------------------------------------------

/-- After an update event, the token's stored root equals
    the trie's actual root. -/
def prop_trieStateConsistency
    (tokenRoot : Root)
    (trieRoot : Root)
    : Prop :=
  tokenRoot = trieRoot

-- -------------------------------------------------------
-- PROPERTY 4: Inverse op round-trip
--
-- QC: for any event applied to any state, applying the
-- inverse restores the original.
-- -------------------------------------------------------

/-- Applying an event then its inverse is identity on
    cage state. -/
def prop_inverseRoundTrip
    (pre : CageState TokenId TxIn Root)
    (event : CageEvent TokenId TxIn Root)
    : Prop :=
  let post := applyEvent pre event
  let invs := inverseOf pre event
  applyInvs post invs = pre

-- -------------------------------------------------------
-- PROPERTY 5: Multi-block rollback
--
-- QC: process a sequence of blocks collecting inverses,
-- roll them all back in reverse, get original state.
-- -------------------------------------------------------

/-- Collecting inverses for a sequence of events and
    replaying them in reverse restores the original. -/
def prop_multiBlockRollback
    (pre : CageState TokenId TxIn Root)
    (events : List (CageEvent TokenId TxIn Root))
    : Prop :=
  let (final, allInvs) := events.foldl
    (fun (cs, invAcc) ev =>
      let invs := inverseOf cs ev
      (applyEvent cs ev, invs :: invAcc))
    (pre, [])
  let restored := allInvs.foldl
    (fun cs invs => applyInvs cs invs)
    final
  restored = pre

end Phase4
