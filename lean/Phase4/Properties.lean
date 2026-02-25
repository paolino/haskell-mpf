/-
  Phase 4 Testable Properties

  Each def here is a predicate that becomes a QC monadic
  property. The Lean type-checks the statement; QC tests
  it against real RocksDB.
-/

import Phase4.Domain

-- Precondition parameters are not used in the Prop body
-- but document requirements for the QC test harness.
set_option linter.unusedVariables false

namespace Phase4

variable {SlotNo TokenId TxIn Root K V : Type}
  [DecidableEq TokenId] [DecidableEq TxIn]
  [DecidableEq Root] [DecidableEq K]

-- =========================================================
-- Cage state operations (reference implementation)
-- =========================================================

-- Apply events to cage state.
-- Note: applyEvent only modifies tokens/requests.
-- Trie visibility and entries are handled separately.
def applyEvent (cs : CageState TokenId TxIn Root K V)
    : CageEvent TokenId TxIn Root
    → CageState TokenId TxIn Root K V
  | .boot tid root =>
    { cs with tokens := (tid, root) :: cs.tokens }
  | .request txIn =>
    { cs with requests := txIn :: cs.requests }
  | .update tid newRoot consumed =>
    { cs with
      tokens := cs.tokens.map fun (t, r) =>
        if t == tid then (t, newRoot) else (t, r)
      requests := cs.requests.filter
        fun tx => consumed.all fun c => !(tx == c) }
  | .retract txIn =>
    { cs with requests :=
        cs.requests.filter fun tx => !(tx == txIn) }
  | .burn tid =>
    { cs with tokens :=
        cs.tokens.filter fun (t, _) => !(t == tid) }

def applyEvents (cs : CageState TokenId TxIn Root K V)
    (events : List (CageEvent TokenId TxIn Root))
    : CageState TokenId TxIn Root K V :=
  events.foldl applyEvent cs

-- Compute inverse ops for an event given current state
def inverseOf (cs : CageState TokenId TxIn Root K V)
    : CageEvent TokenId TxIn Root
    → List (InverseOp TokenId TxIn Root K V)
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

-- Apply an inverse op to cage state
def applyInv (cs : CageState TokenId TxIn Root K V)
    : InverseOp TokenId TxIn Root K V
    → CageState TokenId TxIn Root K V
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
  | .restoreTrieEntry tid k v =>
    { cs with trieEntries := fun t k' =>
        if t == tid && k' == k then some v
        else cs.trieEntries t k' }
  | .removeTrieEntry tid k =>
    { cs with trieEntries := fun t k' =>
        if t == tid && k' == k then none
        else cs.trieEntries t k' }

def applyInvs (cs : CageState TokenId TxIn Root K V)
    (invs : List (InverseOp TokenId TxIn Root K V))
    : CageState TokenId TxIn Root K V :=
  invs.foldl applyInv cs

-- =========================================================
-- Trie visibility operations (reference)
-- =========================================================

/-- Hide a token's trie (burn forward operation). -/
def hideTrie (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId)
    : CageState TokenId TxIn Root K V :=
  { cs with trieVisibility := fun t =>
      if t == tid then .hidden else cs.trieVisibility t }

/-- Unhide a token's trie (burn rollback operation). -/
def unhideTrie (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId)
    : CageState TokenId TxIn Root K V :=
  { cs with trieVisibility := fun t =>
      if t == tid then .visible
      else cs.trieVisibility t }

-- =========================================================
-- PROPERTIES 1-5: Original cage state properties
-- =========================================================

/-- After processing events, the DB state matches the
    reference model applied to the pre-block state. -/
def prop_atomicBlockProcessing
    (pre : CageState TokenId TxIn Root K V)
    (events : List (CageEvent TokenId TxIn Root))
    (dbPost : CageState TokenId TxIn Root K V)
    : Prop :=
  dbPost = applyEvents pre events

/-- Processing blocks then rolling back via inverse ops
    restores the original state. -/
def prop_rollbackCorrectness
    (pre : CageState TokenId TxIn Root K V)
    (event : CageEvent TokenId TxIn Root)
    (post : CageState TokenId TxIn Root K V)
    (rolled : CageState TokenId TxIn Root K V)
    : Prop :=
  let invs := inverseOf pre event
  post = applyEvent pre event
  → rolled = applyInvs post invs
  → rolled = pre

/-- After an update event, the token's stored root equals
    the trie's actual root. -/
def prop_trieStateConsistency
    (tokenRoot : Root)
    (trieRoot : Root)
    : Prop :=
  tokenRoot = trieRoot

/-- Applying an event then its inverse is identity on
    cage state. -/
def prop_inverseRoundTrip
    (pre : CageState TokenId TxIn Root K V)
    (event : CageEvent TokenId TxIn Root)
    : Prop :=
  let post := applyEvent pre event
  let invs := inverseOf pre event
  applyInvs post invs = pre

/-- Collecting inverses for a sequence of events and
    replaying them in reverse restores the original. -/
def prop_multiBlockRollback
    (pre : CageState TokenId TxIn Root K V)
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

-- =========================================================
-- PROPERTIES 6-10: Original structural properties
-- =========================================================

/-- Booting then burning a fresh token restores the
    original state (when the token wasn't present). -/
def prop_bootBurnRoundTrip
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId) (root : Root)
    (hFresh : ∀ r, (tid, r) ∉ cs.tokens)
    : Prop :=
  applyEvent
    (applyEvent cs (.boot tid root)) (.burn tid)
    = cs

/-- Requesting then retracting a fresh TxIn restores
    the original state. -/
def prop_requestRetractRoundTrip
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    (hFresh : txIn ∉ cs.requests)
    : Prop :=
  applyEvent
    (applyEvent cs (.request txIn)) (.retract txIn)
    = cs

/-- An update for one token does not affect other
    tokens' state entries. -/
def prop_updatePreservesOtherTokens
    (cs : CageState TokenId TxIn Root K V)
    (tidA tidB : TokenId) (newRoot rootB : Root)
    (consumed : List TxIn)
    (hNeq : tidA ≠ tidB)
    (hMem : (tidB, rootB) ∈ cs.tokens)
    : Prop :=
  (tidB, rootB)
    ∈ (applyEvent cs
        (.update tidA newRoot consumed)).tokens

/-- Burning one token preserves all other tokens. -/
def prop_burnPreservesOtherTokens
    (cs : CageState TokenId TxIn Root K V)
    (tidA tidB : TokenId) (rootB : Root)
    (hNeq : tidA ≠ tidB)
    (hMem : (tidB, rootB) ∈ cs.tokens)
    : Prop :=
  (tidB, rootB)
    ∈ (applyEvent cs (.burn tidA)).tokens

/-- Boot's inverse is exactly [removeToken tid]. -/
def prop_inverseOfBootCorrect
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId) (root : Root)
    : Prop :=
  inverseOf cs (.boot tid root)
    = [.removeToken tid]

/-- Request's inverse is exactly
    [removeRequest txIn]. -/
def prop_inverseOfRequestCorrect
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : Prop :=
  inverseOf cs (.request txIn)
    = [.removeRequest txIn]

/-- Retract's inverse is exactly
    [restoreRequest txIn]. -/
def prop_inverseOfRetractCorrect
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : Prop :=
  inverseOf cs (.retract txIn)
    = [.restoreRequest txIn]

-- =========================================================
-- PROPERTIES 11-15: Trie rollback properties (NEW)
-- =========================================================

-- -------------------------------------------------------
-- PROPERTY 11: Hide/unhide round-trip
--
-- QC: create → insert → hide → unhide → withTrie
-- succeeds, lookup works.
-- -------------------------------------------------------

/-- Hiding then unhiding preserves visibility. -/
def prop_hideUnhideRoundTrip
    (tid : TokenId)
    (cs : CageState TokenId TxIn Root K V)
    (hVis : cs.trieVisibility tid = .visible)
    : Prop :=
  (unhideTrie (hideTrie cs tid) tid).trieVisibility tid
    = .visible

-- -------------------------------------------------------
-- PROPERTY 12: Hide preserves trie data
--
-- QC: create → insert → hide → unhide → lookup
-- returns same value.
-- -------------------------------------------------------

/-- Hiding doesn't touch trie entries. -/
def prop_hidePreservesTrieData
    (tid : TokenId) (k : K)
    (cs : CageState TokenId TxIn Root K V)
    : Prop :=
  (hideTrie cs tid).trieEntries tid k
    = cs.trieEntries tid k

-- -------------------------------------------------------
-- PROPERTY 13: Trie inverse round-trip
--
-- QC: insert key → compute inverse → apply inverse →
-- lookup = Nothing (if was absent) or old value.
-- -------------------------------------------------------

/-- Inserting then applying removeTrieEntry restores
    absence (when key was previously absent). -/
def prop_trieInsertRemoveRoundTrip
    (tid : TokenId) (k : K) (v : V)
    (cs : CageState TokenId TxIn Root K V)
    (hAbsent : cs.trieEntries tid k = none)
    : Prop :=
  let inserted : CageState TokenId TxIn Root K V :=
    { cs with trieEntries := fun t k' =>
        if t == tid && k' == k then some v
        else cs.trieEntries t k' }
  let inv := InverseOp.removeTrieEntry (K := K) (V := V)
        tid k
  (applyInv inserted inv).trieEntries tid k
    = cs.trieEntries tid k

/-- Deleting then applying restoreTrieEntry restores
    the original value. -/
def prop_trieDeleteRestoreRoundTrip
    (tid : TokenId) (k : K) (v : V)
    (cs : CageState TokenId TxIn Root K V)
    (hPresent : cs.trieEntries tid k = some v)
    : Prop :=
  let deleted : CageState TokenId TxIn Root K V :=
    { cs with trieEntries := fun t k' =>
        if t == tid && k' == k then none
        else cs.trieEntries t k' }
  let inv := InverseOp.restoreTrieEntry tid k v
  (applyInv deleted inv).trieEntries tid k
    = cs.trieEntries tid k

-- -------------------------------------------------------
-- PROPERTY 14: Burn rollback restores trie
--
-- QC: create → insert → burn(hide) →
-- rollback(unhide) → lookup returns value.
-- -------------------------------------------------------

/-- Burning (hiding) then restoring (unhiding) preserves
    trie data for the burned token. -/
def prop_burnRollbackRestoresTrie
    (tid : TokenId) (k : K)
    (cs : CageState TokenId TxIn Root K V)
    : Prop :=
  let burned := hideTrie cs tid
  let restored := unhideTrie burned tid
  restored.trieEntries tid k = cs.trieEntries tid k

-- -------------------------------------------------------
-- PROPERTY 15: Hide preserves other tokens
--
-- QC: create A & B → insert into both → hide A →
-- B lookup unchanged.
-- -------------------------------------------------------

/-- Hiding token A doesn't affect token B's entries
    or visibility. -/
def prop_hidePreservesOtherTokens
    (tidA tidB : TokenId) (k : K)
    (hNeq : tidA ≠ tidB)
    (cs : CageState TokenId TxIn Root K V)
    : Prop :=
  let hidden := hideTrie cs tidA
  hidden.trieEntries tidB k = cs.trieEntries tidB k
  ∧ hidden.trieVisibility tidB = cs.trieVisibility tidB

end Phase4
