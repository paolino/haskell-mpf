/-
  Phase 4 Proved Theorems

  Constructive proofs of cage state invariants.
  These complement the testable properties in
  Properties.lean which are validated by QuickCheck.
-/

import Phase4.Properties

set_option linter.unusedSectionVars false

namespace Phase4

variable {SlotNo TokenId TxIn Root K V : Type}
  [DecidableEq TokenId] [DecidableEq TxIn]
  [DecidableEq Root] [DecidableEq K]

-- -------------------------------------------------------
-- Membership after events
-- -------------------------------------------------------

/-- After booting a token, it is in the token set. -/
theorem boot_mem_tokens
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId) (root : Root)
    : (tid, root) ∈
        (applyEvent cs (.boot tid root)).tokens :=
  .head _

/-- After submitting a request, it is in the request
    set. -/
theorem request_mem_requests
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : txIn ∈
        (applyEvent cs (.request txIn)).requests :=
  .head _

/-- Pre-existing tokens survive a boot event. -/
theorem boot_preserves_mem
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId) (root : Root)
    (x : TokenId × Root) (h : x ∈ cs.tokens)
    : x ∈
        (applyEvent cs (.boot tid root)).tokens :=
  .tail _ h

/-- Pre-existing requests survive a request event. -/
theorem request_preserves_mem
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn) (x : TxIn) (h : x ∈ cs.requests)
    : x ∈
        (applyEvent cs (.request txIn)).requests :=
  .tail _ h

-- -------------------------------------------------------
-- Cross-domain preservation
-- -------------------------------------------------------

/-- Boot does not affect the request set. -/
theorem boot_preserves_requests
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId) (root : Root)
    : (applyEvent cs (.boot tid root)).requests
      = cs.requests :=
  rfl

/-- Request does not affect the token set. -/
theorem request_preserves_tokens
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : (applyEvent cs (.request txIn)).tokens
      = cs.tokens :=
  rfl

/-- Burn does not affect the request set. -/
theorem burn_preserves_requests
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId)
    : (applyEvent cs (.burn tid)).requests
      = cs.requests :=
  rfl

/-- Retract does not affect the token set. -/
theorem retract_preserves_tokens
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : (applyEvent cs (.retract txIn)).tokens
      = cs.tokens :=
  rfl

-- -------------------------------------------------------
-- Inverse operation counts (structural)
-- -------------------------------------------------------

/-- Boot produces exactly one inverse op. -/
theorem inverse_boot_length
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId) (root : Root)
    : (inverseOf cs (.boot tid root)).length = 1 :=
  rfl

/-- Request produces exactly one inverse op. -/
theorem inverse_request_length
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : (inverseOf cs (.request txIn)).length = 1 :=
  rfl

/-- Retract produces exactly one inverse op. -/
theorem inverse_retract_length
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : (inverseOf cs (.retract txIn)).length = 1 :=
  rfl

-- -------------------------------------------------------
-- Inverse structure (what kind of op is produced)
-- -------------------------------------------------------

/-- Boot's inverse is a removeToken. -/
theorem inverse_boot_eq
    (cs : CageState TokenId TxIn Root K V)
    (tid : TokenId) (root : Root)
    : inverseOf cs (.boot tid root)
      = [.removeToken tid] :=
  rfl

/-- Request's inverse is a removeRequest. -/
theorem inverse_request_eq
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : inverseOf cs (.request txIn)
      = [.removeRequest txIn] :=
  rfl

/-- Retract's inverse is a restoreRequest. -/
theorem inverse_retract_eq
    (cs : CageState TokenId TxIn Root K V)
    (txIn : TxIn)
    : inverseOf cs (.retract txIn)
      = [.restoreRequest txIn] :=
  rfl

-- -------------------------------------------------------
-- applyEvents composition
-- -------------------------------------------------------

/-- Processing an empty event list is the identity. -/
theorem applyEvents_nil
    (cs : CageState TokenId TxIn Root K V)
    : applyEvents cs [] = cs :=
  rfl

/-- Processing a singleton list is applyEvent. -/
theorem applyEvents_singleton
    (cs : CageState TokenId TxIn Root K V)
    (e : CageEvent TokenId TxIn Root)
    : applyEvents cs [e] = applyEvent cs e :=
  rfl

-- -------------------------------------------------------
-- Trie visibility theorems (NEW)
-- -------------------------------------------------------

set_option linter.unusedVariables false in
/-- Hide/unhide is identity on visibility for the
    target token. -/
theorem hide_unhide_identity
    (tid : TokenId)
    (cs : CageState TokenId TxIn Root K V)
    (hVis : cs.trieVisibility tid = .visible)
    : (unhideTrie (hideTrie cs tid) tid).trieVisibility tid
      = .visible := by
  simp [hideTrie, unhideTrie]

/-- Hiding preserves trie data (by construction —
    only changes visibility). -/
theorem hide_preserves_data
    (tid : TokenId) (k : K)
    (cs : CageState TokenId TxIn Root K V)
    : (hideTrie cs tid).trieEntries tid k
      = cs.trieEntries tid k := by
  simp [hideTrie]

/-- Hiding token A preserves token B's visibility. -/
theorem hide_preserves_other_visibility
    (tidA tidB : TokenId)
    (hNeq : tidA ≠ tidB)
    (cs : CageState TokenId TxIn Root K V)
    : (hideTrie cs tidA).trieVisibility tidB
      = cs.trieVisibility tidB := by
  simp [hideTrie]
  intro h
  exact absurd h.symm hNeq

set_option linter.unusedVariables false in
/-- Hiding token A preserves token B's trie entries. -/
theorem hide_preserves_other_entries
    (tidA tidB : TokenId) (k : K)
    (hNeq : tidA ≠ tidB)
    (cs : CageState TokenId TxIn Root K V)
    : (hideTrie cs tidA).trieEntries tidB k
      = cs.trieEntries tidB k := by
  simp [hideTrie]

-- -------------------------------------------------------
-- Trie inverse op theorems (NEW)
-- -------------------------------------------------------

/-- Inserting then removing a trie entry restores
    the original absence. -/
theorem trie_inverse_insert_remove
    (tid : TokenId) (k : K) (v : V)
    (cs : CageState TokenId TxIn Root K V)
    (hAbsent : cs.trieEntries tid k = none)
    : let inserted : CageState TokenId TxIn Root K V :=
        { cs with trieEntries := fun t k' =>
            if t == tid && k' == k then some v
            else cs.trieEntries t k' }
      let inv := InverseOp.removeTrieEntry
            (K := K) (V := V) tid k
      (applyInv inserted inv).trieEntries tid k
        = cs.trieEntries tid k := by
  simp [applyInv]
  exact hAbsent.symm

/-- Deleting then restoring a trie entry recovers
    the original value. -/
theorem trie_inverse_delete_restore
    (tid : TokenId) (k : K) (v : V)
    (cs : CageState TokenId TxIn Root K V)
    (hPresent : cs.trieEntries tid k = some v)
    : let deleted : CageState TokenId TxIn Root K V :=
        { cs with trieEntries := fun t k' =>
            if t == tid && k' == k then none
            else cs.trieEntries t k' }
      let inv := InverseOp.restoreTrieEntry tid k v
      (applyInv deleted inv).trieEntries tid k
        = cs.trieEntries tid k := by
  simp [applyInv]
  exact hPresent.symm

-- -------------------------------------------------------
-- Burn rollback theorem (NEW)
-- -------------------------------------------------------

/-- Burning (hide) then restoring (unhide) preserves
    trie data. -/
theorem burn_rollback_preserves_trie
    (tid : TokenId) (k : K)
    (cs : CageState TokenId TxIn Root K V)
    : let burned := hideTrie cs tid
      let restored := unhideTrie burned tid
      restored.trieEntries tid k = cs.trieEntries tid k := by
  simp [hideTrie, unhideTrie]

-- -------------------------------------------------------
-- Combined hide/unhide theorem for other tokens (NEW)
-- -------------------------------------------------------

/-- Hiding token A preserves both entries and visibility
    of token B. -/
theorem hide_preserves_other_tokens
    (tidA tidB : TokenId) (k : K)
    (hNeq : tidA ≠ tidB)
    (cs : CageState TokenId TxIn Root K V)
    : (hideTrie cs tidA).trieEntries tidB k
        = cs.trieEntries tidB k
      ∧ (hideTrie cs tidA).trieVisibility tidB
        = cs.trieVisibility tidB := by
  constructor
  · exact hide_preserves_other_entries tidA tidB k hNeq cs
  · exact hide_preserves_other_visibility tidA tidB hNeq cs

end Phase4
