/-
  Speculation Safety Proofs

  Models the speculative trie execution pattern used by
  `withSpeculativeTrie`. The model captures the essential
  structure: operations run on a workspace copy of the
  database state, produce a result (root hash, proofs),
  then discard the mutated workspace.

  Proved invariants:
  1. Isolation — original state unchanged after speculation
  2. Correctness — speculative result = real result
  3. Read-your-writes — inserts visible within session
  4. Non-interference — per-token speculation is isolated
-/

import Phase4.Domain

namespace Phase4.Speculation

-- -------------------------------------------------------
-- Trie state model
-- -------------------------------------------------------

variable {K V R : Type} [DecidableEq K]

/-- Abstract trie state: a lookup function plus a
    deterministic root-hash derivation. -/
structure TrieState (K V R : Type) where
  entries : K → Option V
  rootOf : (K → Option V) → R

/-- A single trie mutation. -/
inductive TrieOp (K V : Type) where
  | insert (k : K) (v : V)
  | delete (k : K)

/-- Apply one operation to a trie state. -/
def applyOp (s : TrieState K V R)
    : TrieOp K V → TrieState K V R
  | .insert k v =>
    { s with entries := fun k' =>
        if k' = k then some v else s.entries k' }
  | .delete k =>
    { s with entries := fun k' =>
        if k' = k then none else s.entries k' }

/-- Apply a sequence of operations left-to-right. -/
def applyOps (s : TrieState K V R)
    (ops : List (TrieOp K V))
    : TrieState K V R :=
  ops.foldl applyOp s

/-- The root hash of a trie state. -/
def rootHash (s : TrieState K V R) : R :=
  s.rootOf s.entries

-- -------------------------------------------------------
-- Execution models
-- -------------------------------------------------------

/-- Speculative execution: apply ops to a copy, return
    the result, but pair it with the **original** state.
    This models `runSpeculation` which creates a snapshot,
    buffers writes in a workspace, then discards them. -/
def speculate (s : TrieState K V R)
    (ops : List (TrieOp K V))
    : R × TrieState K V R :=
  let workspace := applyOps s ops
  (rootHash workspace, s)

/-- Real execution: apply ops and keep the mutated state.
    This models `runTransactionUnguarded` which commits
    writes to the database. -/
def commit (s : TrieState K V R)
    (ops : List (TrieOp K V))
    : R × TrieState K V R :=
  let newState := applyOps s ops
  (rootHash newState, newState)

-- -------------------------------------------------------
-- THEOREM 1: Isolation
--
-- The original state is unchanged after speculation.
-- This holds by construction: `speculate` returns the
-- original `s` in the second component.
-- -------------------------------------------------------

theorem speculation_preserves_state
    (s : TrieState K V R)
    (ops : List (TrieOp K V))
    : (speculate s ops).2 = s :=
  rfl

-- -------------------------------------------------------
-- THEOREM 2: Correctness
--
-- The root hash computed by speculation equals the root
-- hash that real execution would produce. The only
-- difference between `speculate` and `commit` is what
-- happens to the state — the result is identical.
-- -------------------------------------------------------

theorem speculation_result_eq_commit
    (s : TrieState K V R)
    (ops : List (TrieOp K V))
    : (speculate s ops).1 = (commit s ops).1 :=
  rfl

-- -------------------------------------------------------
-- THEOREM 3: Read-your-writes
--
-- Within a speculative session, an inserted key is
-- immediately visible, and a deleted key is immediately
-- absent.
-- -------------------------------------------------------

theorem ryw_insert_visible
    (s : TrieState K V R) (k : K) (v : V)
    : (applyOp s (.insert k v)).entries k
      = some v := by
  simp [applyOp]

theorem ryw_delete_absent
    (s : TrieState K V R) (k : K)
    : (applyOp s (.delete k)).entries k
      = none := by
  simp [applyOp]

/-- Insert then lookup of a **different** key returns the
    original value (no cross-key interference). -/
theorem ryw_insert_preserves_other
    (s : TrieState K V R) (k k' : K) (v : V)
    (hne : k' ≠ k)
    : (applyOp s (.insert k v)).entries k'
      = s.entries k' := by
  simp [applyOp, hne]

/-- Delete of one key preserves other keys. -/
theorem ryw_delete_preserves_other
    (s : TrieState K V R) (k k' : K)
    (hne : k' ≠ k)
    : (applyOp s (.delete k)).entries k'
      = s.entries k' := by
  simp [applyOp, hne]

-- -------------------------------------------------------
-- THEOREM 4: Non-interference (multi-token)
--
-- A token manager maps TokenId → TrieState. Speculation
-- on one token does not affect any other token's state.
-- -------------------------------------------------------

variable {TokenId : Type} [DecidableEq TokenId]

/-- Per-token trie manager state. -/
def TrieManagerState (TokenId K V R : Type) :=
  TokenId → Option (TrieState K V R)

/-- Speculate on a single token, leaving all others
    untouched. Returns the speculative root and the
    **unchanged** manager state. -/
def speculateToken
    (mgr : TrieManagerState TokenId K V R)
    (tid : TokenId)
    (ops : List (TrieOp K V))
    : Option R × TrieManagerState TokenId K V R :=
  match mgr tid with
  | none => (none, mgr)
  | some s =>
    let workspace := applyOps s ops
    (some (rootHash workspace), mgr)

set_option linter.unusedSectionVars false in
/-- Speculation on any token preserves the entire
    manager state. -/
theorem speculateToken_preserves_mgr
    (mgr : TrieManagerState TokenId K V R)
    (tid : TokenId) (ops : List (TrieOp K V))
    : (speculateToken mgr tid ops).2 = mgr := by
  simp [speculateToken]
  split <;> rfl

set_option linter.unusedSectionVars false in
/-- Speculation on token A does not affect looking up
    token B's state (B ≠ A or B = A, doesn't matter). -/
theorem speculateToken_preserves_other
    (mgr : TrieManagerState TokenId K V R)
    (tidA : TokenId) (ops : List (TrieOp K V))
    (tidB : TokenId)
    : (speculateToken mgr tidA ops).2 tidB
      = mgr tidB := by
  simp [speculateToken]
  split <;> rfl

-- -------------------------------------------------------
-- Composition: multiple ops in one session
-- -------------------------------------------------------

/-- Empty ops = identity on state. -/
theorem applyOps_nil
    (s : TrieState K V R)
    : applyOps s [] = s :=
  rfl

/-- Singleton ops = single applyOp. -/
theorem applyOps_singleton
    (s : TrieState K V R)
    (op : TrieOp K V)
    : applyOps s [op] = applyOp s op :=
  rfl

/-- Speculation with empty ops returns original root. -/
theorem speculate_nil_root
    (s : TrieState K V R)
    : (speculate s []).1 = rootHash s :=
  rfl

/-- Insert-delete on the same key always results in
    `none`, regardless of original state. -/
theorem insert_delete_cancel
    (s : TrieState K V R) (k : K) (v : V)
    : (applyOp (applyOp s (.insert k v))
        (.delete k)).entries k = none := by
  simp [applyOp]

/-- Delete-insert on the same key sets the new value. -/
theorem delete_insert_sets
    (s : TrieState K V R) (k : K) (v : V)
    : (applyOp (applyOp s (.delete k))
        (.insert k v)).entries k = some v := by
  simp [applyOp]

end Phase4.Speculation
