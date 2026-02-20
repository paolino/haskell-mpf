/-
  Phase 4 Proved Theorems

  Constructive proofs of cage state invariants.
  These complement the testable properties in
  Properties.lean which are validated by QuickCheck.
-/

import Phase4.Properties

set_option linter.unusedSectionVars false

namespace Phase4

variable {SlotNo TokenId TxIn Root : Type}
  [DecidableEq TokenId] [DecidableEq TxIn]
  [DecidableEq Root]

-- -------------------------------------------------------
-- Membership after events
-- -------------------------------------------------------

/-- After booting a token, it is in the token set. -/
theorem boot_mem_tokens
    (cs : CageState TokenId TxIn Root)
    (tid : TokenId) (root : Root)
    : (tid, root) ∈
        (applyEvent cs (.boot tid root)).tokens :=
  .head _

/-- After submitting a request, it is in the request
    set. -/
theorem request_mem_requests
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn)
    : txIn ∈
        (applyEvent cs (.request txIn)).requests :=
  .head _

/-- Pre-existing tokens survive a boot event. -/
theorem boot_preserves_mem
    (cs : CageState TokenId TxIn Root)
    (tid : TokenId) (root : Root)
    (x : TokenId × Root) (h : x ∈ cs.tokens)
    : x ∈
        (applyEvent cs (.boot tid root)).tokens :=
  .tail _ h

/-- Pre-existing requests survive a request event. -/
theorem request_preserves_mem
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn) (x : TxIn) (h : x ∈ cs.requests)
    : x ∈
        (applyEvent cs (.request txIn)).requests :=
  .tail _ h

-- -------------------------------------------------------
-- Cross-domain preservation
-- -------------------------------------------------------

/-- Boot does not affect the request set. -/
theorem boot_preserves_requests
    (cs : CageState TokenId TxIn Root)
    (tid : TokenId) (root : Root)
    : (applyEvent cs (.boot tid root)).requests
      = cs.requests :=
  rfl

/-- Request does not affect the token set. -/
theorem request_preserves_tokens
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn)
    : (applyEvent cs (.request txIn)).tokens
      = cs.tokens :=
  rfl

/-- Burn does not affect the request set. -/
theorem burn_preserves_requests
    (cs : CageState TokenId TxIn Root)
    (tid : TokenId)
    : (applyEvent cs (.burn tid)).requests
      = cs.requests :=
  rfl

/-- Retract does not affect the token set. -/
theorem retract_preserves_tokens
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn)
    : (applyEvent cs (.retract txIn)).tokens
      = cs.tokens :=
  rfl

-- -------------------------------------------------------
-- Inverse operation counts (structural)
-- -------------------------------------------------------

/-- Boot produces exactly one inverse op. -/
theorem inverse_boot_length
    (cs : CageState TokenId TxIn Root)
    (tid : TokenId) (root : Root)
    : (inverseOf cs (.boot tid root)).length = 1 :=
  rfl

/-- Request produces exactly one inverse op. -/
theorem inverse_request_length
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn)
    : (inverseOf cs (.request txIn)).length = 1 :=
  rfl

/-- Retract produces exactly one inverse op. -/
theorem inverse_retract_length
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn)
    : (inverseOf cs (.retract txIn)).length = 1 :=
  rfl

-- -------------------------------------------------------
-- Inverse structure (what kind of op is produced)
-- -------------------------------------------------------

/-- Boot's inverse is a removeToken. -/
theorem inverse_boot_eq
    (cs : CageState TokenId TxIn Root)
    (tid : TokenId) (root : Root)
    : inverseOf cs (.boot tid root)
      = [.removeToken tid] :=
  rfl

/-- Request's inverse is a removeRequest. -/
theorem inverse_request_eq
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn)
    : inverseOf cs (.request txIn)
      = [.removeRequest txIn] :=
  rfl

/-- Retract's inverse is a restoreRequest. -/
theorem inverse_retract_eq
    (cs : CageState TokenId TxIn Root)
    (txIn : TxIn)
    : inverseOf cs (.retract txIn)
      = [.restoreRequest txIn] :=
  rfl

-- -------------------------------------------------------
-- applyEvents composition
-- -------------------------------------------------------

/-- Processing an empty event list is the identity. -/
theorem applyEvents_nil
    (cs : CageState TokenId TxIn Root)
    : applyEvents cs [] = cs :=
  rfl

/-- Processing a singleton list is applyEvent. -/
theorem applyEvents_singleton
    (cs : CageState TokenId TxIn Root)
    (e : CageEvent TokenId TxIn Root)
    : applyEvents cs [e] = applyEvent cs e :=
  rfl

end Phase4
