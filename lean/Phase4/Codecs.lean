/-
  Phase 4 Codec Laws

  Abstract specification of prism-based codec laws.
  Includes a proved theorem that round-trip implies
  injectivity. Individual codec properties are tested
  by QuickCheck against the real CBOR implementations
  in Codecs.hs.
-/

namespace Phase4.Codecs

variable {A B : Type}

-- -------------------------------------------------------
-- Prism law specification
-- -------------------------------------------------------

/-- A lawful prism satisfies the round-trip property
    and is therefore injective. -/
structure PrismLaw
    (encode : A → B) (decode : B → Option A) : Prop
  where
  roundTrip : ∀ a, decode (encode a) = some a

-- -------------------------------------------------------
-- Proved theorem: round-trip implies injectivity
-- -------------------------------------------------------

/-- If a codec satisfies the round-trip law, then
    encoding is injective: distinct values produce
    distinct encodings. -/
theorem roundTrip_implies_injective
    (encode : A → B) (decode : B → Option A)
    (hrt : ∀ a, decode (encode a) = some a)
    : ∀ a₁ a₂, encode a₁ = encode a₂ → a₁ = a₂ := by
  intro a₁ a₂ h
  have h₁ := hrt a₁
  have h₂ := hrt a₂
  rw [h] at h₁
  -- h₁ : decode (encode a₂) = some a₁
  -- h₂ : decode (encode a₂) = some a₂
  rw [h₂] at h₁
  -- h₁ : some a₂ = some a₁
  simp at h₁
  exact h₁.symm

/-- Corollary: a lawful prism is injective. -/
theorem prismLaw_injective
    (encode : A → B) (decode : B → Option A)
    (law : PrismLaw encode decode)
    : ∀ a₁ a₂, encode a₁ = encode a₂ → a₁ = a₂ :=
  roundTrip_implies_injective encode decode law.roundTrip

-- -------------------------------------------------------
-- Testable property statements (QC mirrors)
-- -------------------------------------------------------

/-- Codec round-trip: decoding an encoded value recovers
    the original. Mirrors the QC property
    @preview p (review p x) === Just x@. -/
def prop_codecRoundTrip
    (encode : A → B) (decode : B → Option A)
    (a : A) : Prop :=
  decode (encode a) = some a

/-- Codec injectivity: encoding is injective. Mirrors
    the QC property
    @x /= y ==> review p x /= review p y@. -/
def prop_codecInjective
    (encode : A → B) (a₁ a₂ : A) : Prop :=
  encode a₁ = encode a₂ → a₁ = a₂

/-- Codec non-triviality: encoding produces non-empty
    output. Mirrors the QC property
    @not (BS.null (review p x))@. -/
def prop_codecNonEmpty
    (encode : A → List UInt8) (a : A) : Prop :=
  (encode a).length > 0

end Phase4.Codecs
