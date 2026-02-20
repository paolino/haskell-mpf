import Lake
open Lake DSL

package «phase4-invariants» where
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

@[default_target]
lean_lib Phase4 where
  srcDir := "."
