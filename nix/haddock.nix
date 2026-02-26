# Combined haddock documentation for gh-pages deployment.
#
# Copies per-package haddock outputs into a single directory,
# rewrites nix-store file:// links to relative URLs, and
# generates a unified index with haddock --gen-contents.
#
# Usage:
#   nix build .#haddock
#   ls result/          # cardano-mpfs-offchain/ merkle-patricia-forestry/ index.html
{ pkgs, project }:

let
  haddock =
    pkgs.buildPackages.haskell-nix.compiler.ghc984.passthru.haddock or pkgs.haskellPackages.haddock;

  mpfDoc = project.hsPkgs.merkle-patricia-forestry.components.library.doc;

  offchainDoc = project.hsPkgs.cardano-mpfs-offchain.components.library.doc;

  mpfName = "merkle-patricia-forestry";
  offchainName = "cardano-mpfs-offchain";

  mpfHtml = "${mpfDoc}/share/doc/${mpfName}/html";
  offchainHtml = "${offchainDoc}/share/doc/${offchainName}/html";

in pkgs.runCommand "combined-haddock" {
  nativeBuildInputs = [ pkgs.haskell-nix.compiler.ghc984 ];
} ''
  mkdir -p $out

  # Copy both package docs
  cp -R ${mpfHtml} $out/${mpfName}
  cp -R ${offchainHtml} $out/${offchainName}
  chmod -R +w $out

  # Rewrite file:// store-path references to relative URLs
  # so cross-package links work in a browser.
  for f in $(find $out -name "*.html" -o -name "*.json"); do
    dir=$(dirname "$f")
    rel_mpf=$(realpath --relative-to="$dir" "$out/${mpfName}")
    rel_offchain=$(realpath --relative-to="$dir" "$out/${offchainName}")

    sed -i \
      -e "s|file://${mpfHtml}|$rel_mpf|g" \
      -e "s|file://${offchainHtml}|$rel_offchain|g" \
      "$f"
    # Rewrite GHC boot-library file:// refs to Hackage URLs
    ${pkgs.perl}/bin/perl -pi -e \
      's{file:///nix/store/[^/]+-ghc-[\d.]+-doc/share/doc/ghc-[\d.]+/html/libraries/([^/]+)-inplace/}{https://hackage.haskell.org/package/$1/docs/}g' \
      "$f"
  done

  # Generate combined index and contents page
  haddock \
    --gen-contents \
    --gen-index \
    --odir=$out \
    --read-interface=${mpfName},${mpfHtml}/${mpfName}.haddock \
    --read-interface=${offchainName},${offchainHtml}/${offchainName}.haddock \
    || true

  # Copy shared static assets from the offchain docs
  for asset in linuwial.css quick-jump.css quick-jump.min.js \
               haddock-bundle.min.js; do
    if [ -f ${offchainHtml}/$asset ] && [ ! -f $out/$asset ]; then
      cp ${offchainHtml}/$asset $out/
    fi
  done
''
