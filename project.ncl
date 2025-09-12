let inputs = import "./nickel.lock.ncl" in
let organist = inputs.organist in

let 
  import_hs = fun ghcver pkgname =>
    organist.import_nix "nixpkgs#haskell.packages.%{ghcver}.%{pkgname}"
in
let shellFor = fun ghcver =>
  let hspkg = import_hs ghcver in {
    packages = {
      haskell-language-server = hspkg "haskell-language-server",
      fourmolu = organist.import_nix "nixpkgs#haskell.packages.ghc912.fourmolu",
      ghc = organist.import_nix "nixpkgs#haskell.compiler.%{ghcver}",
      cabal-install = hspkg "cabal-install",
      cabal-fmt = hspkg "cabal-fmt",
      cddl = organist.import_nix "nixpkgs#cddl",
    },
  } in

{
  config = {
    shells = organist.shells.Bash,

    shells.build = {
      packages = {},
    },

    shells.dev = shellFor "ghc910",
  }
}
  | organist.OrganistExpression
