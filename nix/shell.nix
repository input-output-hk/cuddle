{ ghc, pkgs, lib, project, pre-commit-hooks }:

let

  toolVersions = {
    cabal = "latest";
    cabal-fmt = "latest";
    fourmolu = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
    stylish-haskell = "latest";
  };

  toolVariants =
    lib.attrsets.mapAttrs
      (_: ghcVariant: lib.attrsets.mapAttrs ghcVariant.tool toolVersions)
      project.projectVariants;

  toolOverrides = {
    ghc910.cabal-fmt = toolVariants.ghc96.cabal-fmt; # cabal-fmt not buildable with ghc9102
    ghc912.cabal-fmt = toolVariants.ghc96.cabal-fmt; # cabal-fmt not buildable with ghc9122
  };

  tools = toolVariants.${ghc} // (toolOverrides.${ghc} or { });

  preCommitCheck = pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSources ../.;

    hooks = {
      cabal-fmt = {
        enable = false;
        package = tools.cabal-fmt;
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
      };
      hlint = {
        enable = true;
        package = tools.hlint;
      };
      stylish-haskell = {
        enable = false;
        package = tools.stylish-haskell;
      };
      nixpkgs-fmt = {
        enable = true;
        package = pkgs.nixpkgs-fmt;
      };
      shellcheck = {
        enable = true;
        package = pkgs.shellcheck;
      };
    };
  };

  commonPkgs = [
    tools.cabal
    tools.cabal-fmt
    tools.fourmolu
    tools.haskell-language-server
    tools.hlint
    tools.stylish-haskell

    pkgs.nixpkgs-fmt
    pkgs.shellcheck

    pkgs.github-cli
    pkgs.act
    pkgs.bzip2
    pkgs.gawk
    pkgs.zlib
    pkgs.cacert
    pkgs.curl
    pkgs.bash
    pkgs.git
    pkgs.which
  ];

  linuxPkgs = lib.optionals pkgs.hostPlatform.isLinux [
  ];

  darwinPkgs = lib.optionals pkgs.hostPlatform.isDarwin [
  ];

  exportLocaleArchive =
    lib.optionalString
      (pkgs.stdenv.hostPlatform.libc == "glibc" && pkgs.glibcLocales != null)
      "export LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive";

in

project.projectVariants.${ghc}.shellFor {
  name = "${project.args.name}-shell-${project.args.compiler-nix-name}";

  nativeBuildInputs = lib.concatLists [
    commonPkgs
    linuxPkgs
    darwinPkgs
  ];

  withHoogle = true;

  exactDeps = true;

  shellHook = ''
    ${preCommitCheck.shellHook}

    prompt() {
      local STATUS=$? SUFFIX TITLE
      if [ $STATUS -ne 0 ]; then
        SUFFIX=' \[\033[1;31m\]('"$STATUS"')\[\033[00m\] '
      fi
      case $TERM in
        xterm*|rxvt*) TITLE='\[\e]2;\W\a\]';;
      esac
      PS1="\[\033[1;32m\][nix-shell:\W]$SUFFIX\$\[\033[0m\]$TITLE "
    }
    PROMPT_COMMAND=prompt

    export LANG=en_US.UTF-8

    ${exportLocaleArchive}
  '';
}
