{ ghc, pkgs, lib, project, pre-commit-hooks }:

let

  toolVersions = {
    cabal = "latest";
    cabal-gild = "latest";
    fourmolu = "latest";
    haskell-language-server = "latest";
    hlint = "latest";
  };

  toolVariants =
    lib.attrsets.mapAttrs
      (_: ghcVariant: lib.attrsets.mapAttrs ghcVariant.tool toolVersions)
      project.projectVariants;

  toolOverrides = {
    # Compiler+tool combinations, eg
    # ghc910.foobar = toolVariants.ghc96.foobar; # foobar not buildable with ghc910
    # ghc912.xyzzy = project.projectVariants.ghc912.tool "xyzzy" "1.2.3";
  };

  tools = toolVariants.${ghc} // (toolOverrides.${ghc} or { });

  preCommitCheck = pre-commit-hooks.lib.${pkgs.system}.run {

    src = lib.cleanSources ../.;

    hooks = {
      cabal-gild = {
        enable = true;
        package = tools.cabal-gild;
      };
      fourmolu = {
        enable = true;
        package = tools.fourmolu;
      };
      hlint = {
        enable = true;
        package = tools.hlint;
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
    tools.cabal-gild
    tools.fourmolu
    tools.haskell-language-server
    tools.hlint

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
