let 
  pkgs = import (builtins.fetchTarball {
    # Descriptive name to make the store path easier to identify
    name = "nixos-20.09";
    # Commit hash for nixos-20.09 as of 23 March 2021
    url = "https://github.com/NixOS/nixpkgs/archive/f8929dce13e729357f31d5b2950cbb097744bed7.tar.gz";
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "06ikqdb5038vkkyx4hi5lw4gksjjndjg7mz0spawnb1gpzhqkavs";}) {};
in

pkgs.stdenv.mkDerivation rec {
  name = "kattis-dev-env";
  src = ./.;
  buildInputs = [
    (pkgs.haskell.packages.ghc884.ghcWithPackages (p: with p; [
      vector QuickCheck parallel containers unordered-containers MemoTrie ghc-core split
    ]))
    pkgs.cabal-install
    pkgs.haskellPackages.haskell-language-server


  ];
  libPath = pkgs.lib.makeLibraryPath buildInputs;
  shellHook = ''
    export LD_LIBRARY_PATH=${libPath}:$LD_LIBRARY_PATH
    export LANG=en_US.UTF-8
  '';
  LOCALE_ARCHIVE =
    if pkgs.stdenv.isLinux
    then "${pkgs.glibcLocales}/lib/locale/locale-archive"
    else "";
}
