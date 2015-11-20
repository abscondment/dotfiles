{
  packageOverrides = super: let self = super.pkgs; in
  {
     myHaskellEnv =
     self.haskellPackages.ghcWithPackages
        (haskellPackages: with haskellPackages; [
           # potentially add other haskell packages here
           # mtl QuickCheck random text alex cpphs happy ghc-paths
           # hfsevents zlib
           cabal-install stack yesod-bin yesod-test_1_4_4
        ]);
  };
  allowUnfree = true;
}