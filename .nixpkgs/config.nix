{
  packageOverrides = super: let self = super.pkgs; in
  {
     myHaskellEnv =
     self.haskellPackages.ghcWithPackages
        (haskellPackages: with haskellPackages; [
           # potentially add other haskell packages here
           # mtl QuickCheck random text alex cpphs happy ghc-paths
           # hfsevents zlib  yesod-test_1_4_4
           stack cabal-install yesod-bin
        ]);
  };
  allowUnfree = true;
}
