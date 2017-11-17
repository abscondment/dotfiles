{
  packageOverrides = pkgs: with pkgs; {
     myPythonEnv = python36.withPackages
       (ps: with ps; [ requests six ]);

     myHaskellEnv = self.haskellPackages.ghcWithPackages
       (haskellPackages: with haskellPackages; [
         # potentially add other haskell packages here
         # mtl QuickCheck random text alex cpphs happy ghc-paths
         # hfsevents zlib  yesod-test_1_4_4
         yesod-bin
       ]);
  };
  allowUnfree = true;
}
