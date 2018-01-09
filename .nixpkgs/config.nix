
{
  packageOverrides = super: let self = super.pkgs; in
  {
    myHaskellEnv = self.haskellPackages.ghcWithPackages
                     (haskellPackages: with haskellPackages; [
                       # libraries
                       # arrows async cgi criterion
                       # tools
                       # cabal-install haskintex
                       stack
                     ]);
  };
  allowUnfree = true;
}
