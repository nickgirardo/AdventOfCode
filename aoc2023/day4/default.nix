let
  pkgs = import <nixpkgs> { }; # pin the channel to ensure reproducibility!
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      parsec =
        self.callHackageDirect
          {
            pkg = "parsec";
            ver = "3.1.17.0";
            sha256 = "sha256-m4xa/LlY8gIIwRQBdKTc1Bo+CJjVhavpr0ycAgglcAk=";
          } { };
    };
  };
in
haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with haskellPackages;
      [ cabal-install
        ghcid
        parsec_3_1_17_0
      ]
    );
}
