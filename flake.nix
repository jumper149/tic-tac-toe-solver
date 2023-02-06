{
  description = "Derive instances for monad transformer stacks";

  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "master";
    };
    monad-control-identity = {
      type = "github";
      owner = "jumper149";
      repo = "monad-control-identity";
      ref = "master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    deriving-trans = {
      type = "github";
      owner = "jumper149";
      repo = "deriving-trans";
      ref = "logict";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, deriving-trans, monad-control-identity }: {

    packages.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      let
        src = nix-gitignore.gitignoreSource [] ./.;
        overlay = self: super: {
          base-orphans = pkgs.haskell.lib.dontCheck super.base-orphans;
          deriving-trans = super.callCabal2nixWithOptions "deriving-trans" deriving-trans.outPath "-f-resourcet" {};
          exceptions = super.exceptions_0_10_7;
          monad-control-identity = super.callCabal2nix "monad-control-identity" monad-control-identity.outPath {};
          mtl = super.mtl_2_3_1;
          transformers = super.transformers_0_6_0_5;
        };
      in (haskellPackages.extend overlay).callCabal2nix "deriving-trans" src {};

    devShells.x86_64-linux.default =
      with import nixpkgs { system = "x86_64-linux"; };
      haskellPackages.shellFor {
        buildInputs = with haskellPackages; [
          cabal-install
          fourmolu
          ghcid
          haskell-language-server
          hlint
          implicit-hie
          rnix-lsp
        ];
        packages = haskellPackages: [
          self.packages.x86_64-linux.default
        ];
        withHoogle = true;
      };

    checks.x86_64-linux.fourmolu =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "fourmolu"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          fourmolu --mode check ./source
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.fourmolu
        ];
      };

    checks.x86_64-linux.hlint =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "hlint"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          hlint ./source
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.hlint
        ];
      };

    checks.x86_64-linux.hie-yaml =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "hie-yaml"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          diff --report-identical-files ./hie.yaml <(gen-hie)
        '';
        installPhase = ''
          mkdir $out
        '';
        buildInputs = [
        ];
        nativeBuildInputs = [
          haskellPackages.implicit-hie
        ];
      };

    checks.x86_64-linux.graphmod =
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "graphmod"; # TODO: Necessary to avoid segmentation fault.
        src = ./.;
        buildPhase = ''
          graphmod > graphmod.out
          dot -Tdot graphmod.out > graphmod.dot
          dot -Tpdf graphmod.out > graphmod.pdf
        '';
        installPhase = ''
          mkdir $out
          cp graphmod.dot $out
          cp graphmod.pdf $out
        '';
        nativeBuildInputs = [
          haskellPackages.graphmod
          pkgs.graphviz
        ];
      };

  };
}
