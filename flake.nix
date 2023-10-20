{
  description = "A development tool for running commands with maximum possible concurrency.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pname = "build_pipeline";
      version = "0.0.14";

      pkgs = import nixpkgs {inherit system;};
      beamPackages = pkgs.beam.packages.erlang_25;
      elixir = beamPackages.elixir_1_14;
      hex = beamPackages.hex;
    in {
      # `nix develop`.
      devShells = {
        default = pkgs.mkShell {
          packages = [
            elixir
          ];
        };
      };
      # `nix fmt`.
      formatter = pkgs.alejandra;
      # `nix build`.
      packages = {
        build-pipeline = pkgs.stdenvNoCC.mkDerivation {
          inherit pname version;
          src = pkgs.nix-gitignore.gitignoreSource [] ./.;
          nativeBuildInputs = [elixir pkgs.makeWrapper];
          buildPhase = ''
            # Expose Nix's hex to Mix.
            export MIX_PATH="${hex}/lib/erlang/lib/hex/ebin"
            export HOME=$PWD/.hex
            mkdir -p $HOME
            mix deps.get
            env MIX_ENV=prod mix escript.build
          '';
          installPhase = ''
            mkdir -p $out/bin
            cp bp $out/bin/bp
            wrapProgram $out/bin/bp \
              --prefix PATH : ${pkgs.lib.makeBinPath [beamPackages.erlang]}
          '';
        };
        default = self.packages.${system}.build-pipeline;
      };
    });
}
