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
      src = pkgs.nix-gitignore.gitignoreSource [] ./.;
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
        build-pipeline = beamPackages.mixRelease {
          inherit pname src version;
          mixFodDeps = beamPackages.fetchMixDeps {
            inherit pname src version;
            hash = "sha256-H7yiBHoxuiqWcNbWwPU5X0Nnv8f6nM8z/ZAfZAGPZjE=";
            mixEnv = "prod";
          };
          postBuild = ''
            env MIX_ENV=prod mix escript.build
          '';
          postInstall = ''
            # Install the `bp` escript.
            mkdir -p $out/bin
            cp bp $out/bin/bp
            wrapProgram $out/bin/bp \
              --prefix PATH : ${pkgs.lib.makeBinPath [beamPackages.erlang]}

            # Clean up the Mix release stuff, most of which we don't need.
            rm -r $out/{lib,releases,erts-*,bin/build_pipeline}
          '';
        };
        default = self.packages.${system}.build-pipeline;
      };
    });
}
