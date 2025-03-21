{
  description = "codosseum/templatespliler: Nix template for Haskell projects";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix.url = "github:numtide/treefmt-nix";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";
    flake-root.url = "github:srid/flake-root";
    pre-commit-hooks.url = "github:cachix/git-hooks.nix";

    servant-prometheus.url = "github:serokell/servant-prometheus?rev=f067e984a953bbc253406f3390c45e0369cee3bb";
    servant-prometheus.flake = false;
  };

  outputs = { self, pre-commit-hooks, ... } @ inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;
      imports = [
        inputs.haskell-flake.flakeModule
        inputs.treefmt-nix.flakeModule
        inputs.flake-root.flakeModule
      ];

      perSystem = { self', system, lib, config, pkgs, ... }: {
        # Our only Haskell project. You can have multiple projects, but this template
        # has only one.
        # See https://github.com/srid/haskell-flake/blob/master/example/flake.nix
        haskellProjects.default = {
          # The base package set (this value is the default)
          # basePackages = pkgs.haskell.packages.ghc96;

          projectRoot = ./.;

          packages = {
            base64.source = "1.0";
            templatespiler-parser.source = ./templatespiler-parser;
            templatespiler-converter.source = ./templatespiler-converter;
            templatespiler-generator.source = ./templatespiler-generator;

            templatespiler-server.source = ./templatespiler-server;

            servant-prometheus.source = inputs.servant-prometheus;
          };

          # Development shell configuration
          devShell = {
            hlsCheck.enable = false;
          };

          # What should haskell-flake add to flake outputs?
          autoWire = [ "packages" "apps" "checks" ]; # Wire all but the devShell
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;

            hooks = {
              treefmt.enable = true;
              treefmt.package = config.treefmt.build.wrapper;
            };
          };
        };

        # Auto formatters. This also adds a flake check to ensure that the
        # source tree was auto formatted.
        treefmt.config = {
          inherit (config.flake-root) projectRootFile;
          package = pkgs.treefmt;

          programs.ormolu.enable = true;
          programs.nixpkgs-fmt.enable = true;
          programs.cabal-fmt.enable = true;
          programs.hlint.enable = true;


          # We use fourmolu
          programs.ormolu.package = pkgs.haskellPackages.fourmolu;
          settings.formatter.ormolu = {
            options = [
              "--ghc-opt"
              "-XImportQualifiedPost"
            ];
          };
        };

        # Default package & app.
        packages.default = self'.packages.server;
        apps.default = self'.apps.server;

        # Default shell.
        devShells.default = pkgs.mkShell {
          name = "templatespliler";
          inherit (self.checks.${system}.pre-commit-check) shellHook;
          nativeBuildInputs = with pkgs; [
            just
            config.treefmt.build.wrapper
            gcc
          ];
          # See https://zero-to-flakes.com/haskell-flake/devshell#composing-devshells
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
            config.flake-root.devShell
            config.treefmt.build.devShell
          ];

        };

        packages.dockerImage = pkgs.dockerTools.buildImage {
          name = "codosseum-org/templatespiler";
          created = "now";
          tag = builtins.substring 0 7 (self.rev or "dev");
          config = {
            Cmd = [ "${pkgs.lib.getExe self'.packages.server}" "server" ];
          };
          copyToRoot = pkgs.buildEnv {
            paths = with pkgs; [
              self'.packages.server
              pkgs.bash
            ];
            name = "server-root";
            pathsToLink = [ "/bin" ];
          };
        };
      };
    };
}
