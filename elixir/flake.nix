{
  description = "A flake for Elixir advent of code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils = { url = "github:numtide/flake-utils"; };
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        inherit (pkgs.lib) optional;
        pkgs = import nixpkgs { inherit system; };
        elixir = pkgs.beam.packages.erlang.elixir;
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            elixir

            # LSP
            elixir_ls
          ]
          ++ optional stdenv.isLinux pkgs.inotify-tools;
        };
      }
    );
}
