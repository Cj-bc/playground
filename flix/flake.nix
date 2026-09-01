{
  description = "playground for flix";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flix.url = "github:Cj-bc/flix.nix";
  };

  outputs = { self, nixpkgs, flix }:
    let eachSystems = systems: op:
          builtins.zipAttrsWith (name: values: builtins.foldl' (acc: v: acc // v) {} values) (builtins.map op systems);
    in eachSystems ["x86_64-linux" "aarch64-linux"] (system:
      let pkgs = import nixpkgs { system = system; overlays = [ flix.overlays.flix_0_75_3 ]; };
      in
        {
          packages.${system}.default = pkgs.flix;
          devShells.${system}.default = pkgs.mkShell {
            buildInputs = [pkgs.flix];
          };
        }
    );
}
