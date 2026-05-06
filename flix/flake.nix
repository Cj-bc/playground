{
  description = "playground for flix";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: 
    let flix_overlay = version: final: prev: {
          flix = prev.flix.overrideAttrs(finalAttrs: previousAttrs: {
            version = version;
            src = prev.fetchurl {
              url = "https://github.com/flix/flix/releases/download/v${version}/flix.jar";
              sha256 = "sha256-Ha5oRDpQ7YuGsaF/ZNx8b+HjTSroxZEjzI3zR3g7NXI=";
            };
          });
        };
      flix_0_71_0 = flix_overlay "0.71.0";
      pkgs-x86_64 = import nixpkgs { system = "x86_64-linux"; overlays = [ flix_0_71_0 ]; };
      pkgs-aarch64 = import nixpkgs { system = "aarch64-linux"; overlays = [ flix_0_71_0 ]; };
      # referenced: https://github.com/numtide/flake-utils/blob/11707dc2f618dd54ca8739b309ec4fc024de578b/lib.nix#L33
      eachSystems = systems: op:
        builtins.zipAttrsWith (name: values: builtins.foldl' (acc: v: acc // v) {} values) (builtins.map op systems);
    in eachSystems ["x86_64-linux" "aarch64-linux"] (system:
      let pkgs = import nixpkgs { system = system; overlays = [ flix_0_71_0 ]; };
      in
        {
          packages.${system}.default = pkgs.flix;
        }
    );
}
