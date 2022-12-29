{
  description = "A basic flake with a shell";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};
      rpkgs = with pkgs.rPackages; [data_table
                                    ggplot2
				    cowplot 
                                    RColorBrewer 
                                    magick 
                                    ggtext 
                                    drc 
                                    ggpubr
                                    readxl 
                                    randomcoloR 
                                    dplyr
                                    svglite ];
    in {
      devShells.default = pkgs.mkShell {
        nativeBuildInputs = [ pkgs.bashInteractive ];
        buildInputs = [pkgs.R rpkgs];
      };
    });
}
