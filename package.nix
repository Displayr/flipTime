{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipTime";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Functions to aggregate time series into strings, and convert
    strings back into dates.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    plotly
    lubridate
    flipU
  ];
}
