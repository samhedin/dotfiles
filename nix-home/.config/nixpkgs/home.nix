{ config, pkgs, ... }:
let
  pkgsUnstable = import <nixpkgs-unstable> {};
in
{
  nixpkgs.overlays = [
    # Emacs, see home-manager for more.
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    }))
  ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # omnisharp-roslyn.enable = true;
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "sam";
  home.homeDirectory = "/home/sam";
  programs.git = {
    enable = true;
    userName = "samhedin";
    userEmail = "sam.hedin@gmail.com";
  };
  programs.emacs = {
    enable = true;
    package = pkgs.emacsPgtkGcc;
    # package = pkgs.emacsGcc;
    extraPackages = (epkgs: [ epkgs.vterm ]);
  };

  programs.texlive.enable = true;
  programs.texlive.extraPackages = tpkgs: {
    inherit (tpkgs)
      scheme-medium cm-super capt-of minted biblatex algorithms tikz-cd caption
      placeins
      csquotes wrapfig braket turnstile dashbox chktex cleveref bussproofs
      latexmk;
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
