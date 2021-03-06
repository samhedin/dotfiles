# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
# chromium -enable-features=UseOzonePlatform -ozone-platform=wayland --disable-gpu-memory-buffer-video-frames
# https://elis.nu/blog/2021/02/detailed-setup-of-screen-sharing-in-sway/
{ config, pkgs, lib, ... }: {

  nix = {
    maxJobs = 16;
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  environment.variables.XDG_CURRENT_DESKTOP = "sway";
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
  ];
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.permittedInsecurePackages = [ "libgit2-0.27.10" ];
  nix.gc = {
    automatic = true;
    dates = "weekly";
    options = "--delete-older-than 30d";
  };
  services.logind.extraConfig = ''
    RuntimeDirectorySize=50G
    HandleLidSwitchDocked=ignore
  '';
  programs.steam.enable = true;
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModulePackages = [ pkgs.linuxPackages_latest.v4l2loopback ];
  boot.extraModprobeConfig = ''
    options v4l2loopback exclusive_caps=1 video_nr=9 card_label="OBS"
  '';

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp6s0.useDHCP = true;

  #https://github.com/NixOS/nixpkgs/pull/123034
  services.pipewire.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  nix.autoOptimiseStore = true;
  services.xserver.enable = true;

  services.xserver = {
   desktopManager.gnome.enable = true;
    displayManager.gdm.enable = true;
    displayManager.gdm.wayland = true;

    extraLayouts.dvorak-ep = {
      description = "dvorak ep";
      languages = [ "sv" ];
      symbolsFile = /home/sam/dotfiles/dvorak-ep.xkb;
    };
  };

  #  Configure keymap in X11
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable CUPS to print documents.
  # services.printing.enable = true;
  services.sshd.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.sam = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };
  nix.allowedUsers = [ "sam" ];

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    let
      unstable = import <nixos-unstable> {config={allowUnfree=true;};};
      machNix = import (builtins.fetchGit {
        url = "https://github.com/DavHau/mach-nix/";
        ref = "refs/tags/3.3.0";
      }) { python = "python39Full"; };
      defaultPythonEnv = machNix.mkPython {
        requirements = ''
          pandas
          numpy
          h5py
          scikit-learn
          pyflakes
          jupyter-console
          isort
          ipywidgets
          debugpy
          torch
          torchvision
          toolz
          keras
          scikit-image
          tqdm
          pygments
          pygetwindow
          qtconsole
          jupyterlab
          pymupdf
          matplotlib
          seaborn
          scipy
          black
        '';
      };
      droidcamdesktop = pkgs.makeDesktopItem {
        name = "droidcamdesktop";
        desktopName = "droidcamdesktop";
        exec = "${unstable.droidcam}/bin/droidcam";
      };
    in [
      stow
      autoflake
      xdotool
      libnotify
      gsettings-desktop-schemas
      lxappearance
      pinentry
      gnome3.seahorse
      libtool
      droidcamdesktop
      gammastep
      smplayer
      mplayer
      julia-stable-bin
      feh
      unstable.swappy
      git
      qbittorrent
      rustup
      unstable.rust-analyzer
      stack
      unstable.nyxt
      htop
      clipman
      wget
      nodejs
      unstable.droidcam
      clojure
      leiningen
      mkl
      pdfgrep
      j4-dmenu-desktop
      # gnuapl
      nodePackages.pyright
      # https://jcodev.eu/posts/using-nix-for-haskell-development-in-emacs-with-lsp/
      unstable.haskell-language-server
      haskellPackages.cabal-install
      haskellPackages.ghc
      google-chrome-beta
      kate
      unstable.cabal2nix
      electrum
      ncdu
      unzip
      imagemagick
      cmake
      ninja
      nixfmt
      vim
      fd
      ripgrep
      grim
      slurp
      # unstable.unrar
      alacritty
      sqlite
      # unstable.vscode
      slack
      defaultPythonEnv
      machNix.mach-nix
      tldr
      wf-recorder
      qt5.qtwayland
      okular
      torbrowser
      gnupg
      kgpg
      gnumake
      unstable.vlc
      gcc
      pandoc
      # zoom-us
      spotify
      unstable.discord-ptb
      papirus-icon-theme
      languagetool
      pkgs.ntfsprogs
    ];

  programs.qt5ct.enable=true;

  fonts.fonts = with pkgs; [
    noto-fonts
    siji
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
    source-sans-pro
    mplus-outline-fonts
    dina-font
    proggyfonts
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };
  location.latitude = 59.37118495540346;
  location.longitude = 18.065956381997143;
  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}

