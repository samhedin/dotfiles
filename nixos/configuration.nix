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
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;
  boot.kernelPackages = pkgs.linuxPackages_5_10;
  boot.kernelModules = [ "v4l2loopback" ];
  boot.extraModulePackages = [ pkgs.linuxPackages_5_10.v4l2loopback ];
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

  services.pipewire.enable = true;
  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-wlr xdg-desktop-portal-gtk xdg-desktop-portal ];
  };

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
  services.xserver.displayManager.gdm.enable = true;
  # services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.displayManager.gdm.wayland = true;

  # services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

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
      machNix = import (builtins.fetchGit {
        url = "https://github.com/DavHau/mach-nix/";
        ref = "refs/tags/3.1.1";
      }) { python = "python38Full"; };
      defaultPythonEnv = machNix.mkPython {
        requirements = ''
                    pandas
                    numpy
                    scikit-learn
                    pyflakes
                    isort
                    tensorflow
                    debugpy
                    torch
                    torchvision
                    toolz
                    opencv-python
                    keras
                    scikit-image
                    tqdm
                    pygments
                    pygetwindow
                    qtconsole
                    jupyterlab
                    pymupdf
                    matplotlib
                    pyperclip
                    scipy
                    black
                  '';
      };
      droidcamdesktop = pkgs.makeDesktopItem {
        name = "droidcamdesktop";
        desktopName = "droidcamdesktop";
        exec = "${droidcam}/bin/droidcam";
      };
    in [
      stow
      autoflake
      xdotool
      libnotify
      docker
      pinentry
      gnome3.seahorse
      wmctrl
      libtool
      droidcamdesktop
      feh
      swappy
      aria
      git
      # tor-browser-bundle-bin
      cask
      qbittorrent
      rustup
      rust-analyzer
      stack
      htop
      wget
      appimage-run
      nodejs
      droidcam
      clojure
      leiningen
      mkl
      mono6
      j
      dotnet-sdk_5
      pdfgrep
      j4-dmenu-desktop
      # gnuapl
      julia
      nodePackages.pyright
      # https://jcodev.eu/posts/using-nix-for-haskell-development-in-emacs-with-lsp/
      haskell-language-server
      haskellPackages.cabal-install
      haskellPackages.ghc
      google-chrome-beta
      cabal2nix
      electrum
      ncdu
      unzip
      imagemagick
      cmake
      chromium
      ninja
      nixfmt
      vim
      fd
      ripgrep
      ark
      grim
      slurp
      unrar
      libreoffice
      alacritty
      sqlite
      vscode
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
      nyxt
      vlc
      gcc
      pandoc
      zoom-us
      spotify
      discord-ptb
      papirus-icon-theme
      languagetool
      pkgs.ntfsprogs
      (pkgs.writeTextFile {
        name = "startsway";
        destination = "/bin/startsway";
        executable = true;
        text = ''
          #! ${pkgs.bash}/bin/bash

          # first import environment variables from the login manager
          systemctl --user import-environment
          # then start the service
          exec systemctl --user start sway.service
        '';
      })
    ];
  systemd.user.targets.sway-session = {
    description = "Sway compositor session";
    documentation = [ "man:systemd.special(7)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = [ "graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
  };

  systemd.user.services.sway = {
    description = "Sway - Wayland window manager";
    documentation = [ "man:sway(5)" ];
    bindsTo = [ "graphical-session.target" ];
    wants = [ "graphical-session-pre.target" ];
    after = [ "graphical-session-pre.target" ];
    # We explicitly unset PATH here, as we want it to be set by
    # systemctl --user import-environment in startsway
    environment.PATH = lib.mkForce null;
    serviceConfig = {
      Type = "simple";
      ExecStart = ''
        ${pkgs.dbus}/bin/dbus-run-session ${pkgs.sway}/bin/sway --debug
      '';
      Restart = "on-failure";
      RestartSec = 1;
      TimeoutStopSec = 10;
    };
  };

  services.redshift = {
    enable = true;
    # Redshift with wayland support isn't present in nixos-19.09 atm. You have to cherry-pick the commit from https://github.com/NixOS/nixpkgs/pull/68285 to do that.
    package = pkgs.redshift-wlr;
  };

  programs.waybar.enable = true;

  systemd.user.services.kanshi = {
    description = "Kanshi output autoconfig ";
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      # kanshi doesn't have an option to specifiy config file yet, so it looks
      # at .config/kanshi/config
      ExecStart = ''
        ${pkgs.kanshi}/bin/kanshi
      '';
      RestartSec = 5;
      Restart = "always";
    };
  };
  fonts.fonts = with pkgs; [
    noto-fonts
    siji
    noto-fonts-cjk
    noto-fonts-emoji
    liberation_ttf
    fira-code
    fira-code-symbols
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
  programs.sway = {
    enable = true;
    wrapperFeatures.gtk = true; # so that gtk works properly
    extraPackages = with pkgs; [
      swaylock
      swayidle
      wl-clipboard
      mako # notification daemon
      alacritty # Alacritty is the default terminal in the config
      dmenu # Dmenu is the default in the config but i recommend wofi since its wayland native
      kanshi
      waybar
      wdisplays
    ];
  };
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

