# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }: {
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./cachix.nix
  ];
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.useOSProber = true;

  networking.hostName = "nixos"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp6s0.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  # };

  services.xserver.enable = true;
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
  services.xserver.displayManager.gdm.wayland = true;

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
    openssh.authorizedKeys.keys = ["ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDHDafR03vQCA1qNsRs1MtRqP3WzjhZkHgVdmatKbzYvJ1UVonAptpnY7aDAnNyzuWNBhdYoCPFLmtQ/gUOEpabm5AOZgbuwpZBR4c3AJddqp8gUmunEmFXPIT/6EqA5MRXGcfqGxwQsdooHELqWn6xcN7Y/LlRtRLRhe2DVmnDrLj1i/Rd02qIpYznlpfo+261PnwxH44sqPpW7SwbZPTvYxqWfQ0z0LRm7ntEI4/Y0A7eu0fCRnrQ/1oeucFP7B6A6IlxViNMeFXc3v/P6LnU6yeLr5CSuU74JxAGUkX8Y0nR1lfWz/ygUM3DQVJuZP4JIlS+dTppw7qkE3kAn/ufHhe8Rw6gT7u0MsF9kDDVsvteDM0wgQdlrHoXAxSBrE/IWMQDQmS4h7PRvyCLuYeWqjrUjm4voWrAQf0tuHoUPr6HvuUeQxJjdghEey1g4bYy1yCbXyBqxHNefDRUXFNi+u7Y392WxBrElELbZUdnolHHSKSoWkklxLjGengy1JtyxbGHfrbOZI6FWFll942LmF7eAFo8xJlQIiuBbfk+RqbC0lCrsHXwQ58hNQHucDirHxJjih6n4Cv1neEzKk7gDwyX7lW136j1f3vwc8EpqIsPHaZpE04JDMQa0jt9FYBKEvq/GcNL2uLoCEI2w2ZTI9paauf/dirDDLC8hhIWiw== sam.hedin@gmail.com"];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    let
      polybar = pkgs.polybar.override { i3Support = true; };
      my-python-packages = python-packages:
        with python-packages; [
          pandas
          numpy
          pytorch
          pytorch-lightning
          torchvision
          tqdm
          pygments
          matplotlib
          jsonpickle
          i3ipc
          scipy
          scikitlearn
          black
        ];
      my-python = python38.withPackages my-python-packages;
    in [
      stow
      autoflake
      git
      wget
      nodejs
      cmake
      ninja
      nixfmt
      vim
      fd
      tint2
      ripgrep
      firefox-wayland
      ark
      unrar
      alacritty
      fira-code
      sqlite
      slack
      my-python
      rust-analyzer
      chromium
      vlc
      gcc
      deluge
      pandoc
      rustup
      libreoffice
      teamviewer
      zoom-us
      spotify
      aspell
      aspellDicts.en
      aspellDicts.sv
      aspellDicts.en-computers
      aspellDicts.en-science
      openssl
      j4-dmenu-desktop
      discord-ptb
      polybar
      arc-theme
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

