[
  (self: super: with super; {
    stable = import (fetchTarball http://nixos.org/channels/nixos-20.09/nixexprs.tar.xz) {};
    master = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {};
    omnisharp = import (fetchTarball https://github.com/ericdallo/nixpkgs/archive/update-msbuild.tar.gz) {};
  })

  (import (builtins.fetchTarball https://github.com/nubank/nixpkgs/archive/master.tar.gz))
]
