{config, pkgs, ...}:
let
    compiledLayout = pkgs.runCommand "keyboard-layout" {} ""
    ${pkgs.xorg.xkbcomp}/bin/xkbcomp ${layout.xkb} $out
    '';
in {
}
