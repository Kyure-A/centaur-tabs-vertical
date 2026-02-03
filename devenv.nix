{ pkgs, ... }:
{
  packages = [
    pkgs.emacs
    pkgs.eask-cli
  ];

  scripts.test.exec = ''
    eask install
    eask test ert ./test/*.el
  '';
}
