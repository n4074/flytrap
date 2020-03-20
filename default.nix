{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
    agent = ./agent;
    reflex-codemirror = ./thirdparty/reflex-widgets/reflex-codemirror;
    reflex-dom-svg = ./thirdparty/reflex-dom-svg;
  };

  useWarp = true;

  shells = {
    ghc = [
      "common"
      "frontend"
      "backend"
      "agent"
      "reflex-codemirror" 
    ];
    ghcjs = ["common" "frontend" "backend" "agent" "reflex-codemirror" ];
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  overrides = self: super: {
    #haskell-src-exts = self.callHackage "haskell-src-exts" "1.20.3" {}; # pinned due to derive dependecy in reflex-codemirror
    #free = self.callCabal2nix "free" (pkgs.fetchFromGitHub {
    #  owner = "ekmett";
    #  repo = "free";
    #  rev = "a0c5bef18b9609377f20ac6a153a20b7b94578c9";
    #  sha256 = "0vh3hj5rj98d448l647jc6b6q1km4nd4k01s9rajgkc2igigfp6s";
    #}) {};
    reflex-binary = self.callCabal2nix "reflex-binary" (pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-binary";
      rev = "d07e439fc0a76c015740c2e3a0d0629dd6a741b0";
      sha256 = "0v0ii0fwv08vb9dah2b0f2ln93nn0d7gmg6m0jb0dyzlidwf7g2j";
    }) {};
    reflex-basic-host = self.callCabal2nix "reflex-basic-host" (pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-basic-host";
      rev = "f39732465082dbdfcff742f430e370ccbea80b02";
      sha256 = "1rj5wg2yzycwxjxpmmp2lwr6v0piibrpkr4dsn83r3p72ih0x4p0";
    }) {};
    reflex-backend-websocket = pkgs.haskell.lib.appendPatch (self.callCabal2nix "reflex-backend-websocket" (pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-backend-websocket";
      rev = "ddd4229b4a804801c7eebcc26d75aade59fd0d85";
      sha256 = "1a8ahzf5j4xpm4r79lxsfpm5663lf1g41i8k52bi6c71zkk0j0cq";
    }) {}) ./nix/patches/reflex-backend-websocket-0.1.0.0.patch;
    uri = pkgs.haskell.lib.doJailbreak (self.callHackage "uri" "0.1.6.5" {}); # marked as broken in nix
  };
})
