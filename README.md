nix-shell -A shells.ghc --run "ghcid --command \"cabal new-repl $1\""
