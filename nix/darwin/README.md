# darwin flake

To run:

```
./rebuild.sh
```

## First time run

The first time you run this you will not have nixos-darwin installed.

Therefore, you must run a different command:

```
$ nix run nix-darwin --extra-experimental-feature nix-command --extra-experimental-feature flakes -- switch --impure --flake ~/.config/nix
```

Note the use of `--impure` since secrets are passed via the environment.

## References

- [Home Manager Option Search](https://home-manager-options.extranix.com/)
- 
