# dotfiles

These are the configuraiton files (["dotfiles"](http://dotfiles.github.io/))
for my Arch Linux setup and OSX. Previously, I managed them using GNU Stow.

Nowadays I use [Nix](https://nixos.org/) with
[Home Manager](https://github.com/nix-community/home-manager). This tool gives
me declarative management over my dotfiles, and I'm currently porting them to
use [Nix Flakes](https://wiki.nixos.org/wiki/Flakes).

> **Note:** Be sure to enable [storage
> optimization](https://wiki.nixos.org/wiki/Storage_optimization) if you're
> using flakes, otherwise Nix wastes a lot of space.

## Building

Today, I have separate flakes for each system I maintain:

- [NixOS](./nix/blackbeta)
- [Arch](./nix/arch)
- [macOS](./nix/darwin)

The Darwin system is special because it actually maintains the host system. The
other two simply manage my homedir.
