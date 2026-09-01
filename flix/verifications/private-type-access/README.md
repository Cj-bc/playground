# private-type-in-piblic-type

"Private" datatype in `Private` module is declared as private. however, it seems like it is accessible from other modules(`Another`/`Main`).
this project compiles fine.  

I'm quite not sure why... am I misunderstanding something?

## how to run

This directory doesn't contains flake.nix, instead I use one from [grand-parent's](../../flake.nix).
Therefore, you can run this with:

```sh
$ nix develop -c flix run
```
