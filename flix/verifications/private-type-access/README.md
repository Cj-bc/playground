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

# few more things I tried

## non companion enum has the same behaviour

from [documentation](https://doc.flix.dev/companion-modules.html), I assumed that name of `Private`was exported because it's companion name. 

> The companion’s name is exported from the module. This means that Color can refer to both the module and the enum. We can refer to a case as Color.Red or as Color.Color.Red.
>
> https://doc.flix.dev/companion-modules.html

So I added `NonCompanionPrivate` type, which is neither companion nor public. 
but it still succeeds compilation, so it isn't because `Private` is companion. 

## calling private method results in compilation failure

when calling private method(https://github.com/Cj-bc/playground/commit/3a14d34e8962ff03c9edb459612053098de0864b), compilation fails and error below is returned. 

```
-- Resolution Error [E0237] -------------------------------------- src/Main.flix
>> Definition 'Private.privateMethod' is not accessible from the module ''.
                                                                         16 |     println(privateMethod())
                 ^^^^^^^^^^^^^
                 inaccessible definition                                                                                                          Tip: Mark the definition as 'pub'.
```
