## Platformexs

Repository aimed to sharpen coding skills from well known platforms, such as:
[Hackerrank](https://hackerrank.com), [Exercism](https://exercism.org/).

We're using GHCup to manage Haskell toolchain, alongside VSCode.

> _Disclaimer!_
> 
> When installing GHCup, don't install HLS hence it'll be managed by VSCode Haskell Extension.

**Commands**

```sh
$ stack build # to install dependencies

$ stack test  # to run the tests

$ stack repl  # to enter the repl
```

**GHCi Configs**

Personally I like to have a .ghci at ${workspaceRootFolder} with some extensions
to improve the GHCi experience

```hs
:set prompt "\x03BB> "
:set prompt-cont " > "
:set +t
```