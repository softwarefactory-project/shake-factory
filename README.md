# shake-factory

[Shake][shake] functions to build project in Software Factory.

To build a project using `ShakeFactory` run this command:

```
podman run -it --rm -v $(pwd):/data:Z quay.io/software-factory/shake-factory
```

## Install from source

You need to install the `ShakeFactory` library before using it.
You will need the latest cabal-install (version 3), for example on fedora:

```
sudo dnf copr enable -y petersen/cabal-install && sudo dnf install -y cabal-install ghc
cabal install --lib lib:shake-factory dhall shake shake-dhall text bytestring containers casing
```

This install the library in `~/.cabal` and it creates an environment in `~/.ghc`.
Re-installation may fail because of a [cabal issue](https://github.com/haskell/cabal/issues/6394), fix that by removing the `~/.ghc` directory first.

Once installed you can verify it is working:

```
$ ghci
Prelude> import ShakeFactory
Prelude ShakeFactory> :browse ShakeFactory
...
```

## Build the image

Once the library is installed, you can build the container image:

```
# Build
shake container

# Publish
shake quay.io/software-factory/shake-factory
```

[shake]: https://shakebuild.com/
