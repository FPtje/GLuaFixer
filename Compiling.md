# Compiling glualint

This document describes how glualint is to be compiled. This guide applies to Windows, Linux and hopefully OSX.

## Linux/OSX

For Linux and OSX, the compilation uses Nix. Please install Nix version 2.4 or higher from [NixOS.org](https://nixos.org/download.html).
Nix 2.4 or higher is needed to enable the flakes feature that is introduced in 2.4.

Once installed, Nix flakes need to be enabled by editing `/etc/nix/nix.conf` and adding the following line:

```conf
experimental-features = nix-command flakes
```

Then, in the root of this repository, run the following command to build this project:

```bash
nix develop --command ./AGGenerator.sh
nix develop --command cabal v1-build
```

This will create the `glualint` executable at `dist/build/glualint/glualint`.

Note: OSX building is neither tested nor supported. It may work, it may not.

## Windows

The compilation for Windows more difficult, since the dependencies have to be installed manually.

### Prerequisites

In order to build glualint, you need the following programs:

- ghc
- cabal (often called cabal-install)
- On Windows: MinGW

These programs can be installed through the [Haskell platform](https://www.haskell.org/platform/), although with Linux, using your distribution's package manager might be a cleaner solution.

Make sure the programs are in your `PATH`.

### Compilation commands

Open a terminal (or command prompt in Windows) in the root folder of the repository (the folder with `gluafixer.cabal` file).
Run the following commands in order:

```bash
AGGenerator.bat
cabal build glualint
```

Once the process succeeds, you will find an executable called `glualint` in `dist/build/glualint/`. That executable is what you need. Follow the [Installation instructions](https://github.com/FPtje/GLuaFixer#installing) to install it.

## Troubleshooting

Those three cabal commands could fail. Here are some of the problems and their solutions.

----

> Cannot find executable `cabal`, `ghc`, `gcc` or some other executable.

You failed to install cabal and/or ghc and/or MinGW. Make sure they're in your `PATH`.

----

> Something something cannot find compilation tools or whatever

This happens when MinGW isn't installed properly. Ghc comes with MinGW, add the bin folder to `PATH`.

----

> Something about conflicting versions of cabal being installed when installing dependencies (specifically, UUAGC)

I had this once, it was terrible. Turns out this happens when there are multiple versions of cabal installed.

1. Open a terminal (or command prompt) somewhere (**not** in the glualint folder).
2. run `ghc-pkg list`
3. Find the oldest version of cabal you can find
4. Copy its name (with the version tag probably)
5. Run `ghc-pkg unregister <COPIEDNAME>`

If that doesn't work, go look in your home folder's cabal folder and try to find the fucker manually. Just delete its folder when you find it.

----

> Build error, type error on `cabal build`

Hold on, that shouldn't fucking happen. Glualint is known to compile on ghc 7.10 and 7.10.3. It *probably* won't work on ghc versions below that.

1. Run `ghc --version`.
2. If the reported version is anything below 7.10, upgrade ghc and don't complain to me.
3. If it's *over* 7.10.3, hold on tight to your balls and wait for an update.
4. If you think by now my shit should work for the newer version because it has been around for long enough, create an issue.

----
