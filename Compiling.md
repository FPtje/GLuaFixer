# Compiling glualint

This document describes how glualint is to be compiled. This guide applies to Windows, Linux and OSX.

## Prerequisites

In order to build glualint, you need the following programs:

- ghc
- cabal (often called cabal-install)
- On Windows: MinGW

These programs can be installed through the [Haskell platform](https://www.haskell.org/platform/), although with Linux, using your distribution's package manager might be a cleaner solution.

Make sure the programs are in your `PATH`.

## Compiling glualint
Open a terminal (or command prompt in Windows) in the root folder of the repository (the folder with `gluafixer.cabal` file).
Run the following commands in order:

```bash
# Depending on OS, for Linux run AGGenerator.sh, on Windows run the AGGenerator.bat
./AGGenerator.sh
cabal build glualint
```

The first command makes sure that any dependencies are installed in a local sandbox. The second command installs the dependencies. The third command actually builds glualint.

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
