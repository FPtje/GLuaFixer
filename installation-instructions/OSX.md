# Installation instructions

`glualint` is built as an executable that may still need some extra steps to
work. Please run the following two commands in a terminal:

```bash
echo | brew install libffi gmp
sudo cp /usr/local/opt/libffi/lib/libffi.8.dylib /usr/local/lib/libffi.8.dylib
```

`glualint` requires the two libraries of `libffi` and `gmp`. The copy command
makes sure that the library file can be found when running the executable.

## glualint on M1 devices

Builds of `glualint` are only provided for the x86_64 version of OSX. If
you have a device with an Apple Silicon (M1/M2/M3/etc.) chip, this compiled 
version of `glualint` requires you install Rosetta first. Follow the 
instructions to install Rosetta here: https://support.apple.com/en-us/102527.
