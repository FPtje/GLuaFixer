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

The OSX build of `glualint` sadly only works on the x86_64 version of OSX. If
you have a device with an M1 chip, this compiled version of `glualint` will
sadly not be compatible. This is because M1 devices run on a differen CPU
architecture than what this version of `glualint` is compiled for.

If indeed you would like to get this to work on M1 device, it should be possible
to compile `glualint` from scratch, though your mileage may vary.
