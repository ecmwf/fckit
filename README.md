FCKit
=====

[![fckit release version](https://img.shields.io/github/release/ecmwf/fckit.svg)](https://github.com/ecmwf/fckit/releases/latest)
[![travis master](https://img.shields.io/travis/ecmwf/fckit/master.svg?label=master&logo=travis)](http://travis-ci.org/ecmwf/fckit "master")
[![travis develop](https://img.shields.io/travis/ecmwf/fckit/develop.svg?label=develop&logo=travis)](http://travis-ci.org/ecmwf/fckit "develop")
[![codecov](https://codecov.io/gh/ecmwf/fckit/branch/develop/graph/badge.svg)](https://codecov.io/gh/ecmwf/fckit)

Fortran toolkit for interoperating Fortran with C/C++.

In addition useful algorithms from ecKit are wrapped with Fortran.

Project website and reference documentation on released versions:
https://confluence.ecmwf.int/display/FCKIT

## fctest

Unit Testing Framwork for Fortran, made easy.

- C Preprocessor Macros are used to make writing tests extremely fast
- Tests in one file are bundled in a Test Suite (Fortran Module)
- Python script generates a main program for a Test Suite
- Driven by CMake build system ( and ctest )

### To use in your ecbuild project

Simply add following line to your project's CMakeLists.txt

```
ecbuild_add_option( FEATURE FCTEST  DEFAULT ${ENABLE_TESTS}
                    DESCRIPTION "Fortran Unit Testing Framework"
                    REQUIRED_PACKAGES "NAME fckit" )
```

See src/examples folder how to add and create the unit-tests.

## fckit

Various Fortran modules helpful to create mixed-language applications

- MPI
- Logging

### Offline build of fckit Python virtual environment

1. On a networked machine, create a complete wheelhouse in `<source-dir>/artifacts`:

```
./populate
```

By default, wheels are downloaded for the calling system and Python interpreter. For a different
target, specify compatible platform and Python versions. For example, for Python 3.10 on Linux x86-64:

```
FCKIT_WHEEL_ARCH=manylinux_2_17_x86_64 FCKIT_WHEEL_PYTHON_VERSION=310 ./populate
```

`ruamel.yaml.clib` contains platform-specific code, so these values must match the target. Available
compatibility tags can be inspected with `python3 -m pip debug --verbose`.

2. Copy the `artifacts` directory to the offline system.

3. Pass its location when configuring fckit:

```
cmake ... -DARTIFACTS_DIR=<path-to-artifacts-dir>
```

This makes pip install exclusively from the wheelhouse using `--no-index --find-links`, without
contacting a package index.

### License

Please read LICENSE.

---------------------------------------------------------------------

ECMWF
