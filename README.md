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

An offline build/installation of the fckit Python virtual environment can be completed as follows:

1. Download all necessary Python dependencies of src/fckit/fckit_yaml_reader. `ruamel.yaml.clib`
is not a pure Python package, so we have to ensure a wheel compatible with the target platform is
downloaded. pip compatibility tags for any system can be displayed using `python3 -m pip debug --verbose`,
and buit-distributions (i.e. wheels) for ruamel.yaml.clib can be found [here](https://pypi.org/project/ruamel.yaml.clib/#files).
For a linux installation based on an x86 architecture using Python3.10, the following command can be used:

```
FCKIT_WHEEL_ARCH=manylinux_2_17_x86_64 FCKIT_WHEEL_PYTHON_VERSION=310 ./populate
```

This will download all the wheels to `<source-dir>/artifacts.` It should
be noted that if `FCKIT_WHEEL_ARCH` and `FCKIT_WHEEL_PYTHON_VERSION`
are not specified then the wheels are downloaded for the calling system's Python interpreter.

2. scp/rsync/copy the directory containing the dependencies to the offline system.

3. Add the path to the `artifacts` directory to the fckit CMake configuration step, i.e. `-DARTIFACTS_DIR=<path-to-artifacts-dir>`.

### License

Please read LICENSE.

---------------------------------------------------------------------

ECMWF
