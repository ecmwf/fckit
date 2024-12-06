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

1. Configure fckit (in the CMake sense) on a system with internet access and download Python dependencies:

```
python3 -m pip download -r <fckit-build-dir>/fckit_venv_requirements.txt -d <dir-to-store-dependencies>
```

2. scp/rsync/copy the directory containing the dependencies to the offline system.

If fckit is being built using the same C compiler as the system Python installation, you can procede straight
to step 8. If fckit is however being built using a different C compiler than that used to build the system
Python installation, then a few more steps are needed:

3. Create a temporary virtual environment and load it:

```
python3 -m venv tmp_venv
source tmp_venv/bin/activate
```

4. Install the pre-cached setuptools in the temporary virtual environment:

```
python3 -m pip install --no-build-isolation --no-index --find-links=<dir-containing-dependencies> setuptools==65.5.0
```

5. Load the C compiler used to build the system Python and build a wheel for ruamel.yaml.clib:

```
python3 -m pip wheel --no-build-isolation --no-index --find-links=<dir-containing-dependencies> fckit/contrib/ruamel.yaml.clib-0.2.12 -w <dir-containing-dependencies>
```

6. Delete the temporary virtual environment:

```
deactivate
rm -rf tmp_venv
```

7. Change line 2 in fckit/cmake/fckit_venv_requirements.txt to `ruamel.yaml.clib==0.2.12`.

8. Add the following two arguments to the fckit CMake configuration step:

```
-DENABLE_FCKIT_VENV_OFFLINE=ON -DFCKIT_VENV_WHEEL_DIR=<dir-containing-dependencies>
```

### License

Please read LICENSE.

---------------------------------------------------------------------

ECMWF
