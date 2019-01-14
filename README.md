# FCKit

Fortran toolkit for interoperating Fortran with C/C++.

In addition useful algorithms from ecKit are wrapped with Fortran.

Project website and reference documentation on released versions:
https://software.ecmwf.int/wiki/display/FCKIT

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
                    REQUIRED_PACKAGES "PROJECT fckit" )
```

See src/examples folder how to add and create the unit-tests.

## fckit

Various Fortran modules helpful to create mixed-language applications

- MPI
- Logging

### License

Please read LICENSE.

---------------------------------------------------------------------

ECMWF
