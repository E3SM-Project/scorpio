# Software for Caching Output and Reads for Parallel I/O (SCORPIO)

A high-level Parallel I/O Library for structured grid applications. This library was derived from the [Parallel I/O library](https://github.com/NCAR/ParallelIO).

## Documentation

The latest documentation is available [here](https://docs.e3sm.org/scorpio/html/index.html).
For complete documentation, also see the HTML documentation in the docs/html directory.

## Nightly Tests

The results of our nightly tests on multiple platforms can be found on our
cdash site at [http://my.cdash.org/index.php?project=E3SM_SCORPIO](http://my.cdash.org/index.php?project=E3SM_SCORPIO).

## Dependencies

SCORPIO can use NetCDF (version 4.3.3+), PnetCDF (version 1.6.0+) or
ADIOS (version 2.6.0+) for I/O.

Ideally, the NetCDF version should be built with MPI, which requires that it
be linked with an MPI-enabled version of HDF5.  Optionally, NetCDF can be 
built with DAP support, which introduces a dependency on CURL.  Additionally,
HDF5, itself, introduces dependencies on LIBZ and (optionally) SZIP.

## Configuring with CMake

To configure the build, SCORPIO requires CMake version 2.8.12+.  The typical
configuration with CMake can be done as follows:

```
CC=mpicc CXX=mpicxx FC=mpif90 cmake [-DOPTION1=value1 -DOPTION2=value2 ...] /path/to/pio/source
```

where `mpicc`, `mpicxx` and `mpif90` are the appropriate MPI-enabled compiler wrappers for your system.

The `OPTIONS` section typically should consist of pointers to the install
locations for various dependencies, assuming these dependencies are not 
located in *canonical* search locations.  

For each dependency `XXX`, one can specify the location of its 
installation path with the CMake variable `XXX_PATH`.  If the `C` and
`Fortran` libraries for the dependency are installed in different locations
(such as can be done with NetCDF), then you can specify individually
`XXX_C_PATH` and `XXX_Fortran_PATH`.  Hence, you can specify the locations
of both NetCDF-C and NetCDF-Fortran, as well as PnetCDF, with the following
CMake configuration line:

```
CC=mpicc CXX=mpicxx FC=mpif90 cmake -DNetCDF_C_PATH=/path/to/netcdf-c \
                         -DNetCDF_Fortran_PATH=/path/to/netcdf-fortran \
                         -DPnetCDF_PATH=/path/to/pnetcdf \
                         /path/to/pio/source
```

This works for the dependencies: `NetCDF`, `PnetCDF`, `HDF5`, `LIBZ`, `SZIP`.

### Additional CMake Options

Additional configuration options can be specified on the command line.

The `PIO_ENABLE_TIMING` option can be set to `ON` or `OFF` to enable or
disable the use of GPTL timing in the SCORPIO libraries.  This feature requires
the GPTL C library for the SCORPIO `C` library and the GPTL Fortran library with
the `perf_mod.mod` and `perf_utils.mod` interface modules.  If these GPTL
libraries are already installed on the system, the user can point SCORPIO to the
location of these libraries with the `GPTL_PATH` variable (or, individually,
`GPTL_C_PATH` and `GPTL_Fortran_Perf_PATH` variables).  However, if these
GPTL libraries are not installed on the system, and GPTL cannot be found,
then SCORPIO will build its own internal version of GPTL.

If PnetCDF is not installed on the system, the user can disable its use by
setting `-DWITH_PNETCDF=OFF`.  This will disable the search for PnetCDF on the
system and disable the use of PnetCDF from within SCORPIO.

NetCDF is optional if PnetCDF is used, and the user can disable its use by
setting `-DWITH_NETCDF=OFF`.  This will disable the search for NetCDF on the
system and disable the use of NetCDF from within SCORPIO.

ADIOS is another I/O library supported by SCORPIO, and the user can enable
it by setting `-DWITH_ADIOS2=ON`. The path to the ADIOS install location
needs to be specified by setting the `ADIOS2_DIR` environment variable to
the install path.

If the user wishes to disable the SCORPIO tests, then the user can set the
variable `-DPIO_ENABLE_TESTS=OFF`.  This will entirely disable the CTest 
testing suite, as well as remove all of the test build targets.

If you wish to install SCORPIO in a safe location for use later with other
software, you may set the `CMAKE_INSTALL_PREFIX` variable to point to the
desired install location.

## Building

Once you have successfully configured SCORPIO with CMake in a build directory.
From within the build directory, build SCORPIO with:

```
make
```

This will build the `pioc` and `piof` libraries.

## Testing

If you desire to do testing, and `PIO_ENABLE_TESTS=ON` (which is the default
setting), you may build the test executables with:

```
make tests
```

Once the tests have been built, you may run tests with:

```
ctest
```

If you have not run `make tests` before you run `ctest`, then you will see
all of the tests fail.

Alternatively, you may build the test executables and then run tests 
immediately with:

```
make check
```

(similar to the typical `make check` Autotools target).

**NOTE:** It is important to note that these tests are designed to run in parallel.
If you are on one of the supported supercomputing platforms (i.e., NERSC, NWSC, ALCF, 
etc.), then the `ctest` command will assume that the tests will be run in an appropriately
configured and scheduled parallel job.  This can be done by requesting an interactive
session from the login nodes and then running `ctest` from within the interactive
terminal.  Alternatively, this can be done by running the `ctest` command from a
job submission script.  It is important to understand, however, that `ctest` itself
will preface all of the test executable commands with the appropriate `mpirun`/`mpiexec`/`runjob`/etc.
Hence, you should not further preface the `ctest` command with these MPI launchers.

## Installing

Once you have built the SCORPIO libraries, you may install them in the location
specified by the `CMAKE_INSTALL_PREFIX`.  To do this, simply type:

```
make install
```

If the internal GPTL libraries were built (because GPTL could not be found
and the `PIO_ENABLE_TIMING` variable is set to `ON`), then these libraries
will be installed with SCORPIO.

## Acknowledgement

The SCORPIO library can be acknowledged in publications by referring to the
publication and software below,

```
@inproceedings{inproceedings,
author = {Krishna, Jayesh and Wu, Danqing and Jacob, Robert and Ganyushin, Dmitry},
year = {2025},
month = {06},
pages = {712-721},
title = {SCORPIO: A Parallel I/O library for Exascale Earth System Models},
doi = {10.1109/IPDPSW66978.2025.00112}
}
```

```
@misc{ doecode_36752,
title = {SCORPIO},
author = {Krishna, Jayesh and Wu, Danqing and Kurc, Tahsin and Edwards, Jim and Hartnett, Edward},
abstractNote = {The SCORPIO (Software for Caching Output and Reads for Parallel I/O) library provides a portable application programming interface (API) for describing, reading and storing array-oriented data that is distributed in memory across multiple processes. It is used by all the model components in DOE's Energy Exascale Earth System Model (E3SM) for reading input data and writing model output. The library also supports rearranging data distributed across multiple processes before using low-level I/O libraries like PnetCDF, NetCDF, and ADIOS to write the data to the file system. The library was derived from the Parallel I/O library.},
}
```

## License

The SCORPIO library is available under a BSD 3-clause license. Please see
[LICENSE](https://github.com/E3SM-Project/scorpio/blob/master/LICENSE) for details.
