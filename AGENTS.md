# AGENTS.md

SCORPIO (Software for Caching Output and Reads for Parallel I/O) is a high-level Parallel I/O Library for structured grid applications.

## Project Overview

SCORPIO (Software for Caching Output and Reads for Parallel I/O) is a high-level Parallel I/O Library. The library is used by the E3SM (Energy Exascale Earth System Model) earth system model for all I/O.

- **Website:** https://github.com/E3SM-Project/scorpio
- **Documentation:** https://docs.e3sm.org/scorpio/
- **DOI:** https://www.osti.gov/doecode/biblio/36752

## Architecture

- The Fortran interface library source is in src/flib directory
- The C (main/core library) library source is in src/clib directory
- The tests are in tests/general and tests/cunit directories
- The examples are in the examples directory

## Build Commands

SCORPIO uses CMake for configuring the library. The library requires a C, C++ and Fortran compiler and an MPI library. SCORPIO uses low-level I/O libraries like PnetCDF (https://parallel-netcdf.github.io), NetCDF (https://www.unidata.ucar.edu/software/netcdf), HDF5 (https://www.hdfgroup.org/solutions/hdf5/) and ADIOS (https://adios2.readthedocs.io/en/latest/) for I/O.

To configure SCORPIO (source in /path/to/scorpio/source directory) using CMake with the PnetCDF library (installed at /path/to/pnetcdf) use the command below,

```
CC=mpicc CXX=mpicxx FC=mpif90 cmake -DPnetCDF_PATH=/path/to/pnetcdf /path/to/scorpio/source
```

After configuring the library to build it use make,

```
make
```

## Development Workflow
- All changes are made on a development branch off master
- The development branches are named using the pattern : ```<github username>/<feature description>```
- The changes in a branch are merged to the develop branch for nightly testing (Run using Jenkins and results published on CDASH at http://my.cdash.org/index.php?project=E3SM\_SCORPIO)
- Once the nightly testing is successful the PR is manually merged to the master branch
- Code formatting for C++ sources is mentioned in the library documentation (https://docs.e3sm.org/scorpio/html/contributing_code.html)

## Testing instructions
- Find the CI (Github Actions workflow) plan in the .github/workflows directory
- Add/update tests for the code that you change

## PR instructions
- Run the CI workflow (SCORPIO + PnetCDF - build and test) in the .github/workflows directory before committing any changes
- Do not automatically merge PRs to master or develop branch
