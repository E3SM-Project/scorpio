#==============================================================================
#
#  This file sets the environment variables needed to configure and build
#  on Argonne General Compute Environment (GCE)
#
#==============================================================================

# Assume all package locations (NetCDF, PnetCDF, HDF5, etc) are already
# set with existing environment variables: NETCDF, PNETCDF, HDF5, etc.

# Define the extra CMake configure options
set (CTEST_CONFIGURE_OPTIONS "-Wno-dev -DCMAKE_VERBOSE_MAKEFILE=TRUE")
if (NOT DEFINED ENV{DISABLE_NETCDF})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DNetCDF_PATH=$ENV{NETCDFROOT}")
    if (DEFINED ENV{HDF5ROOT})
        set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DHDF5_PATH=$ENV{HDF5ROOT}")
    endif ()
    if (DEFINED ENV{ZLIBROOT})
        set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DLIBZ_PATH=$ENV{ZLIBROOT}")
    endif ()
    if (DEFINED ENV{SZIPROOT})
        set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DSZIP_PATH=$ENV{SZIPROOT}")
    endif ()
endif ()
if (NOT DEFINED ENV{DISABLE_PNETCDF})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPnetCDF_PATH=$ENV{PNETCDFROOT}")
endif ()

# If ENABLE_COVERAGE environment variable is set, then enable code coverage
if (DEFINED ENV{ENABLE_COVERAGE})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_ENABLE_COVERAGE=ON")
endif ()

# If VALGRIND_CHECK environment variable is set, then enable memory leak check using Valgrind
if (DEFINED ENV{VALGRIND_CHECK})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_VALGRIND_CHECK=ON")
endif ()

# If USE_MALLOC environment variable is set, then use native malloc (instead of bget package)
if (DEFINED ENV{USE_MALLOC})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_USE_MALLOC=ON")
endif ()

# If ENABLE_LARGE_TESTS environment variable is set, then enable large (file, processes) tests
if (DEFINED ENV{ENABLE_LARGE_TESTS})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_ENABLE_LARGE_TESTS=ON")
endif ()

# If DISABLE_PNETCDF environment variable is set, then configure PIO without PnetCDF
if (DEFINED ENV{DISABLE_PNETCDF})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DWITH_PNETCDF=OFF")
endif ()

# If DISABLE_NETCDF environment variable is set, then configure PIO without NetCDF
if (DEFINED ENV{DISABLE_NETCDF})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DWITH_NETCDF=OFF")
endif ()

# If ENABLE_ADIOS environment variable is set, then enable the use of ADIOS type
if (DEFINED ENV{ENABLE_ADIOS})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DWITH_ADIOS2=ON")
endif ()

# If ENABLE_INTERNAL_TIMING environment variable is set, then gather and print GPTL timing stats
if (DEFINED ENV{ENABLE_INTERNAL_TIMING})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_ENABLE_INTERNAL_TIMING=ON")
endif ()

# If ENABLE_LOGGING environment variable is set, then enable debug logging (large output possible)
if (DEFINED ENV{ENABLE_LOGGING})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_ENABLE_LOGGING=ON")
endif ()

# If USE_MPISERIAL environment variable is set, then enable mpi-serial support (instead of MPI)
if (DEFINED ENV{USE_MPISERIAL})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_USE_MPISERIAL=ON")
endif ()

# If MICRO_TIMING environment variable is set, then enable internal micro timers
if (DEFINED ENV{MICRO_TIMING})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_MICRO_TIMING=ON")
endif ()

# If ENABLE_ADIOS_BP2NC_TEST environment variable is set, then enable testing of BP to NetCDF conversion
if (DEFINED ENV{ENABLE_ADIOS_BP2NC_TEST})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DADIOS_BP2NC_TEST=ON")
endif ()

# If ENABLE_ADIOS_NO_DECOMPS environment variable is set, then save no decomposition data to ADIOS BP files
if (DEFINED ENV{ENABLE_ADIOS_NO_DECOMPS})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DADIOS_NO_DECOMPS=ON")
endif ()

# If ENABLE_ADIOS_USE_COMPRESSION environment variable is set, then enable data compression methods in ADIOS (e.g., Blosc2, BZip2)
if (DEFINED ENV{ENABLE_ADIOS_USE_COMPRESSION})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DADIOS_USE_COMPRESSION=ON")
endif ()

# If ENABLE_ADIOS_USE_LOSSY_COMPRESSION environment variable is set, then enable lossy compression methods in ADIOS (e.g., SZ, MGARD, ZFP)
if (DEFINED ENV{ENABLE_ADIOS_USE_LOSSY_COMPRESSION})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DADIOS_USE_LOSSY_COMPRESSION=ON")
endif ()

# If ENABLE_TESTS environment variable is set, then enable the testing builds
if (DEFINED ENV{ENABLE_TESTS})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_ENABLE_TESTS=ON")
endif ()

# If ENABLE_EXAMPLES environment variable is set, then enable SCORPIO examples
if (DEFINED ENV{ENABLE_EXAMPLES})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_ENABLE_EXAMPLES=ON")
endif ()

# If USE_INDEP_MODE environment variable is set, then use PnetCDF independent data mode
if (DEFINED ENV{USE_INDEP_MODE})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_USE_INDEP_MODE=ON")
endif ()

# If DISABLE_TIMING environment variable is set, then disable the use of the GPTL timing library
if (DEFINED ENV{DISABLE_TIMING})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_ENABLE_TIMING=OFF")
endif ()

# If ENABLE_HDF5 environment variable is set, then enable the use of HDF5 type
if (DEFINED ENV{ENABLE_HDF5})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DWITH_HDF5=ON")
    if (DEFINED ENV{DISABLE_NETCDF})
        if (DEFINED ENV{HDF5ROOT})
            set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DHDF5_PATH=$ENV{HDF5ROOT}")
        endif ()
        if (DEFINED ENV{ZLIBROOT})
            set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DLIBZ_PATH=$ENV{ZLIBROOT}")
        endif ()
        if (DEFINED ENV{SZIPROOT})
            set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DSZIP_PATH=$ENV{SZIPROOT}")
        endif ()
    endif ()
endif ()

# If USE_FORTRAN_LEGACY_LIB environment variable is set, then use the legacy Fortran library
if (DEFINED ENV{USE_FORTRAN_LEGACY_LIB})
    set (CTEST_CONFIGURE_OPTIONS "${CTEST_CONFIGURE_OPTIONS} -DPIO_USE_FORTRAN_LEGACY_LIB=ON")
endif ()
