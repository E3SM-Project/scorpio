include (LibMPI)

message(STATUS "===== Configuring SCORPIO PnetCDF tests... =====")
#==============================================================================
#  HELPER MACROS
#==============================================================================
include(SPIOUtils)

#==============================================================================
#  SET THE COMPILER OPTIONS
#==============================================================================
include_directories("${SCORPIO_SOURCE_DIR}/tests/cunit" "${SCORPIO_SOURCE_DIR}/src/clib" "${SCORPIO_BINARY_DIR}/src/clib")

# Compiler-specific compiler options
string (TOUPPER "${CMAKE_C_COMPILER_ID}" CMAKE_C_COMPILER_NAME)
if (CMAKE_C_COMPILER_NAME STREQUAL "CRAY")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -h std=c99")
elseif (CMAKE_C_COMPILER_NAME STREQUAL "PGI")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -c99")
elseif (CMAKE_C_COMPILER_NAME STREQUAL "NVHPC")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -c99")
else ()
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
endif ()

string (TOUPPER "${CMAKE_CXX_COMPILER_ID}" CMAKE_CXX_COMPILER_NAME)
if (CMAKE_CXX_COMPILER_NAME STREQUAL "CRAY")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -h std=c++11")
else ()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
endif ()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0")

#==============================================================================
#  FIND EXTERNAL LIBRARIES/DEPENDENCIES
#==============================================================================
#===== MPI =====
if (PIO_USE_MPISERIAL)
  find_package (MPISERIAL COMPONENTS C REQUIRED)
  if (MPISERIAL_C_FOUND)
    set (CMAKE_REQUIRED_INCLUDES ${MPISERIAL_C_INCLUDE_DIRS})
  endif ()
else ()
  find_package (MPI REQUIRED)
  set (CMAKE_REQUIRED_INCLUDES ${MPI_INCLUDE_PATH})
endif ()

#===== GPTL =====
if (PIO_ENABLE_TIMING)
  find_package (GPTL COMPONENTS C QUIET)
  if (GPTL_C_FOUND)
    message (STATUS "Found GPTL C: ${GPTL_C_LIBRARIES}")
  else ()
    include_directories(${PROJECT_SOURCE_DIR}/../src/gptl)
    message (STATUS "Using internal GPTL C library for timing")
  endif ()
endif ()

#===== PnetCDF-C =====
if (WITH_PNETCDF)
  find_package (PnetCDF ${PNETCDF_MIN_VER_REQD} COMPONENTS C)
  if (PnetCDF_FOUND)
    message(STATUS "PnetCDF C library dependencies: ${PnetCDF_C_LIBRARY}")
    include_directories (${PnetCDF_C_INCLUDE_DIRS})
    link_libraries (${PnetCDF_C_LIBRARIES})
  else ()
    message(STATUS "Could not find PnetCDF library, disable PnetCDF tests")
  endif ()
else ()
  message(STATUS "Disabling support for PnetCDF tests")
endif ()

#==============================================================================
#  DEFINE THE TARGETS AND TESTS
#==============================================================================

# Exclude tests that require more than 1 procs when using the MPI serial library
if (WITH_PNETCDF)
  add_spio_executable (test_pnetcdf TRUE "" test_pnetcdf.c)
  add_spio_executable (test_pnetcdf_4d TRUE "" test_pnetcdf_4d.cpp)
endif ()
add_dependencies (tests test_pnetcdf test_pnetcdf_4d)

set (SPIO_TEST_MAX_NPROCS 4)

# Test Timeout in seconds.
if (PIO_VALGRIND_CHECK)
  set (DEFAULT_TEST_TIMEOUT 480)
else ()
  set (DEFAULT_TEST_TIMEOUT 240)
endif ()

if (WITH_PNETCDF)
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_pnetcdf${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_pnetcdf
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
    add_mpi_test(test_pnetcdf_4d${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_pnetcdf_4d
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
endif ()
