#==============================================================================
# Capture build settings for building a separate project (e.g. replay tool)
#==============================================================================
set(SCORPIO_BUILD_CACHE_FILE_BANNER "#CMAKE_PROJECT_NAME=SCORPIO\n")
set(SCORPIO_BUILD_CMAKE_CXX_COMPILER "set(CMAKE_CXX_COMPILER \"${CMAKE_CXX_COMPILER}\" CACHE FILEPATH \"The CXX Compiler used in SCORPIO build\" FORCE)\n")
set(SCORPIO_BUILD_CMAKE_CXX_FLAGS "set(CMAKE_CXX_FLAGS \"${CMAKE_CXX_FLAGS}\" CACHE STRING \"The CXX Compiler flags used in SCORPIO build\" FORCE)\n")
set(SCORPIO_BUILD_CMAKE_C_COMPILER "set(CMAKE_C_COMPILER \"${CMAKE_C_COMPILER}\" CACHE FILEPATH \"The C Compiler used in SCORPIO build\" FORCE)\n")
set(SCORPIO_BUILD_CMAKE_C_FLAGS "set(CMAKE_C_FLAGS \"${CMAKE_C_FLAGS}\" CACHE STRING \"The C Compiler flags used in SCORPIO build\" FORCE)\n")
set(SCORPIO_BUILD_CMAKE_EXE_LINKER_FLAGS "set(CMAKE_EXE_LINKER_FLAGS \"${CMAKE_EXE_LINKER_FLAGS}\" CACHE STRING \"The Linker flags used in SCORPIO build\" FORCE)\n")
set(SCORPIO_BUILD_CMAKE_LINKER "set(CMAKE_LINKER \"${CMAKE_LINKER}\" CACHE FILEPATH \"The Linker used in SCORPIO build\" FORCE)\n")
set(SCORPIO_BUILD_CMAKE_MAKE_PROGRAM "set(CMAKE_MAKE_PROGRAM \"${CMAKE_MAKE_PROGRAM}\" CACHE FILEPATH \"The Make program used in SCORPIO build\" FORCE)\n")

set(SCORPIO_BUILD_CACHE_FILE "${CMAKE_BINARY_DIR}/SCORPIO_CMakeCache.txt")
message(STATUS "Caching compiler settings in ${SCORPIO_BUILD_CACHE_FILE}...")
file(WRITE ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CACHE_FILE_BANNER})
file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_CXX_COMPILER})
file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_CXX_FLAGS})
file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_C_COMPILER})
file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_C_FLAGS})
file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_EXE_LINKER_FLAGS})
file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_LINKER})
file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_MAKE_PROGRAM})

#==============================================================================
#  FIND EXTERNAL LIBRARIES
#==============================================================================

# Look for GPTL
if (PIO_ENABLE_TIMING)
  find_package (GPTL COMPONENTS C QUIET)
  find_package (GPTL COMPONENTS Fortran_Perf QUIET)
endif ()

#==============================================================================
#  INCLUDE SOURCE DIRECTORIES
#==============================================================================

# Build the internal GPTL library if its not available
if (PIO_ENABLE_TIMING)
  if (NOT GPTL_C_FOUND OR NOT GPTL_Fortran_Perf_FOUND)
    message(STATUS "Could not find external GPTL library, using the internal GPTL library instead")
    add_subdirectory (gptl)
  else ()
    message(STATUS "Found external GPTL library")
  endif ()
endif ()

# Add the C library
add_subdirectory (clib)

# Add the Fortran library
if (PIO_ENABLE_FORTRAN)
  # Saving Fortran build settings in cache file
  set(SCORPIO_BUILD_CMAKE_Fortran_COMPILER "set(CMAKE_Fortran_COMPILER \"${CMAKE_Fortran_COMPILER}\" CACHE FILEPATH \"The Fortran Compiler used in SCORPIO build\" FORCE)\n")
  set(SCORPIO_BUILD_CMAKE_Fortran_FLAGS "set(CMAKE_Fortran_FLAGS \"${CMAKE_Fortran_FLAGS}\" CACHE STRING \"The Fortran Compiler flags used in SCORPIO build\" FORCE)\n")
  file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_Fortran_COMPILER})
  file(APPEND ${SCORPIO_BUILD_CACHE_FILE} ${SCORPIO_BUILD_CMAKE_Fortran_FLAGS})

  message(STATUS "Enabling the Fortran interface...")

  # Check if prereqs for new Fortran interface is available
  check_macro (HAVE_IS_CONTIGUOUS
    NAME CheckIsContiguous.f90
    HINTS ${CMAKE_MODULE_PATH}
    COMMENT "Checking whether IS_CONTIGUOUS (F2008) is available")

  if (NOT PIO_USE_FORTRAN_LEGACY_LIB)
    if (NOT HAVE_IS_CONTIGUOUS)
      message(WARNING "Some F2008 features required for new Fortran interface is missing. Forcing use of the legacy Fortran interface")
      set (PIO_USE_FORTRAN_LEGACY_LIB ON)
    endif ()
  endif ()

  if (PIO_USE_FORTRAN_LEGACY_LIB)
    message(STATUS "Using the Fortran legacy interface library...")
    add_subdirectory (flib_legacy)
  else ()
    add_subdirectory (flib)
  endif ()
endif ()
