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
