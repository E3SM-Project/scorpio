message(STATUS "===== Configuring SCORPIO Performance tests/tools ... =====")
#==============================================================================
#  SET THE LINKER LANGUAGE
#==============================================================================
string (TOUPPER "${CMAKE_Fortran_COMPILER_ID}" CMAKE_FORTRAN_COMPILER_NAME)
# The PIO library is written using C, C++ and Fortran languages
# IBM compilers require Fortran/C/C++ mixed language programs
# to be linked with the C++ linker. Other compilers require the
# Fortran linker.
if (CMAKE_FORTRAN_COMPILER_NAME STREQUAL "XL")
  SET(PIO_LINKER_LANGUAGE CXX)
else ()
  SET(PIO_LINKER_LANGUAGE Fortran)
endif ()

#==============================================================================
#  FIND EXTERNAL LIBRARIES/DEPENDENCIES
#==============================================================================
#===== MPI =====
if (PIO_USE_MPISERIAL)
  find_package (MPISERIAL COMPONENTS Fortran REQUIRED)
else ()
  find_package (MPI REQUIRED)
endif ()

# Check for MPI Fortran module
find_path(MPIMOD_PATH
  NAMES mpi.mod MPI.mod
  HINTS ${MPI_Fortran_INCLUDE_PATH})
if (PIO_ENABLE_TIMING)
  find_package (GPTL COMPONENTS Fortran_Perf QUIET)
endif ()

check_macro (MPI_HAS_Fortran_MOD
  NAME TryMPIMod.f90
  HINTS ${CMAKE_MODULE_PATH}
  DEFINITIONS -I${MPIMOD_PATH}
  COMMENT "whether MPI Fortran module is supported")
if (${MPI_HAS_Fortran_MOD})
  message (STATUS "MPI Fortran module verified and enabled.")
else ()
  message (STATUS "MPI Fortran module failed verification and therefore disabled.")
endif ()

#===== GPTL =====
if (PIO_ENABLE_TIMING)
  find_package (GPTL COMPONENTS Fortran_Perf QUIET)
  if (GPTL_Fortran_Perf_FOUND)
    message (STATUS "Found GPTL Fortran Perf: ${GPTL_Fortran_Perf_LIBRARIES}")
    include_directories(${GPTL_Fortran_Perf_INCLUDE_DIRS})
  else ()
    message (STATUS "Using internal GPTL Fortran library for timing")
    include_directories(${PROJECT_BINARY_DIR}/../src/gptl)
  endif ()
endif ()

#==============================================================================
#  DEFINE THE TARGETS
#==============================================================================
if (PIO_ENABLE_TIMING)
  # This tool requires GPTL
  add_executable (pioperf EXCLUDE_FROM_ALL
    pioperformance.F90)
  set_property(TARGET pioperf PROPERTY LINKER_LANGUAGE ${PIO_LINKER_LANGUAGE})
  target_link_libraries (pioperf piof)
  add_dependencies (tests pioperf)
endif ()

include_directories("${PROJECT_BINARY_DIR}/../src/flib" "${PROJECT_BINARY_DIR}/../src/flib_legacy")
add_executable (pioperf_rearr EXCLUDE_FROM_ALL
  pioperformance_rearr.F90)
set_property(TARGET pioperf_rearr PROPERTY LINKER_LANGUAGE ${PIO_LINKER_LANGUAGE})
target_link_libraries (pioperf_rearr piof)
add_dependencies (tests pioperf_rearr)

#==============================================================================
#  SET THE COMPILER OPTIONS
#==============================================================================
if (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mismatch_all" )
  #    target_compile_options (gptl
  #        PRIVATE -mismatch_all)
endif ()

# Add compiler-specifc options for the tools
if ("${CMAKE_Fortran_COMPILER_ID}" STREQUAL "GNU")
  if (PIO_ENABLE_TIMING)
    # This tool requires GPTL
    target_compile_options (pioperf
      PRIVATE -ffree-line-length-none)
  endif ()
  target_compile_options (pioperf_rearr
    PRIVATE -ffree-line-length-none)
  if (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL "10.0")
    set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch" )
  endif ()
endif()
