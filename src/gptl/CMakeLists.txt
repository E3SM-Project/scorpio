cmake_minimum_required (VERSION 3.7)
project (GPTL C Fortran)
include (CheckFunctionExists)
include(CheckSymbolExists)
include (FortranCInterface)
FortranCInterface_HEADER(cmake_fortran_c_interface.h
  MACRO_NAMESPACE "FCI_")
include (LibCheck)

message(STATUS "===== Configuring GPTL library... =====")
#==============================================================================
#  DEFINE THE TARGET
#==============================================================================

set (GPTL_C_SRCS GPTLget_memusage.c
  GPTLprint_memusage.c
  GPTLutil.c
  f_wrappers.c
  gptl.c
  gptl_papi.c)

set (GPTL_Fortran_SRCS perf_mod.F90
  perf_utils.F90)

set (GPTL_Fortran_MODS ${CMAKE_CURRENT_BINARY_DIR}/perf_mod.mod
  ${CMAKE_CURRENT_BINARY_DIR}/perf_utils.mod)

add_library (gptl ${GPTL_Fortran_SRCS} ${GPTL_C_SRCS})

target_include_directories (gptl
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

target_compile_definitions (gptl
  PRIVATE INCLUDE_CMAKE_FCI)

if (PIO_ENABLE_TIMING)
  if (PIO_ENABLE_INTERNAL_TIMING)
    target_compile_definitions (gptl
      PRIVATE GPTL_ASSERT_ON_RECURSION)
  endif ()
endif ()

if (CMAKE_SYSTEM_NAME MATCHES "AIX")
  target_compile_definitions (gptl
    PRIVATE _AIX)
else ()
  target_compile_definitions (gptl
    PRIVATE ${CMAKE_SYSTEM_DIRECTIVE})
endif ()

target_compile_definitions (gptl
  PUBLIC ${CMAKE_Fortran_COMPILER_DIRECTIVE})

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

if (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -mismatch_all" )
  #    target_compile_options (gptl
  #        PRIVATE -mismatch_all)
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  if (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL "10.0")
    set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch" )
  endif ()
endif ()
if (CMAKE_Fortran_COMPILER_ID STREQUAL "XL")
  set ( CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qextname=flush" )
endif ()

#==============================================================================
#  DEFINE THE INSTALL
#==============================================================================

# Library
install (TARGETS gptl EXPORT spio-targets-gptl DESTINATION lib)

# Header/Include File
install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/gptl.h DESTINATION include)

# Fortran Modules
install (FILES ${GPTL_Fortran_MODS} DESTINATION include)

# Install the cmake config file with targets to the cmake directory
install (EXPORT spio-targets-gptl
          FILE spio-targets-gptl.cmake
          DESTINATION lib/scorpio/cmake
          NAMESPACE scorpio::)
#==============================================================================
#  DEFINE THE DEPENDENCIES
#==============================================================================

#===== PAPI =====
if (ENABLE_PAPI)
  find_package (PAPI)
  if (PAPI_FOUND)
    target_compile_definitions (gptl
      PUBLIC HAVE_PAPI)
    target_include_directories (gptl
      PUBLIC ${PAPI_INCLUDE_DIRECTORIES})
    target_link_libraries (gptl
      PUBLIC ${PAPI_LIBRARIES})
  endif ()
endif ()

#===== LIBRT =====
if (ENABLE_LIBRT)
  find_package (LIBRT)
  if (LIBRT_FOUND)
    target_compile_definitions (gptl
      PUBLIC HAVE_LIBRT)
    target_include_directories (gptl
      PUBLIC ${LIBRT_INCLUDE_DIRECTORIES})
    target_link_libraries (gptl
      PUBLIC ${LIBRT_LIBRARIES})
  endif ()
endif ()

#===== MPI =====
if (PIO_USE_MPISERIAL)
  find_package (MPISERIAL COMPONENTS C Fortran REQUIRED)
  if (MPISERIAL_C_FOUND AND MPISERIAL_Fortran_FOUND)
    target_compile_definitions (gptl
      PRIVATE HAVE_MPI)
    target_include_directories (gptl
      PUBLIC ${MPISERIAL_C_INCLUDE_DIRS}
      PUBLIC ${MPISERIAL_Fortran_INCLUDE_DIRS})
    target_link_libraries (gptl
      PUBLIC ${MPISERIAL_C_LIBRARIES}
      PUBLIC ${MPISERIAL_Fortran_LIBRARIES})

    set (MPI_C_LIBRARIES ${MPISERIAL_C_LIBRARIES})
    set (MPI_Fortran_INCLUDE_PATH ${MPISERIAL_Fortran_INCLUDE_DIRS})
  endif ()
else ()
  find_package (MPI REQUIRED COMPONENTS C Fortran)
  if (MPI_C_FOUND AND MPI_Fortran_FOUND)
    target_compile_definitions (gptl
      PUBLIC HAVE_MPI)
  endif ()
endif ()

# Check MPI library for Comm_f2c function
if (PIO_USE_MPISERIAL)
  if (MPISERIAL_C_FOUND)
    set (CMAKE_REQUIRED_INCLUDES ${MPISERIAL_C_INCLUDE_DIRS})
    set (CMAKE_REQUIRED_LIBRARIES ${MPISERIAL_C_LIBRARIES})
  endif ()
else ()
  set (CMAKE_REQUIRED_INCLUDES ${MPI_INCLUDE_PATH})
  set (CMAKE_REQUIRED_LIBRARIES ${MPI_C_LIBRARIES})
endif ()
check_symbol_exists (MPI_Comm_f2c "mpi.h" MPI_HAS_COMM_F2C)
if (NOT MPI_HAS_COMM_F2C)
  message(STATUS "Could not find symbol 'MPI_Comm_f2c'")
  target_compile_definitions(gptl
    PRIVATE NO_COMM_F2C)
else ()
  message(STATUS "Found symbol 'MPI_Comm_f2c' in mpi.h")
endif ()

# Check for MPI Fortran module
find_path(MPIMOD_PATH
  NAMES mpi.mod MPI.mod
  PATHS ${MPI_Fortran_INCLUDE_PATH})
check_macro (MPI_HAS_Fortran_MOD
  NAME TryMPIMod.f90
  HINTS ${CMAKE_MODULE_PATH}
  DEFINITIONS -I${MPIMOD_PATH}
  COMMENT "whether MPI Fortran module is supported")
if (${MPI_HAS_Fortran_MOD})
  message (STATUS "MPI Fortran module verified and enabled.")
else ()
  message (STATUS "MPI Fortran module failed verification and therefore disabled.")
  target_compile_definitions (gptl
    PUBLIC NO_MPIMOD)
endif ()

#===== GetTimeOfDay =====
if (NOT DEFINED SYSTEM_HAS_GETTIMEOFDAY)
  get_target_property (GPTL_LINK_LIBRARIES gptl LINK_LIBRARIES)
  set (CMAKE_REQUIRED_LIBRARIES ${GPTL_LINK_LIBRARIES})
  check_function_exists (gettimeofday GETTIMEOFDAY)
  if (NOT GETTIMEOFDAY)
    target_compile_definitions(gptl
      PUBLIC NO_GETTIMEOFDAY)
  endif ()
  set (SYSTEM_HAS_GETTIMEOFDAY ${GETTIMEOFDAY}
    CACHE INTERNAL "Whether the gettimeofday function could be found")
endif ()
