###-------------------------------------------------------------------------###
### CMakeList.txt for adios2pio 
###-------------------------------------------------------------------------###

message(STATUS "===== Configuring ADIOS to NetCDF Conversion tool... =====")

# Add CMake modules in Scorpio
list (APPEND CMAKE_MODULE_PATH ${SCORPIO_SOURCE_DIR}/cmake)

#==============================================================================
#  HELPER MACROS
#==============================================================================
include(SPIOUtils)

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
    message (STATUS "Using internal GPTL C library for timing")
  endif ()
endif ()

#===== PnetCDF-C =====
if (WITH_PNETCDF)
  find_package (PnetCDF ${PNETCDF_MIN_VER_REQD} COMPONENTS C)
  if (PnetCDF_FOUND)
    if (PnetCDF_VERSION VERSION_LESS ${PNETCDF_MIN_VER_REQD})
      message(STATUS "WARNING: Cannot find a compatible version of the PnetCDF library, disabling support for PnetCDF")
      set(PnetCDF_FOUND false)
      set(PnetCDF_C_FOUND false)
    endif ()
  endif ()
endif ()

#===== NetCDF-C =====
if (WITH_NETCDF)
  find_package (NetCDF ${NETCDF_C_MIN_VER_REQD} COMPONENTS C)
  if (NetCDF_FOUND)
    if (NetCDF_VERSION VERSION_LESS ${NETCDF_C_MIN_VER_REQD})
      message(STATUS "WARNING: Cannot find a compatible version of the NetCDF C library, disabling support for NetCDF")
      set(NetCDF_FOUND false)
      set(NetCDF_C_FOUND false)
    endif ()
  endif ()
endif ()

#===== HDF5-C =====
if (WITH_HDF5)
  find_package (HDF5 COMPONENTS C)
endif ()

#===== ADIOS-C =====
if ((WITH_ADIOS2) AND (NOT ADIOS2_FOUND))
  # ADIOS 2.8.0 overwrites/resets CMAKE_MODULE_PATH, so cache and restore it after
  # finding ADIOS
  set(SPIO_CMAKE_MODULE_PATH_BACKUP ${CMAKE_MODULE_PATH})
  find_package (ADIOS2 ${ADIOS_MIN_VER_REQD})
  set(CMAKE_MODULE_PATH ${SPIO_CMAKE_MODULE_PATH_BACKUP})
  if (ADIOS2_FOUND)
    if (ADIOS2_VERSION VERSION_LESS ${ADIOS_MIN_VER_REQD})
      message(STATUS "WARNING: Cannot find a compatible version of the ADIOS library, disabling support for ADIOS")
      set(ADIOS2_FOUND false)
    endif ()
  endif ()
endif ()

# We assume that either PnetCDF or NetCDF is available
if (NOT PnetCDF_FOUND AND NOT NetCDF_FOUND)
  message (FATAL_ERROR "Must have PnetCDF and/or NetCDF C libraries")
endif ()

#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
SET(SRC ${SCORPIO_SOURCE_DIR}/tools/util/argparser.cxx adios2pio-nm.cxx)
add_library(adios2pio-nm-lib ${SCORPIO_SOURCE_DIR}/tools/util/spio_misc_tool_utils.cxx adios2pio-nm-lib.cxx)
include_directories(
  "${PROJECT_SOURCE_DIR}"   # to find foo/foo.h
  "${PROJECT_BINARY_DIR}")  # to find foo/config.h
add_spio_executable(adios2pio-nm.exe TRUE "" ${SRC})

#==============================================================================
#  SET THE COMPILER OPTIONS
#==============================================================================

# Adding PIO definitions - defined in the root directory
ADD_DEFINITIONS(${PIO_DEFINITIONS})

# Include PIO include and lib directories
INCLUDE_DIRECTORIES(${PIO_INCLUDE_DIRS})
LINK_DIRECTORIES(${PIO_LIB_DIR})

# Compiler-specific compiler options
string (TOUPPER "${CMAKE_CXX_COMPILER_ID}" CMAKE_CXX_COMPILER_NAME)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0")
if (CMAKE_CXX_COMPILER_NAME STREQUAL "CRAY")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -h std=c++11")
else ()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
endif ()

target_include_directories(adios2pio-nm-lib PRIVATE
  ${SCORPIO_SOURCE_DIR}/src/clib
  ${SCORPIO_BINARY_DIR}/src/clib
  ${SCORPIO_SOURCE_DIR}/tools/util
  ${PnetCDF_C_INCLUDE_DIRS}
  ${NETCDF_C_INCLUDE_DIRS}
  ${HDF5_C_INCLUDE_DIRS}
  ${PIO_C_EXTRA_INCLUDE_DIRS})

target_include_directories(adios2pio-nm.exe PRIVATE
  ${SCORPIO_SOURCE_DIR}/src/clib
  ${SCORPIO_BINARY_DIR}/src/clib
  ${SCORPIO_SOURCE_DIR}/tools/util
  ${PnetCDF_C_INCLUDE_DIRS}
  ${NETCDF_C_INCLUDE_DIRS}
  ${HDF5_C_INCLUDE_DIRS}
  ${PIO_C_EXTRA_INCLUDE_DIRS})

# Adding path to internal GPTL lib, if needed
if (PIO_ENABLE_TIMING)
  if (NOT GPTL_C_FOUND)
    target_include_directories(adios2pio-nm-lib
      PRIVATE ${PROJECT_SOURCE_DIR}/../src/gptl)
    target_include_directories(adios2pio-nm.exe
      PRIVATE ${PROJECT_SOURCE_DIR}/../src/gptl)
  endif ()
endif ()

# System and compiler CPP directives
target_compile_definitions (adios2pio-nm-lib
  PRIVATE ${CMAKE_SYSTEM_DIRECTIVE})
target_compile_definitions (adios2pio-nm-lib
  PUBLIC ${CMAKE_C_COMPILER_DIRECTIVE})
# Skip MPI C++ headers/bindings for MPICH lib
target_compile_definitions (adios2pio-nm-lib
  PUBLIC MPICH_SKIP_MPICXX)
# Skip MPI C++ headers/bindings for OpenMPI lib
target_compile_definitions (adios2pio-nm-lib
  PUBLIC OMPI_SKIP_MPICXX)
# Skip MPI C++ headers/bindings for SGI MPT lib
target_compile_definitions (adios2pio-nm-lib
  PUBLIC MPI_NO_CPPBIND)

# Set external lib compiler/link flags
if (PnetCDF_C_FOUND)
  target_include_directories (adios2pio-nm-lib
    PUBLIC ${PnetCDF_C_INCLUDE_DIRS})
  target_compile_definitions (adios2pio-nm-lib
    PUBLIC _PNETCDF)
  target_link_libraries (adios2pio-nm-lib
    PUBLIC ${PnetCDF_C_LIBRARIES})
else ()
  target_compile_definitions (adios2pio-nm-lib
    PUBLIC _NOPNETCDF)
endif ()

if (NetCDF_C_FOUND)
  target_include_directories (adios2pio-nm-lib
    PUBLIC ${NetCDF_C_INCLUDE_DIRS})
  target_compile_definitions (adios2pio-nm-lib
    PUBLIC _NETCDF)
  target_link_libraries (adios2pio-nm-lib
    PUBLIC ${NetCDF_C_LIBRARIES})
  if (${NetCDF_C_HAS_PARALLEL})
    target_compile_definitions (adios2pio-nm-lib
      PUBLIC _NETCDF4)
  endif ()
else ()
  target_compile_definitions (adios2pio-nm-lib
    PUBLIC _NONETCDF)
endif ()

if (ADIOS2_FOUND)
  target_compile_definitions (adios2pio-nm-lib
    PRIVATE _ADIOS2)
  target_link_libraries (adios2pio-nm-lib
    PUBLIC adios2::adios2)
else ()
  target_compile_definitions (adios2pio-nm-lib
    PUBLIC _NOADIOS)
endif ()

# Add the extra (user-specified) compile/link options
target_include_directories (adios2pio-nm-lib
  PUBLIC ${PIO_C_EXTRA_INCLUDE_DIRS})
target_link_libraries (adios2pio-nm-lib
  PUBLIC ${PIO_C_EXTRA_LIBRARIES})
target_compile_options (adios2pio-nm-lib
  PRIVATE ${PIO_C_EXTRA_COMPILE_OPTIONS})
target_compile_definitions (adios2pio-nm-lib
  PUBLIC ${PIO_C_EXTRA_COMPILE_DEFINITIONS})
if (PIO_C_EXTRA_LINK_FLAGS)
  set_target_properties(adios2pio-nm-lib PROPERTIES
    LINK_FLAGS ${PIO_C_EXTRA_LINK_FLAGS})
endif ()

TARGET_LINK_LIBRARIES(adios2pio-nm-lib PRIVATE pioc)
TARGET_LINK_LIBRARIES(adios2pio-nm.exe PRIVATE adios2pio-nm-lib)

#==============================================================================
#  INSTALL
#==============================================================================
# Install libadios2pio-nm-lib.a
install (TARGETS adios2pio-nm-lib
          EXPORT spio-targets-adios
          DESTINATION lib)

# Install the header File
install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/adios2pio-nm-lib.h DESTINATION include)

# Install the ADIOS to NetCDF conversion tool
install (TARGETS adios2pio-nm.exe DESTINATION bin)

# Install the cmake config file with targets to the cmake directory
install (EXPORT spio-targets-adios
          FILE spio-targets-adios.cmake
          DESTINATION lib/scorpio/cmake
          NAMESPACE scorpio::)
