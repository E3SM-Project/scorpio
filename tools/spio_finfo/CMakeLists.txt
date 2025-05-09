###-------------------------------------------------------------------------###
### CMakeList.txt for SCORPIO file info tool
###-------------------------------------------------------------------------###

message(STATUS "===== Configuring SCORPIO File info tool... =====")
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
  find_package (GPTL COMPONENTS C)
  if (GPTL_C_FOUND)
    message (STATUS "Found GPTL C: ${GPTL_C_LIBRARIES}")
    include_directories(${GPTL_C_INCLUDE_DIRS})
  else ()
    message (STATUS "Using internal GPTL C library for timing")
    include_directories(${PROJECT_SOURCE_DIR}/../src/gptl)
  endif ()
endif ()

#===== NetCDF-C =====
if (WITH_NETCDF)
  find_package (NetCDF ${NETCDF_C_MIN_VER_REQD} COMPONENTS C)
  if (NetCDF_FOUND)
    if (NetCDF_VERSION VERSION_LESS ${NETCDF_C_MIN_VER_REQD})
      message(STATUS "WARNING: Cannot find a compatible version of the NetCDF C library, disabling support for NetCDF")
      set(NetCDF_FOUND false)
    endif ()
  endif ()
endif ()
#===== PnetCDF-C =====
if (WITH_PNETCDF)
  find_package (PnetCDF ${PNETCDF_MIN_VER_REQD} COMPONENTS C)
  if (PnetCDF_FOUND)
    if (PnetCDF_VERSION VERSION_LESS ${PNETCDF_MIN_VER_REQD})
      message(STATUS "WARNING: Cannot find a compatible version of the PnetCDF library, disabling support for PnetCDF")
      set(PnetCDF_FOUND false)
    endif ()
  endif ()
endif ()
#===== ADIOS-C =====
if (WITH_ADIOS)
  # ADIOS 2.8.0 overwrites/resets CMAKE_MODULE_PATH, so cache and restore it
  # after finding ADIOS
  set(SPIO_CMAKE_MODULE_PATH_BACKUP ${CMAKE_MODULE_PATH})
  find_package (ADIOS ${ADIOS_MIN_VER_REQD})
  set(CMAKE_MODULE_PATH ${SPIO_CMAKE_MODULE_PATH_BACKUP})
  if (ADIOS2_FOUND)
    if (ADIOS2_VERSION VERSION_LESS ${ADIOS_MIN_VER_REQD})
      message(STATUS "WARNING: Cannot find a compatible version of the ADIOS library, disabling support for ADIOS")
      set(PIO_USE_ADIOS 0)
      set(ADIOS2_FOUND false)
    endif ()
  endif ()
endif ()

#==============================================================================
#  DEFINE THE TARGET
#==============================================================================
set(SRC ${SCORPIO_SOURCE_DIR}/tools/util/argparser.cxx
    ${SCORPIO_SOURCE_DIR}/tools/util/spio_misc_tool_utils.cxx
    ${SCORPIO_SOURCE_DIR}/tools/util/spio_lib_info.cxx
    spio_file_test_utils.cxx
    spio_finfo.cxx
    spio_finfo_tool.cxx)
add_spio_executable(spio_finfo.exe TRUE "" ${SRC})

#==============================================================================
#  SET THE COMPILER OPTIONS
#==============================================================================
# Adding Scorpio definitions - defined in the root directory
add_definitions(${PIO_DEFINITIONS})

# Include Scorpio include and lib directories
#include_directories(${PIO_INCLUDE_DIRS})
#link_directories(${PIO_LIB_DIR})

# Enable C++11 support
string (TOUPPER "${CMAKE_CXX_COMPILER_ID}" CMAKE_CXX_COMPILER_NAME)
if (CMAKE_CXX_COMPILER_NAME STREQUAL "CRAY")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -h std=c++11")
else ()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
endif ()

#include_directories(
#  "${PROJECT_SOURCE_DIR}"   # to find foo/foo.h
#  "${PROJECT_BINARY_DIR}")  # to find foo/config.h

link_directories(${PIO_LIB_DIR})
target_include_directories(spio_finfo.exe PRIVATE
  ${PIO_INCLUDE_DIRS}
  ${SCORPIO_BINARY_DIR}/src/clib
  ${SCORPIO_SOURCE_DIR}/src/clib
  ${SCORPIO_SOURCE_DIR}/tools/util
  ${NetCDF_C_INCLUDE_DIRS}
  ${PnetCDF_C_INCLUDE_DIRS} 
  ${PIO_C_EXTRA_INCLUDE_DIRS})

# Skip MPI C++ headers/bindings for MPICH lib
target_compile_definitions (spio_finfo.exe
  PUBLIC MPICH_SKIP_MPICXX)
# Skip MPI C++ headers/bindings for OpenMPI lib
target_compile_definitions (spio_finfo.exe
  PUBLIC OMPI_SKIP_MPICXX)
# Skip MPI C++ headers/bindings for SGI MPT lib
target_compile_definitions (spio_finfo.exe
  PUBLIC MPI_NO_CPPBIND)

# Add external lib compile/link flags
if (NetCDF_C_FOUND)
  target_include_directories (spio_finfo.exe
    PRIVATE ${NetCDF_C_INCLUDE_DIRS})
  target_compile_definitions (spio_finfo.exe
    PRIVATE _NETCDF)
  target_link_libraries (spio_finfo.exe
    PRIVATE ${NetCDF_C_LIBRARIES})
  if (${NetCDF_C_HAS_PARALLEL})
    target_compile_definitions (spio_finfo.exe
      PRIVATE _NETCDF4)
  endif ()
else ()
  target_compile_definitions (spio_finfo.exe
    PRIVATE _NONETCDF)
endif ()

if (PnetCDF_C_FOUND)
  target_include_directories (spio_finfo.exe
    PRIVATE ${PnetCDF_C_INCLUDE_DIRS})
  target_compile_definitions (spio_finfo.exe
    PRIVATE _PNETCDF)
  target_link_libraries (spio_finfo.exe
    PRIVATE ${PnetCDF_C_LIBRARIES})
else ()
  target_compile_definitions (spio_finfo.exe
    PRIVATE _NOPNETCDF)
endif ()

if (ADIOS_FOUND)
  target_include_directories (spio_finfo.exe
    PRIVATE ${ADIOS_INCLUDE_DIRS})
  target_compile_definitions (spio_finfo.exe
    PRIVATE _ADIOS)
  target_link_libraries (spio_finfo.exe
    PRIVATE ${ADIOS_LIBRARIES})
else ()
  target_compile_definitions (spio_finfo.exe
    PRIVATE _NOADIOS)
endif ()

# Adding extra (user-specified) compile/link options
target_include_directories (spio_finfo.exe
  PRIVATE ${PIO_C_EXTRA_INCLUDE_DIRS})
target_link_libraries (spio_finfo.exe
  PRIVATE ${PIO_C_EXTRA_LIBRARIES})
target_compile_options (spio_finfo.exe
  PRIVATE ${PIO_C_EXTRA_COMPILE_OPTIONS})
target_compile_definitions (spio_finfo.exe
  PRIVATE ${PIO_C_EXTRA_COMPILE_DEFINITIONS})
if (PIO_C_EXTRA_LINK_FLAGS)
  set_target_properties(spio_finfo.exe PROPERTIES
    LINK_FLAGS ${PIO_C_EXTRA_LINK_FLAGS})
endif ()
#==============================================================================
#  INSTALL
#==============================================================================
# Binary utilities
install (TARGETS spio_finfo.exe DESTINATION bin)

