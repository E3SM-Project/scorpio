cmake_minimum_required (VERSION 2.8.12)
include (CheckFunctionExists)
include(CheckTypeSize)
include(SPIOTypeUtils)
project (PIOC C CXX)

message(STATUS "===== Configuring SCORPIO C library... =====")
#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================

# Add sources for libpioc.a
add_library (pioc topology.c pio_mpi_timer.c pio_timer.c pio_file.c
  pioc_support.c pio_lists.c pio_print.c
  pioc.c pioc_sc.c pio_spmd.c pio_rearrange.c pio_nc4.c bget.c
  pio_nc.c pio_put_nc.c pio_get_nc.c pio_getput_int.c pio_msg.c pio_varm.c
  pio_darray.c pio_darray_int.c spio_hash.cpp pio_sdecomps_regex.cpp spio_io_summary.cpp
  spio_ltimer.cpp spio_serializer.cpp spio_file_mvcache.cpp)

#==============================================================================
#  FIND EXTERNAL LIBRARIES/DEPENDENCIES
#==============================================================================

#===== MPI =====
if (PIO_USE_MPISERIAL)
  find_package (MPISERIAL COMPONENTS C REQUIRED)
  if (MPISERIAL_C_FOUND)
    message(STATUS "Found MPI serial library: ${MPISERIAL_C_INCLUDE_DIRS}")
  else ()
    message(WARNING "Unable to find the MPI serial library (SCORPIO was configured to use the MPI serial library)")
  endif ()
else ()
  find_package (MPI REQUIRED COMPONENTS C)
endif ()

#===== GPTL =====
if (PIO_ENABLE_TIMING)
  find_package (GPTL COMPONENTS C)
  if (GPTL_C_FOUND)
    message (STATUS "GPTL C library dependencies: ${GPTL_C_LIBRARIES}")
    target_include_directories (pioc
      PUBLIC ${GPTL_C_INCLUDE_DIRS})
    target_link_libraries (pioc
      PUBLIC ${GPTL_C_LIBRARIES})
  else ()
    message (STATUS "Using internal GPTL C library for timing")
    target_include_directories (pioc
      PRIVATE ${PROJECT_SOURCE_DIR}/../gptl)
    target_link_libraries (pioc
      PUBLIC gptl)
  endif ()
  target_compile_definitions (pioc
    PUBLIC SPIO_ENABLE_GPTL_TIMING
    PRIVATE TIMING)
  if (PIO_ENABLE_INTERNAL_TIMING)
    target_compile_definitions (pioc
      PUBLIC SPIO_ENABLE_GPTL_TIMING_INTERNAL
      PRIVATE TIMING_INTERNAL)
  endif ()
endif ()

#===== NetCDF-C =====
if (WITH_NETCDF)
  find_package (NetCDF ${NETCDF_C_MIN_VER_REQD} COMPONENTS C)
  if (NetCDF_FOUND)
    message(STATUS "NetCDF C library dependencies: ${NetCDF_C_LIBRARIES}")
    set(PIO_USE_NETCDF 1)
    target_include_directories (pioc
      PUBLIC ${NetCDF_C_INCLUDE_DIRS})
    target_link_libraries (pioc
      PUBLIC ${NetCDF_C_LIBRARIES})
    if (${NetCDF_C_HAS_PARALLEL})
      set(PIO_USE_NETCDF4 1)
    else ()
      set(PIO_USE_NETCDF4 0)
    endif ()
    if (${NetCDF_C_LOGGING_ENABLED})
      target_compile_definitions (pioc
        PRIVATE NETCDF_C_LOGGING_ENABLED)
      # netcdf.h needs this to be defined to use netCDF logging.
      target_compile_definitions (pioc
        PRIVATE LOGGING)
    endif()
    if (${NetCDF_C_NC__ENDDEF_EXISTS})
      target_compile_definitions (pioc
        PRIVATE NETCDF_C_NC__ENDDEF_EXISTS)
    endif()
  else ()
    message(STATUS "Could not find NetCDF C library, disabling support for NetCDF")
    set(PIO_USE_NETCDF 0)
    set(PIO_USE_NETCDF4 0)
  endif ()
else ()
  message(STATUS "Disabling support for NetCDF")
  set(PIO_USE_NETCDF 0)
  set(PIO_USE_NETCDF4 0)
endif ()

#===== PnetCDF-C =====
if (WITH_PNETCDF)
  find_package (PnetCDF ${PNETCDF_MIN_VER_REQD} COMPONENTS C)
  if (PnetCDF_FOUND)
    message(STATUS "PnetCDF C library dependencies: ${PnetCDF_C_LIBRARY}")
    set(PIO_USE_PNETCDF 1)
    target_include_directories (pioc
      PUBLIC ${PnetCDF_C_INCLUDE_DIRS})
    target_link_libraries (pioc
      PUBLIC ${PnetCDF_C_LIBRARIES})

    # Check library for varn functions
    set (CMAKE_REQUIRED_LIBRARIES ${PnetCDF_C_LIBRARY})
    check_function_exists (ncmpi_get_varn PnetCDF_C_HAS_VARN)
    if (PnetCDF_C_HAS_VARN)
      target_compile_definitions(pioc
        PRIVATE USE_PNETCDF_VARN
        PRIVATE USE_PNETCDF_VARN_ON_READ)
    endif()
  else ()
    message(STATUS "Could not find PnetCDF library, disabling support for PnetCDF")
    set(PIO_USE_PNETCDF 0)
  endif ()
else ()
  message(STATUS "Disabling support for PnetCDF")
  set(PIO_USE_PNETCDF 0)
endif ()

#===== ADIOS-C =====
if (WITH_ADIOS2)
  # ADIOS 2.8.0 overwrites/resets CMAKE_MODULE_PATH, so cache and restore it after
  # finding ADIOS
  set(SPIO_CMAKE_MODULE_PATH_BACKUP ${CMAKE_MODULE_PATH})
  find_package (ADIOS2 ${ADIOS_MIN_VER_REQD})
  set(CMAKE_MODULE_PATH ${SPIO_CMAKE_MODULE_PATH_BACKUP})
  if (ADIOS2_FOUND)
    message(STATUS "Found ADIOS library")
    set(PIO_USE_ADIOS 1)
    target_compile_definitions (pioc
      PUBLIC _ADIOS2)
    target_link_libraries (pioc
      PUBLIC adios2::adios2 adios2pio-nm-lib)
  else ()
    message(STATUS "Could not find ADIOS library, disabling support for ADIOS")
    set(PIO_USE_ADIOS 0)
  endif ()
else ()
  message(STATUS "Disabling support for ADIOS (default)")
  set(PIO_USE_ADIOS 0)
endif ()

#===== HDF5-C =====
if (WITH_HDF5)
  find_package (HDF5 COMPONENTS HL C)
  if (HDF5_C_FOUND)
    message(STATUS "HDF5 C library dependencies: ${HDF5_C_LIBRARIES} ${HDF5_HL_LIBRARIES}")
    set(PIO_USE_HDF5 1)
    target_compile_definitions (pioc
      PUBLIC _HDF5)
    target_include_directories (pioc
      PUBLIC ${HDF5_C_INCLUDE_DIRS} ${HDF5_HL_INCLUDE_DIRS})
    target_link_libraries (pioc
      PUBLIC ${HDF5_C_LIBRARIES} ${HDF5_HL_LIBRARIES} ${CMAKE_DL_LIBS})
  else ()
    message(STATUS "Could not find HDF5 library, disabling support for HDF5")
    set(PIO_USE_HDF5 0)
  endif ()
else ()
  message(STATUS "Disabling support for HDF5")
  set(PIO_USE_HDF5 0)
endif ()

# We expect PnetCDF or NetCDF to be available
if (NOT PnetCDF_C_FOUND AND NOT NetCDF_C_FOUND)
  message (FATAL_ERROR "Could not find PnetCDF and NetCDF libraries. SCORPIO requires PnetCDF and/or NetCDF C libraries")
endif ()

#==============================================================================
#  CONFIGURE TYPE CHECKS
#==============================================================================
get_pio_offset_type()

message(STATUS "Using ${PIO_OFFSET_C_TYPENAME} for PIO Offset. sizeof(PIO_Offset) = ${PIO_OFFSET_SIZE} bytes")

check_type_size("size_t" SIZEOF_SIZE_T)
message(STATUS "sizeof(size_t) = ${SIZEOF_SIZE_T} bytes")
CHECK_TYPE_SIZE("long long" SIZEOF_LONG_LONG)
message(STATUS "sizeof(long long) = ${SIZEOF_LONG_LONG} bytes")
if (NOT ${SIZEOF_SIZE_T} EQUAL ${SIZEOF_LONG_LONG})
  message (FATAL_ERROR "size_t and long long must be the same size!")
endif ()
if (NOT ${SIZEOF_LONG_LONG} EQUAL 8)
  message (FATAL_ERROR "long long ints must be 64-bit!")
endif ()

# Note: This check needs to happen after the MPI library is found
# Add paths to MPI headers in the include path before calling
# check_type_size
if (PIO_USE_MPISERIAL)
  if (MPISERIAL_C_FOUND)
    set (CMAKE_REQUIRED_INCLUDES ${MPISERIAL_C_INCLUDE_DIRS})
  endif ()
else ()
  set (CMAKE_REQUIRED_INCLUDES ${MPI_INCLUDE_PATH})
endif ()
# Add mpi.h into the list of standard headers, so that we can
# check the size of MPI_Offset
SET(CMAKE_EXTRA_INCLUDE_FILES "mpi.h")
check_type_size("MPI_Offset" SIZEOF_MPI_OFFSET)
message(STATUS "sizeof(MPI_Offset) = ${SIZEOF_MPI_OFFSET} bytes")
SET(CMAKE_EXTRA_INCLUDE_FILES)

#==============================================================================
#  SET THE COMPILER OPTIONS
#==============================================================================
# set up include-directories
include_directories(
  "${PROJECT_SOURCE_DIR}"   # to find foo/foo.h
  "${PROJECT_BINARY_DIR}")  # to find foo/config.h

# Include the clib source and binary directory
target_include_directories (pioc
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories (pioc
  PRIVATE ${CMAKE_CURRENT_BINARY_DIR})

# Include the install include dir for all dep projects
target_include_directories (pioc
  PUBLIC $<INSTALL_INTERFACE:include>)

# System and compiler CPP directives
target_compile_definitions (pioc
  PRIVATE ${CMAKE_SYSTEM_DIRECTIVE})
target_compile_definitions (pioc
  PUBLIC ${CMAKE_C_COMPILER_DIRECTIVE})
# Skip MPI C++ headers/bindings for MPICH lib
target_compile_definitions (pioc
  PUBLIC MPICH_SKIP_MPICXX)
# Skip MPI C++ headers/bindings for OpenMPI lib
target_compile_definitions (pioc
  PUBLIC OMPI_SKIP_MPICXX)
# Skip MPI C++ headers/bindings for SGI MPT lib
target_compile_definitions (pioc
  PUBLIC MPI_NO_CPPBIND)

# Add user-specified include/libs/compiler/link options
target_include_directories (pioc
  PUBLIC ${PIO_C_EXTRA_INCLUDE_DIRS})
target_link_libraries (pioc
  PUBLIC ${PIO_C_EXTRA_LIBRARIES})
target_compile_options (pioc
  PRIVATE ${PIO_C_EXTRA_COMPILE_OPTIONS})
target_compile_definitions (pioc
  PUBLIC ${PIO_C_EXTRA_COMPILE_DEFINITIONS})
if (PIO_C_EXTRA_LINK_FLAGS)
  set_target_properties(pioc PROPERTIES
    LINK_FLAGS ${PIO_C_EXTRA_LINK_FLAGS})
endif ()

# At least on Titan + Cray MPI, MPI_Irsends are buggy
# causing hangs during I/O
# Force Scorpio to use MPI_Isends instead of the default
# MPI_Irsends
target_compile_definitions (pioc
  PRIVATE USE_MPI_ISEND_FOR_FC)

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

if (ADIOS_BP2NC_TEST)
  message(STATUS "Building ADIOS to NetCDF conversion tool")
  target_compile_definitions (pioc
    PUBLIC _ADIOS_BP2NC_TEST)
endif ()

# The MPI library detection was done in the top level
if (MPISERIAL_C_FOUND)
  target_compile_definitions (pioc
    PUBLIC MPI_SERIAL)
  target_include_directories (pioc
    PUBLIC ${MPISERIAL_C_INCLUDE_DIRS})
  target_link_libraries (pioc
    PUBLIC ${MPISERIAL_C_LIBRARIES})
  set (WITH_PNETCDF FALSE)
endif ()

# Hack to circumvent IBM XL (16.1.1-3) internal compiler error by
# disabling optimization for pioc_support.c
cmake_host_system_information (RESULT FQDN_SITENAME QUERY FQDN)
if (FQDN_SITENAME MATCHES "^.*summit[.]olcf")
  if (CMAKE_C_COMPILER_NAME STREQUAL "XL")
    message(STATUS "Disabling compiler optimization for pioc_support.c (to prevent internal compiler error with the XL compiler)")
    set_source_files_properties(pioc_support.c PROPERTIES COMPILE_FLAGS "-qnoopt -qnosmp")
  endif()
endif()

# Set variables to be written out in pio_config.h (generated from pio_config.h.in)
#====== PIO_MICRO_TIMING ======
if (PIO_MICRO_TIMING)
  message(STATUS "Enabling Micro timers...")
  set(USE_MICRO_TIMING 1)
else ()
  message(STATUS "Disabling Micro timers... (default, use -DPIO_MICRO_TIMING:BOOL=ON to enable micro timers)")
  set(USE_MICRO_TIMING 0)
endif ()

#==============================================================================
#  INSTALL
#==============================================================================

# Install libpioc.a
install (TARGETS pioc
          EXPORT spio-targets-pioc
          DESTINATION lib)

# Install the header file
install (FILES ${CMAKE_CURRENT_SOURCE_DIR}/pio.h DESTINATION include)

# Generate and install pio_config.h
configure_file (
  "${PROJECT_SOURCE_DIR}/pio_config.h.in"
  "${PROJECT_BINARY_DIR}/pio_config.h"
  )

# Install PIO config Include/Header File
install (FILES ${PROJECT_BINARY_DIR}/pio_config.h DESTINATION include)

# Install the cmake config file with targets to the cmake directory
install (EXPORT spio-targets-pioc
          FILE spio-targets-pioc.cmake
          DESTINATION lib/scorpio/cmake
          NAMESPACE scorpio::)
