###-------------------------------------------------------------------------###
### CMakeList.txt for examples using pio
###-------------------------------------------------------------------------###

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
    include_directories(${GPTL_C_INCLUDE_DIRS})
  else ()
    include_directories(${PROJECT_SOURCE_DIR}/../src/gptl)
    message (STATUS "Using internal GPTL C library for timing")
  endif ()
endif ()

#===== HDF5 ZFP compression plugin =========
if (WITH_HDF5)
  set(H5Z_ZFP_USE_STATIC_LIBS ON)
  # Append the HDF5 ZFP plugin path into list of directories searched for
  # finding the plugin headers and library. Since the HDF5 ZFP plugin also
  # requires ZFP, user needs to set ZFP_DIR (along with H5Z_ZFP_DIR &
  # H5Z_ZFP_PATH) if the plugin dir/path is set.
  list(APPEND CMAKE_PREFIX_PATH "$ENV{H5Z_ZFP_PATH}")
  #set(CMAKE_FIND_DEBUG_MODE TRUE)
  find_package(H5Z_ZFP)
  if (H5Z_ZFP_FOUND)
    message (STATUS "Found ZFP plugin for HDF5")
  else ()
    message (STATUS "Could not find ZFP plugin for HDF5, disabling ZFP with HDF5")
  endif ()
endif ()

#==============================================================================
#  SET THE COMPILER OPTIONS
#==============================================================================
# Adding PIO definitions - defined in the root directory
ADD_DEFINITIONS(${PIO_DEFINITIONS})

INCLUDE_DIRECTORIES(${PIO_INCLUDE_DIRS}
                    ${PROJECT_BINARY_DIR}/../src/clib
                    ${PROJECT_SOURCE_DIR}/../src/clib
                    ${PROJECT_SOURCE_DIR}/../util)
LINK_DIRECTORIES(${PIO_LIB_DIR})

#==============================================================================
#  BUILD EXECUTABLE
#==============================================================================
SET(SRC examplePio.cxx)
add_spio_executable(examplePio_cxx TRUE "" ${SRC})

if (WITH_HDF5)
  SET(SRC ${PROJECT_SOURCE_DIR}/../util/argparser.cxx hdf5.cpp)
  add_spio_executable(hdf5_example TRUE "" ${SRC})
  if (H5Z_ZFP_FOUND)
    target_compile_definitions(hdf5_example PRIVATE _SPIO_HAS_H5Z_ZFP)
    target_link_libraries(hdf5_example PRIVATE h5z_zfp::h5z_zfp)
  endif ()
endif ()
