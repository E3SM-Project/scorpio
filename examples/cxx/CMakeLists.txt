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
SET(SRC ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)
add_library(argparser OBJECT ${SRC})

SET(SRC examplePio.cxx)
add_spio_executable(examplePio_cxx TRUE "" ${SRC})
target_link_libraries(examplePio_cxx PRIVATE $<TARGET_OBJECTS:argparser>)

SET(SRC e3sm_fgi.cpp e3sm_f.cpp e3sm_g.cpp e3sm_i.cpp)
add_spio_executable(e3sm_fgi TRUE "" ${SRC})
target_link_libraries(e3sm_fgi PRIVATE $<TARGET_OBJECTS:argparser>)

if (WITH_HDF5)
  SET(SRC ${PROJECT_SOURCE_DIR}/../util/argparser.cxx hdf5.cpp)
  add_spio_executable(hdf5_example TRUE "" ${SRC})
endif ()
