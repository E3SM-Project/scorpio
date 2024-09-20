cmake_minimum_required (VERSION 3.7)
project (SCORPIO_REPLAY C CXX Fortran)

message(STATUS "===== Configuring SCORPIO Replay tool... =====")

# The project version number.
set(VERSION_MAJOR   1   CACHE STRING "Project major version number.")
set(VERSION_MINOR   0   CACHE STRING "Project minor version number.")
set(VERSION_PATCH   0   CACHE STRING "Project patch version number.")
mark_as_advanced(VERSION_MAJOR VERSION_MINOR VERSION_PATCH)

# All the config required for building the replay tool will be
# gathered via including the SCORPIO build cache file.
# The SCORPIO install SOURCE directory needs to be provided
# via SCORPIO_DIR

if(SCORPIO_BUILD_CACHE)
  message(STATUS "Loading SCORPIO build cache : ${SCORPIO_BUILD_CACHE}")
  load_cache(${SCORPIO_BUILD_CACHE})
endif()

#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
# SCORPIO Replay tool sources
file(GLOB pio_replay_src "spio_replay*.cpp")

add_executable(spio_replay.exe ${pio_replay_src})

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

#===== SCORPIO =====
find_package(SCORPIO REQUIRED)
