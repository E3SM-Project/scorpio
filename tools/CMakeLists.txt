###-------------------------------------------------------------------------###
### CMakeList.txt for examples using pio
###-------------------------------------------------------------------------###
project (SPIOTools CXX C Fortran)

if (WITH_ADIOS2)
  # ADIOS 2.8.0 overwrites/resets CMAKE_MODULE_PATH, so cache and restore it
  # after finding ADIOS
  set(SPIO_CMAKE_MODULE_PATH_BACKUP ${CMAKE_MODULE_PATH})
  find_package (ADIOS2 ${ADIOS_MIN_VER_REQD})
  set(CMAKE_MODULE_PATH ${SPIO_CMAKE_MODULE_PATH_BACKUP})
  if (ADIOS2_FOUND)
    if (ADIOS2_VERSION VERSION_LESS ${ADIOS_MIN_VER_REQD})
      message(STATUS "WARNING: Cannot find a compatible version of the ADIOS library, disabling support for ADIOS")
      set(ADIOS2_FOUND false)
    endif ()
  endif ()
  if (ADIOS2_FOUND)
    ADD_SUBDIRECTORY(adios2pio-nm)
  endif (ADIOS2_FOUND)
endif(WITH_ADIOS2)
ADD_SUBDIRECTORY(spio_finfo)
