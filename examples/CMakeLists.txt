###-------------------------------------------------------------------------###
### CMakeList.txt for examples using pio
###-------------------------------------------------------------------------###
project (SPIOExamples CXX C Fortran)

message(STATUS "===== Configuring SCORPIO Examples... =====")
if (PIO_ENABLE_FORTRAN)
  ADD_SUBDIRECTORY(f03)
  ADD_SUBDIRECTORY(c)
  ADD_SUBDIRECTORY(cxx)
else()
  ADD_SUBDIRECTORY(c)
  ADD_SUBDIRECTORY(cxx)
endif()
ADD_SUBDIRECTORY(adios)
