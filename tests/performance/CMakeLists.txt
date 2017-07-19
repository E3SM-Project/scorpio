
SET(SRC pioperformance_rearr.F90)

INCLUDE_DIRECTORIES(${PIO_INCLUDE_DIRS})
LINK_DIRECTORIES(${PIO_LIB_DIR})
ADD_EXECUTABLE(pioperf_rearr ${SRC})
if(${PIO_BUILD_TIMING})
  TARGET_LINK_LIBRARIES(pioperf_rearr pio timing)
else()
  TARGET_LINK_LIBRARIES(pioperf_rearr pio)
endif()


