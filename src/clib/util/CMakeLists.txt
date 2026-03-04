#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C lib utils... =====")
# Util source - Utils used across the library
set (spio_util_src
  spio_ltimer.cpp
  spio_dt_converter.cpp
  spio_dbg_ds.cpp
  pio_timer.cpp
  bget.cpp
  topology.cpp
  spio_hash.cpp
  spio_tracer.cpp
  spio_tracer_mdata.cpp
  spio_tracer_decomp.cpp
  spio_serializer.cpp
  pio_mpi_timer.cpp)

add_library (spio_util OBJECT ${spio_util_src})
target_include_directories(spio_util
  PUBLIC .
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_BINARY_DIR}/..)
target_link_libraries(spio_util
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
