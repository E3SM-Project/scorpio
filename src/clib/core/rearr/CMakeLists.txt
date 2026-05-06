#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C Core (Data Rearrangers)... =====")

set (spio_core_rearr_src
  pio_rearrange.cpp
  pio_rearr_utils.cpp
  pio_rearr_contig.cpp
  spio_rearrange_any.cpp)

add_library (spio_core_rearr OBJECT
  ${spio_core_rearr_src})

target_include_directories(spio_core_rearr
  PUBLIC .
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../.. ${CMAKE_CURRENT_BINARY_DIR}/../.. ${CMAKE_CURRENT_SOURCE_DIR}/../../util)

target_link_libraries(spio_core_rearr
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
