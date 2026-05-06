#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C Core (I/O libraries)... =====")

add_subdirectory (hdf5)

add_library (spio_core_iolib OBJECT
  $<TARGET_OBJECTS:spio_core_iolib_hdf5>)

target_include_directories(spio_core_iolib
  PUBLIC .
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../.. ${CMAKE_CURRENT_BINARY_DIR}/../.. ${CMAKE_CURRENT_SOURCE_DIR}/../../util)

target_link_libraries(spio_core_iolib
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
