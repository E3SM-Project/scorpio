#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C Core... =====")
# SCORPIO C library - core
set (spio_core_src
  pio_spmd.cpp
  pioc_support.cpp
  pioc.cpp
  pio_nc.cpp
  pio_getput_int.cpp
  pio_file.cpp
  pio_darray_int.cpp
  pio_darray.cpp
  pio_varm.cpp
  pio_nc4.cpp
  pioc_sc.cpp
  pio_put_nc.cpp
  pio_msg.cpp
  pio_get_nc.cpp)

add_subdirectory (util)
add_subdirectory (progress_engine)
add_subdirectory (rearr)
add_subdirectory (iolib)

add_library (spio_core OBJECT
  $<TARGET_OBJECTS:spio_core_util>
  $<TARGET_OBJECTS:spio_core_progress_engine>
  $<TARGET_OBJECTS:spio_core_rearr>
  $<TARGET_OBJECTS:spio_core_iolib>
  $<TARGET_OBJECTS:spio_core_iolib_hdf5>
  ${spio_core_src})

set (spio_clib_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/..")
set (spio_clib_bin_dir "${CMAKE_CURRENT_BINARY_DIR}/..")

target_include_directories(spio_core
  PUBLIC .
  PRIVATE ${spio_clib_src_dir} ${spio_clib_bin_dir} ${spio_clib_src_dir}/util ${CMAKE_CURRENT_SOURCE_DIR}/util ${CMAKE_CURRENT_SOURCE_DIR}/progress_engine ${CMAKE_CURRENT_SOURCE_DIR}/rearr ${CMAKE_CURRENT_SOURCE_DIR}/iolib/hdf5)

target_link_libraries(spio_core
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
