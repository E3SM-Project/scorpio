#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C Core (progress engine)... =====")
# SCORPIO C library - core
set (spio_core_progress_engine_src
  spio_async_utils.cpp
  spio_async_tpool.cpp
  spio_async_tcomm.cpp)

add_library (spio_core_progress_engine OBJECT
  ${spio_core_progress_engine_src})

set (spio_clib_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/../..")
set (spio_clib_bin_dir "${CMAKE_CURRENT_BINARY_DIR}/../..")

target_include_directories(spio_core_progress_engine
  PUBLIC .
  PRIVATE ${spio_clib_src_dir} ${spio_clib_bin_dir} ${spio_clib_src_dir}/util ${spio_clib_src_dir}/core ${spio_clib_src_dir}/core/util ${spio_clib_src_dir}/core/iolib/hdf5)

target_link_libraries(spio_core_progress_engine
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
