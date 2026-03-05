#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C Core (util)... =====")
# SCORPIO C library - core
set (spio_core_util_src
  spio_decomp_txt_logger.cpp
  spio_decomp_nc_logger.cpp
  spio_decomp_map_info_pool.cpp
  pio_lists.cpp
  spio_io_summary.cpp
  spio_file_mvcache.cpp
  pio_sdecomps_regex.cpp)

add_library (spio_core_util OBJECT
  ${spio_core_util_src})

set (spio_clib_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/../..")
set (spio_clib_bin_dir "${CMAKE_CURRENT_BINARY_DIR}/../..")

target_include_directories(spio_core_util
  PUBLIC .
  PRIVATE ${spio_clib_src_dir} ${spio_clib_bin_dir} ${spio_clib_src_dir}/util ${spio_clib_src_dir}/core)

target_link_libraries(spio_core_util
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
