#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C Core... =====")
# SCORPIO C library - core
set (spio_core_src
  spio_async_utils.cpp
  spio_async_tpool.cpp
  spio_async_hdf5_utils.cpp
  pio_spmd.cpp
  spio_hdf5_utils.cpp
  spio_decomp_txt_logger.cpp
  spio_decomp_nc_logger.cpp
  spio_decomp_map_info_pool.cpp
  spio_async_tcomm.cpp
  pioc_support.cpp
  pioc.cpp
  pio_rearrange.cpp
  pio_rearr_utils.cpp
  pio_rearr_contig.cpp
  pio_print.cpp
  pio_nc.cpp
  pio_lists.cpp
  pio_getput_int.cpp
  pio_file.cpp
  pio_darray_int.cpp
  pio_darray.cpp
  spio_serializer.cpp
  pio_varm.cpp
  pio_nc4.cpp
  spio_rearrange_any.cpp
  spio_io_summary.cpp
  spio_file_mvcache.cpp
  pioc_sc.cpp
  pio_sdecomps_regex.cpp
  pio_put_nc.cpp
  pio_msg.cpp
  pio_get_nc.cpp)

add_library (spio_core OBJECT ${spio_core_src})
target_include_directories(spio_core
  PUBLIC .
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_BINARY_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/../util)
target_link_libraries(spio_core
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
