#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C Core (hdf5)... =====")
# HDF5 Support
set (spio_core_iolib_hdf5_src
  spio_async_hdf5_utils.cpp
  spio_hdf5_utils.cpp)

set (spio_clib_src_dir "${CMAKE_CURRENT_SOURCE_DIR}/../../..")
set (spio_clib_bin_dir "${CMAKE_CURRENT_BINARY_DIR}/../../..")

add_library (spio_core_iolib_hdf5 OBJECT ${spio_core_iolib_hdf5_src})
target_include_directories(spio_core_iolib_hdf5
  PUBLIC .
  PRIVATE ${spio_clib_src_dir} ${spio_clib_bin_dir} ${spio_clib_src_dir}/util ${spio_clib_src_dir}/core ${spio_clib_src_dir}/core/util ${spio_clib_src_dir}/core/progress_engine)
target_link_libraries(spio_core_iolib_hdf5
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
