#==============================================================================
#  DEFINE THE TARGET LIBRARY
#==============================================================================
message(STATUS "===== Configuring SCORPIO C API ... =====")
# PIO API source
set (pio_api_src
  spio_lib_api.cpp
  spio_ddata_api.cpp
  spio_dim_api.cpp
  spio_err_api.cpp
  spio_file_api.cpp
  spio_file_md_api.cpp
  spio_get_att_api.cpp
  spio_get_utils.cpp
  spio_get_var1_api.cpp
  spio_get_vara_api.cpp
  spio_get_var_api.cpp
  spio_get_varm_api.cpp
  spio_get_vars_api.cpp
  spio_io_decomp_api.cpp
  spio_io_sys_api.cpp
  spio_misc_att_api.cpp
  spio_misc_var_api.cpp
  spio_put_att_api.cpp
  spio_put_var1_api.cpp
  spio_put_vara_api.cpp
  spio_put_var_api.cpp
  spio_put_varm_api.cpp
  spio_put_vars_api.cpp)

add_library (pioc_api OBJECT ${pio_api_src})
target_include_directories(pioc_api
  PUBLIC .
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/.. ${CMAKE_CURRENT_BINARY_DIR}/.. ${CMAKE_CURRENT_SOURCE_DIR}/../util ${SCORPIO_SOURCE_DIR}/util)
target_link_libraries(pioc_api
  PUBLIC spio_default_public_options
  PRIVATE spio_default_private_options)
