#==============================================================================
#  HELPER MACROS
#==============================================================================
# Parse compression options
# Call spio_parse_comp_opts() with the compression options to parse it
# e.g. cmake -DSPIO_COMPRESSION_OPTIONS=
#       "COMPRESSION_LIBRARY=ADIOS2:Blosc2:Zstd;
#       SPIO_ADIOS2_BLOSC2_COMPRESSION_LEVEL=2;
#       SPIO_ADIOS2_BLOSC2_SHUFFLE_METHOD=BLOSC_BITSHUFFLE;
#       COMPRESSION_LIBRARY=HDF5:Blosc2:Zstd;
#       SPIO_HDF5_BLOSC2_COMPRESSION_LEVEL=2;
#       SPIO_HDF5_BLOSC2_SHUFFLE_METHOD=BLOSC_BITSHUFFLE;"

# Parse HDF5 + BLOSC2 compression options
macro (spio_parse_hdf5_blosc2_comp_opts comp_opts)
  message(STATUS "Parsing HDF5:Blosc2 compression options")
  foreach(opt ${comp_opts})
    # Split each opt, OPTION_NAME=OPTION_VAL, to name & val
    string(REGEX MATCHALL "[^=]+" opt_args ${opt})
    list(LENGTH opt_args num_opt_args)
    if(num_opt_args EQUAL 2)
      list(GET opt_args 0 comp_lib_arg_name)
      list(GET opt_args 1 comp_lib_arg_val)
      if("${comp_lib_arg_name}" STREQUAL "SPIO_HDF5_BLOSC2_COMPRESSION_LEVEL")
        set(SPIO_HDF5_BLOSC2_COMPRESSION_LEVEL ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_HDF5_BLOSC2_COMPRESSION_LIBRARY")
        set(SPIO_HDF5_BLOSC2_COMPRESSION_LIBRARY ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_HDF5_BLOSC2_SHUFFLE_METHOD")
        set(SPIO_HDF5_BLOSC2_SHUFFLE_METHOD ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "COMPRESSION_LIBRARY")
        # The rest of the options are for another library, e.g. ADIOS options
        break()
      else()
        message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
      endif()
    else()
      message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
    endif()
  endforeach()
endmacro()

# Parse HDF5 + ZFP compression options
macro (spio_parse_hdf5_zfp_comp_opts comp_opts)
  message(STATUS "Parsing HDF5:zfp compression options")
  foreach(opt ${comp_opts})
    # Split each opt, OPTION_NAME=OPTION_VAL, to name & val
    string(REGEX MATCHALL "[^=]+" opt_args ${opt})
    list(LENGTH opt_args num_opt_args)
    if(num_opt_args EQUAL 2)
      list(GET opt_args 0 comp_lib_arg_name)
      list(GET opt_args 1 comp_lib_arg_val)
      if("${comp_lib_arg_name}" STREQUAL "SPIO_HDF5_ZFP_COMPRESSION_MODE")
        set(SPIO_HDF5_ZFP_COMPRESSION_MODE ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_HDF5_ZFP_COMPRESSION_RATE")
        set(SPIO_HDF5_ZFP_COMPRESSION_RATE ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_HDF5_ZFP_PRECISION")
        set(SPIO_HDF5_ZFP_PRECISION ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_HDF5_ZFP_ACCURACY")
        set(SPIO_HDF5_ZFP_ACCURACY ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "COMPRESSION_LIBRARY")
        # The rest of the options are for another library, e.g. ADIOS options
        break()
      else()
        message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
      endif()
    else()
      message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
    endif()
  endforeach()
endmacro()

# Parse all HDF5 compression options
# e.g. spio_parse_hdf5_comp_opts("HDF5:BLOSC2:ZSTD", LIST_OF_COMPRESSION_OPTS)
macro (spio_parse_hdf5_comp_opts comp_lib comp_opts)
  message(STATUS "Parsing HDF5 compression options")
  string(TOUPPER "${comp_lib}" comp_lib)
  string(STRIP "${comp_lib}" comp_lib)
  # Split compression library qualified name, "HDF5:BLOSC2:ZSTD" to individual lib names
  string(REGEX MATCHALL "[^:]+" comp_lib_detail ${comp_lib})
  # Expecting comp_lib = "HDF5:BLOSC2:ZSTD" OR "HDF5:ZFP"
  list(LENGTH comp_lib_detail num_comp_lib_detail)
  if(num_comp_lib_detail GREATER 1)
    list(GET comp_lib_detail 0 io_lib_name)
    list(GET comp_lib_detail 1 comp_lib_name)
    if("${comp_lib_name}" STREQUAL "ZFP")
      spio_parse_hdf5_zfp_comp_opts("${comp_opts}")
    elseif("${comp_lib_name}" STREQUAL "BLOSC2")
      if(num_comp_lib_detail GREATER 2)
        list(GET comp_lib_detail 2 comp_internal_lib_name)
        if("${comp_internal_lib_name}" STREQUAL "ZSTD")
          spio_parse_hdf5_blosc2_comp_opts("${comp_opts}")
        else()
          message(STATUS "ERROR: Invalid HDF5 compression option for BLOSC2 compression library. Internal compression library specified (${comp_internal_lib_name}) is not supported")
        endif()
      else()
        message(STATUS "ERROR: Invalid HDF5 compression option for BLOSC2 compression library. Internal compression library (e.g. ZSTD) was not specified")
      endif()
    else()
      message(STATUS "ERROR: Invalid HDF5 compression option, compression library specified (\"${comp_lib_name}\") is not supported")
    endif()
  else()
    message(STATUS "ERROR: Invalid HDF5 compression option, compression library specified (\"${comp_lib_detail}\") is not supported")
  endif()
endmacro()

# Parse ADIOS2 + Blosc2 compression options
macro (spio_parse_adios2_blosc2_comp_opts comp_opts)
  message(STATUS "Parsing ADIOS2:Blosc2 compression options")
  foreach(opt ${comp_opts})
    # Split each opt, OPTION_NAME=OPTION_VAL, to name & val
    string(REGEX MATCHALL "[^=]+" opt_args ${opt})
    list(LENGTH opt_args num_opt_args)
    if(num_opt_args EQUAL 2)
      list(GET opt_args 0 comp_lib_arg_name)
      list(GET opt_args 1 comp_lib_arg_val)
      if("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_BLOSC2_COMPRESSION_LEVEL")
        set(SPIO_ADIOS2_BLOSC2_COMPRESSION_LEVEL ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_BLOSC2_COMPRESSION_LIBRARY")
        set(SPIO_ADIOS2_BLOSC2_COMPRESSION_LIBRARY ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_BLOSC2_SHUFFLE_METHOD")
        set(SPIO_ADIOS2_BLOSC2_SHUFFLE_METHOD ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "COMPRESSION_LIBRARY")
        # The rest of the options are for another library, e.g. HDF5 options
        break()
      else()
        message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
      endif()
    else()
      message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
    endif()
  endforeach()
endmacro()

# Parse ADIOS2 + ZFP compression options
macro (spio_parse_adios2_zfp_comp_opts comp_opts)
  message(STATUS "Parsing ADIOS2:zfp compression options")
  foreach(opt ${comp_opts})
    # Split each opt, OPTION_NAME=OPTION_VAL, to name & val
    string(REGEX MATCHALL "[^=]+" opt_args ${opt})
    list(LENGTH opt_args num_opt_args)
    if(num_opt_args EQUAL 2)
      list(GET opt_args 0 comp_lib_arg_name)
      list(GET opt_args 1 comp_lib_arg_val)
      if("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_ZFP_COMPRESSION_MODE")
        set(SPIO_ADIOS2_ZFP_COMPRESSION_MODE ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_ZFP_COMPRESSION_RATE")
        set(SPIO_ADIOS2_ZFP_COMPRESSION_RATE ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_ZFP_PRECISION")
        set(SPIO_ADIOS2_ZFP_PRECISION ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_ZFP_ACCURACY")
        set(SPIO_ADIOS2_ZFP_ACCURACY ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "COMPRESSION_LIBRARY")
        # The rest of the options are for another library, e.g. HDF5 options
        break()
      else()
        message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
      endif()
    else()
      message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
    endif()
  endforeach()
endmacro()

# Parse ADIOS2 + SZ compression options
macro (spio_parse_adios2_sz_comp_opts comp_opts)
  message(STATUS "Parsing ADIOS2:SZ compression options")
  foreach(opt ${comp_opts})
    # Split each opt, OPTION_NAME=OPTION_VAL, to name & val
    string(REGEX MATCHALL "[^=]+" opt_args ${opt})
    list(LENGTH opt_args num_opt_args)
    if(num_opt_args EQUAL 2)
      list(GET opt_args 0 comp_lib_arg_name)
      list(GET opt_args 1 comp_lib_arg_val)
      if("${comp_lib_arg_name}" STREQUAL "SPIO_ADIOS2_SZ_ACCURACY")
        set(SPIO_ADIOS2_SZ_ACCURACY ${comp_lib_arg_val})
      elseif("${comp_lib_arg_name}" STREQUAL "COMPRESSION_LIBRARY")
        # The rest of the options are for another library, e.g. HDF5 options
        break()
      else()
        message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
      endif()
    else()
      message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option, ignoring option")
    endif()
  endforeach()
endmacro()

# Parse all ADIOS2 compression options
# e.g. spio_parse_adios2_comp_opts("ADIOS2:BLOSC2:ZSTD", LIST_OF_COMPRESSION_OPTS)
macro (spio_parse_adios2_comp_opts comp_lib comp_opts)
  message(STATUS "Parsing ADIOS2 compression options")
  string(TOUPPER "${comp_lib}" comp_lib)
  string(STRIP "${comp_lib}" comp_lib)
  # Split compression library qualified name, "ADIOS2:BLOSC2:ZSTD" to individual lib names
  string(REGEX MATCHALL "[^:]+" comp_lib_detail ${comp_lib})
  # Expecting comp_lib = "ADIOS2:BLOSC2:ZSTD", "ADIOS2:SZ" or "ADIOS2:ZFP"
  list(LENGTH comp_lib_detail num_comp_lib_detail)
  if(num_comp_lib_detail GREATER 1)
    list(GET comp_lib_detail 0 io_lib_name)
    list(GET comp_lib_detail 1 comp_lib_name)
    if("${comp_lib_name}" STREQUAL "ZFP")
      spio_parse_adios2_zfp_comp_opts("${comp_opts}")
    elseif("${comp_lib_name}" STREQUAL "SZ")
      spio_parse_adios2_sz_comp_opts("${comp_opts}")
    elseif("${comp_lib_name}" STREQUAL "BLOSC2")
      if(num_comp_lib_detail GREATER 2)
        list(GET comp_lib_detail 2 comp_internal_lib_name)
        if("${comp_internal_lib_name}" STREQUAL "ZSTD")
          spio_parse_adios2_blosc2_comp_opts("${comp_opts}")
        else()
          message(STATUS "ERROR: Invalid ADIOS2 compression option for BLOSC2 compression library. Internal compression library specified (${comp_internal_lib_name}) is not supported")
        endif()
      else()
        message(STATUS "ERROR: Invalid ADIOS2 compression option for BLOSC2 compression library. Internal compression library (e.g. ZSTD) was not specified")
      endif()
    else()
      message(STATUS "ERROR: Invalid ADIOS2 compression option, compression library specified (\"${comp_lib_name}\") is not supported")
    endif()
  else()
    message(STATUS "ERROR: Invalid ADIOS2 compression option, compression library specified (\"${comp_lib_detail}\") is not supported")
  endif()
endmacro()

macro (spio_set_default_comp_opts)
  # HDF5 + Blosc2
  set(SPIO_HDF5_BLOSC2_COMPRESSION_LEVEL 1)
  set(SPIO_HDF5_BLOSC2_COMPRESSION_LIBRARY "BLOSC_ZSTD")
  # BLOSC_NOSHUFFLE (0), BLOSC_SHUFFLE (1) : Byte shuffle,
  # BLOSC_BITSHUFFLE (2) : Bit shuffle
  set(SPIO_HDF5_BLOSC2_SHUFFLE_METHOD "BLOSC_BITSHUFFLE")

  # HDF5 + ZFP : Available options are
  # "H5Z_ZFP_MODE_RATE" : Fixed bit rate (The number of bits for representing compressed data is fixed)
  # "H5Z_ZFP_MODE_PRECISION" : Fixed precision (Variable bit rate, The number of bits in original value is fixed)
  # "H5Z_ZFP_MODE_ACCURACY" : Fixed accuracy (Variable bit rate, The absolute error between compressed and original value is bound)
  # "H5Z_ZFP_MODE_REVERSIBLE" : Lossless compression
  set(SPIO_HDF5_ZFP_COMPRESSION_MODE "H5Z_ZFP_MODE_ACCURACY")
  set(SPIO_HDF5_ZFP_COMPRESSION_RATE 16)
  set(SPIO_HDF5_ZFP_PRECISION 16)
  set(SPIO_HDF5_ZFP_ACCURACY 0.001)

  # ADIOS2 + Blosc2
  set(SPIO_ADIOS2_BLOSC2_COMPRESSION_LEVEL "1")
  set(SPIO_ADIOS2_BLOSC2_COMPRESSION_LIBRARY "zstd")
  set(SPIO_ADIOS2_BLOSC2_SHUFFLE_METHOD "BLOSC_BITSHUFFLE")

  # ADIOS2 + ZFP
  # "ADIOS2_ZFP_MODE_RATE" : Fixed bit rate (no of bits is fixed in compressed value)
  # "ADIOS2_ZFP_MODE_PRECISION" : Fixed precision Variable bit rate (no of bits is fixed in original value)
  # "ADIOS2_ZFP_MODE_ACCURACY" : Fixed accuracy Variable bit rate (absolute error btw compressed and original value is less than upper bound)
  # "ADIOS2_ZFP_MODE_REVERSIBLE" (lossless)
  set(SPIO_ADIOS2_ZFP_COMPRESSION_MODE "ADIOS2_ZFP_MODE_ACCURACY")
  set(SPIO_ADIOS2_ZFP_COMPRESSION_RATE 16)
  set(SPIO_ADIOS2_ZFP_PRECISION 16)
  set(SPIO_ADIOS2_ZFP_ACCURACY "0.001")

  # ADIOS2 + SZ
  set(SPIO_ADIOS2_SZ_ACCURACY "0.001")
endmacro()

macro (spio_parse_comp_opts comp_opts)
  message(STATUS "Parsing compression options (-DSPIO_COMPRESSION_OPTIONS=\"${comp_opts}\")")
  set(copts)
  list(APPEND copts ${comp_opts})
  # Start idx for options after "COMPRESSION_LIBRARY=xxx"
  set(start_idx_opts 0)
  foreach(opt IN LISTS copts)
    # FIXME: Avoid reparsing options
    math(EXPR start_idx_opts "${start_idx_opts} + 1")
    string(REGEX MATCHALL "[^=]+" opt_args ${opt})
    list(LENGTH opt_args num_opt_args)
    if(num_opt_args EQUAL 2)
      list(GET opt_args 0 comp_lib_opt_name)
      list(GET opt_args 1 comp_lib)
      if(NOT ("${comp_lib_opt_name}" STREQUAL "COMPRESSION_LIBRARY"))
        # Skip other options - the rest of the options need to be parsed by library specific calls below
        continue()
      endif()
      string(TOUPPER "${comp_lib}" comp_lib)
      list(SUBLIST copts ${start_idx_opts} -1 copts_after_comp_lib_opt)
      # Check if compression library is hdf5
      string(FIND "${comp_lib}" "HDF5" pos)
      if(${pos} EQUAL 0)
        spio_parse_hdf5_comp_opts(${comp_lib} "${copts_after_comp_lib_opt}")
      else()
        # Check if compression library is adios
        string(FIND "${comp_lib}" "ADIOS2" pos)
        if(${pos} EQUAL 0)
          spio_parse_adios2_comp_opts(${comp_lib} "${copts_after_comp_lib_opt}")
        else()
          message(STATUS "ERROR: Invalid compression library (\"${comp_lib}\") specified in the compression option in options list (\"${comp_opts}\") in option \"${opt}\". Expected \"HDF5\" or \"ADIOS2\". Compression options will be ignored")
          break()
        endif()
      endif()
    else()
      message(STATUS "ERROR: Invalid compression option (\"${opt}\"), cannot parse option. Ignoring the option")
    endif()
  endforeach()
endmacro()
