include (LibMPI)

#============================================================================
# Check if two Fortran INTEGER types have the same size
#
# Input Args,
# FINTEGER_KIND1 : First INTEGER type kind
# FINTEGER_KIND2 : Second INTEGER type kind
#
# Returns,
# check_ftype_size_eq_RET :  Set to TRUE if the INTEGER(KIND=FINTEGER_KIND1)
#                             and INTEGER(KIND=FINTEGER_KIND2) have the same
#                             size, set to FALSE otherwise
#============================================================================
function (check_ftype_size_eq FINTEGER_KIND1 FINTEGER_KIND2)
  # Read the template program and replace Fortran type templates
  # with the Fortran types provided by the user

  # The template program is TryMPI_OFFSET_TYPE.F90 in <SCORPIO_SRC>/cmake
  set (templ_pgm_fname "TryMPI_OFFSET_TYPE.F90")
  unset (templ_pgm_file)
  find_file (templ_pgm_file
             NAMES ${templ_pgm_fname}
             HINTS ${CMAKE_MODULE_PATH})

  unset (check_ftype_size_eq_RET)
  if (templ_pgm_file)
    set (tmp_pgm_path ${CMAKE_CURRENT_BINARY_DIR}${CMAKE_FILES_DIRECTORY})
    # The program below is built to check if the type sizes match
    set (tmp_pgm_fname "TryMPI_OFFSET_SZ_${FINTEGER_KIND1}_${FINTEGER_KIND2}.F90")
    # FIXME: Use cmake_path to combine paths once we move the min cmake version to >= 3.20
    set (tmp_pgm_full_path_fname ${tmp_pgm_path}/${tmp_pgm_fname})

    unset (templ_pgm_src)
    # Read the template program source
    file (READ ${templ_pgm_file} templ_pgm_src)
    #message (STATUS "DEBUG: Read file ${temp_pgm_file} : ${templ_pgm_src}")

    # Replace templates for target kinds with the integer kinds to test
    string (REGEX REPLACE "<SPIO_ACTUAL_TARGET_KIND>" ${FINTEGER_KIND1} pgm_src ${templ_pgm_src})
    string (REGEX REPLACE "<SPIO_PTR_TARGET_KIND>" ${FINTEGER_KIND2} pgm_src ${pgm_src})
    #message (STATUS "DEBUG: Writing ${tmp_pgm_full_path_fname} : ${pgm_src}")

    # Write out the final program that is used for testing whether the type sizes match
    file(WRITE ${tmp_pgm_full_path_fname} ${pgm_src})

    # Try compiling the program, if the compilation succeeds the type sizes match
    message (STATUS "Checking whether Fortran type INTEGER(KIND=${FINTEGER_KIND1}) has the same size as INTEGER(KIND=${FINTEGER_KIND2})")
    unset (ftype_size_is_eq)
    if (PIO_USE_MPISERIAL)
      find_package (MPISERIAL COMPONENTS Fortran REQUIRED)
      if (MPISERIAL_Fortran_FOUND)
        #message(STATUS "MPI serial Fortran library dependencies : ${MPISERIAL_Fortran_LIBRARIES}")
        set (tmp_pgm_defns "-DNO_MPIMOD")
        set (tmp_pgm_include_dirs ${MPISERIAL_Fortran_INCLUDE_DIRS})
        set (tmp_pgm_libs ${MPISERIAL_Fortran_LIBRARIES})
      else ()
        message (FATAL_ERROR "Could not find MPI serial library")
      endif ()
    endif ()
    if (MPI_MOD_PATH)
      list (APPEND tmp_pgm_defns " -I${MPI_MOD_PATH}")
    endif ()

    try_compile (ftype_size_is_eq
                ${tmp_pgm_path}
                SOURCES ${tmp_pgm_full_path_fname}
                COMPILE_DEFINITIONS ${tmp_pgm_defns}
                CMAKE_FLAGS "-DINCLUDE_DIRECTORIES=${tmp_pgm_include_dirs}"
                LINK_LIBRARIES ${tmp_pgm_libs}
                OUTPUT_VARIABLE build_out)
    #message (STATUS "Compile result: ${ftype_size_is_eq}")

    # The variable "build_out" contains the compiler output, uncomment the message below
    # to view the compiler output (for debugging)
    #message (STATUS "Compile output: ${build_out}")

    set (check_ftype_size_eq_RET ${ftype_size_is_eq} PARENT_SCOPE) 

    # Remove the temp program source file
    file (REMOVE ${tmp_pgm_full_path_fname})
  else ()
    message (WARNING "Could not check whether Fortran type sizes match. Missing template file, \"${templ_pgm_fname}\" in module path: \"${CMAKE_MODULE_PATH}\"")
    set (check_ftype_size_eq_RET FALSE PARENT_SCOPE)
  endif ()

endfunction ()

#============================================================================
# Get PIO Offset type
# Input:
#   None
# Output:
#   PIO_OFFSET_SIZE : Size of the PIO Offset type
#   PIO_OFFSET_C_TYPENAME : The C type to use for PIO Offsets
#   PIO_OFFSET_C_MPITYPENAME : The MPI type to use for PIO Offsets in C
#   PIO_OFFSET_F_TYPENAME : The Fortran type to use for PIO Offsets
#   PIO_OFFSET_F_MPITYPENAME : The MPI type to use for PIO Offsets in Fortran
#   PIO_OFFSET_F_KIND : The Fortran type KIND to use for PIO Offsets
#   PIO_OFFSET_F2C_TYPE_KIND : The Fortran type KIND to use for passing
#                               PIO Offsets from Fortran to C
# C types:
# *  Find C type for PIO offsets. If MPI_Offset is available use it.
#    If MPI_Offset is not available use "long long", "long" or "int"
#    as long as there is a corresponding MPI C type, a compatible
#    Fortran type (all values represented by the Fortran type needs
#    to be represented by the C type) and a Fortran to C type to
#    pass the offset from the Fortran interface to C
# Fortran types:
# *  If MPI_Offset (C type) is available we expect MPI_OFFSET_KIND to
#    be available too (currently we do not support libraries that 
#    do not have this behavior)
#    
# Note: This function should be called after the MPI library is found
#============================================================================
function (get_pio_offset_type)

  # The 3 lists below are assumed to be of the same length (same number of elements)
  # and element with index i in each list is associated with the elements with the
  # same index in the other lists
  set (c_offset_types "long long" "long" "int")
  set (mpi_c_types "MPI_LONG_LONG" "MPI_LONG" "MPI_INT")
  set (f2c_offset_kinds "C_LONG_LONG" "C_LONG" "C_INT")

  # Find C types for MPI_Offset
  if (PIO_USE_MPISERIAL)
    if (MPISERIAL_C_FOUND)
      set (CMAKE_REQUIRED_INCLUDES ${MPISERIAL_C_INCLUDE_DIRS})
    endif ()
  else ()
    set (CMAKE_REQUIRED_INCLUDES ${MPI_INCLUDE_PATH})
  endif ()

  # Add mpi.h into the list of standard headers, so that we can
  # check the size of MPI_Offset
  set (CMAKE_EXTRA_INCLUDE_FILES "mpi.h")
  check_type_size ("MPI_Offset" sizeof_c_mpi_offset)

  if (sizeof_c_mpi_offset GREATER "0")
    # MPI_Offset (C MPI Offset type) is available
    # Assuming that the Fortran MPI_OFFSET_KIND (Fortran MPI Offset type)
    # is also available
    set (PIO_OFFSET_SIZE ${sizeof_c_mpi_offset})
    set (PIO_OFFSET_SIZE ${sizeof_c_mpi_offset} PARENT_SCOPE)
    set (PIO_OFFSET_C_TYPENAME "MPI_Offset")
    set (PIO_OFFSET_C_TYPENAME "MPI_Offset" PARENT_SCOPE)
    set (PIO_OFFSET_C_MPITYPENAME "MPI_OFFSET")
    set (PIO_OFFSET_C_MPITYPENAME "MPI_OFFSET" PARENT_SCOPE)
    set (PIO_OFFSET_F_TYPENAME "INTEGER")
    set (PIO_OFFSET_F_TYPENAME "INTEGER" PARENT_SCOPE)
    set (PIO_OFFSET_F_MPITYPENAME "MPI_OFFSET")
    set (PIO_OFFSET_F_MPITYPENAME "MPI_OFFSET" PARENT_SCOPE)
    set (PIO_OFFSET_F_KIND "MPI_OFFSET_KIND")
    set (PIO_OFFSET_F_KIND "MPI_OFFSET_KIND" PARENT_SCOPE)

    # Find the Fortran to C type that matches the size of the Fortran MPI
    # Offset type
    set (f2ctype_found FALSE)
    foreach (f2ctype_kind IN LISTS f2c_offset_kinds)
      set (check_ftype_size_eq_RET FALSE)
      check_ftype_size_eq ("MPI_OFFSET_KIND" ${f2ctype_kind})
      if (${check_ftype_size_eq_RET})
        set (f2ctype_found TRUE)
        set (PIO_OFFSET_F2C_TYPE_KIND ${f2ctype_kind})
        set (PIO_OFFSET_F2C_TYPE_KIND ${f2ctype_kind} PARENT_SCOPE)
        break ()
      endif ()
    endforeach ()
    if (NOT ${f2ctype_found})
      set (PIO_OFFSET_F2C_TYPE_KIND "PIO_OFFSET_F_UNKNOWN_TYPE")
      set (PIO_OFFSET_F2C_TYPE_KIND "PIO_OFFSET_F_UNKNOWN_TYPE" PARENT_SCOPE)
      message (FATAL_ERROR "Could not find a Fortran type for passing PIO Offsets from Fortran to C")
    else ()
      # message (STATUS "Using ${PIO_OFFSET_F_TYPENAME}(KIND=${PIO_OFFSET_F2C_TYPE_KIND}) for passing PIO Offsets from Fortran to C")
    endif ()
  else ()
    # MPI_Offset (C MPI Offset type) is not available
    # Find sizes of candidate C types for PIO Offset.
    set (type_found FALSE)
    set (ctype_idx "0")
    foreach (ctype IN LISTS c_offset_types)
      check_type_size (${ctype} sizeof_ctype)
      if (sizeof_ctype GREATER "0")
        # If a C type (e.g. long) is available we assume that the corresponding
        # Fortran to C type (e.g. C_LONG) is also available
        set (type_found TRUE)
        set (PIO_OFFSET_SIZE ${sizeof_ctype})
        set (PIO_OFFSET_SIZE ${sizeof_ctype} PARENT_SCOPE)
        set (PIO_OFFSET_C_TYPENAME ${ctype})
        set (PIO_OFFSET_C_TYPENAME ${ctype} PARENT_SCOPE)
        list (GET mpi_c_types ${ctype_idx} tmp_mpi_c_type)
        set (PIO_OFFSET_C_MPITYPENAME ${tmp_mpi_c_type})
        set (PIO_OFFSET_C_MPITYPENAME ${tmp_mpi_c_type} PARENT_SCOPE)
        set (PIO_OFFSET_F_TYPENAME "INTEGER")
        set (PIO_OFFSET_F_TYPENAME "INTEGER" PARENT_SCOPE)
        list (GET f2c_offset_kinds ${ctype_idx} tmp_f2c_type)
        set (PIO_OFFSET_F_KIND ${tmp_f2c_type})
        set (PIO_OFFSET_F_KIND ${tmp_f2c_type} PARENT_SCOPE)
        set (PIO_OFFSET_F2C_TYPE_KIND ${tmp_f2c_type})
        set (PIO_OFFSET_F2C_TYPE_KIND ${tmp_f2c_type}  PARENT_SCOPE)
        break ()
      endif ()
      math (EXPR ctype_idx "${ctype_idx} + 1")
    endforeach ()

    # SCORPIO currently does not use MPI calls with PIO_OFFSET_KIND ints
    # in Fortran (Fortran interface), so leave the MPI type as unknown
    set (PIO_OFFSET_F_MPITYPENAME "PIO_OFFSET_UNKNOWN_MPI_TYPE")
    set (${PIO_OFFSET_F_MPITYPENAME} "PIO_OFFSET_UNKNOWN_MPI_TYPE" PARENT_SCOPE)
    if (NOT ${type_found})
      set (PIO_OFFSET_SIZE "0")
      set (PIO_OFFSET_SIZE "0" PARENT_SCOPE)
      set (PIO_OFFSET_C_TYPENAME "PIO_OFFSET_C_UNKNOWN_TYPE")
      set (PIO_OFFSET_C_TYPENAME "PIO_OFFSET_C_UNKNOWN_TYPE" PARENT_SCOPE)
      set (PIO_OFFSET_C_MPITYPENAME "PIO_OFFSET_C_UNKNOWN_MPI_TYPE")
      set (PIO_OFFSET_C_MPITYPENAME "PIO_OFFSET_C_UNKNOWN_MPI_TYPE" PARENT_SCOPE)
      set (PIO_OFFSET_F_TYPENAME "PIO_OFFSET_F_UNKNOWN_TYPE")
      set (PIO_OFFSET_F_TYPENAME "PIO_OFFSET_F_UNKNOWN_TYPE" PARENT_SCOPE)
      set (PIO_OFFSET_F_KIND "PIO_OFFSET_F_UNKNOWN_TYPE_KIND")
      set (PIO_OFFSET_F_KIND "PIO_OFFSET_F_UNKNOWN_TYPE_KIND" PARENT_SCOPE)
      set (PIO_OFFSET_F2C_TYPE_KIND "PIO_OFFSET_F_UNKNOWN_TYPE_KIND")
      set (PIO_OFFSET_F2C_TYPE_KIND "PIO_OFFSET_F_UNKNOWN_TYPE_KIND" PARENT_SCOPE)
      message(FATAL_ERROR "Could not find a type for representing PIO Offsets")
    else ()
      message(STATUS "Using ${PIO_OFFSET_C_TYPENAME} for representing PIO Offsets in C")
      # message(STATUS "Using ${PIO_OFFSET_F_TYPENAME}(KIND=${PIO_OFFSET_F_KIND}) for representing PIO Offsets in Fortran")
      # message(STATUS "Using ${PIO_OFFSET_F_TYPENAME}(KIND=${PIO_OFFSET_F2C_TYPE_KIND}) for passing PIO Offsets from Fortran to C")
    endif ()
  endif ()

endfunction()
