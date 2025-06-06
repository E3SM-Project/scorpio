# - Try to find NetCDF
#
# This can be controlled by setting the NetCDF_PATH (or, equivalently, the
# NETCDF environment variable), or NetCDF_<lang>_PATH CMake variables, where
# <lang> is the COMPONENT language one needs.
#
# Once done, this will define:
#
#   NetCDF_<lang>_FOUND        (BOOL) - system has NetCDF
#   NetCDF_<lang>_IS_SHARED    (BOOL) - whether library is shared/dynamic
#   NetCDF_<lang>_INCLUDE_DIR  (PATH) - Location of the C header file
#   NetCDF_<lang>_INCLUDE_DIRS (LIST) - the NetCDF include directories
#   NetCDF_<lang>_LIBRARY      (FILE) - Path to the C library file
#   NetCDF_<lang>_LIBRARIES    (LIST) - link these to use NetCDF
#
# The available COMPONENTS are: C Fortran
# If no components are specified, it assumes only C
include (LibFind)
include (LibCheck)
include (FindPackageHandleStandardArgs)

# Define NetCDF C Component
define_package_component (NetCDF DEFAULT
                          COMPONENT C
                          INCLUDE_NAMES netcdf.h
                          LIBRARY_NAMES netcdf)

# Define NetCDF Fortran Component
define_package_component (NetCDF
                          COMPONENT Fortran
                          INCLUDE_NAMES netcdf.mod netcdf.inc
                          LIBRARY_NAMES netcdff)

# Search for list of valid components requested
find_valid_components (NetCDF)

#==============================================================================
# SEARCH FOR VALIDATED COMPONENTS
foreach (NCDFcomp IN LISTS NetCDF_FIND_VALID_COMPONENTS)
    # If not found already, search...
    if (NOT NetCDF_${NCDFcomp}_FOUND)

        # Manually add the MPI include and library dirs to search paths
        # and search for the package component
        if (MPI_${NCDFcomp}_FOUND)
            initialize_paths (NetCDF_${NCDFcomp}_PATHS
                              INCLUDE_DIRECTORIES ${MPI_${NCDFcomp}_INCLUDE_PATH}
                              LIBRARIES ${MPI_${NCDFcomp}_LIBRARIES})
        endif ()
        find_package_component(NetCDF COMPONENT ${NCDFcomp}
                               PATHS ${NetCDF_${NCDFcomp}_PATHS})

        # Continue only if component found
        if (NetCDF_${NCDFcomp}_FOUND)

            # Checks
            if (NCDFcomp STREQUAL C)

                # Check version
                check_version (NetCDF
                               NAME "netcdf_meta.h"
                               HINTS ${NetCDF_C_INCLUDE_DIRS}
                               MACRO_REGEX "NC_VERSION_")

                find_package_handle_standard_args(NetCDF
                  FOUND_VAR NetCDF_FOUND
                  REQUIRED_VARS NetCDF_${NCDFcomp}_LIBRARY NetCDF_${NCDFcomp}_INCLUDE_DIR
                  VERSION_VAR NetCDF_VERSION)

                # Check for parallel support
                check_macro (NetCDF_C_HAS_PARALLEL
                             NAME TryNetCDF_PARALLEL.c
                             HINTS ${CMAKE_MODULE_PATH}
                             DEFINITIONS -I${NetCDF_C_INCLUDE_DIR}
                             COMMENT "whether NetCDF has parallel support")

            endif ()

            # Dependencies
            if (NCDFcomp STREQUAL C AND NOT NetCDF_C_IS_SHARED)

                # DEPENDENCY: PnetCDF (if PnetCDF enabled)
                check_macro (NetCDF_C_HAS_PNETCDF
                             NAME TryNetCDF_PNETCDF.c
                             HINTS ${CMAKE_MODULE_PATH}
                             DEFINITIONS -I${NetCDF_C_INCLUDE_DIR}
                             COMMENT "whether NetCDF has PnetCDF support")
                if (NetCDF_C_HAS_PNETCDF)
                    find_package (PnetCDF COMPONENTS C)
                    if (CURL_FOUND)
                        list (APPEND NetCDF_C_INCLUDE_DIRS ${PnetCDF_C_INCLUDE_DIRS})
                        list (APPEND NetCDF_C_LIBRARIES ${PnetCDF_C_LIBRARIES})
                    endif ()
                endif ()

                # DEPENDENCY: CURL (If DAP enabled)
                check_macro (NetCDF_C_HAS_DAP
                             NAME TryNetCDF_DAP.c
                             HINTS ${CMAKE_MODULE_PATH}
                             DEFINITIONS -I${NetCDF_C_INCLUDE_DIR}
                             COMMENT "whether NetCDF has DAP support")
                if (NetCDF_C_HAS_DAP)
                    find_package (CURL)
                    if (CURL_FOUND)
                        list (APPEND NetCDF_C_INCLUDE_DIRS ${CURL_INCLUDE_DIRS})
                        list (APPEND NetCDF_C_LIBRARIES ${CURL_LIBRARIES})
                    endif ()
                endif ()

                # DEPENDENCY: HDF5
                find_package (HDF5 COMPONENTS HL C)
                if (HDF5_C_FOUND)
                    list (APPEND NetCDF_C_INCLUDE_DIRS ${HDF5_C_INCLUDE_DIRS}
                                                       ${HDF5_HL_INCLUDE_DIRS})
                    list (APPEND NetCDF_C_LIBRARIES ${HDF5_C_LIBRARIES}
                                                    ${HDF5_HL_LIBRARIES})
                endif ()

                # DEPENDENCY: LIBDL Math
                list (APPEND NetCDF_C_LIBRARIES -ldl -lm)

            elseif (NCDFcomp STREQUAL Fortran AND NOT NetCDF_Fortran_IS_SHARED)

                # DEPENDENCY: NetCDF
                set (orig_comp ${NCDFcomp})
                set (orig_comps ${NetCDF_FIND_VALID_COMPONENTS})
                find_package (NetCDF COMPONENTS C)
                set (NetCDF_FIND_VALID_COMPONENTS ${orig_comps})
                set (NCDFcomp ${orig_comp})
                if (NetCDF_C_FOUND)
                    list (APPEND NetCDF_Fortran_INCLUDE_DIRS ${NetCDF_C_INCLUDE_DIRS})
                    list (APPEND NetCDF_Fortran_LIBRARIES ${NetCDF_C_LIBRARIES})
                endif ()

            endif ()

            # Additional checks after all dependencies are appended
            if (NCDFcomp STREQUAL C)
                set(CMAKE_REQUIRED_INCLUDES ${NetCDF_C_INCLUDE_DIR})
                set(CMAKE_REQUIRED_LIBRARIES ${NetCDF_C_LIBRARIES})

                # Check if logging enabled
                CHECK_FUNCTION_EXISTS(nc_set_log_level NetCDF_C_LOGGING_ENABLED)

                # Check if nc__enddef is still available on future NetCDF implementations
                CHECK_FUNCTION_EXISTS(nc__enddef NetCDF_C_NC__ENDDEF_EXISTS)
            endif ()

            if (NCDFcomp STREQUAL "C")
              set (config_exe_name nc-config)
            else()
              set (config_exe_name nf-config)
            endif()
            find_program(NetCDF_${NCDFcomp}_CONFIG_EXE
                          NAMES ${config_exe_name}
                          HINTS ${NetCDF_${NCDFcomp}_INCLUDE_DIR}/../bin)
            if (NetCDF_${NCDFcomp}_CONFIG_EXE)
              message(STATUS "NetCDF_${NCDFcomp}_CONFIG_EXE = ${NetCDF_${NCDFcomp}_CONFIG_EXE}")
              execute_process(COMMAND "${NetCDF_${NCDFcomp}_CONFIG_EXE}" "--has-nczarr"
                              OUTPUT_VARIABLE NetCDF_${NCDFcomp}_has_nczarr
                              OUTPUT_STRIP_TRAILING_WHITESPACE)
              if(${NetCDF_${NCDFcomp}_has_nczarr} STREQUAL "yes")
                message(STATUS "NetCDF (${NCDFcomp} component) has NCZarr support")
                set(NetCDF_${NCDFcomp}_HAS_NCZARR TRUE CACHE BOOL "Has NCZarr support")
              else ()
                message(STATUS "NetCDF (${NCDFcomp} component) does not have NCZarr support")
              endif ()
            else ()
              message(STATUS "NetCDF_${NCDFcomp}_CONFIG_EXE not found. Disabling checks for NCZarr support.")
            endif ()
        endif ()

    endif ()

endforeach ()

# Handle QUIET/REQUIRED, and set <PKG>_FOUND if all required components were found
find_package_handle_standard_args (NetCDF HANDLE_COMPONENTS)
