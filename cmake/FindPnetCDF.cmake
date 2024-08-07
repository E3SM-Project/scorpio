# - Try to find PnetCDF
#
# This can be controlled by setting the PnetCDF_PATH (or, equivalently, the 
# PNETCDF environment variable), or PnetCDF_<lang>_PATH CMake variables, where
# <lang> is the COMPONENT language one needs.
#
# Once done, this will define:
#
#   PnetCDF_<lang>_FOUND        (BOOL) - system has PnetCDF
#   PnetCDF_<lang>_IS_SHARED    (BOOL) - whether library is shared/dynamic
#   PnetCDF_<lang>_INCLUDE_DIR  (PATH) - Location of the C header file
#   PnetCDF_<lang>_INCLUDE_DIRS (LIST) - the PnetCDF include directories
#   PnetCDF_<lang>_LIBRARY      (FILE) - Path to the C library file
#   PnetCDF_<lang>_LIBRARIES    (LIST) - link these to use PnetCDF
#
# The available COMPONENTS are: C, Fortran
# If no components are specified, it assumes only C
include (LibFind)
include (LibCheck)
include (FindPackageHandleStandardArgs)

# Define PnetCDF C Component
define_package_component (PnetCDF DEFAULT
                          COMPONENT C
                          INCLUDE_NAMES pnetcdf.h
                          LIBRARY_NAMES pnetcdf)

# Define PnetCDF Fortran Component
define_package_component (PnetCDF
                          COMPONENT Fortran
                          INCLUDE_NAMES pnetcdf.mod pnetcdf.inc
                          LIBRARY_NAMES pnetcdf)

# Search for list of valid components requested
find_valid_components (PnetCDF)

#==============================================================================
# SEARCH FOR VALIDATED COMPONENTS
foreach (PNCDFcomp IN LISTS PnetCDF_FIND_VALID_COMPONENTS)

    # If not found already, search...
    if (NOT PnetCDF_${PNCDFcomp}_FOUND)

        # Manually add the MPI include and library dirs to search paths
        # and search for the package component
        if (MPI_${PNCDFcomp}_FOUND)
            initialize_paths (PnetCDF_${PNCDFcomp}_PATHS
                              INCLUDE_DIRECTORIES ${MPI_${PNCDFcomp}_INCLUDE_PATH}
                              LIBRARIES ${MPI_${PNCDFcomp}_LIBRARIES})
        endif ()
        find_package_component(PnetCDF COMPONENT ${PNCDFcomp}
                               PATHS ${PnetCDF_${PNCDFcomp}_PATHS})

        # Continue only if component found
        if (PnetCDF_${PNCDFcomp}_FOUND)
        
            # Check version
            check_version (PnetCDF
                           NAME "pnetcdf.h"
                           HINTS ${PnetCDF_${PNCDFcomp}_INCLUDE_DIR}
                           MACRO_REGEX "PNETCDF_VERSION_")

            find_package_handle_standard_args(PnetCDF
              FOUND_VAR PnetCDF_FOUND
              REQUIRED_VARS PnetCDF_${PNCDFcomp}_LIBRARY PnetCDF_${PNCDFcomp}_INCLUDE_DIR
              VERSION_VAR PnetCDF_VERSION)
        endif ()
            
    endif ()
    
endforeach ()

# Handle QUIET/REQUIRED, and set <PKG>_FOUND if all required components were found
find_package_handle_standard_args (PnetCDF HANDLE_COMPONENTS)
