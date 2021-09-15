# - Try to find LIBLOWFIVE
#
# This can be controlled by setting the LIBLOWFIVE_PATH (or, equivalently, the 
# LIBLOWFIVE environment variable).
#
# Once done, this will define:
#
#   LIBLOWFIVE_FOUND        (BOOL) - system has LIBLOWFIVE
#   LIBLOWFIVE_IS_SHARED    (BOOL) - whether library is shared/dynamic
#   LIBLOWFIVE_INCLUDE_DIR  (PATH) - Location of the C header file
#   LIBLOWFIVE_INCLUDE_DIRS (LIST) - the LIBLOWFIVE include directories
#   LIBLOWFIVE_LIBRARY      (FILE) - Path to the C library file
#   LIBLOWFIVE_LIBRARIES    (LIST) - link these to use LIBLOWFIVE
#
include (LibFind)

# Define LIBLOWFIVE package
define_package_component (LIBLOWFIVE
                          INCLUDE_NAMES lowfive/vol-base.hpp
                          LIBRARY_NAMES lowfive)

# SEARCH FOR PACKAGE
if (NOT LIBLOWFIVE_FOUND)

    # Search for the package
    find_package_component(LIBLOWFIVE)

endif ()
