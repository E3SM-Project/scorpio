# Install script for directory: /home/tkurc/codar/acme/ParallelIO/src/flib

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "0")
endif()

if("${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/home/tkurc/codar/acme/ParallelIO/build/src/flib/libpiof.a")
endif()

if("${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pio.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pio_nf.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pio_types.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/piolib_mod.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pionfget_mod.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pio_kinds.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pio_support.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/piodarray.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pionfatt_mod.mod"
    "/home/tkurc/codar/acme/ParallelIO/build/src/flib/pionfput_mod.mod"
    )
endif()

