include (LibMPI)
include (PIOTest)
include (SPIOUtils)

include_directories("${SCORPIO_SOURCE_DIR}/tests/functional" "${SCORPIO_SOURCE_DIR}/tests/util" "${SCORPIO_SOURCE_DIR}/util" "${SCORPIO_SOURCE_DIR}/src/clib" "${SCORPIO_BINARY_DIR}/src/clib")

string (TOUPPER "${CMAKE_CXX_COMPILER_ID}" CMAKE_CXX_COMPILER_NAME)
if (CMAKE_CXX_COMPILER_NAME STREQUAL "CRAY")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -h std=c++14")
else ()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
endif ()

if (PIO_TEST_CLOSE_OPEN_FOR_SYNC)
  add_definitions(-DPIO_TEST_CLOSE_OPEN_FOR_SYNC)
endif()

#===== MPI =====
if (PIO_USE_MPISERIAL)
  find_package (MPISERIAL COMPONENTS C REQUIRED)
  if (MPISERIAL_C_FOUND)
    set (CMAKE_REQUIRED_INCLUDES ${MPISERIAL_C_INCLUDE_DIRS})
  endif ()
else ()
  find_package (MPI REQUIRED)
  set (CMAKE_REQUIRED_INCLUDES ${MPI_INCLUDE_PATH})
endif ()

#===== GPTL =====
if (PIO_ENABLE_TIMING)
  find_package (GPTL COMPONENTS C QUIET)
  if (GPTL_C_FOUND)
    message (STATUS "Found GPTL C: ${GPTL_C_LIBRARIES}")
  else ()
    include_directories(${PROJECT_SOURCE_DIR}/../src/gptl)
    message (STATUS "Using internal GPTL C library for timing")
  endif ()
endif ()

# Test Timeout (16 min = 960 sec)
set (DEFAULT_TEST_TIMEOUT 1800)
set (DEFAULT_TEST_MAXNUMPROCS 4)

# Use environment variable PIO_TF_MAXNUMPROCS to set the maximum number
# of MPI processes used to run the tests (default is 4)
# This setting is typically used to run PIO Fortran test suite on a
# cluster (with larger number of MPI processes)
if (DEFINED ENV{PIO_TF_MAXNUMPROCS})
  set(PIO_TF_MAXNUMPROCS $ENV{PIO_TF_MAXNUMPROCS})
else ()
  set(PIO_TF_MAXNUMPROCS ${DEFAULT_TEST_MAXNUMPROCS})
endif ()

# Since the testing framework exponentially increases the number of
# I/O processes and stride we need to limit the number of combinations
# of MPI processes + I/O processes + stride to limit the total test suite
# execution time. We divide tests into multiple tiers based on their
# nightly test execution times and limit the number of I/O processes and
# strides for costly tiers.
# Cost tiers C1, C2, C3, ... : cost(C1) < cost(C2) < cost(C3), ...
# Approx cost : cost(C1) < 1s, cost(C2) < 10s, cost(C3) >= 10s

# Strides are always restricted to 1,2 for all cost tiers
# (to reduce the number of test combinations)
set (PIO_TF_STRIDE_UB "2")

# Cost tier C1 variables - low cost, very less restrictions
# Only allow upto 8 I/O procs & strides upto 2
set (PIO_TF_C1_UB "8")
set (PIO_TF_C1_MAXNUMIOPROCS ${PIO_TF_MAXNUMPROCS})
set (PIO_TF_C1_MAXSTRIDE ${PIO_TF_MAXNUMPROCS})

if (PIO_TF_C1_MAXNUMIOPROCS GREATER PIO_TF_C1_UB)
  set (PIO_TF_C1_MAXNUMIOPROCS ${PIO_TF_C1_UB})
endif ()
if (PIO_TF_C1_MAXSTRIDE GREATER PIO_TF_STRIDE_UB)
  set (PIO_TF_C1_MAXSTRIDE ${PIO_TF_STRIDE_UB})
endif ()

# Cost tier C2 variables - medium cost, some restrictions
# Only allow upto 4 I/O procs & strides upto 2
set (PIO_TF_C2_UB "4")
set (PIO_TF_C2_MAXNUMIOPROCS ${PIO_TF_MAXNUMPROCS})
set (PIO_TF_C2_MAXSTRIDE ${PIO_TF_MAXNUMPROCS})

if (PIO_TF_C2_MAXNUMIOPROCS GREATER PIO_TF_C2_UB)
  set (PIO_TF_C2_MAXNUMIOPROCS ${PIO_TF_C2_UB})
endif ()
if (PIO_TF_C2_MAXSTRIDE GREATER PIO_TF_STRIDE_UB)
  set (PIO_TF_C2_MAXSTRIDE ${PIO_TF_STRIDE_UB})
endif ()

# Cost tier C3 variables - high cost, limit combinations tested
# Only allow upto 2 I/O procs & strides upto 2
set (PIO_TF_C3_UB "2")
set (PIO_TF_C3_MAXNUMIOPROCS ${PIO_TF_MAXNUMPROCS})
set (PIO_TF_C3_MAXSTRIDE ${PIO_TF_MAXNUMPROCS})

if (PIO_TF_C3_MAXNUMIOPROCS GREATER PIO_TF_C3_UB)
  set (PIO_TF_C3_MAXNUMIOPROCS ${PIO_TF_C3_UB})
endif ()
if (PIO_TF_C3_MAXSTRIDE GREATER PIO_TF_STRIDE_UB)
  set (PIO_TF_C3_MAXSTRIDE ${PIO_TF_STRIDE_UB})
endif ()

add_spio_executable(pio_decomp_tests_cxx TRUE "" pio_decomp_tests.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_decomp_tests_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_decomp_tests_cxx
  MINNUMPROCS 1
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 1
  MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}
  MINSTRIDE 1
  MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})

add_spio_executable(pio_fail TRUE "" pio_fail.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_fail
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_fail
  MINNUMPROCS 2
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 1
  MAXNUMIOPROCS ${PIO_TF_C1_MAXNUMIOPROCS}
  MINSTRIDE 1
  MAXSTRIDE ${PIO_TF_C1_MAXSTRIDE}
  TIMEOUT ${DEFAULT_TEST_TIMEOUT}
  WILLFAIL)

if (PIO_ENABLE_LARGE_TESTS)
  add_spio_executable(pio_decomp_large_tests_cxx TRUE "" pio_decomp_large_tests.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

  add_pio_test(pio_decomp_large_tests_cxx
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_decomp_large_tests_cxx
    MINNUMPROCS 2
    MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
    MINNUMIOPROCS 2
    MAXNUMIOPROCS 2
    MINSTRIDE 1
    MAXSTRIDE 1
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
endif ()

add_spio_executable(pio_init_finalize_cxx TRUE "" pio_init_finalize.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_init_finalize_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_init_finalize_cxx
  MINNUMPROCS 1
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 1
  MAXNUMIOPROCS ${PIO_TF_C1_MAXNUMIOPROCS}
  MINSTRIDE 1
  MAXSTRIDE ${PIO_TF_C1_MAXSTRIDE}
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})

add_spio_executable(pio_file_simple_tests_cxx TRUE "" pio_file_simple_tests.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_file_simple_tests_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_file_simple_tests_cxx
  MINNUMPROCS 2
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 1
  MAXNUMIOPROCS 2
  MINSTRIDE 1
  MAXSTRIDE 1
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})

add_spio_executable(pio_file_fail_cxx TRUE "" pio_file_fail.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_file_fail_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_file_fail_cxx
  MINNUMPROCS 4
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 2
  MAXNUMIOPROCS 2
  MINSTRIDE 2
  MAXSTRIDE 2
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})

add_spio_executable(ncdf_eh_one_test_cxx TRUE "" ncdf_eh_one_test.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(ncdf_eh_one_test_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/ncdf_eh_one_test_cxx
  MINNUMPROCS 4
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 2
  MAXNUMIOPROCS ${PIO_TF_C2_MAXNUMIOPROCS}
  MINSTRIDE 1
  MAXSTRIDE ${PIO_TF_C2_MAXSTRIDE}
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})

add_spio_executable(pio_set_hint_cxx TRUE "" pio_set_hint.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_set_hint_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_set_hint_cxx
  MINNUMPROCS 1
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 1
  MAXNUMIOPROCS ${PIO_TF_C1_MAXNUMIOPROCS}
  MINSTRIDE 1
  MAXSTRIDE ${PIO_TF_C1_MAXSTRIDE}
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})

add_spio_executable(pio_copy_cxx TRUE "" pio_copy.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_copy_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_copy_cxx
  MINNUMPROCS 1
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 1
  MAXNUMIOPROCS ${PIO_TF_C1_MAXNUMIOPROCS}
  MINSTRIDE 1
  MAXSTRIDE ${PIO_TF_C1_MAXSTRIDE}
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})

add_spio_executable(pio_async_decomp_tests_cxx TRUE "" pio_async_decomp_tests.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)

add_pio_test(pio_async_decomp_tests_cxx
  EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/pio_async_decomp_tests_cxx
  MINNUMPROCS 3
  MAXNUMPROCS ${PIO_TF_MAXNUMPROCS}
  MINNUMIOPROCS 1
  MAXNUMIOPROCS 1
  MINSTRIDE 1
  MAXSTRIDE 1
  TIMEOUT ${DEFAULT_TEST_TIMEOUT})
