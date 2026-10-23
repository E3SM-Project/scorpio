include (LibMPI)

message(STATUS "===== Configuring SCORPIO C/C++ tests... =====")
#==============================================================================
#  HELPER MACROS
#==============================================================================
include(SPIOUtils)

#==============================================================================
#  SET THE COMPILER OPTIONS
#==============================================================================
# FIXME: Use an interface lib to track these include deps
include_directories("${SCORPIO_SOURCE_DIR}/tests/cunit" "${SCORPIO_SOURCE_DIR}/util" "${SCORPIO_SOURCE_DIR}/src/clib" "${SCORPIO_BINARY_DIR}/src/clib" "${SCORPIO_SOURCE_DIR}/src/clib/core" "${SCORPIO_SOURCE_DIR}/src/clib/core/util" "${SCORPIO_SOURCE_DIR}/src/clib/core/rearr" "${SCORPIO_SOURCE_DIR}/src/clib/core/progress_engine" "${SCORPIO_SOURCE_DIR}/src/clib/core/iolib/hdf5" "${SCORPIO_SOURCE_DIR}/src/clib/util")

# Compiler-specific compiler options
string (TOUPPER "${CMAKE_C_COMPILER_ID}" CMAKE_C_COMPILER_NAME)
if (CMAKE_C_COMPILER_NAME STREQUAL "CRAY")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -h std=c99")
elseif (CMAKE_C_COMPILER_NAME STREQUAL "PGI")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -c99")
elseif (CMAKE_C_COMPILER_NAME STREQUAL "NVHPC")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -c99")
else ()
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=c99")
endif ()

string (TOUPPER "${CMAKE_CXX_COMPILER_ID}" CMAKE_CXX_COMPILER_NAME)
if (CMAKE_CXX_COMPILER_NAME STREQUAL "CRAY")
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -h std=c++14")
else ()
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
endif ()

set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0")

#==============================================================================
#  FIND EXTERNAL LIBRARIES/DEPENDENCIES
#==============================================================================
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

#==============================================================================
#  DEFINE THE TARGETS AND TESTS
#==============================================================================

add_library (test_common_obj OBJECT test_common.cpp ${SCORPIO_SOURCE_DIR}/util/argparser.cxx)
target_link_libraries (test_common_obj PRIVATE pioc)

# Exclude tests that require more than 1 procs when using the MPI serial library
if (NOT PIO_USE_MPISERIAL)
  add_spio_executable (test_intercomm2 TRUE "" test_intercomm2.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_async_simple TRUE "" test_async_simple.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_async_3proc TRUE "" test_async_3proc.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_async_4proc TRUE "" test_async_4proc.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_iosystem2_simple TRUE "" test_iosystem2_simple.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_iosystem2_simple2 TRUE "" test_iosystem2_simple2.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_iosystem2 TRUE "" test_iosystem2.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_iosystem3_simple TRUE "" test_iosystem3_simple.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_iosystem3_simple2 TRUE "" test_iosystem3_simple2.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_iosystem3 TRUE "" test_iosystem3.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_pioc TRUE "" test_pioc.cpp test_shared.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_pioc_unlim TRUE "" test_pioc_unlim.cpp test_shared.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_pioc_putget TRUE "" test_pioc_putget.cpp test_shared.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_pioc_fill TRUE "" test_pioc_fill.cpp test_shared.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_darray TRUE "" test_darray.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_darray_multi TRUE "" test_darray_multi.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_darray_multivar TRUE "" test_darray_multivar.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_darray_multivar2 TRUE "" test_darray_multivar2.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_darray_1d TRUE "" test_darray_1d.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_darray_3d TRUE "" test_darray_3d.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_decomp_uneven TRUE "" test_decomp_uneven.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_decomps TRUE "" test_decomps.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_rearr TRUE "" test_rearr.cpp OBJ_LIBS test_common_obj)
  add_dependencies (tests test_intercomm2 test_async_simple test_async_3proc test_async_4proc
                    test_iosystem2_simple test_iosystem2_simple2 test_iosystem2 test_iosystem3_simple
                    test_iosystem3_simple2 test_iosystem3 test_pioc test_pioc_unlim test_pioc_putget
                    test_pioc_fill test_darray test_darray_multi test_darray_multivar test_darray_multivar2
                    test_darray_1d test_darray_3d test_decomp_uneven test_decomps test_rearr)
  if (WITH_HDF5)
    add_spio_executable (test_darray_hdf5_contig TRUE "" test_darray_hdf5_contig.cpp OBJ_LIBS test_common_obj)
    add_spio_executable (test_putget_hdf5 TRUE "" test_putget_hdf5.cpp OBJ_LIBS test_common_obj)
    add_dependencies (tests test_darray_hdf5_contig test_putget_hdf5)
  endif ()
  if (PIO_USE_MALLOC)
    add_spio_executable (test_darray_async_simple TRUE "" test_darray_async_simple.cpp OBJ_LIBS test_common_obj)
    add_spio_executable (test_darray_async TRUE "" test_darray_async.cpp OBJ_LIBS test_common_obj)
    add_spio_executable (test_darray_async_many TRUE "" test_darray_async_many.cpp OBJ_LIBS test_common_obj)
    add_dependencies (tests test_darray_async_simple test_darray_async test_darray_async_many)
  endif ()
endif ()
add_spio_executable (test_spmd TRUE "" test_spmd.cpp OBJ_LIBS test_common_obj)
add_spio_executable (test_spio_ltimer TRUE "" test_spio_ltimer.cpp)
add_spio_executable (test_spio_serializer TRUE "" test_spio_serializer.cpp)
add_spio_executable (test_spio_tree TRUE "" test_spio_tree.cpp)
add_spio_executable (test_spio_file_mvcache TRUE "" test_spio_file_mvcache.cpp)
add_spio_executable (test_spio_sort_utils TRUE "" test_spio_sort_utils.cpp)
add_spio_executable (test_spio_rearr_utils_gather TRUE "" test_spio_rearr_utils_gather.cpp)
add_spio_executable (test_spio_rearr_utils_scatter TRUE "" test_spio_rearr_utils_scatter.cpp)
add_spio_executable (test_spio_rearr_utils_alltoall TRUE "" test_spio_rearr_utils_alltoall.cpp)
add_spio_executable (test_spio_rearr_contig TRUE "" test_spio_rearr_contig.cpp OBJ_LIBS test_common_obj)
add_spio_executable (test_spio_rearr_contig_nvars TRUE "" test_spio_rearr_contig_nvars.cpp)
add_spio_executable (test_spio_rearr_contig_fillval TRUE "" test_spio_rearr_contig_fillval.cpp)
add_spio_executable (test_spio_decomp_logger TRUE "" test_spio_decomp_logger.cpp)
add_spio_executable (test_sdecomp_regex TRUE "" test_sdecomp_regex.cpp OBJ_LIBS test_common_obj)
add_spio_executable(test_req_block_wait TRUE "" test_req_block_wait.cpp OBJ_LIBS test_common_obj)
add_spio_executable(test_spio_dt_converter TRUE "" test_spio_dt_converter.cpp)
add_spio_executable(test_spio_select_io_desc_info TRUE "" test_spio_select_io_desc_info.cpp OBJ_LIBS test_common_obj)
add_spio_executable(test_spio_iodesc_utils TRUE "" test_spio_iodesc_utils.cpp)
add_spio_executable(test_spio_iosys_utils TRUE "" test_spio_iosys_utils.cpp)
add_spio_executable(test_spio_async_tcomm TRUE "" test_spio_async_tcomm.cpp)
add_dependencies (tests test_spmd test_spio_ltimer test_spio_serializer test_spio_tree
                  test_spio_file_mvcache test_spio_sort_utils test_spio_rearr_utils_gather
                  test_spio_rearr_utils_scatter test_spio_rearr_utils_alltoall
                  test_spio_rearr_contig test_spio_rearr_contig_nvars test_spio_rearr_contig_fillval
                  test_spio_decomp_logger test_sdecomp_regex test_req_block_wait
                  test_spio_dt_converter test_spio_select_io_desc_info test_spio_iodesc_utils
                  test_spio_iosys_utils test_spio_async_tcomm)

if(PIO_USE_ASYNC_WR_THREAD)
  add_spio_executable (test_mtq TRUE "" test_async_mtq.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_mtq_signal TRUE "" test_async_mtq_signal.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_mtq_pool TRUE "" test_async_tpool.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_spio_rearr_contig_async TRUE "" test_spio_rearr_contig_async.cpp OBJ_LIBS test_common_obj)
  add_spio_executable (test_spio_contig_async_io_desc_info TRUE "" test_spio_contig_async_io_desc_info.cpp OBJ_LIBS test_common_obj)
  add_dependencies(tests test_mtq test_mtq_signal test_mtq_pool test_spio_rearr_contig_async
                    test_spio_contig_async_io_desc_info)
endif()

set (SPIO_TEST_MAX_NPROCS 4)

# Test Timeout in seconds.
if (PIO_VALGRIND_CHECK)
  set (DEFAULT_TEST_TIMEOUT 480)
else ()
  set (DEFAULT_TEST_TIMEOUT 240)
endif ()

# All tests need a certain number of tasks, but they should be able to
# run successfully with more than they need. Test this by providing an
# extra processor for each C test.
set (AT_LEAST_TWO_TASKS 3)
set (AT_LEAST_THREE_TASKS 4)
set (AT_LEAST_FOUR_TASKS 5)

add_test(NAME test_spio_ltimer COMMAND test_spio_ltimer)
add_test(NAME test_spio_serializer COMMAND test_spio_serializer)
add_test(NAME test_spio_tree COMMAND test_spio_tree)
add_test(NAME test_spio_file_mvcache COMMAND test_spio_file_mvcache)
add_test(NAME test_spio_dt_converter COMMAND test_spio_dt_converter)
add_test(NAME test_spio_select_io_desc_info COMMAND test_spio_select_io_desc_info)
add_test(NAME test_spio_async_tcomm COMMAND test_spio_async_tcomm)
add_test(NAME test_sdecomp_regex COMMAND test_sdecomp_regex)
add_test(NAME test_spio_sort_utils COMMAND test_spio_sort_utils)

if(PIO_USE_ASYNC_WR_THREAD)
  add_test(NAME test_mtq COMMAND test_mtq)
  add_test(NAME test_mtq_signal COMMAND test_mtq_signal)
  add_test(NAME test_mtq_pool COMMAND test_mtq_pool)
endif()

if (PIO_USE_MPISERIAL)
  add_test(NAME test_pioc
    COMMAND test_pioc)
else ()
  add_mpi_test(test_req_block_wait1
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_req_block_wait
    NUMPROCS 1
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_req_block_wait2
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_req_block_wait
    NUMPROCS 2
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_req_block_wait3
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_req_block_wait
    NUMPROCS 3
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_req_block_wait4
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_req_block_wait
    NUMPROCS 4
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  # Needs exactly 2 procs so a disjoint sync-only-vs-async-only I/O comm info
  # (see test_driver() in test_spio_iodesc_utils.cpp) can be constructed; a
  # no-op (always passes) when the library was not configured with WITH_HDF5.
  add_mpi_test(test_spio_iodesc_utils
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_iodesc_utils
    NUMPROCS 2
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_iosys_utils${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_iosys_utils
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_rearr_utils_gather${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_rearr_utils_gather
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_rearr_utils_scatter${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_rearr_utils_scatter
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_rearr_utils_alltoall${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_rearr_utils_alltoall
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_rearr_contig${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_rearr_contig
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  if(PIO_USE_ASYNC_WR_THREAD)
    # FIXME: Fix the comm split logic in the tests below so that it runs with
    # any number of procs

    # Needs at least 2 procs so the sync and async I/O comms used in this test can
    # genuinely have different sizes (see test_spio_rearr_contig_async.cpp)
    foreach(nproc RANGE 2 ${SPIO_TEST_MAX_NPROCS})
      add_mpi_test(test_spio_rearr_contig_async${nproc}
        EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_rearr_contig_async
        NUMPROCS ${nproc}
        TIMEOUT ${DEFAULT_TEST_TIMEOUT})
    endforeach()
    # Needs at least 2 procs so the maplen == 0 edge case can give one rank an
    # empty compmap while other ranks still have data (see
    # test_spio_contig_async_io_desc_info.cpp)
    foreach(nproc RANGE 2 ${SPIO_TEST_MAX_NPROCS})
      add_mpi_test(test_spio_contig_async_io_desc_info${nproc}
        EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_contig_async_io_desc_info
        NUMPROCS ${nproc}
        TIMEOUT ${DEFAULT_TEST_TIMEOUT})
    endforeach()
  endif()
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_rearr_contig_nvars${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_rearr_contig_nvars
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_rearr_contig_fillval${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_rearr_contig_fillval
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  foreach(nproc RANGE 1 ${SPIO_TEST_MAX_NPROCS})
    add_mpi_test(test_spio_decomp_logger${nproc}
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spio_decomp_logger
      NUMPROCS ${nproc}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endforeach()
  add_mpi_test(test_spmd
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_spmd
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_rearr
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_rearr
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_intercomm2
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_intercomm2
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_async_simple
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_async_simple
    NUMPROCS ${AT_LEAST_TWO_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_async_3proc
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_async_3proc
    NUMPROCS ${AT_LEAST_THREE_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_async_4proc
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_async_4proc
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_iosystem2_simple
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_iosystem2_simple
    NUMPROCS ${AT_LEAST_TWO_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_iosystem2_simple2
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_iosystem2_simple2
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_iosystem2
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_iosystem2
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_iosystem3_simple
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_iosystem3_simple
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_iosystem3_simple2
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_iosystem3_simple2
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_iosystem3
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_iosystem3
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_pioc
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_pioc
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_pioc_unlim
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_pioc_unlim
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_pioc_putget
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_pioc_putget
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_pioc_fill
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_pioc_fill
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_darray
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_darray_multi
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_multi
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_darray_multivar
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_multivar
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_darray_multivar2
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_multivar2
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_darray_1d
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_1d
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_darray_3d
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_3d
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  if (WITH_HDF5)
    # FIXME: Make sure that the test works with any number of MPI procs
    add_mpi_test(test_darray_hdf5_contig
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_hdf5_contig
      NUMPROCS ${AT_LEAST_FOUR_TASKS}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
    add_mpi_test(test_putget_hdf5
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_putget_hdf5
      NUMPROCS 1
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endif ()
  add_mpi_test(test_decomp_uneven
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_decomp_uneven
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  add_mpi_test(test_decomps
    EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_decomps
    NUMPROCS ${AT_LEAST_FOUR_TASKS}
    TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  if(PIO_USE_MALLOC)
    add_mpi_test(test_darray_async_simple
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_async_simple
      NUMPROCS ${AT_LEAST_FOUR_TASKS}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
    add_mpi_test(test_darray_async
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_async
      NUMPROCS ${AT_LEAST_FOUR_TASKS}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
    add_mpi_test(test_darray_async_many
      EXECUTABLE ${CMAKE_CURRENT_BINARY_DIR}/test_darray_async
      NUMPROCS ${AT_LEAST_FOUR_TASKS}
      TIMEOUT ${DEFAULT_TEST_TIMEOUT})
  endif ()
endif ()
