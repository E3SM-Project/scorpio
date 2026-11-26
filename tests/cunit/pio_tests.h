/**
 * @file
 * Include file for tests for the Parallel IO library.
 */

#ifndef _PIO_TESTS_H
#define _PIO_TESTS_H

#include <unistd.h> /* Include this for the sleep function. */
#include <assert.h>

/* Timing header may need to be included. */
#ifdef TIMING
#include <gptl.h>
#endif

/** The number of possible output netCDF output flavors available to
 * the ParallelIO library. */
#define NUM_FLAVORS 4

/** Number of netCDF types. */
#define NUM_NETCDF_TYPES 12

/* Number of NetCDF classic types. */
#define NUM_CLASSIC_TYPES 6

/* Number of NetCDF-4 types. */
#define NUM_NETCDF4_TYPES 12

/* Number of PIO rearrangers. */
#define NUM_REARRANGERS 2

/* Number of sample files constructed for these tests. */
#define NUM_SAMPLES 5

/** Error code for when things go wrong. */
#define ERR_CHECK 1109
#define ERR_INIT 1110
#define ERR_AWFUL 1111
#define ERR_WRONG 1112
#define ERR_GPTL 1113
#define ERR_MPI 1114

/** The meaning of life, the universe, and everything. */
#define TEST_VAL_42 42

/** Handle MPI errors. This should only be used with MPI library
 * function calls. */
#define MPIERR(e) do {                                                  \
        char err_buffer[MPI_MAX_ERROR_STRING];                          \
        int resultlen;                                                  \
                                                                        \
        MPI_Error_string(e, err_buffer, &resultlen);                    \
        fprintf(stderr, "MPI error, line %d, file %s: %s\n", __LINE__, __FILE__, err_buffer); \
        MPI_Finalize();                                                 \
        return ERR_AWFUL;                                               \
    } while (0)

/** Handle non-MPI errors by finalizing the MPI library and exiting
 * with an exit code. */
#define ERR(e) do {                                                     \
        fprintf(stderr, "%d Error %d in %s, line %d\n", my_rank, e, __FILE__, __LINE__); \
        MPI_Finalize();                                                 \
        return e;                                                       \
    } while (0)

/* Initialize MPI for a cunit test. When PIO_USE_ASYNC_WR_THREAD is
 * enabled the HDF5 async write path makes MPI calls directly from its
 * own worker threads, concurrently with calls made by the main thread -
 * this requires MPI_THREAD_MULTIPLE, which plain MPI_Init() does not
 * request (and is not required to provide). Mirrors the --threaded
 * code path the Fortran test generator (pio_tf_f90gen.pl) already uses
 * for the same macro. Returns the same contract as MPI_Init(): 0 on
 * success, or a nonzero code to pass to MPIERR()/report as a failure.
 *
 * A header-only (static inline) helper, not a test_common_obj function,
 * since several cunit test executables do not link test_common_obj. */
static inline int pio_test_mpi_init(int *argc, char ***argv)
{
    int ret;

#if PIO_USE_ASYNC_WR_THREAD
    int provided;

    if ((ret = MPI_Init_thread(argc, argv, MPI_THREAD_MULTIPLE, &provided)))
        return ret;

    if (provided < MPI_THREAD_MULTIPLE)
    {
        fprintf(stderr, "ERROR: MPI_Init_thread() could not provide MPI_THREAD_MULTIPLE "
                "(got %d); required since PIO_USE_ASYNC_WR_THREAD is enabled and the "
                "async HDF5 write path makes MPI calls directly from its own worker "
                "threads, concurrently with the main thread\n", provided);
        return ERR_AWFUL;
    }

    return 0;
#else
    return MPI_Init(argc, argv);
#endif
}

#if defined(__cplusplus)
extern "C" {
#endif

/* Function prototypes. */
int pio_test_init(int argc, char **argv, int *my_rank, int *ntasks, int target_ntasks, MPI_Comm *test_comm);
int pio_test_init2(int argc, char **argv, int *my_rank, int *ntasks, int min_ntasks,
                   int max_ntasks, int log_level, MPI_Comm *test_comm);
int create_nc_sample(int sample, int iosysid, int format, char *filename, int my_rank, int *ncid);
int check_nc_sample(int sample, int iosysid, int format, char *filename, int my_rank, int *ncid);
int create_nc_sample_0(int iosysid, int format, char *filename, int my_rank, int *ncid);
int check_nc_sample_0(int iosysid, int format, char *filename, int my_rank, int *ncid);
int create_nc_sample_1(int iosysid, int format, char *filename, int my_rank, int *ncid);
int check_nc_sample_1(int iosysid, int format, char *filename, int my_rank, int *ncid);
int create_nc_sample_2(int iosysid, int format, char *filename, int my_rank, int *ncid);
int check_nc_sample_2(int iosysid, int format, char *filename, int my_rank, int *ncid);
int create_nc_sample_64bit_offset(int iosysid, int format, char *filename, int my_rank, int *ncid);
int check_nc_sample_64bit_offset(int iosysid, int format, char *filename, int my_rank, int *ncid);
int create_nc_sample_64bit_data(int iosysid, int format, char *filename, int my_rank, int *ncid);
int check_nc_sample_64bit_data(int iosysid, int format, char *filename, int my_rank, int *ncid);
int get_iotypes(int *num_flavors, int *flavors);
int get_iotype_name(int iotype, char *name);
int pio_test_finalize(MPI_Comm *test_comm);
int test_async2(int my_rank, int num_flavors, int *flavor, MPI_Comm test_comm,
                int component_count, int num_io_procs, int target_ntasks, char *test_name);
int test_no_async2(int my_rank, int num_flavors, int *flavor, MPI_Comm test_comm, int target_ntasks,
                   int x_dim_len, int y_dim_len);
int test_all(int iosysid, int num_flavors, int *flavor, int my_rank, MPI_Comm test_comm,
             int async);
int run_test_main(int argc, char **argv, int min_ntasks, int max_ntasks,
                  int log_level, char *test_name, int *dim_len, int component_count,
                  int num_io_procs);

/* Create a 2D decomposition used in some tests. */
int create_decomposition_2d(int ntasks, int my_rank, int iosysid, int *dim_len_2d, int *ioid,
                            int pio_type);

#if defined(__cplusplus)
}
#endif

#endif /* _PIO_TESTS_H */
