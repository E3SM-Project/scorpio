#include <stdio.h>
#include <limits.h>
#include <stdlib.h>
#include <libgen.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <mpi.h>
#include <pio.h>
#include "pio_config.h"
#include <assert.h>
#include <math.h>
#include <string.h>
#ifdef SPIO_ENABLE_GPTL_TIMING
#include <gptl.h>
#endif
#if PIO_USE_NETCDF4
#include <netcdf.h>
#endif

#if PIO_USE_NETCDF4
#define ERR(rank, ret, err_msg) do{\
  if(ret != NC_NOERR){\
    printf("[%d]: FATAL ERROR : ", rank); \
    printf(err_msg); \
    printf(" (err = %d, line no = %d)\n", ret, __LINE__);\
    return ret; \
  }\
}while(0)
#endif

#define NDIMS 3
#define DIM_TIME_SZ 1
#define DIM_LEV_SZ 5
#define DIM_NCOLS_SZ_PER_PROC 10
#define FILE_PATH_MAX_SZ PATH_MAX+1

#if PIO_USE_NETCDF4

int get_nczarr_fname(MPI_Comm comm, int comm_rank, int comm_sz,
                      const char *fname, size_t fname_sz, char *nczarr_fname, size_t nczarr_fname_sz)
{
  const char *nczarr_prefix = "file://";
  const char *nczarr_suffix = "#mode=nczarr,file";
  char real_fname[FILE_PATH_MAX_SZ + 1] = "\0";

  assert(nczarr_fname_sz >= fname_sz);

  int real_fname_len = 0;
  if(comm_rank == 0){
    char real_dname[FILE_PATH_MAX_SZ + 1];
    char dname[FILE_PATH_MAX_SZ + 1];
    char bname[FILE_PATH_MAX_SZ + 1];

    strncpy(dname, fname, FILE_PATH_MAX_SZ); dname[FILE_PATH_MAX_SZ] = '\0';
    strncpy(bname, fname, FILE_PATH_MAX_SZ); bname[FILE_PATH_MAX_SZ] = '\0';

    char *dname_ptr = dirname(dname);
    char *bname_ptr = basename(bname);

    char *real_dname_ptr = realpath(dname_ptr, real_dname);
    ERR(comm_rank, (real_dname_ptr != NULL) ? 0 : -1, "Converting relative file path/name to full/real path failed");

    snprintf(real_fname, FILE_PATH_MAX_SZ, "%s/%s", real_dname_ptr, bname_ptr);

    //int ret = mkdir(real_fname, S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IWGRP | S_IXGRP);
    //ERR(comm_rank, ((ret == -1) && (errno == EEXIST)) ? 0 : ret, "Creating directory, for NCZarr file, failed");

    real_fname_len = strlen(real_fname) + 1;
  }

  MPI_Bcast(&real_fname_len, 1, MPI_INT, 0, comm);
  assert(((int )nczarr_fname_sz) >= real_fname_len);

  MPI_Bcast(real_fname, real_fname_len, MPI_CHAR, 0, comm);

  snprintf(nczarr_fname, nczarr_fname_sz, "%s%s%s", nczarr_prefix, real_fname, nczarr_suffix);

  return NC_NOERR;
}

int test_nc_file_ops(MPI_Comm comm, const char *fname, size_t fname_sz, int omode)
{
  int ret = NC_NOERR;
  int fh = -1;
  int comm_rank, comm_sz;
  const char *dim_names[] = {"time", "lev", "ncols"};
  int dim_ids[NDIMS];
  int varid = -1;

  MPI_Comm_rank(comm, &comm_rank);
  MPI_Comm_size(comm, &comm_sz);

  ret = nc_create_par(fname, omode, comm, MPI_INFO_NULL, &fh); ERR(comm_rank, ret, "Creating file failed");

  size_t dim_sz[NDIMS] = {DIM_TIME_SZ, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  for(int i=0; i < NDIMS; i++){
    ret = nc_def_dim(fh, dim_names[i], dim_sz[i], &dim_ids[i]); ERR(comm_rank, ret, "Defining dimension in file failed");
  }

  double var[DIM_TIME_SZ][DIM_LEV_SZ][DIM_NCOLS_SZ_PER_PROC];
  for(int i = 0; i < DIM_TIME_SZ; i++){
    for(int j = 0; j < DIM_LEV_SZ; j++){
      for(int k = 0; k < DIM_NCOLS_SZ_PER_PROC; k++){
        var[i][j][k] = i * (DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC * comm_sz) + j * (DIM_NCOLS_SZ_PER_PROC * comm_sz) + (DIM_NCOLS_SZ_PER_PROC * comm_rank) + k;
      }
    }
  }
  ret = nc_def_var(fh, "tmp_var", NC_DOUBLE, NDIMS, dim_ids, &varid); ERR(comm_rank, ret, "Defining variable (\"tmp_var\") failed");

  ret = nc_enddef(fh); ERR(comm_rank, ret, "Ending define mode failed");

  char info[] = "SCORPIO test";
  ret = nc_put_att_text(fh, NC_GLOBAL, "info", strlen(info) + 1, info); ERR(comm_rank, ret, "Adding global attribute (\"info\" failed");

  ret = nc_var_par_access(fh, varid, NC_COLLECTIVE); ERR(comm_rank, ret, "Setting parallel/collective access for variable failed");

  size_t starts[NDIMS] = {0, 0, comm_rank * DIM_NCOLS_SZ_PER_PROC};
  size_t counts[NDIMS] = {DIM_TIME_SZ, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC};
  ret = nc_put_vara_double(fh, varid, starts, counts, (const double *)var); ERR(comm_rank, ret, "Writing variable in parallel failed");

  ret = nc_close(fh); ERR(comm_rank, ret, "Closing file failed");

  if(comm_rank == 0){
    printf("Testing %s : SUCCESS\n", fname); fflush(stdout);
  }

  return ret;
}

int test_netcdf4p(MPI_Comm comm)
{
  int omode = NC_MPIIO | NC_CLOBBER | NC_NETCDF4;
  const char fname[] = "spio_test_netcdf4p.nc";

  return test_nc_file_ops(comm, fname, strlen(fname) + 1, omode);
}

int test_netcdf4p_nczarr(MPI_Comm comm)
{
  int ret = NC_NOERR;
  int omode = NC_CLOBBER | NC_NETCDF4;
  const char fname[] = "./spio_test_netcdf4p_nczarr.file";
  char nczarr_fname[FILE_PATH_MAX_SZ + 1] = "\0";
  int comm_rank, comm_sz;

  MPI_Comm_rank(comm, &comm_rank);
  MPI_Comm_size(comm, &comm_sz);

  ret = get_nczarr_fname(comm, comm_rank, comm_sz, fname, strlen(fname) + 1, nczarr_fname, FILE_PATH_MAX_SZ + 1);
  ERR(comm_rank, ret, "Getting NCZarr file name failed");

  return test_nc_file_ops(comm, nczarr_fname, strlen(nczarr_fname) + 1, omode);
}
#endif

int main(int argc, char *argv[])
{
  int ret = NC_NOERR;

#if PIO_USE_NETCDF4
#ifdef SPIO_ENABLE_GPTL_TIMING
  GPTLinitialize();
#endif
  MPI_Init(&argc, &argv);

  ret = test_netcdf4p(MPI_COMM_WORLD);

  if(ret == NC_NOERR){
    if((argc > 1) && (strcmp(argv[1], "--test-nczarr") == 0)){
      ret = test_netcdf4p_nczarr(MPI_COMM_WORLD);
    }
  }

  MPI_Finalize();
#ifdef SPIO_ENABLE_GPTL_TIMING
  GPTLfinalize();
#endif
#endif

  return ret;
}
