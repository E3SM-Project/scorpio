#include <iostream>
#include <string>
#include <cassert>
#include <stdexcept>

#include <mpi.h>
#include <pio.h>
#include "pio_config.h"
#include <cassert>
#ifdef SPIO_ENABLE_GPTL_TIMING
#include <gptl.h>
#endif
#if PIO_USE_HDF5
#include <hdf5.h>
#ifdef _SPIO_HAS_H5Z_ZFP
#include "H5Zzfp_lib.h"
#include "H5Zzfp_props.h"
#endif
#endif

template<typename T>
static inline void check_err(int rank, T ret, const char *err_msg, int line_num)
{
  if(ret < 0){
    std::string err_msg = std::string("[") + std::to_string(rank) + std::string("]: FATAL ERROR : ")
      + err_msg + std::string(" (err = ") + std::to_string(static_cast<int>(ret))
      + std::string(", line no = ") + std::to_string(line_num) + std::string(")");
#if PIO_USE_HDF5
//    H5EPrint(H5E_DEFAULT, stderr);
#endif
    throw std::runtime_error(err_msg.c_str());
  }
}

template<typename T>
static inline void check_err_print_rank0(int rank, T ret, int line_num, const char *err_msg)
{
  if(rank == 0){
    check_err(rank, ret, line_num, err_msg);
  }
}

#define NDIMS 3
#define DIM_TIME_SZ 1
#define DIM_LEV_SZ 5
#define DIM_NCOLS_SZ_PER_PROC 10

#if PIO_USE_HDF5

int test_hdf5_file_ops(MPI_Comm comm, int comm_rank, const std::string &fname, hid_t dcpid)
{
  hid_t fh;
  int ret = 0;
  herr_t hret = 0;
  int comm_sz;
  const char *dim_names[NDIMS] = {"time", "lev", "ncols"};

  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  /* =============== Create the file ================= */
  hid_t fpid = H5Pcreate(H5P_FILE_ACCESS); check_err(comm_rank, (fpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating file failed", __LINE__);
  hret = H5Pset_fapl_mpio(fpid, comm, MPI_INFO_NULL); check_err(comm_rank, hret, "Setting file access property list failed", __LINE__);
  fh = H5Fcreate(fname.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, fpid); check_err(comm_rank, (fh != H5I_INVALID_HID) ? 0 : -1, "Creating file failed", __LINE__);
  H5Pclose(fpid);

  /* =============== Defined the variable dimensions ================ */
  /* Define the dimensions of the variable - as attributes in HDF5 */
  hid_t sid = H5Screate(H5S_SCALAR); check_err(comm_rank, (sid != H5I_INVALID_HID) ? 0 : -1, "Creating scalar dataspace for defining dimensions failed", __LINE__);
  //hsize_t gdim_sz[NDIMS] = {DIM_TIME_SZ, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hsize_t gdim_sz[NDIMS] = {H5S_UNLIMITED, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  for(int i=0; i < NDIMS; i++){
    hsize_t sz = gdim_sz[i];
    hid_t aid = H5Acreate2(fh, dim_names[i], H5T_NATIVE_HSIZE, sid, H5P_DEFAULT, H5P_DEFAULT); check_err(comm_rank, (fh != H5I_INVALID_HID) ? 0 : -1, "Creating attribute for dimension failed", __LINE__);
    hret = H5Awrite(aid, H5T_NATIVE_HSIZE, &sz); check_err(comm_rank, hret, "Writing attribute for dimension failed", __LINE__);
    H5Aclose(aid);
  }
  H5Sclose(sid);

  float var[DIM_TIME_SZ][DIM_LEV_SZ][DIM_NCOLS_SZ_PER_PROC];
  for(int i = 0; i < DIM_TIME_SZ; i++){
    for(int j = 0; j < DIM_LEV_SZ; j++){
      for(int k = 0; k < DIM_NCOLS_SZ_PER_PROC; k++){
        var[i][j][k] = i * (DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC * comm_sz) + j * (DIM_NCOLS_SZ_PER_PROC * comm_sz) + (DIM_NCOLS_SZ_PER_PROC * comm_rank) + k;
      }
    }
  }

  /* =============== Defined the variable ================ */

  /* Create a property list for chunking across time dimension */
  hsize_t gchunk_dim_sz[NDIMS] = {1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  //hid_t dcpid = H5Pcreate(H5P_DATASET_CREATE); check_err(comm_rank, (dcpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating varaible failed", __LINE__);
  hret = H5Pset_chunk(dcpid, NDIMS, gchunk_dim_sz); check_err(comm_rank, hret, "Setting chunking for time dimension of the variable failed", __LINE__);

  /* Define the variable dimensions */
  /* FIXME: Why does var_gdim_sz differ from gdim_sz : time dim is NOT allowed to be UNLIMITED */
  hsize_t var_gdim_sz[NDIMS] = {1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hid_t fsid;
  fsid = H5Screate_simple(NDIMS, var_gdim_sz, NULL); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Creating dataspace for defining varaible dimensions failed", __LINE__);

  hid_t varid = H5Dcreate2(fh, "tmp_var", H5T_NATIVE_FLOAT, fsid, H5P_DEFAULT, dcpid, H5P_DEFAULT); check_err(comm_rank, (varid != H5I_INVALID_HID) ? 0 : -1, "Defining/Creating variable failed", __LINE__);

  H5Sclose(fsid);
  //H5Pclose(dcpid);

  /* =============== Write data ================= */
  hid_t dxpid = H5Pcreate(H5P_DATASET_XFER); check_err(comm_rank, (dxpid != H5I_INVALID_HID) ? 0 : -1, "Creating dataset transfer property list for writing data failed", __LINE__);
  hret = H5Pset_dxpl_mpio(dxpid, H5FD_MPIO_COLLECTIVE); check_err(comm_rank, hret, "Setting dataset transfer property to MPIO Collective mode failed", __LINE__);

  //int num_reqs = DIM_TIME_SZ;

  /*
  size_t starts[NDIMS] = {0, 0, comm_rank * DIM_NCOLS_SZ_PER_PROC};
  size_t counts[NDIMS] = {DIM_TIME_SZ, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC};
  ret = nc_put_vara_double(fh, varid, starts, counts, (const double *)var); ERR(comm_rank, ret, "Writing variable in parallel failed");
  */

  fsid = H5Dget_space(varid); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Getting dataspace associated with variable failed", __LINE__);
  H5S_seloper_t op = H5S_SELECT_SET;
  for(int ilev = 0; ilev < DIM_LEV_SZ; ilev++){
    hsize_t starts[NDIMS] = {0, ilev, comm_rank * DIM_NCOLS_SZ_PER_PROC};
    hsize_t counts[NDIMS] = {1, 1, DIM_NCOLS_SZ_PER_PROC};
    hret = H5Sselect_hyperslab(fsid, op, starts, NULL, counts, NULL); check_err(comm_rank, hret, "Selecting hyperslab of the dataset to write failed", __LINE__);
    op = H5S_SELECT_OR;
  }

  hsize_t nelems = DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC;
  hid_t msid = H5Screate_simple(1, &nelems, NULL); check_err(comm_rank, (msid != H5I_INVALID_HID) ? 0 : -1, "Creating memory space for write data failed", __LINE__);
  for(int tstep = 0; tstep < DIM_TIME_SZ; tstep++){
    hsize_t var_gdim_sz[NDIMS] = {tstep + 1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
    hret = H5Dextend(varid, var_gdim_sz); check_err(comm_rank, hret, "Extending, across unlimited time dimension, dataspace for variable failed", __LINE__);
    hret = H5Dwrite(varid, H5T_NATIVE_FLOAT, msid, fsid, dxpid, var[tstep]); check_err(comm_rank, hret, "Writing data to variable failed", __LINE__);
  }
  H5Sclose(msid); H5Sclose(fsid);

  /* ============== Finalize / close file =============== */
  H5Pclose(dxpid); H5Dclose(varid); H5Fclose(fh);


  return ret;
}

int test_hdf5(MPI_Comm comm, int comm_rank)
{
  std::string fname("spio_test_hdf5.h5");
  int ret = -1;
  hid_t dcpid = H5Pcreate(H5P_DATASET_CREATE); check_err(comm_rank, (dcpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating varaible failed", __LINE__);
  ret = test_hdf5_file_ops(comm, comm_rank, fname, dcpid);
  H5Pclose(dcpid);

  if((ret == 0) && (comm_rank == 0)){
    std::cout << "Testing " << fname.c_str() << " (no compression) : SUCCESS\n";
  }

  return ret;
}

int test_hdf5_zfp(MPI_Comm comm, int comm_rank)
{
  std::string fname("spio_test_hdf5.h5");
  int ret = -1;
  herr_t hret;

#ifdef _SPIO_HAS_H5Z_ZFP
  double acc = 0.01;

  ret = H5Z_zfp_initialize(); check_err(comm_rank, ret, "Intializing HDF5 ZFP compression library failed", __LINE__);
  hid_t dcpid = H5Pcreate(H5P_DATASET_CREATE); check_err(comm_rank, (dcpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating varaible failed", __LINE__);
  hret = H5Pset_zfp_accuracy(dcpid, acc); check_err(comm_rank, hret, "Setting HDF5 ZFP compression accuracy on property list failed", __LINE__);
  ret = test_hdf5_file_ops(comm, comm_rank, fname, dcpid);
  H5Pclose(dcpid);
  H5Z_zfp_finalize();

  if((ret == 0) && (comm_rank == 0)){
    std::cout << "Testing " << fname.c_str() << " (with compression) : SUCCESS\n";
  }

#else
  if(comm_rank == 1){
    std::cout << "ERROR: HDF5 ZFP plugin not available...\n";
  }
#endif
  return ret;
}

#endif

int main(int argc, char *argv[])
{
  MPI_Comm comm = MPI_COMM_WORLD;
  int comm_rank = -1;
  int ret = NC_NOERR;

#if PIO_USE_HDF5
#ifdef SPIO_ENABLE_GPTL_TIMING
  GPTLinitialize();
#endif
  MPI_Init(&argc, &argv);
  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);

  ret = test_hdf5(comm, comm_rank);

  if(ret == NC_NOERR){
    if((argc > 1) && (strcmp(argv[1], "--test-zfp") == 0)){
      ret = test_hdf5_zfp(comm, comm_rank);
    }
  }

  if(ret != 0){
    if(comm_rank == 0){
      std::cerr << "ERROR: Test failed, ret = " << ret << "\n" << std::flush;
    }
  }
  else{
    if(comm_rank == 0){
      std::cout << "Test SUCCESS\n" << std::flush;
    }
  }

  MPI_Finalize();
#ifdef SPIO_ENABLE_GPTL_TIMING
  GPTLfinalize();
#endif
#endif

  return ret;
}
