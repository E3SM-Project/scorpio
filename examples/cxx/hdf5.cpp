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
#ifdef _SPIO_HAS_H5Z_BLOSC2
#include "blosc2_filter.h"
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
    H5Eprint2(H5E_DEFAULT, stderr);
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

/* Test writing 3d distributed variables */
int test_hdf5_3d_dist_vars(MPI_Comm comm, int comm_rank, int comm_sz, hid_t fh, hid_t dcpid)
{
  int ret = 0;
  herr_t hret = 0;
  const char *dim_names[NDIMS] = {"time", "lev", "ncols"};
  hsize_t gdim_sz[NDIMS] = {H5S_UNLIMITED, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};

  /* =============== Define the variable dimensions ================ */
  float var_dist[DIM_TIME_SZ][DIM_LEV_SZ][DIM_NCOLS_SZ_PER_PROC];
  for(int i = 0; i < DIM_TIME_SZ; i++){
    for(int j = 0; j < DIM_LEV_SZ; j++){
      for(int k = 0; k < DIM_NCOLS_SZ_PER_PROC; k++){
        var_dist[i][j][k] = static_cast<float>(i * (DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC * comm_sz) + j * (DIM_NCOLS_SZ_PER_PROC * comm_sz) + (DIM_NCOLS_SZ_PER_PROC * comm_rank) + k);
      }
    }
  }

  /* =============== Define the variables ================ */

  /* Define the distributed variable - data distributed across processes */
  /* Create a property list for chunking across time dimension */
  hsize_t gchunk_dim_sz[NDIMS] = {1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hret = H5Pset_chunk(dcpid, NDIMS, gchunk_dim_sz); check_err(comm_rank, hret, "Setting chunking for time dimension of the variable failed", __LINE__);

  /* Define the variable dimensions */
  /* FIXME: Why does var_gdim_sz differ from gdim_sz : time dim is NOT allowed to be UNLIMITED */
  hsize_t var_gdim_sz[NDIMS] = {1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hid_t fsid;
  fsid = H5Screate_simple(NDIMS, var_gdim_sz, NULL); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Creating dataspace for defining varaible dimensions failed", __LINE__);

  hid_t varid_dist = H5Dcreate2(fh, "tmp_var_dist", H5T_NATIVE_FLOAT, fsid, H5P_DEFAULT, dcpid, H5P_DEFAULT); check_err(comm_rank, (varid_dist != H5I_INVALID_HID) ? 0 : -1, "Defining/Creating variable (distributed) failed", __LINE__);

  H5Sclose(fsid);

  /* =============== Write data ================= */

  hid_t dxpid = H5I_INVALID_HID;
  H5S_seloper_t op = H5S_SELECT_SET;
  hsize_t nelems = 0;
  hid_t msid = H5I_INVALID_HID;

  /* =============== Write distributed variable ================= */
  dxpid = H5Pcreate(H5P_DATASET_XFER); check_err(comm_rank, (dxpid != H5I_INVALID_HID) ? 0 : -1, "Creating dataset transfer property list for writing data failed", __LINE__);
  hret = H5Pset_dxpl_mpio(dxpid, H5FD_MPIO_COLLECTIVE); check_err(comm_rank, hret, "Setting dataset transfer property to MPIO Collective mode failed", __LINE__);

  fsid = H5Dget_space(varid_dist); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Getting dataspace associated with variable failed", __LINE__);
  for(int ilev = 0; ilev < DIM_LEV_SZ; ilev++){
    hsize_t starts[NDIMS] = {0, ilev, comm_rank * DIM_NCOLS_SZ_PER_PROC};
    hsize_t counts[NDIMS] = {1, 1, DIM_NCOLS_SZ_PER_PROC};
    hret = H5Sselect_hyperslab(fsid, op, starts, NULL, counts, NULL); check_err(comm_rank, hret, "Selecting hyperslab of the dataset to write failed", __LINE__);
    op = H5S_SELECT_OR;
  }

  nelems = DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC;
  msid = H5Screate_simple(1, &nelems, NULL); check_err(comm_rank, (msid != H5I_INVALID_HID) ? 0 : -1, "Creating memory space for write data failed", __LINE__);
  for(int tstep = 0; tstep < DIM_TIME_SZ; tstep++){
    hsize_t var_gdim_sz[NDIMS] = {tstep + 1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
    hret = H5Dextend(varid_dist, var_gdim_sz); check_err(comm_rank, hret, "Extending, across unlimited time dimension, dataspace for variable failed", __LINE__);
    hret = H5Dwrite(varid_dist, H5T_NATIVE_FLOAT, msid, fsid, dxpid, var_dist[tstep]); check_err(comm_rank, hret, "Writing data to variable failed", __LINE__);
  }
  H5Sclose(msid); H5Sclose(fsid); H5Pclose(dxpid);

  H5Dclose(varid_dist);

  return ret;
}

int test_hdf5_3d_non_dist_vars(MPI_Comm comm, int comm_rank, int comm_sz, hid_t fh, hid_t dcpid)
{
  int ret = 0;
  herr_t hret = 0;
  const char *dim_names[NDIMS] = {"time", "lev", "ncols"};
  hsize_t gdim_sz[NDIMS] = {H5S_UNLIMITED, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};

  /* =============== Define the variable dimensions ================ */
  float var_non_dist[DIM_TIME_SZ][DIM_LEV_SZ][DIM_NCOLS_SZ_PER_PROC * comm_sz];
  for(int i = 0; i < DIM_TIME_SZ; i++){
    for(int j = 0; j < DIM_LEV_SZ; j++){
      for(int k = 0; k < DIM_NCOLS_SZ_PER_PROC * comm_sz; k++){
        var_non_dist[i][j][k] = static_cast<float>(i * (DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC * comm_sz) + j * (DIM_NCOLS_SZ_PER_PROC * comm_sz) + k);
      }
    }
  }

  /* =============== Define the variables ================ */

  /* Define the non distributed variable - data is NOT distributed across processes (all data in all procs) */
  /* Create a property list for chunking across time dimension */
  hsize_t gchunk_dim_sz[NDIMS] = {1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hret = H5Pset_chunk(dcpid, NDIMS, gchunk_dim_sz); check_err(comm_rank, hret, "Setting chunking for time dimension of the variable failed", __LINE__);

  /* Define the variable dimensions */
  /* FIXME: Why does var_gdim_sz differ from gdim_sz : time dim is NOT allowed to be UNLIMITED */
  hsize_t var_gdim_sz[NDIMS] = {1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hid_t fsid;
  fsid = H5Screate_simple(NDIMS, var_gdim_sz, NULL); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Creating dataspace for defining varaible dimensions failed", __LINE__);

  hid_t varid_non_dist = H5Dcreate2(fh, "tmp_var_non_dist", H5T_NATIVE_FLOAT, fsid, H5P_DEFAULT, dcpid, H5P_DEFAULT); check_err(comm_rank, (varid_non_dist != H5I_INVALID_HID) ? 0 : -1, "Defining/Creating variable (not distributed) failed", __LINE__);

  H5Sclose(fsid);

  /* =============== Write data ================= */
  hid_t dxpid = H5I_INVALID_HID;
  H5S_seloper_t op = H5S_SELECT_SET;
  hsize_t nelems = 0;
  hid_t msid = H5I_INVALID_HID;

  /* =============== Write non-distributed variable ================= */
  dxpid = H5Pcreate(H5P_DATASET_XFER); check_err(comm_rank, (dxpid != H5I_INVALID_HID) ? 0 : -1, "Creating dataset transfer property list for writing non distributed data failed", __LINE__);

  /* Note: To mix H5FD_MPI_COLLECTIVE and H5FD_MPI_INDIVIDUAL data transfer properties for different variables users need to
   * explicitly turn off MPI I/O file syncing requirement in HDF5 (export HDF5_DO_MPI_FILE_SYNC=0)
   * Although mixing H5FD_MPI_COLLECTIVE & H5FD_MPI_INDIVIDUAL data transfer properties for different variables is supported,
   * H5FD_MPI_INDIVIDUAL data transfer property cannot be used for datasets with filters. Filters require collective (H5FD_MPI_COLLECTIVE)
   * data transfer property
   */
  hret = H5Pset_dxpl_mpio(dxpid, H5FD_MPIO_COLLECTIVE); check_err(comm_rank, hret, "Setting dataset transfer property to MPIO Collective mode failed", __LINE__);
  hret = H5Pset_dxpl_mpio_collective_opt(dxpid, H5FD_MPIO_INDIVIDUAL_IO); check_err(comm_rank, hret, "Setting dataset transferi collective property to Individual I/O failed", __LINE__);

  fsid = H5Dget_space(varid_non_dist); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Getting dataspace associated with variable failed", __LINE__);
  op = H5S_SELECT_SET;
  if(comm_rank == 0){
    for(int ilev = 0; ilev < DIM_LEV_SZ; ilev++){
      hsize_t starts[NDIMS] = {0, ilev, 0};
      hsize_t counts[NDIMS] = {1, 1, DIM_NCOLS_SZ_PER_PROC * comm_sz};
      hret = H5Sselect_hyperslab(fsid, op, starts, NULL, counts, NULL); check_err(comm_rank, hret, "Selecting hyperslab of the dataset to write failed", __LINE__);
      op = H5S_SELECT_OR;
    }
  }
  else{
    for(int ilev = 0; ilev < DIM_LEV_SZ; ilev++){
      hsize_t starts[NDIMS] = {0, 0, 0};
      hsize_t counts[NDIMS] = {0, 0, 0};
      hret = H5Sselect_hyperslab(fsid, op, starts, NULL, counts, NULL); check_err(comm_rank, hret, "Selecting hyperslab of the dataset to write failed", __LINE__);
      op = H5S_SELECT_OR;
    }
  }

  if(comm_rank == 0){
    nelems = DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC * comm_sz;
  }
  else{
    nelems = 0;
  }

  msid = H5Screate_simple(1, &nelems, NULL); check_err(comm_rank, (msid != H5I_INVALID_HID) ? 0 : -1, "Creating memory space for write data failed", __LINE__);
  for(int tstep = 0; tstep < DIM_TIME_SZ; tstep++){
    hsize_t var_gdim_sz[NDIMS] = {tstep + 1, DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
    hret = H5Dextend(varid_non_dist, var_gdim_sz); check_err(comm_rank, hret, "Extending, across unlimited time dimension, dataspace for variable failed", __LINE__);
    hret = H5Dwrite(varid_non_dist, H5T_NATIVE_FLOAT, msid, fsid, dxpid, var_non_dist[tstep]); check_err(comm_rank, hret, "Writing data to variable failed", __LINE__);
  }

  H5Sclose(msid); H5Sclose(fsid); H5Pclose(dxpid);

  H5Dclose(varid_non_dist);

  return ret;
}

int test_hdf5_3d_vars(MPI_Comm comm, int comm_rank, int comm_sz, hid_t fh, hid_t dcpid)
{
  int ret = 0;

  ret = test_hdf5_3d_dist_vars(comm, comm_rank, comm_sz, fh, dcpid); check_err(comm_rank, ret, "Writing 3D distributed variable failed", __LINE__);
  ret = test_hdf5_3d_non_dist_vars(comm, comm_rank, comm_sz, fh, dcpid); check_err(comm_rank, ret, "Writing 3D non-distributed variable failed", __LINE__);

  return ret;
}

int test_hdf5_2d_non_dist_vars(MPI_Comm comm, int comm_rank, int comm_sz, hid_t fh, hid_t dcpid)
{
  int ret = 0;
  herr_t hret = 0;

  const int NDIMS2 = 2;
  assert(NDIMS >= NDIMS2);

  const char *dim_names[NDIMS2] = {"lev", "ncols"};
  hsize_t gdim_sz[NDIMS2] = {DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};

  /* =============== Define the variable dimensions ================ */
  float var_non_dist[DIM_LEV_SZ][DIM_NCOLS_SZ_PER_PROC * comm_sz];
  for(int j = 0; j < DIM_LEV_SZ; j++){
    for(int k = 0; k < DIM_NCOLS_SZ_PER_PROC * comm_sz; k++){
      var_non_dist[j][k] = static_cast<float>(j * (DIM_NCOLS_SZ_PER_PROC * comm_sz) + k);
    }
  }

  /* =============== Define the variables ================ */

  /* Define the non distributed variable - data is NOT distributed across processes (all data in all procs) */
  /* Create a property list for chunking across lev dimension */
  hsize_t gchunk_dim_sz[NDIMS2] = {DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hret = H5Pset_chunk(dcpid, NDIMS2, gchunk_dim_sz); check_err(comm_rank, hret, "Setting chunking for lev dimension of the variable failed", __LINE__);

  /* Define the variable dimensions */
  /* FIXME: Why does var_gdim_sz differ from gdim_sz : time dim is NOT allowed to be UNLIMITED */
  hsize_t var_gdim_sz[NDIMS2] = {DIM_LEV_SZ, DIM_NCOLS_SZ_PER_PROC * comm_sz};
  hid_t fsid;
  fsid = H5Screate_simple(NDIMS2, var_gdim_sz, NULL); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Creating dataspace for defining varaible dimensions failed", __LINE__);

  hid_t varid_non_dist = H5Dcreate2(fh, "tmp_var_2d_non_dist", H5T_NATIVE_FLOAT, fsid, H5P_DEFAULT, dcpid, H5P_DEFAULT); check_err(comm_rank, (varid_non_dist != H5I_INVALID_HID) ? 0 : -1, "Defining/Creating variable (not distributed) failed", __LINE__);

  H5Sclose(fsid);

  /* =============== Write data ================= */
  hid_t dxpid = H5I_INVALID_HID;
  H5S_seloper_t op = H5S_SELECT_SET;
  hsize_t nelems = 0;
  hid_t msid = H5I_INVALID_HID;

  /* =============== Write non-distributed variable ================= */
  dxpid = H5Pcreate(H5P_DATASET_XFER); check_err(comm_rank, (dxpid != H5I_INVALID_HID) ? 0 : -1, "Creating dataset transfer property list for writing non distributed data failed", __LINE__);

  /* Note: To mix H5FD_MPI_COLLECTIVE and H5FD_MPI_INDIVIDUAL data transfer properties for different variables users need to
   * explicitly turn off MPI I/O file syncing requirement in HDF5 (export HDF5_DO_MPI_FILE_SYNC=0)
   * Although mixing H5FD_MPI_COLLECTIVE & H5FD_MPI_INDIVIDUAL data transfer properties for different variables is supported,
   * H5FD_MPI_INDIVIDUAL data transfer property cannot be used for datasets with filters. Filters require collective (H5FD_MPI_COLLECTIVE)
   * data transfer property
   */
  hret = H5Pset_dxpl_mpio(dxpid, H5FD_MPIO_COLLECTIVE); check_err(comm_rank, hret, "Setting dataset transfer property to MPIO Collective mode failed", __LINE__);
  hret = H5Pset_dxpl_mpio_collective_opt(dxpid, H5FD_MPIO_INDIVIDUAL_IO); check_err(comm_rank, hret, "Setting dataset transferi collective property to Individual I/O failed", __LINE__);

  fsid = H5Dget_space(varid_non_dist); check_err(comm_rank, (fsid != H5I_INVALID_HID) ? 0 : -1, "Getting dataspace associated with variable failed", __LINE__);
  op = H5S_SELECT_SET;
  if(comm_rank == 0){
    for(int ilev = 0; ilev < DIM_LEV_SZ; ilev++){
      hsize_t starts[NDIMS2] = {ilev, 0};
      hsize_t counts[NDIMS2] = {1, DIM_NCOLS_SZ_PER_PROC * comm_sz};
      hret = H5Sselect_hyperslab(fsid, op, starts, NULL, counts, NULL); check_err(comm_rank, hret, "Selecting hyperslab of the dataset to write failed", __LINE__);
      op = H5S_SELECT_OR;
    }
  }
  else{
    for(int ilev = 0; ilev < DIM_LEV_SZ; ilev++){
      hsize_t starts[NDIMS2] = {0, 0};
      hsize_t counts[NDIMS2] = {0, 0};
      hret = H5Sselect_hyperslab(fsid, op, starts, NULL, counts, NULL); check_err(comm_rank, hret, "Selecting hyperslab of the dataset to write failed", __LINE__);
      op = H5S_SELECT_OR;
    }
  }

  if(comm_rank == 0){
    nelems = DIM_LEV_SZ * DIM_NCOLS_SZ_PER_PROC * comm_sz;
  }
  else{
    nelems = 0;
  }

  msid = H5Screate_simple(1, &nelems, NULL); check_err(comm_rank, (msid != H5I_INVALID_HID) ? 0 : -1, "Creating memory space for write data failed", __LINE__);
  hret = H5Dwrite(varid_non_dist, H5T_NATIVE_FLOAT, msid, fsid, dxpid, var_non_dist); check_err(comm_rank, hret, "Writing data to variable failed", __LINE__);

  H5Sclose(msid); H5Sclose(fsid); H5Pclose(dxpid);

  H5Dclose(varid_non_dist);

  return ret;
}

int test_hdf5_2d_vars(MPI_Comm comm, int comm_rank, int comm_sz, hid_t fh, hid_t dcpid)
{
  int ret = 0;

  ret = test_hdf5_2d_non_dist_vars(comm, comm_rank, comm_sz, fh, dcpid); check_err(comm_rank, ret, "Writing 2D non-distributed variable failed", __LINE__);

  return ret;
}

int test_hdf5_vars(MPI_Comm comm, int comm_rank, int comm_sz, hid_t fh, hid_t dcpid)
{
  int ret = 0;
  herr_t hret = 0;
  const char *dim_names[NDIMS] = {"time", "lev", "ncols"};

  /* =============== Define the variable dimensions ================ */
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

  ret = test_hdf5_3d_vars(comm, comm_rank, comm_sz, fh, dcpid); check_err(comm_rank, ret, "Writing 3D variables failed", __LINE__);
  ret = test_hdf5_2d_vars(comm, comm_rank, comm_sz, fh, dcpid); check_err(comm_rank, ret, "Writing 2D variables failed", __LINE__);

  return ret;
}

int test_hdf5_file_ops(MPI_Comm comm, int comm_rank, int comm_sz, const std::string &fname, hid_t dcpid)
{
  hid_t fh;
  int ret = 0;
  herr_t hret = 0;
  const char *dim_names[NDIMS] = {"time", "lev", "ncols"};

  /* =============== Create the file ================= */
  hid_t fpid = H5Pcreate(H5P_FILE_ACCESS); check_err(comm_rank, (fpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating file failed", __LINE__);
  hret = H5Pset_fapl_mpio(fpid, comm, MPI_INFO_NULL); check_err(comm_rank, hret, "Setting file access property list failed", __LINE__);
  fh = H5Fcreate(fname.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, fpid); check_err(comm_rank, (fh != H5I_INVALID_HID) ? 0 : -1, "Creating file failed", __LINE__);
  H5Pclose(fpid);

  /* Test writing vars */
  ret = test_hdf5_vars(comm, comm_rank, comm_sz, fh, dcpid); check_err(comm_rank, ret, "Testing writing hdf5 vars failed", __LINE__);

  /* ============== Finalize / close file =============== */
  H5Fclose(fh);

  return ret;
}

int test_hdf5(MPI_Comm comm, int comm_rank, int comm_sz)
{
  std::string fname("spio_test_hdf5.h5");
  int ret = -1;
  hid_t dcpid = H5Pcreate(H5P_DATASET_CREATE); check_err(comm_rank, (dcpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating varaible failed", __LINE__);
  ret = test_hdf5_file_ops(comm, comm_rank, comm_sz, fname, dcpid);
  H5Pclose(dcpid);

  if((ret == 0) && (comm_rank == 0)){
    std::cout << "Testing " << fname.c_str() << " (no compression) : SUCCESS\n";
  }

  return ret;
}

int test_hdf5_zfp(MPI_Comm comm, int comm_rank, int comm_sz)
{
  std::string fname("spio_test_hdf5_zfp.h5");
  int ret = -1;
  herr_t hret;

#ifdef _SPIO_HAS_H5Z_ZFP
  double acc = 0.01;

  ret = H5Z_zfp_initialize(); check_err(comm_rank, ret, "Intializing HDF5 ZFP compression library failed", __LINE__);
  hid_t dcpid = H5Pcreate(H5P_DATASET_CREATE); check_err(comm_rank, (dcpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating varaible failed", __LINE__);
  hret = H5Pset_zfp_accuracy(dcpid, acc); check_err(comm_rank, hret, "Setting HDF5 ZFP compression accuracy on property list failed", __LINE__);
  ret = test_hdf5_file_ops(comm, comm_rank, comm_sz, fname, dcpid);
  H5Pclose(dcpid);
  H5Z_zfp_finalize();

  if((ret == 0) && (comm_rank == 0)){
    std::cout << "Testing " << fname.c_str() << " (with ZFP compression) : SUCCESS\n";
  }

#else
  if(comm_rank == 1){
    std::cout << "ERROR: HDF5 ZFP plugin not available...\n";
  }
#endif
  return ret;
}

int test_hdf5_zstd(MPI_Comm comm, int comm_rank, int comm_sz)
{
  std::string fname("spio_test_hdf5_zstd.h5");
  int ret = -1;
  herr_t hret;

#ifdef _SPIO_HAS_H5Z_BLOSC2
  char *version, *date;
  unsigned int cd_values[7];

  ret = register_blosc2(NULL, NULL); check_err(comm_rank, ret, "Registering HDF5 Blosc2 compression library failed", __LINE__);
  hid_t dcpid = H5Pcreate(H5P_DATASET_CREATE); check_err(comm_rank, (dcpid != H5I_INVALID_HID) ? 0 : -1, "Creating property list for creating varaible failed", __LINE__);
  cd_values[4] = 1; // compression level
  cd_values[5] = 1; // shuffle on
  cd_values[6] = BLOSC_ZSTD; // Use ZSTD for compression
  hret = H5Pset_filter(dcpid, FILTER_BLOSC2, H5Z_FLAG_OPTIONAL, 7, cd_values); check_err(comm_rank, hret, "Setting the BLOSC2 filter property list failed", __LINE__);
  ret = test_hdf5_file_ops(comm, comm_rank, comm_sz, fname, dcpid);
  H5Pclose(dcpid);

  if((ret == 0) && (comm_rank == 0)){
    std::cout << "Testing " << fname.c_str() << " (with BLOSC2+ZSTD compression) : SUCCESS\n";
  }

#else
  if(comm_rank == 1){
    std::cout << "ERROR: HDF5 BLOSC2 filter not available...\n";
  }
#endif
  return ret;
}

#endif

int main(int argc, char *argv[])
{
  MPI_Comm comm = MPI_COMM_WORLD;
  int comm_rank = -1, comm_sz = 0;
  int ret = NC_NOERR;

#if PIO_USE_HDF5
#ifdef SPIO_ENABLE_GPTL_TIMING
  GPTLinitialize();
#endif
  MPI_Init(&argc, &argv);
  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  ret = test_hdf5(comm, comm_rank, comm_sz);

  if(ret == NC_NOERR){
    if((argc > 1) && (strcmp(argv[1], "--test-zfp") == 0)){
      ret = test_hdf5_zfp(comm, comm_rank, comm_sz);
    }
    if((argc > 1) && (strcmp(argv[1], "--test-zstd") == 0)){
      ret = test_hdf5_zstd(comm, comm_rank, comm_sz);
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
