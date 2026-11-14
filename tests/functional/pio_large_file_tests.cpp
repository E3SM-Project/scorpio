#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <functional>
#include <utility>

#include "pio.h"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#include "spio_test_framework.hpp"
#include "spio_test_check.hpp"
#include "spio_test_common.hpp"

/* Information about a PIO file format used to test large files/vars */
struct Pio_file_fmt_info {
  std::string name;
  int pio_flag;
  /* Maximum record size, in bytes, for the file format */
  PIO_Offset max_rec_sz;
  PIO_Offset max_large_rec_sz;
};

/* Fill in the supported file formats and the related information */
std::vector<Pio_file_fmt_info> init_file_fmts()
{
  const PIO_Offset ONE_K = static_cast<PIO_Offset>(1) << 10;
  const PIO_Offset TWO_G = static_cast<PIO_Offset>(1) << 31;
  const PIO_Offset FOUR_G = static_cast<PIO_Offset>(1) << 32;

  std::vector<Pio_file_fmt_info> pio_file_fmts = {
    /* max number represented by a 32-bit signed integer = 2**31 - 1
     * NetCDF documentation says max_rec_sz should be 2**31 - 4, but
     * it no longer works, so trying out 2**31 - 2**10 instead.
     * max_large_rec_sz is the max size of another variable that could be
     * created if we already have a var of size (2GiB - 1KiB) and a dummy
     * var of 10 elements
     */
    {"CDF1", PIO_CLOBBER, TWO_G - ONE_K, ONE_K / 2 - 1},
    /* max record size, CDF2 = 2**32-4 */
    {"CDF2", PIO_CLOBBER | PIO_64BIT_OFFSET, TWO_G - 1, FOUR_G - 4},
    /* max record size, CDF5 = ??, try larger than 4G */
    {"CDF5", PIO_CLOBBER | PIO_64BIT_DATA, TWO_G - 1, FOUR_G + 1}
  };

  return pio_file_fmts;
}

/* Perform 1d writes and reads for all supported file types (CDF1, CDF2, CDF5).
 * This mirrors the Fortran source (tests/general/pio_large_file_tests.F90.in),
 * which disables this test via an unconditional early RETURN before
 * performing any I/O - so this port is just a stub that logs and returns.
 */
template<typename T>
int nc_wr_rd_large_1d_bc(SPIO_TF::Test_framework &tf)
{
  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO, "Skipping the write test...\n");
  return PIO_NOERR;
}

/* Define large vars (near the max record size of the file format) for all
 * supported file types (CDF1, CDF2, CDF5) - the file is not written to, just
 * defined and closed.
 */
template<typename T>
int nc_def_large_1d_bc(SPIO_TF::Test_framework &tf)
{
  std::vector<Pio_file_fmt_info> pio_file_fmts = init_file_fmts();

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename = "test_pio_large_file_tests.testfile";
  const int DUMMY_DIM_SZ = 10;

  for(std::size_t f = 0; f < pio_file_fmts.size(); f++){
    const Pio_file_fmt_info &fmt = pio_file_fmts[f];
    for(int i = 0; i < num_flavors; i++){
      int ncid = -1, iotype = flavors[i];
      int pio_dim = -1, pio_large_dim = -1, pio_last_var_dim = -1;
      int pio_var = -1, pio_large_var = -1, pio_last_var = -1;
      char iotype_name[PIO_MAX_NAME + 1];

      get_iotype_name(iotype, iotype_name);
      tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
          std::string("Testing : PIO file format : ") + fmt.name +
          " : PIO file format : " + iotype_name + "\n");

      PIO_Offset nbytes = static_cast<PIO_Offset>(sizeof(T));
      /* max_rec_sz is in bytes, find the max dim allowed for this file format */
      int dim = static_cast<int>(fmt.max_rec_sz / nbytes);
      PIO_Offset large_dim = fmt.max_large_rec_sz / nbytes;
      /* The last var/dim is just a place holder var */
      int last_var_dim_sz = DUMMY_DIM_SZ;

      int ret = PIOc_createfile(tf.get_iosysid(), &ncid, &iotype, filename.c_str(), fmt.pio_flag);
      if(!check_ret(tf, ret, "Could not create file " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", (PIO_Offset)dim, &pio_dim);
      if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_def_dim(ncid, "PIO_TF_test_large_dim", large_dim, &pio_large_dim);
      if(!check_ret(tf, ret, "Failed to define a dim (large dim) : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_def_dim(ncid, "PIO_TF_test_last_var_dim", (PIO_Offset)last_var_dim_sz, &pio_last_var_dim);
      if(!check_ret(tf, ret, "Failed to define a dim (for last dummy var) : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_def_var(ncid, "PIO_TF_test_var", Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
      if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_def_var(ncid, "PIO_TF_test_large_var", Pio_type_map<T>::value, 1, &pio_large_dim, &pio_large_var);
      if(!check_ret(tf, ret, "Failed to define a large var : " + filename)){
        return PIO_EINTERNAL;
      }

      /* The last var is special, with relaxed constraints - we just define
       * it and ignore it. All tests are done for the other var
       */
      ret = PIOc_def_var(ncid, "PIO_TF_test_last_var", Pio_type_map<T>::value, 1, &pio_last_var_dim, &pio_last_var);
      if(!check_ret(tf, ret, "Failed to define the last var : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_enddef(ncid);
      if(!check_ret(tf, ret, "Failed to end def mode : " + filename)){
        return PIO_EINTERNAL;
      }

      PIOc_closefile(ncid);

      /* Note: Unlike nc_write_large_1d_fval() below, the file is
       * intentionally NOT deleted here - matches the Fortran source, which
       * leaves the PIO_deletefile() call commented out.
       */
    }
  }

  return PIO_NOERR;
}

/* Write a large 1d array with fillvalues (the holes are implicit) */
template<typename T>
int nc_write_large_1d_fval(SPIO_TF::Test_framework &tf)
{
  const int VEC_LOCAL_SZ = 1000000;
  /* The buffer fillvalue to be used when writing data */
  const T BUF_FILLVAL = static_cast<T>(-2);

  /* compdof is only specified for valid data values, the data holes are
   * implicitly stated (by not specifying them rather than filling it with 0s)
   */
  int wcompdof_sz = std::min(tf.get_comm_rank() + 1, VEC_LOCAL_SZ);
  std::vector<PIO_Offset> wcompdof(wcompdof_sz);
  for(int i = 0; i < wcompdof_sz; i++){
    wcompdof[i] = static_cast<PIO_Offset>(VEC_LOCAL_SZ) * tf.get_comm_rank() + (i + 1);
  }

  /* Read everything - including fillvalues that should have been written
   * for locations unspecified in wcompdof(:), i.e., wcompdof[wcompdof_sz:VEC_LOCAL_SZ)
   */
  std::vector<PIO_Offset> rcompdof(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    rcompdof[i] = static_cast<PIO_Offset>(VEC_LOCAL_SZ) * tf.get_comm_rank() + (i + 1);
  }

  std::vector<T> wbuf(wcompdof_sz);
  std::vector<T> exp_val(VEC_LOCAL_SZ);
  /* The first wcompdof_sz values, wbuf[0:wcompdof_sz), are valid in wbuf */
  for(int i = 0; i < wcompdof_sz; i++){
    wbuf[i] = static_cast<T>(wcompdof[i]);
    exp_val[i] = wbuf[i];
  }
  /* We expect the values [wcompdof_sz:VEC_LOCAL_SZ) to be read as the user
   * specified fill value
   */
  for(int i = wcompdof_sz; i < VEC_LOCAL_SZ; i++){
    exp_val[i] = BUF_FILLVAL;
  }

  std::vector<T> rbuf(VEC_LOCAL_SZ, static_cast<T>(0));

  int dim = VEC_LOCAL_SZ * tf.get_comm_size();

  int wioid = -1, rioid = -1;
  int ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          wcompdof_sz, wcompdof.data(), &wioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (wr decomp)")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, &dim,
          VEC_LOCAL_SZ, rcompdof.data(), &rioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED (rd decomp)")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename = "test_pio_decomp_fillval_tests.testfile";

  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, pio_dim = -1, pio_var = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : PIO_TF_DATA_TYPE : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", (PIO_Offset)dim, &pio_dim);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, "PIO_TF_test_var", Pio_type_map<T>::value, 1, &pio_dim, &pio_var);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    /* Write the variable out, user specified fillvalue = BUF_FILLVAL */
    ret = PIOc_write_darray(ncid, pio_var, wioid, wcompdof_sz, wbuf.data(), &BUF_FILLVAL);
    if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, "PIO_TF_test_var", &pio_var);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = static_cast<T>(0);
    }

    ret = PIOc_read_darray(ncid, pio_var, rioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, exp_val, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    PIOc_closefile(ncid);

    /* Note: Unlike nc_def_large_1d_bc() above, the file IS deleted here */
    PIOc_deletefile(tf.get_iosysid(), filename.c_str());
  }

  PIOc_freedecomp(tf.get_iosysid(), rioid);
  PIOc_freedecomp(tf.get_iosysid(), wioid);

  return PIO_NOERR;
}

int main(int argc, char *argv[])
{
  int ret = MPI_SUCCESS, nerrs = 0;
#if PIO_USE_ASYNC_WR_THREAD
  int mpi_thread_support_provided = MPI_THREAD_SINGLE;
  ret = MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &mpi_thread_support_provided);
#else
  ret = MPI_Init(&argc, &argv);
#endif
  if(ret != MPI_SUCCESS){
    std::cerr << "MPI_Init() FAILED, ret = " << ret << "\n";
    return ret;
  }

  std::vector<std::pair<std::string, std::function<int(SPIO_TF::Test_framework &)> > > rfuncs = {
    {"nc_wr_rd_large_1d_bc_int", nc_wr_rd_large_1d_bc<int>},
    {"nc_wr_rd_large_1d_bc_float", nc_wr_rd_large_1d_bc<float>},
    {"nc_wr_rd_large_1d_bc_double", nc_wr_rd_large_1d_bc<double>},
    {"nc_def_large_1d_bc_int", nc_def_large_1d_bc<int>},
    {"nc_def_large_1d_bc_float", nc_def_large_1d_bc<float>},
    {"nc_def_large_1d_bc_double", nc_def_large_1d_bc<double>},
    {"nc_write_large_1d_fval_int", nc_write_large_1d_fval<int>},
    {"nc_write_large_1d_fval_float", nc_write_large_1d_fval<float>},
    {"nc_write_large_1d_fval_double", nc_write_large_1d_fval<double>}
  };

  SPIO_TF::Test_framework tf("pio_large_file_tests_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  if(tf.init_iosystem(PIO_REARR_BOX) == PIO_NOERR){
    /* Fortran's PIO_TF test driver defaults to PIO_BCAST_ERROR (see
     * pio_tf_err_handler_ in tests/general/util/pio_tutil.F90), so that
     * PIOc_* calls that hit a (possibly expected) low-level I/O library
     * error - e.g. nc_def_large_1d_bc()'s near-the-format-limit file
     * definitions - return an error code instead of aborting via
     * PIO_INTERNAL_ERROR (the iosystem's default when no handler has
     * been set). Match that here. */
    int old_handler = 0;
    PIOc_set_iosystem_error_handling(tf.get_iosysid(), PIO_BCAST_ERROR, &old_handler);

    nerrs += tf.run();
    tf.finalize_iosystem();
  }
  else{
    nerrs++;
  }

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
