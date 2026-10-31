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

/* The number of elements (in the decomposed/distributed dimension)
 * on each process
 */
const int VEC_LOCAL_SZ = 7;

/* The number of frames (records) written out for the record variable
 * tests - used by the record/limited-record subroutines added in a
 * later task but declared here, at file scope, alongside VEC_LOCAL_SZ
 */
const int NFRAMES = 3;

/* The file used by all the tests in this file */
const std::string FNAME("test_pio_decomp_simple_tests.testfile");

/* Get a 1d block decomposition, with num_extra_dims leading dimensions
 * of length 1, used to test decompositions associated with variables
 * that have one or more extra (non-decomposed) outermost dimensions.
 * # Each proc has VEC_LOCAL_SZ elements
 * e.g. For VEC_LOCAL_SZ = 2, num_extra_dims = 0,
 * e.g. gdims = [sz * VEC_LOCAL_SZ]
 * e.g. For VEC_LOCAL_SZ = 2, num_extra_dims = 2,
 * e.g. gdims = [1, 1, sz * VEC_LOCAL_SZ]
 */
void get_extra_dims_decomp_info(int rank, int sz, int num_extra_dims,
    std::vector<int> &gdims, std::vector<PIO_Offset> &compdof)
{
  gdims.assign(num_extra_dims, 1);
  gdims.push_back(VEC_LOCAL_SZ * sz);

  compdof.resize(VEC_LOCAL_SZ);
  for(std::size_t i = 0; i < static_cast<std::size_t>(VEC_LOCAL_SZ); i++){
    compdof[i] = static_cast<PIO_Offset>(VEC_LOCAL_SZ * rank + (i + 1));
  }
}

/* Write (and read back) a 1d non-record variable, using a decomposition
 * that has num_extra_dims extra outermost dimensions (each of length 1)
 */
template<typename T>
int nc_wr_rd_1d_extra_dims_impl(SPIO_TF::Test_framework &tf, int num_extra_dims,
    const std::string &test_name)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;

  std::vector<int> gdims;
  std::vector<PIO_Offset> compdof;
  get_extra_dims_decomp_info(tf.get_comm_rank(), tf.get_comm_size(), num_extra_dims,
      gdims, compdof);

  /* NOTE: The Fortran source (pio_decomp_extra_dims.F90.in) writes a uniform
   * per-rank value here (wbuf = pio_tf_world_rank_). We deliberately write
   * per-element compdof values instead - a strictly stronger roundtrip check
   * (it also catches intra-rank permutation bugs that a uniform buffer
   * cannot), and it keeps this function consistent with the record-group
   * functions' compdof-based formula.
   */
  std::vector<T> wbuf(VEC_LOCAL_SZ);
  for(std::size_t i = 0; i < wbuf.size(); i++){
    wbuf[i] = static_cast<T>(compdof[i]);
  }

  int ioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value,
          static_cast<int>(gdims.size()), gdims.data(), VEC_LOCAL_SZ,
          compdof.data(), &ioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  int dim_len = VEC_LOCAL_SZ * tf.get_comm_size();
  std::vector<T> rbuf(VEC_LOCAL_SZ, 0);
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : ") + test_name + " : " + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), FNAME.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dim_len, &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + FNAME)){
      return PIO_EINTERNAL;
    }

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
    if(!check_ret(tf, ret, "Failed to write darray : " + FNAME)){
      return PIO_EINTERNAL;
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), FNAME.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + FNAME)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + FNAME)){
      return PIO_EINTERNAL;
    }
#endif

    for(std::size_t j = 0; j < rbuf.size(); j++){
      rbuf[j] = 0;
    }
    ret = PIOc_read_darray(ncid, varid, ioid, VEC_LOCAL_SZ, rbuf.data());
    if(!check_ret(tf, ret, "Failed to read darray : " + FNAME)){
      return PIO_EINTERNAL;
    }

    if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + FNAME)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write a 1d non-record variable, use a 1d decomposition
 * that has no extra outermost dimensions
 */
template<typename T>
int nc_decomp_no_extra_dims(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_impl<T>(tf, 0, "nc_decomp_no_extra_dims");
}

/* Write a 1d non-record variable, use a 2d decomposition
 * that has one extra outermost dimension (length set to 1)
 */
template<typename T>
int nc_decomp_1_extra_dim(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_impl<T>(tf, 1, "nc_decomp_1_extra_dim");
}

/* Write a 1d non-record variable, use a 3d decomposition
 * that has two extra outermost dimensions (lengths set to 1)
 */
template<typename T>
int nc_decomp_2_extra_dims(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_impl<T>(tf, 2, "nc_decomp_2_extra_dims");
}

/* Write (and read back) multiple frames of a 1d record variable, using a
 * decomposition that has num_extra_dims leading dimensions (each of length
 * 1). The record/time dimension is NOT part of the decomposition - only the
 * row (decomposed) dimension is. time_dim_len is either PIO_UNLIMITED or a
 * fixed length (NFRAMES), to cover both the unlimited and limited record dim
 * cases.
 */
template<typename T>
int nc_wr_rd_1d_extra_dims_rec_impl(SPIO_TF::Test_framework &tf, int num_extra_dims,
    PIO_Offset time_dim_len, const std::string &test_name)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;

  std::vector<int> gdims;
  std::vector<PIO_Offset> compdof;
  get_extra_dims_decomp_info(tf.get_comm_rank(), tf.get_comm_size(), num_extra_dims,
      gdims, compdof);

  int ioid = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value,
          static_cast<int>(gdims.size()), gdims.data(), VEC_LOCAL_SZ,
          compdof.data(), &ioid, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  int dim_len = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, row_dimid = -1, time_dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : ") + test_name + " : " + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), FNAME.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_row", dim_len, &row_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_time", time_dim_len, &time_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + FNAME)){
      return PIO_EINTERNAL;
    }

    int dimids[2] = { time_dimid, row_dimid };
    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 2, dimids, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + FNAME)){
      return PIO_EINTERNAL;
    }

    for(int f = 0; f < NFRAMES; f++){
      std::vector<T> wbuf(VEC_LOCAL_SZ);
      for(std::size_t i = 0; i < wbuf.size(); i++){
        wbuf[i] = static_cast<T>(compdof[i] + f * (VEC_LOCAL_SZ * tf.get_comm_size()));
      }

      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + FNAME)){
        return PIO_EINTERNAL;
      }

      /* Write the current frame */
      ret = PIOc_write_darray(ncid, varid, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
      if(!check_ret(tf, ret, "Failed to write darray : " + FNAME)){
        return PIO_EINTERNAL;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[i]), FNAME.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + FNAME)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + FNAME)){
      return PIO_EINTERNAL;
    }
#endif

    for(int f = 0; f < NFRAMES; f++){
      std::vector<T> exp_val(VEC_LOCAL_SZ);
      for(std::size_t i = 0; i < exp_val.size(); i++){
        exp_val[i] = static_cast<T>(compdof[i] + f * (VEC_LOCAL_SZ * tf.get_comm_size()));
      }

      std::vector<T> rbuf(VEC_LOCAL_SZ, static_cast<T>(0));
      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + FNAME)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_read_darray(ncid, varid, ioid, VEC_LOCAL_SZ, rbuf.data());
      if(!check_ret(tf, ret, "Failed to read darray : " + FNAME)){
        return PIO_EINTERNAL;
      }

      if(!check_val(tf, rbuf, exp_val, "Got wrong val, frame = " + std::to_string(f))){
        return PIO_EINTERNAL;
      }
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + FNAME)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), FNAME.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + FNAME)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Write multiple frames of a 1d record variable (with an unlimited time
 * dimension), use a 1d decomposition that has no extra outermost dimensions
 */
template<typename T>
int nc_decomp_no_extra_dims_rec(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_rec_impl<T>(tf, 0, PIO_UNLIMITED, "nc_decomp_no_extra_dims_rec");
}

/* Write multiple frames of a 2d record variable (with an unlimited time
 * dimension), use a 2d decomposition that has one extra outermost dimension
 * (length set to 1)
 */
template<typename T>
int nc_decomp_1_extra_dim_rec(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_rec_impl<T>(tf, 1, PIO_UNLIMITED, "nc_decomp_1_extra_dim_rec");
}

/* Write multiple frames of a 3d record variable (with an unlimited time
 * dimension), use a 3d decomposition that has two extra outermost dimensions
 * (lengths set to 1)
 */
template<typename T>
int nc_decomp_2_extra_dims_rec(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_rec_impl<T>(tf, 2, PIO_UNLIMITED, "nc_decomp_2_extra_dims_rec");
}

/* Write multiple frames of a 1d record variable (with a fixed-length,
 * limited, time dimension), use a 1d decomposition that has no extra
 * outermost dimensions
 */
template<typename T>
int nc_decomp_no_extra_dims_lim_rec(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_rec_impl<T>(tf, 0, static_cast<PIO_Offset>(NFRAMES), "nc_decomp_no_extra_dims_lim_rec");
}

/* Write multiple frames of a 2d record variable (with a fixed-length,
 * limited, time dimension), use a 2d decomposition that has one extra
 * outermost dimension (length set to 1)
 */
template<typename T>
int nc_decomp_1_extra_dim_lim_rec(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_rec_impl<T>(tf, 1, static_cast<PIO_Offset>(NFRAMES), "nc_decomp_1_extra_dim_lim_rec");
}

/* Write multiple frames of a 3d record variable (with a fixed-length,
 * limited, time dimension), use a 3d decomposition that has two extra
 * outermost dimensions (lengths set to 1)
 */
template<typename T>
int nc_decomp_2_extra_dims_lim_rec(SPIO_TF::Test_framework &tf)
{
  return nc_wr_rd_1d_extra_dims_rec_impl<T>(tf, 2, static_cast<PIO_Offset>(NFRAMES), "nc_decomp_2_extra_dims_lim_rec");
}

int main(int argc, char *argv[])
{
  int ret = MPI_SUCCESS, nerrs = 0;
  ret = MPI_Init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    std::cerr << "MPI_Init() FAILED, ret = " << ret << "\n";
    return ret;
  }

  std::vector<std::pair<std::string, std::function<int(SPIO_TF::Test_framework &)> > > rfuncs = {
    {"nc_decomp_no_extra_dims_int", nc_decomp_no_extra_dims<int>},
    {"nc_decomp_no_extra_dims_float", nc_decomp_no_extra_dims<float>},
    {"nc_decomp_no_extra_dims_double", nc_decomp_no_extra_dims<double>},
    {"nc_decomp_1_extra_dim_int", nc_decomp_1_extra_dim<int>},
    {"nc_decomp_1_extra_dim_float", nc_decomp_1_extra_dim<float>},
    {"nc_decomp_1_extra_dim_double", nc_decomp_1_extra_dim<double>},
    {"nc_decomp_2_extra_dims_int", nc_decomp_2_extra_dims<int>},
    {"nc_decomp_2_extra_dims_float", nc_decomp_2_extra_dims<float>},
    {"nc_decomp_2_extra_dims_double", nc_decomp_2_extra_dims<double>},
    {"nc_decomp_no_extra_dims_rec_int", nc_decomp_no_extra_dims_rec<int>},
    {"nc_decomp_no_extra_dims_rec_float", nc_decomp_no_extra_dims_rec<float>},
    {"nc_decomp_no_extra_dims_rec_double", nc_decomp_no_extra_dims_rec<double>},
    {"nc_decomp_1_extra_dim_rec_int", nc_decomp_1_extra_dim_rec<int>},
    {"nc_decomp_1_extra_dim_rec_float", nc_decomp_1_extra_dim_rec<float>},
    {"nc_decomp_1_extra_dim_rec_double", nc_decomp_1_extra_dim_rec<double>},
    {"nc_decomp_2_extra_dims_rec_int", nc_decomp_2_extra_dims_rec<int>},
    {"nc_decomp_2_extra_dims_rec_float", nc_decomp_2_extra_dims_rec<float>},
    {"nc_decomp_2_extra_dims_rec_double", nc_decomp_2_extra_dims_rec<double>},
    {"nc_decomp_no_extra_dims_lim_rec_int", nc_decomp_no_extra_dims_lim_rec<int>},
    {"nc_decomp_no_extra_dims_lim_rec_float", nc_decomp_no_extra_dims_lim_rec<float>},
    {"nc_decomp_no_extra_dims_lim_rec_double", nc_decomp_no_extra_dims_lim_rec<double>},
    {"nc_decomp_1_extra_dim_lim_rec_int", nc_decomp_1_extra_dim_lim_rec<int>},
    {"nc_decomp_1_extra_dim_lim_rec_float", nc_decomp_1_extra_dim_lim_rec<float>},
    {"nc_decomp_1_extra_dim_lim_rec_double", nc_decomp_1_extra_dim_lim_rec<double>},
    {"nc_decomp_2_extra_dims_lim_rec_int", nc_decomp_2_extra_dims_lim_rec<int>},
    {"nc_decomp_2_extra_dims_lim_rec_float", nc_decomp_2_extra_dims_lim_rec<float>},
    {"nc_decomp_2_extra_dims_lim_rec_double", nc_decomp_2_extra_dims_lim_rec<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_extra_dims_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  int rearrs[] = {PIO_REARR_BOX, PIO_REARR_SUBSET, PIO_REARR_CONTIG};
  for(int rearr : rearrs){
    if(tf.init_iosystem(rearr) != PIO_NOERR){
      nerrs++;
      continue;
    }
    nerrs += tf.run();
    tf.finalize_iosystem();
  }

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
