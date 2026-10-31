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

/* Write/read multiple frames of a 1d variable with an unlimited dimension.
 * This test does not use a decomposition (no PIOc_InitDecomp) - each frame
 * is a single, non-distributed value written/read with PIOc_put_vara/
 * PIOc_get_vara (start/count based hyperslab I/O).
 */
template<typename T>
int nc_wr_rd_1d_unlim_dim(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int NFRAMES = 6;
  const std::string PIO_VAR_NAME("PIO_TF_test_var_1d_unlim");
  const std::string filename("test_pio_decomp_frame_tests.testfile");

  std::vector<T> exp_val(NFRAMES);
  for(int f = 0; f < NFRAMES; f++){
    exp_val[f] = static_cast<T>(f + 1);
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_rd_1d_unlim_dim : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_time", PIO_UNLIMITED, &dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 1, &dimid, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    for(int f = 0; f < NFRAMES; f++){
      PIO_Offset start[1] = { static_cast<PIO_Offset>(f) };
      PIO_Offset count[1] = { 1 };
      T wbuf[1] = { static_cast<T>(f + 1) };

      /* Write the current frame */
      ret = PIOc_put_vara(ncid, varid, start, count, wbuf);
      if(!check_ret(tf, ret, "Failed to write var : " + filename)){
        return PIO_EINTERNAL;
      }
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

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(int f = 0; f < NFRAMES; f++){
      PIO_Offset start[1] = { static_cast<PIO_Offset>(f) };
      PIO_Offset count[1] = { 1 };
      std::vector<T> rbuf(1, static_cast<T>(0));

      ret = PIOc_get_vara(ncid, varid, start, count, rbuf.data());
      if(!check_ret(tf, ret, "Failed to read var : " + filename)){
        return PIO_EINTERNAL;
      }

      std::vector<T> frame_exp_val(1, exp_val[f]);
      if(!check_val(tf, rbuf, frame_exp_val, "Got wrong val, frame = " + std::to_string(f))){
        return PIO_EINTERNAL;
      }
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  return PIO_NOERR;
}

/* Write/read multiple frames of a 2d variable (row dim + unlimited/time dim)
 * using a single decomposition (shared between write and read) over the
 * row dimension, with PIOc_setframe selecting the frame before each
 * PIOc_write_darray/PIOc_read_darray call.
 */
template<typename T>
int nc_wr_rd_2d_unlim_dim(SPIO_TF::Test_framework &tf)
{
  int ret = PIO_NOERR;
  const int NFRAMES = 6;
  const int VEC_LOCAL_SZ = 7;
  const std::string PIO_VAR_NAME("PIO_TF_test_var");
  const std::string filename("test_pio_decomp_simple_tests.testfile");

  /* Data evenly distributed across all procs - each proc has VEC_LOCAL_SZ
   * elements on each frame
   */
  std::size_t vec_local_sz = VEC_LOCAL_SZ;
  int dims = VEC_LOCAL_SZ * tf.get_comm_size();
  PIO_Offset start = static_cast<PIO_Offset>(VEC_LOCAL_SZ) * tf.get_comm_rank() + 1;

  std::vector<PIO_Offset> compdof(vec_local_sz);
  for(std::size_t i = 0; i < vec_local_sz; i++){
    compdof[i] = start - 1 + static_cast<PIO_Offset>(i + 1);
  }

  int gdims[1] = { dims };
  int iodesc = -1;
  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, gdims,
          static_cast<int>(vec_local_sz), compdof.data(), &iodesc, NULL, NULL, NULL);
  if(!check_ret(tf, ret, "PIOc_InitDecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int fl = 0; fl < num_flavors; fl++){
    int ncid = -1, row_dimid = -1, time_dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[fl], iotype_name);
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_wr_rd_2d_unlim_dim : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_CLOBBER);
    if(!check_ret(tf, ret, "Could not create file " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_row", dims, &row_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim_time", PIO_UNLIMITED, &time_dimid);
    if(!check_ret(tf, ret, "Failed to define a dim : " + filename)){
      return PIO_EINTERNAL;
    }

    int dimids[2] = { time_dimid, row_dimid };
    ret = PIOc_def_var(ncid, PIO_VAR_NAME.c_str(), Pio_type_map<T>::value, 2, dimids, &varid);
    if(!check_ret(tf, ret, "Failed to define a var : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(!check_ret(tf, ret, "Failed to end redef mode : " + filename)){
      return PIO_EINTERNAL;
    }

    for(int f = 0; f < NFRAMES; f++){
      std::vector<T> wbuf(vec_local_sz);
      for(std::size_t i = 0; i < vec_local_sz; i++){
        wbuf[i] = static_cast<T>(compdof[i] + f * dims);
      }

      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      /* Write the current frame */
      ret = PIOc_write_darray(ncid, varid, iodesc, static_cast<int>(wbuf.size()), wbuf.data(), NULL);
      if(!check_ret(tf, ret, "Failed to write darray : " + filename)){
        return PIO_EINTERNAL;
      }
    }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_openfile(tf.get_iosysid(), &ncid, &(flavors[fl]), filename.c_str(), PIO_NOWRITE);
    if(!check_ret(tf, ret, "Could not reopen file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_inq_varid(ncid, PIO_VAR_NAME.c_str(), &varid);
    if(!check_ret(tf, ret, "Could not inq var : " + filename)){
      return PIO_EINTERNAL;
    }
#else
    ret = PIOc_sync(ncid);
    if(!check_ret(tf, ret, "Failed to sync file : " + filename)){
      return PIO_EINTERNAL;
    }
#endif

    for(int f = 0; f < NFRAMES; f++){
      std::vector<T> exp_val(vec_local_sz);
      for(std::size_t i = 0; i < vec_local_sz; i++){
        exp_val[i] = static_cast<T>(compdof[i] + f * dims);
      }

      std::vector<T> rbuf(vec_local_sz, static_cast<T>(0));
      ret = PIOc_setframe(ncid, varid, f);
      if(!check_ret(tf, ret, "Failed to set frame : " + filename)){
        return PIO_EINTERNAL;
      }

      ret = PIOc_read_darray(ncid, varid, iodesc, static_cast<int>(rbuf.size()), rbuf.data());
      if(!check_ret(tf, ret, "Failed to read darray : " + filename)){
        return PIO_EINTERNAL;
      }

      if(!check_val(tf, rbuf, exp_val, "Got wrong val, frame = " + std::to_string(f))){
        return PIO_EINTERNAL;
      }
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file : " + filename)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(!check_ret(tf, ret, "Failed to delete file : " + filename)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), iodesc);
  if(!check_ret(tf, ret, "PIOc_freedecomp() FAILED")){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}
