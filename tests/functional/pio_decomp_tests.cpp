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

template<typename T>
int nc_write_1d_darray(SPIO_TF::Test_framework &tf)
{
  const int VEC_LOCAL_SZ = 7;
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ret = PIO_NOERR;
  int ioid = -1;
  int dims[1];
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  std::vector<T> buf(VEC_LOCAL_SZ);

  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = VEC_LOCAL_SZ * tf.get_comm_rank() + (i + 1);
  }
  dims[0] = VEC_LOCAL_SZ * tf.get_comm_size();
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    buf[i] = tf.get_comm_rank();
  }

  ret = PIOc_InitDecomp(tf.get_iosysid(), Pio_type_map<T>::value, 1, dims,
          VEC_LOCAL_SZ, compdof.data(), &ioid, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_InitDecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  const std::string filename("test_pio_decomp_simple_tests.testfile");
  for(int i = 0; i < num_flavors; i++){
    int ncid = -1, dimid = -1, varid = -1;
    char iotype_name[PIO_MAX_NAME + 1];

    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : nc_write_1d_darray : ") + iotype_name + "\n");

    ret = PIOc_createfile(tf.get_iosysid(), &ncid, &(flavors[i]), filename.c_str(), PIO_CLOBBER);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Could not create file " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, "PIO_TF_test_dim", dims[0], &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a dim : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_var(ncid, "PIO_TF_test_var", Pio_type_map<T>::value, 1, &dimid, &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define a var : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to end redef mode : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* Write the variable out */
    ret = PIOc_write_darray(ncid, varid, ioid, VEC_LOCAL_SZ, buf.data(), NULL);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to write darray : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    /* FIXME: Verify the written output */

    ret = PIOc_closefile(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to close file : " + filename + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_deletefile(tf.get_iosysid(), filename.c_str());
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to delete file : " + filename + "\n");
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_freedecomp(tf.get_iosysid(), ioid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: PIOc_freedecomp() FAILED, ret = " + std::to_string(ret) + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
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
    {"nc_write_1d_darray_int", nc_write_1d_darray<int>},
    {"nc_write_1d_darray_float", nc_write_1d_darray<float>},
    {"nc_write_1d_darray_double", nc_write_1d_darray<double>}
  };

  SPIO_TF::Test_framework tf("pio_decomp_tests", MPI_COMM_WORLD, rfuncs);
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
