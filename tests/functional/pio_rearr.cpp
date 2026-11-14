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

const int PIO_TF_MAX_STR_LEN = 128;

/* Create a file with a global attribute (filename) */
static int create_file(SPIO_TF::Test_framework &tf, int iosysid, int iotype,
    const std::string &fname, const std::string &attname, const std::string &dimname)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  int ncid = -1, dimid = -1, varid = -1;

  int ret = PIOc_createfile(iosysid, &ncid, &iotype, fname.c_str(), PIO_CLOBBER);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to create dummy file :" + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, dimname.c_str(), PIO_TF_MAX_STR_LEN, &dimid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to define dim " + dimname + " in file :" + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, attname.c_str(), PIO_CHAR, 1, &dimid, &varid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to define var " + attname + " in file :" + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, varid, attname.c_str(), PIO_CHAR, fname.size(), fname.c_str());
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to put att " + attname + " in file :" + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(ncid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to enddef, file :" + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to close file :" + fname + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Open a file and perform read and write */
static int open_and_check_rdwr(SPIO_TF::Test_framework &tf, MPI_Comm comm, int iosysid,
    int iotype, const std::string &fname)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const int VEC_LOCAL_SZ = 3;
  const std::string var_name("test_1d_var");
  const std::string dim_name("test_dim_1d_var");

  int comm_rank = 0, comm_sz = 0;
  MPI_Comm_size(comm, &comm_sz);
  MPI_Comm_rank(comm, &comm_rank);

  int dims[1];
  dims[0] = VEC_LOCAL_SZ * comm_sz;

  int ncid = -1;
  int ret = PIOc_openfile(iosysid, &ncid, &iotype, fname.c_str(), PIO_WRITE);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to open:" + fname + "\n");
    return PIO_EINTERNAL;
  }

  int dimid = -1, varid = -1;
  ret = PIOc_inq_dimid(ncid, dim_name.c_str(), &dimid);
  if(ret != PIO_NOERR){
    /* Define the required dim/var */
    ret = PIOc_redef(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to redef:" + fname + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_def_dim(ncid, dim_name.c_str(), dims[0], &dimid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define dim:" + fname + "\n");
      return PIO_EINTERNAL;
    }

    /* Assume var is not defined either */
    ret = PIOc_def_var(ncid, var_name.c_str(), PIO_REAL, 1, &dimid, &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to define var:" + fname + "\n");
      return PIO_EINTERNAL;
    }

    ret = PIOc_enddef(ncid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to enddef:" + fname + "\n");
      return PIO_EINTERNAL;
    }
  }
  else{
    /* Assume var was also defined */
    ret = PIOc_inq_varid(ncid, var_name.c_str(), &varid);
    if(ret != PIO_NOERR){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          "ERROR: Failed to inq var:" + fname + "\n");
      return PIO_EINTERNAL;
    }
  }

  /* Compdof is a simple reverse (of chunks of VEC_LOCAL_SZ) of the file offsets
   * e.g. VEC_LOCAL_SZ = 3, with 2 procs
   * [4 5 6] [1 2 3]
   */
  int start_compdof = (comm_sz - comm_rank - 1) * VEC_LOCAL_SZ;
  std::vector<PIO_Offset> compdof(VEC_LOCAL_SZ);
  std::vector<float> wbuf(VEC_LOCAL_SZ);
  for(int i = 0; i < VEC_LOCAL_SZ; i++){
    compdof[i] = start_compdof + (i + 1);
    wbuf[i] = static_cast<float>(compdof[i]);
  }
  std::vector<float> rbuf(VEC_LOCAL_SZ, 0);

  int ioid = -1;
  ret = PIOc_InitDecomp(iosysid, PIO_REAL, 1, dims, VEC_LOCAL_SZ, compdof.data(),
          &ioid, NULL, NULL, NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to init decomp:" + fname + "\n");
    return PIO_EINTERNAL;
  }

  /* Write and read back the data */
  ret = PIOc_write_darray(ncid, varid, ioid, VEC_LOCAL_SZ, wbuf.data(), NULL);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to write darray: " + fname + "\n");
    return PIO_EINTERNAL;
  }

#ifdef PIO_TEST_CLOSE_OPEN_FOR_SYNC
  ret = PIOc_closefile(ncid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to close file : " + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_openfile(iosysid, &ncid, &iotype, fname.c_str(), PIO_NOWRITE);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Could not reopen file " + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(ncid, var_name.c_str(), &varid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Could not inq var :" + fname + "\n");
    return PIO_EINTERNAL;
  }
#else
  ret = PIOc_sync(ncid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to sync file : " + fname + "\n");
    return PIO_EINTERNAL;
  }
#endif

  ret = PIOc_read_darray(ncid, varid, ioid, VEC_LOCAL_SZ, rbuf.data());
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to read darray: " + fname + "\n");
    return PIO_EINTERNAL;
  }

  if(!check_val(tf, rbuf, wbuf, "Got wrong val")){
    return PIO_EINTERNAL;
  }

  ret = PIOc_freedecomp(iosysid, ioid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to free decomp : " + fname + "\n");
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(ret != PIO_NOERR){
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        "ERROR: Failed to close file : " + fname + "\n");
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

int test_rearrs_base(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const std::string fname("pio_test_rearrs_base.nc");
  const std::string attname("filename");
  const std::string dimname("filename_dim");
  const int NUM_REARRANGERS = 3;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX, PIO_REARR_ANY};
  const char *rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX", "PIO_REARR_ANY"};

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing iotype: ") + iotype_name + "\n");

    int ret = create_file(tf, tf.get_iosysid(), flavors[i], fname, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    MPI_Comm dup_comm;
    int mpi_ret = MPI_Comm_dup(tf.get_comm(), &dup_comm);
    if(mpi_ret != MPI_SUCCESS){
      return PIO_EINTERNAL;
    }
    int dup_comm_rank = 0, dup_comm_sz = 0;
    MPI_Comm_rank(dup_comm, &dup_comm_rank);
    MPI_Comm_size(dup_comm, &dup_comm_sz);

    for(int j = 0; j < NUM_REARRANGERS; j++){
      logger.log(SPIO_Util::Logger::Log_level::INFO,
          std::string("Testing rearr : ") + rearrs_info[j] + "\n");
      for(int k = 0; k < dup_comm_sz; k++){
        int dup_iosys_base = k;
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            "Base = " + std::to_string(dup_iosys_base) + "\n");

        int dup_iosysid = -1;
        ret = PIOc_Init_Intracomm(dup_comm, dup_comm_sz, 1, dup_iosys_base,
                rearrs[j], &dup_iosysid);
        if(ret != PIO_NOERR){
          return PIO_EINTERNAL;
        }

        int old_handler = PIO_INTERNAL_ERROR;
        PIOc_set_iosystem_error_handling(dup_iosysid, PIO_BCAST_ERROR, &old_handler);

        ret = open_and_check_rdwr(tf, dup_comm, dup_iosysid, flavors[i], fname);
        if(ret != PIO_NOERR){
          return PIO_EINTERNAL;
        }

        ret = PIOc_finalize(dup_iosysid);
        if(ret != PIO_NOERR){
          return PIO_EINTERNAL;
        }
      }
    }

    ret = PIOc_deletefile(tf.get_iosysid(), fname.c_str());
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    mpi_ret = MPI_Comm_free(&dup_comm);
    if(mpi_ret != MPI_SUCCESS){
      return PIO_EINTERNAL;
    }
  }

  return PIO_NOERR;
}

/* Test different combinations of the rearrangers to test the compatibility
 * between different rearrangers:
 * init/rd+wr/finalize with one rearranger followed by another and try
 * all combinations ((subset,box),(box,subset),(subset,subset),(box,box))
 */
int test_rearrs_combs(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const std::string fname("pio_rearrs_combs.nc");
  const std::string attname("filename");
  const std::string dimname("filename_dim");
  const int NUM_REARRANGERS = 2;
  const int MAX_PERMS = 4;
  const int rearrs_perms[MAX_PERMS][NUM_REARRANGERS] = {
    {PIO_REARR_SUBSET, PIO_REARR_BOX},
    {PIO_REARR_BOX, PIO_REARR_SUBSET},
    {PIO_REARR_SUBSET, PIO_REARR_SUBSET},
    {PIO_REARR_BOX, PIO_REARR_BOX}
  };
  const char *rearrs_perms_info[MAX_PERMS][NUM_REARRANGERS] = {
    {"PIO_REARR_SUBSET", "PIO_REARR_BOX"},
    {"PIO_REARR_BOX", "PIO_REARR_SUBSET"},
    {"PIO_REARR_SUBSET", "PIO_REARR_SUBSET"},
    {"PIO_REARR_BOX", "PIO_REARR_BOX"}
  };

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[i], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing iotype: ") + iotype_name + "\n");

    int ret = create_file(tf, tf.get_iosysid(), flavors[i], fname, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    MPI_Comm dup_comm;
    int mpi_ret = MPI_Comm_dup(tf.get_comm(), &dup_comm);
    if(mpi_ret != MPI_SUCCESS){
      return PIO_EINTERNAL;
    }
    int dup_comm_sz = 0;
    MPI_Comm_size(dup_comm, &dup_comm_sz);

    /* Try different combinations of rearrangers */
    for(int k = 0; k < MAX_PERMS; k++){
      for(int j = 0; j < NUM_REARRANGERS; j++){
        logger.log(SPIO_Util::Logger::Log_level::INFO,
            std::string("Testing rearr : ") + rearrs_perms_info[k][j] + "\n");

        int dup_iosysid = -1;
        ret = PIOc_Init_Intracomm(dup_comm, dup_comm_sz, 1, 0,
                rearrs_perms[k][j], &dup_iosysid);
        if(ret != PIO_NOERR){
          return PIO_EINTERNAL;
        }

        int old_handler = PIO_INTERNAL_ERROR;
        PIOc_set_iosystem_error_handling(dup_iosysid, PIO_BCAST_ERROR, &old_handler);

        ret = open_and_check_rdwr(tf, dup_comm, dup_iosysid, flavors[i], fname);
        if(ret != PIO_NOERR){
          return PIO_EINTERNAL;
        }

        ret = PIOc_finalize(dup_iosysid);
        if(ret != PIO_NOERR){
          return PIO_EINTERNAL;
        }
      }
    }

    ret = PIOc_deletefile(tf.get_iosysid(), fname.c_str());
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    mpi_ret = MPI_Comm_free(&dup_comm);
    if(mpi_ret != MPI_SUCCESS){
      return PIO_EINTERNAL;
    }
  }

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
    {"test_rearrs_base", test_rearrs_base},
    {"test_rearrs_combs", test_rearrs_combs}
  };

  SPIO_TF::Test_framework tf("pio_rearr_cxx", MPI_COMM_WORLD, rfuncs);
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
