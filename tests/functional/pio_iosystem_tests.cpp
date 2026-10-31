#include <iostream>
#include <string>
#include <vector>
#include <cstring>
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

/* Split tf's comm into an odd-ranks sub-communicator and an even-ranks
 * sub-communicator - every rank ends up in exactly one of the two, both
 * non-null.
 */
static int split_world_odd_even(MPI_Comm world_comm, int world_rank,
    MPI_Comm *new_comm, int *new_rank, int *new_size, bool *is_even)
{
  int color = 0;

  *new_comm = MPI_COMM_NULL;
  *new_rank = 0;
  *new_size = 0;

  if((world_rank % 2) == 0){
    *is_even = true;
    color = 1;
  }
  else{
    *is_even = false;
    color = 0;
  }

  int mpi_ret = MPI_Comm_split(world_comm, color, 0, new_comm);
  if(mpi_ret != MPI_SUCCESS){
    return PIO_EINTERNAL;
  }

  MPI_Comm_rank(*new_comm, new_rank);
  MPI_Comm_size(*new_comm, new_size);

  return PIO_NOERR;
}

/* Split tf's comm into an even-ranks sub-communicator only - odd ranks get
 * MPI_COMM_NULL back (MPI_UNDEFINED color), matching the Fortran's own
 * split_world_only_even.
 */
static int split_world_only_even(MPI_Comm world_comm, int world_rank,
    MPI_Comm *new_comm, int *new_rank, int *new_size, bool *is_even)
{
  int color = 0;

  *new_comm = MPI_COMM_NULL;
  *new_rank = 0;
  *new_size = 0;

  if((world_rank % 2) == 0){
    *is_even = true;
    color = 1;
  }
  else{
    *is_even = false;
    color = MPI_UNDEFINED;
  }

  int mpi_ret = MPI_Comm_split(world_comm, color, 0, new_comm);
  if(mpi_ret != MPI_SUCCESS){
    return PIO_EINTERNAL;
  }

  if(*new_comm != MPI_COMM_NULL){
    MPI_Comm_rank(*new_comm, new_rank);
    MPI_Comm_size(*new_comm, new_size);
  }

  return PIO_NOERR;
}

/* Create a file with a char variable holding a global attribute (filename).
 * comm is the (possibly sub-world) communicator that the given iosysid
 * spans, used to scope the pass/fail checks below.
 */
static int create_file(SPIO_TF::Test_framework &tf, MPI_Comm comm,
    int iosysid, int iotype, const std::string &fname,
    const std::string &attname, const std::string &dimname)
{
  int ncid = -1, dimid = -1, varid = -1;

  int ret = PIOc_createfile(iosysid, &ncid, &iotype, fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, comm, ret, "Failed to create dummy file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, dimname.c_str(), PIO_TF_MAX_STR_LEN, &dimid);
  if(!check_ret(tf, comm, ret, "Failed to define dim " + dimname + "in file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, attname.c_str(), PIO_CHAR, 1, &dimid, &varid);
  if(!check_ret(tf, comm, ret, "Failed to define var " + attname + " in file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, varid, attname.c_str(), PIO_CHAR, fname.size(), fname.c_str());
  if(!check_ret(tf, comm, ret, "Failed to put att " + attname + " in file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, comm, ret, "Failed to close file :" + fname)){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Check the contents of file : the global attribute 'filename' should be
 * equal to the name of the file, fname. comm scopes the pass/fail checks. */
static bool check_file(SPIO_TF::Test_framework &tf, MPI_Comm comm, int ncid,
    const std::string &fname, const std::string &attname, const std::string &dimname)
{
  int dimid = -1, varid = -1;

  int ret = PIOc_inq_dimid(ncid, dimname.c_str(), &dimid);
  if(!check_ret(tf, comm, ret, "Failed to find dim " + dimname + "in file :" + fname)){
    return false;
  }

  ret = PIOc_inq_varid(ncid, attname.c_str(), &varid);
  if(!check_ret(tf, comm, ret, "Failed to find var " + attname + " in file :" + fname)){
    return false;
  }

  std::vector<char> val(PIO_TF_MAX_STR_LEN, '\0');
  ret = PIOc_get_att(ncid, varid, attname.c_str(), val.data());
  if(!check_ret(tf, comm, ret, "Failed to get att " + attname + " in file :" + fname)){
    return false;
  }

  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      "val = " + std::string(val.data()) + ", fname =" + fname + "\n");

  return passert(tf, comm, (std::strncmp(val.data(), fname.c_str(), fname.size()) == 0),
      "Attribute value is not the expected value");
}

/* Open and check the contents of file : open it and check the global
 * attribute 'filename' (should be equal to the name of the file, fname).
 * comm is the (possibly sub-world) communicator that the given iosysid
 * spans, used to scope the pass/fail checks below.
 */
static int open_and_check_file(SPIO_TF::Test_framework &tf, MPI_Comm comm,
    int iosysid, int iotype, int *ncid, const std::string &fname,
    const std::string &attname, const std::string &dimname, bool disable_fclose)
{
  int ret = PIOc_openfile(iosysid, ncid, &iotype, fname.c_str(), PIO_WRITE);
  if(!check_ret(tf, comm, ret, "Failed to open:" + fname)){
    return PIO_EINTERNAL;
  }

  if(!check_file(tf, comm, *ncid, fname, attname, dimname)){
    return PIO_EINTERNAL;
  }

  if(!disable_fclose){
    ret = PIOc_closefile(*ncid);
    if(!check_ret(tf, comm, ret, "Failed to close file :" + fname)){
      return PIO_EINTERNAL;
    }
  }

  return PIO_NOERR;
}

/* Create a file with one iosystem - with all procs, and open/read with
 * another iosystem - subset (odd/even) of procs */
int two_iosystems_odd_even(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const std::string fname1("pio_iosys_test_file1.nc");
  const std::string fname2("pio_iosys_test_file2.nc");
  const std::string attname("filename");
  const std::string dimname("filename_dim");
  int nerrs = 0;

  /* Split world to odd and even procs */
  MPI_Comm odd_even_comm = MPI_COMM_NULL;
  int odd_even_rank = 0, odd_even_size = 0;
  bool is_even = false;
  int ret = split_world_odd_even(tf.get_comm(), tf.get_comm_rank(),
      &odd_even_comm, &odd_even_rank, &odd_even_size, &is_even);
  if(ret != PIO_NOERR){
    return PIO_EINTERNAL;
  }

  int odd_even_iosysid = -1;
  ret = PIOc_Init_Intracomm(odd_even_comm, odd_even_size, 1, 0, PIO_REARR_SUBSET,
          &odd_even_iosysid);
  if(!check_ret(tf, odd_even_comm, ret, "Failed to init second iosystem")){
    return PIO_EINTERNAL;
  }

  int old_handler = PIO_INTERNAL_ERROR;
  PIOc_set_iosystem_error_handling(odd_even_iosysid, PIO_BCAST_ERROR, &old_handler);

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  /* Open two different files, created by all procs, from odd and even
   * procs separately, using the second iosystem */
  for(int f = 0; f < num_flavors; f++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[f], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : two_iosystems_odd_even : ") + iotype_name + "\n");

    /* Create two files to be opened later - world - all procs */
    ret = create_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], fname1, attname, dimname);
    if(!check_ret(tf, tf.get_comm(), ret, "Failed to create file :" + fname1)){
      nerrs++;
      continue;
    }

    ret = create_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], fname2, attname, dimname);
    if(!check_ret(tf, tf.get_comm(), ret, "Failed to create file :" + fname2)){
      nerrs++;
      continue;
    }

    /* Open file1 from even processes and file2 from odd processes */
    const std::string &fname = is_even ? fname1 : fname2;

    int ncid = -1;
    ret = open_and_check_file(tf, odd_even_comm, odd_even_iosysid, flavors[f], &ncid,
            fname, attname, dimname, false);
    if(!check_ret(tf, tf.get_comm(), ret, "Checking contents of file failed :" + fname)){
      nerrs++;
      continue;
    }
  }

  ret = PIOc_finalize(odd_even_iosysid);
  if(!check_ret(tf, odd_even_comm, ret, "Failed to finalize second iosystem")){
    return PIO_EINTERNAL;
  }

  int mpi_ret = MPI_Comm_free(&odd_even_comm);
  if(mpi_ret != MPI_SUCCESS){
    return PIO_EINTERNAL;
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Create a file with one iosystem - with all procs, and open/read with
 * two iosystems - (1) with all procs (2) subset (even) of procs */
int two_iosystems_even_all(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const std::string fname1("pio_iosys_test_file1.nc");
  const std::string fname2("pio_iosys_test_file2.nc");
  const std::string attname("filename");
  const std::string dimname("filename_dim");
  int ncid1 = -1, ncid2 = -1;
  const bool disable_fclose = true;
  int ret = PIO_NOERR;
  int nerrs = 0;

  /* Split world to only even procs - odd procs get MPI_COMM_NULL */
  MPI_Comm odd_even_comm = MPI_COMM_NULL;
  int odd_even_rank = 0, odd_even_size = 0;
  bool is_even = false;
  ret = split_world_only_even(tf.get_comm(), tf.get_comm_rank(),
      &odd_even_comm, &odd_even_rank, &odd_even_size, &is_even);
  if(ret != PIO_NOERR){
    return PIO_EINTERNAL;
  }

  int odd_even_iosysid = -1;
  if(is_even){
    ret = PIOc_Init_Intracomm(odd_even_comm, odd_even_size, 1, 0, PIO_REARR_SUBSET,
            &odd_even_iosysid);
    if(!check_ret(tf, odd_even_comm, ret, "Failed to init second iosystem")){
      return PIO_EINTERNAL;
    }

    int old_handler = PIO_INTERNAL_ERROR;
    PIOc_set_iosystem_error_handling(odd_even_iosysid, PIO_BCAST_ERROR, &old_handler);
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int f = 0; f < num_flavors; f++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[f], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : two_iosystems_even_all : ") + iotype_name + "\n");

    /* Create two files to be opened later - world - all procs */
    ret = create_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], fname1, attname, dimname);
    if(!check_ret(tf, tf.get_comm(), ret, "Failed to create file :" + fname1)){
      nerrs++;
      continue;
    }

    ret = create_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], fname2, attname, dimname);
    if(!check_ret(tf, tf.get_comm(), ret, "Failed to create file :" + fname2)){
      nerrs++;
      continue;
    }

    /* Open file1 from all even processes - PIO called from all even
     * processes with the odd_even_iosys */
    if(is_even){
      ret = open_and_check_file(tf, odd_even_comm, odd_even_iosysid, flavors[f], &ncid1,
              fname1, attname, dimname, disable_fclose);
    }
    if(!check_ret(tf, tf.get_comm(), ret, "Checking contents of file failed :" + fname1)){
      if(is_even){
        PIOc_closefile(ncid1);
      }
      nerrs++;
      continue;
    }

    /* Open file2 from all processes - PIO called from all processes with
     * the primary iosystem */
    ret = open_and_check_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], &ncid2,
            fname2, attname, dimname, disable_fclose);
    if(!check_ret(tf, tf.get_comm(), ret, "Checking contents of file failed :" + fname2)){
      if(is_even){
        PIOc_closefile(ncid1);
      }
      PIOc_closefile(ncid2);
      nerrs++;
      continue;
    }

    /* Check contents of the files again - PIO called from even processes
     * separately with odd_even_iosys */
    if(is_even){
      bool ok = check_file(tf, odd_even_comm, ncid1, fname1, attname, dimname);
      ret = ok ? PIO_NOERR : PIO_EINTERNAL;
    }
    if(!check_ret(tf, tf.get_comm(), ret, "Checking contents of file failed :" + fname1)){
      if(disable_fclose){
        if(is_even){
          PIOc_closefile(ncid1);
        }
        PIOc_closefile(ncid2);
      }
      nerrs++;
      continue;
    }

    bool ok2 = check_file(tf, tf.get_comm(), ncid2, fname2, attname, dimname);
    ret = ok2 ? PIO_NOERR : PIO_EINTERNAL;
    if(!check_ret(tf, tf.get_comm(), ret, "Checking contents of file failed :" + fname2)){
      if(disable_fclose){
        if(is_even){
          PIOc_closefile(ncid1);
        }
        PIOc_closefile(ncid2);
      }
      nerrs++;
      continue;
    }

    if(disable_fclose){
      if(is_even){
        PIOc_closefile(ncid1);
      }
      PIOc_closefile(ncid2);
    }
  }

  if(is_even){
    ret = PIOc_finalize(odd_even_iosysid);
    if(!check_ret(tf, odd_even_comm, ret, "Failed to finalize second iosystem")){
      return PIO_EINTERNAL;
    }

    int mpi_ret = MPI_Comm_free(&odd_even_comm);
    if(mpi_ret != MPI_SUCCESS){
      return PIO_EINTERNAL;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
}

/* Create a file with one iosystem - with even procs, and open/read with
 * the same iosystem - with odd procs */
int one_iosystem_odd_even(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const std::string fname("pio_iosys_test_file.nc");
  const std::string attname("filename");
  const std::string dimname("filename_dim");
  int ret = PIO_NOERR;
  int nerrs = 0;

  /* Split world to odd and even procs */
  MPI_Comm odd_even_comm = MPI_COMM_NULL;
  int odd_even_rank = 0, odd_even_size = 0;
  bool is_even = false;
  ret = split_world_odd_even(tf.get_comm(), tf.get_comm_rank(),
      &odd_even_comm, &odd_even_rank, &odd_even_size, &is_even);
  if(ret != PIO_NOERR){
    return PIO_EINTERNAL;
  }

  int odd_even_iosysid = -1;
  ret = PIOc_Init_Intracomm(odd_even_comm, odd_even_size, 1, 0, PIO_REARR_SUBSET,
          &odd_even_iosysid);
  if(!check_ret(tf, odd_even_comm, ret, "Failed to init second iosystem")){
    return PIO_EINTERNAL;
  }

  int old_handler = PIO_INTERNAL_ERROR;
  PIOc_set_iosystem_error_handling(odd_even_iosysid, PIO_BCAST_ERROR, &old_handler);

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int f = 0; f < num_flavors; f++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[f], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : one_iosystem_odd_even : ") + iotype_name + "\n");

    /* Create one file to be opened later - even procs */
    if(is_even){
      ret = create_file(tf, odd_even_comm, odd_even_iosysid, flavors[f], fname, attname, dimname);
    }
    if(!check_ret(tf, tf.get_comm(), ret, "Failed to create file :" + fname)){
      nerrs++;
      continue;
    }

    /* Open file from odd procs */
    if(!is_even){
      int ncid = -1;
      ret = open_and_check_file(tf, odd_even_comm, odd_even_iosysid, flavors[f], &ncid,
              fname, attname, dimname, false);
    }
    if(!check_ret(tf, tf.get_comm(), ret, "Checking contents of file failed :" + fname)){
      nerrs++;
      continue;
    }

    PIOc_deletefile(tf.get_iosysid(), fname.c_str());
  }

  ret = PIOc_finalize(odd_even_iosysid);
  if(!check_ret(tf, odd_even_comm, ret, "Failed to finalize second iosystem")){
    return PIO_EINTERNAL;
  }

  int mpi_ret = MPI_Comm_free(&odd_even_comm);
  if(mpi_ret != MPI_SUCCESS){
    return PIO_EINTERNAL;
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
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
    {"two_iosystems_odd_even", two_iosystems_odd_even},
    {"two_iosystems_even_all", two_iosystems_even_all},
    {"one_iosystem_odd_even", one_iosystem_odd_even}
  };

  SPIO_TF::Test_framework tf("pio_iosystem_tests_cxx", MPI_COMM_WORLD, rfuncs);
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
