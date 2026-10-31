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
 * non-null (unlike split_world_only_even in the Fortran reference, which
 * deliberately uses MPI_UNDEFINED for the excluded group - not needed
 * here, and not ported: it is unused dead code in this specific Fortran
 * file, see this task's plan for the ruling).
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

/* Create a file with a char variable holding a global attribute (filename) */
static int create_file(SPIO_TF::Test_framework &tf, int iosysid, int iotype,
    const std::string &fname, const std::string &attname, const std::string &dimname)
{
  int ncid = -1, dimid = -1, varid = -1;

  int ret = PIOc_createfile(iosysid, &ncid, &iotype, fname.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to create dummy file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(ncid, dimname.c_str(), PIO_TF_MAX_STR_LEN, &dimid);
  if(!check_ret(tf, ret, "Failed to define dim " + dimname + "in file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(ncid, attname.c_str(), PIO_CHAR, 1, &dimid, &varid);
  if(!check_ret(tf, ret, "Failed to define var " + attname + " in file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(ncid, varid, attname.c_str(), PIO_CHAR, fname.size(), fname.c_str());
  if(!check_ret(tf, ret, "Failed to put att " + attname + " in file :" + fname)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_closefile(ncid);
  if(!check_ret(tf, ret, "Failed to close file :" + fname)){
    return PIO_EINTERNAL;
  }

  return PIO_NOERR;
}

/* Check the contents of file : the global attribute 'filename' should be
 * equal to the name of the file, fname */
static bool check_file(SPIO_TF::Test_framework &tf, int ncid, const std::string &fname,
    const std::string &attname, const std::string &dimname)
{
  int dimid = -1, varid = -1;

  int ret = PIOc_inq_dimid(ncid, dimname.c_str(), &dimid);
  if(!check_ret(tf, ret, "Failed to find dim " + dimname + "in file :" + fname)){
    return false;
  }

  ret = PIOc_inq_varid(ncid, attname.c_str(), &varid);
  if(!check_ret(tf, ret, "Failed to find var " + attname + " in file :" + fname)){
    return false;
  }

  std::vector<char> val(PIO_TF_MAX_STR_LEN, '\0');
  ret = PIOc_get_att(ncid, varid, attname.c_str(), val.data());
  if(!check_ret(tf, ret, "Failed to get att " + attname + " in file :" + fname)){
    return false;
  }

  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      "val = " + std::string(val.data()) + ", fname =" + fname + "\n");

  return passert(tf, (std::strncmp(val.data(), fname.c_str(), fname.size()) == 0),
      "Attribute value is not the expected value");
}

/* Open and check the contents of file : open it and check the global
 * attribute 'filename' (should be equal to the name of the file, fname) */
static int open_and_check_file(SPIO_TF::Test_framework &tf, int iosysid, int iotype,
    int *ncid, const std::string &fname, const std::string &attname,
    const std::string &dimname, bool disable_fclose)
{
  int ret = PIOc_openfile(iosysid, ncid, &iotype, fname.c_str(), PIO_WRITE);
  if(!check_ret(tf, ret, "Failed to open:" + fname)){
    return PIO_EINTERNAL;
  }

  if(!check_file(tf, *ncid, fname, attname, dimname)){
    return PIO_EINTERNAL;
  }

  if(!disable_fclose){
    ret = PIOc_closefile(*ncid);
    if(!check_ret(tf, ret, "Failed to close file :" + fname)){
      return PIO_EINTERNAL;
    }
  }

  return PIO_NOERR;
}

/* Create three files with one iosystem - with all procs, and open/read with
 * another iosystem - subset (odd/even) of procs */
int three_files_two_iosystems_odd_even(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const std::string fname0("pio_iosys_test_file0.nc");
  const std::string fname1("pio_iosys_test_file1.nc");
  const std::string fname2("pio_iosys_test_file2.nc");
  const std::string attname("filename");
  const std::string dimname("filename_dim");

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
  if(!check_ret(tf, ret, "Failed to init second iosystem")){
    return PIO_EINTERNAL;
  }

  int old_handler = PIO_INTERNAL_ERROR;
  PIOc_set_iosystem_error_handling(odd_even_iosysid, PIO_BCAST_ERROR, &old_handler);

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  /* Open two different files and close it with two different iosystems */
  for(int f = 0; f < num_flavors; f++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[f], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : three_files_two_iosystems_odd_even : ") + iotype_name + "\n");

    /* Create three files to be opened later - world - all procs */
    ret = create_file(tf, tf.get_iosysid(), flavors[f], fname0, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    ret = create_file(tf, tf.get_iosysid(), flavors[f], fname1, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    ret = create_file(tf, tf.get_iosysid(), flavors[f], fname2, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    /* Open file0 from all procs - disable close */
    int ncid0 = -1;
    ret = open_and_check_file(tf, tf.get_iosysid(), flavors[f], &ncid0, fname0,
            attname, dimname, true);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    /* Open file1 from even processes and file2 from odd processes -
     * disable close */
    const std::string &fname = is_even ? fname1 : fname2;

    int ncid = -1;
    ret = open_and_check_file(tf, odd_even_iosysid, flavors[f], &ncid, fname,
            attname, dimname, true);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    /* Make sure that we can still check the contents of the file */
    if(!check_file(tf, ncid, fname, attname, dimname)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid);
    if(!check_ret(tf, ret, "Failed to close file :" + fname)){
      return PIO_EINTERNAL;
    }

    ret = PIOc_closefile(ncid0);
    if(!check_ret(tf, ret, "Failed to close file :" + fname0)){
      return PIO_EINTERNAL;
    }
  }

  ret = PIOc_finalize(odd_even_iosysid);
  if(!check_ret(tf, ret, "Failed to finalize second iosystem")){
    return PIO_EINTERNAL;
  }

  int mpi_ret = MPI_Comm_free(&odd_even_comm);
  if(mpi_ret != MPI_SUCCESS){
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
    {"three_files_two_iosystems_odd_even", three_files_two_iosystems_odd_even}
  };

  SPIO_TF::Test_framework tf("pio_iosystem_tests2_cxx", MPI_COMM_WORLD, rfuncs);
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
