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
const int NUM_COMMS = 2;

/* Split tf's comm into NUM_COMMS=2 sub-communicators that are disjoint
 * except both contain rank == overlapped_rank. Two-pass structure (compute
 * both groups first, free world_group only after both are computed, then
 * create both communicators, freeing each group right after creating its
 * comm) mirrors the Fortran reference exactly.
 */
static int split_world_two_with_overlap(MPI_Comm world_comm, int world_sz,
    MPI_Comm new_comms[], int new_ranks[], int new_sizes[], int overlapped_rank)
{
  const int NUM_GROUP_RANGES = 2;
  MPI_Group new_groups[NUM_COMMS];

  for(int i = 0; i < NUM_COMMS; i++){
    new_comms[i] = MPI_COMM_NULL;
    new_ranks[i] = -1;
    new_sizes[i] = 0;
  }

  MPI_Group world_group;
  MPI_Comm_group(world_comm, &world_group);

  for(int i = 0; i < NUM_COMMS; i++){
    int nranges = 0;
    int new_group_ranges[NUM_GROUP_RANGES][3];

    if(world_sz == 1){
      nranges = 1;
      new_group_ranges[0][0] = overlapped_rank;
      new_group_ranges[0][1] = overlapped_rank;
      new_group_ranges[0][2] = 1;
    }
    else if((overlapped_rank / NUM_COMMS) != i){
      nranges = 2;
      /* Include rank == overlapped_rank */
      new_group_ranges[0][0] = overlapped_rank;
      new_group_ranges[0][1] = overlapped_rank;
      new_group_ranges[0][2] = 1;

      /* Include other processes split evenly among NUM_COMMS */
      new_group_ranges[1][0] = i;
      new_group_ranges[1][1] = i + (world_sz / NUM_COMMS - 1) * NUM_COMMS;
      new_group_ranges[1][2] = NUM_COMMS;
    }
    else{
      nranges = 1;
      /* rank == overlapped_rank is already included in this range */
      new_group_ranges[0][0] = i;
      new_group_ranges[0][1] = i + (world_sz / NUM_COMMS - 1) * NUM_COMMS;
      new_group_ranges[0][2] = NUM_COMMS;
    }

    MPI_Group_range_incl(world_group, nranges, new_group_ranges, &new_groups[i]);
  }

  MPI_Group_free(&world_group);

  for(int i = 0; i < NUM_COMMS; i++){
    int mpi_ret = MPI_Comm_create(world_comm, new_groups[i], &new_comms[i]);
    MPI_Group_free(&new_groups[i]);
    if(mpi_ret != MPI_SUCCESS){
      return PIO_EINTERNAL;
    }

    if(new_comms[i] != MPI_COMM_NULL){
      MPI_Comm_rank(new_comms[i], &new_ranks[i]);
      MPI_Comm_size(new_comms[i], &new_sizes[i]);
    }
    else{
      new_ranks[i] = -1;
      new_sizes[i] = 0;
    }
  }

  return PIO_NOERR;
}

/* Create a file with a char variable holding a global attribute (filename) */
static int create_file(SPIO_TF::Test_framework &tf, MPI_Comm comm, int iosysid, int iotype,
    const std::string &fname, const std::string &attname, const std::string &dimname)
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
 * equal to the name of the file, fname */
static bool check_file(SPIO_TF::Test_framework &tf, MPI_Comm comm, int ncid, const std::string &fname,
    const std::string &attname, const std::string &dimname)
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
 * attribute 'filename' (should be equal to the name of the file, fname) */
static int open_and_check_file(SPIO_TF::Test_framework &tf, MPI_Comm comm, int iosysid, int iotype,
    int *ncid, const std::string &fname, const std::string &attname,
    const std::string &dimname, bool disable_fclose)
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

/* Create three files with one iosystem - with all procs, and open/read with
 * two different iosystems - subset of procs each. The two iosystems overlap
 * at rank 0 (and are disjoint otherwise) */
int three_files_two_iosystems_with_overlap(SPIO_TF::Test_framework &tf)
{
  SPIO_Util::Logger::MPI_logger<std::ofstream> logger = tf.get_logger();
  const std::string fname0("pio_iosys_test_file0.nc");
  const std::string fname1("pio_iosys_test_file1.nc");
  const std::string fname2("pio_iosys_test_file2.nc");
  const std::string attname("filename");
  const std::string dimname("filename_dim");

  /* Split world into two comms, overlapping only at rank 0 */
  MPI_Comm overlapped_comms[NUM_COMMS];
  int overlapped_comm_ranks[NUM_COMMS];
  int overlapped_comm_sizes[NUM_COMMS];
  int ret = split_world_two_with_overlap(tf.get_comm(), tf.get_comm_size(),
      overlapped_comms, overlapped_comm_ranks, overlapped_comm_sizes, 0);
  if(ret != PIO_NOERR){
    return PIO_EINTERNAL;
  }

  int overlapped_iosysid[NUM_COMMS];
  for(int i = 0; i < NUM_COMMS; i++){
    overlapped_iosysid[i] = -1;
    if(overlapped_comms[i] != MPI_COMM_NULL){
      int pio_base_rank = 0, pio_num_iotasks = 0;
      /* If we have more than 1 proc, make sure that the io tasks start
       * from 1, since rank=0 is always shared by all overlapped_comms */
      if(overlapped_comm_sizes[i] > 1){
        pio_base_rank = 1;
        pio_num_iotasks = overlapped_comm_sizes[i] - 1;
      }
      else{
        pio_base_rank = 0;
        pio_num_iotasks = overlapped_comm_sizes[i];
      }

      ret = PIOc_Init_Intracomm(overlapped_comms[i], pio_num_iotasks, 1,
              pio_base_rank, PIO_REARR_SUBSET, &overlapped_iosysid[i]);
      if(!check_ret(tf, overlapped_comms[i], ret, "Failed to init overlapped iosystem")){
        return PIO_EINTERNAL;
      }

      int old_handler = PIO_INTERNAL_ERROR;
      PIOc_set_iosystem_error_handling(overlapped_iosysid[i], PIO_BCAST_ERROR, &old_handler);
    }
  }

  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  for(int f = 0; f < num_flavors; f++){
    char iotype_name[PIO_MAX_NAME + 1];
    get_iotype_name(flavors[f], iotype_name);
    logger.log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing : three_files_two_iosystems_with_overlap : ") + iotype_name + "\n");

    /* Create three files to be opened later - world - all procs */
    ret = create_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], fname0, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    ret = create_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], fname1, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    ret = create_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], fname2, attname, dimname);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    /* Open file0 from all procs - disable close */
    int ncid0 = -1;
    ret = open_and_check_file(tf, tf.get_comm(), tf.get_iosysid(), flavors[f], &ncid0,
            fname0, attname, dimname, true);
    if(ret != PIO_NOERR){
      return PIO_EINTERNAL;
    }

    /* The two overlapped comms operate on different files. The file is
     * toggled *before* each use, starting from fname1, so comm 0 actually
     * opens fname2 and comm 1 opens fname1 - ported verbatim from the
     * Fortran's pointer-swap-before-use sequence. */
    const std::string *fname = &fname1;
    const std::string *fnames[NUM_COMMS];
    int ncids[NUM_COMMS];
    for(int j = 0; j < NUM_COMMS; j++){
      fname = (fname == &fname1) ? &fname2 : &fname1;
      fnames[j] = fname;
      ncids[j] = -1;
      if(overlapped_comms[j] != MPI_COMM_NULL){
        ret = open_and_check_file(tf, overlapped_comms[j], overlapped_iosysid[j], flavors[f],
                &ncids[j], *fname, attname, dimname, true);
        if(ret != PIO_NOERR){
          return PIO_EINTERNAL;
        }

        /* Make sure that we can still check the contents of the file */
        if(!check_file(tf, overlapped_comms[j], ncids[j], *fname, attname, dimname)){
          return PIO_EINTERNAL;
        }
      }
    }

    for(int j = 0; j < NUM_COMMS; j++){
      if(overlapped_comms[j] != MPI_COMM_NULL){
        ret = PIOc_closefile(ncids[j]);
        if(!check_ret(tf, overlapped_comms[j], ret, "Failed to close file :" + *fnames[j])){
          return PIO_EINTERNAL;
        }
      }
    }

    ret = PIOc_closefile(ncid0);
    if(!check_ret(tf, ret, "Failed to close file :" + fname0)){
      return PIO_EINTERNAL;
    }
  }

  for(int i = 0; i < NUM_COMMS; i++){
    if(overlapped_comms[i] != MPI_COMM_NULL){
      ret = PIOc_finalize(overlapped_iosysid[i]);
      if(!check_ret(tf, overlapped_comms[i], ret, "Failed to finalize overlapped iosystem")){
        return PIO_EINTERNAL;
      }

      int mpi_ret = MPI_Comm_free(&overlapped_comms[i]);
      if(mpi_ret != MPI_SUCCESS){
        return PIO_EINTERNAL;
      }
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
    {"three_files_two_iosystems_with_overlap", three_files_two_iosystems_with_overlap}
  };

  SPIO_TF::Test_framework tf("pio_iosystem_tests3_cxx", MPI_COMM_WORLD, rfuncs);
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
