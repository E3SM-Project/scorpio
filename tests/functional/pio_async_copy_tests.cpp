#include <algorithm>
#include <iostream>
#include <string>
#include <vector>
#include <functional>
#include <utility>
#include <cstring>

#include "pio.h"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#include "spio_test_framework.hpp"
#include "spio_test_check.hpp"
#include "spio_test_common.hpp"

/* Split tf.get_comm() (the test's world communicator) into ncomms disjoint
 * sub-communicators of as-equal-as-possible size. The last sub-comm
 * absorbs any remainder ranks. gcomm_idx is the 1-based index (1..ncomms)
 * of the sub-comm this rank lands in, mirroring
 * tests/general/pio_async_file_tests.F90.in's split_world_ncomms(). */
static void split_world_ncomms(const SPIO_TF::Test_framework &tf, int ncomms,
    MPI_Comm &new_comm, int &new_rank, int &new_size, int &gcomm_idx)
{
  new_comm = MPI_COMM_NULL;
  new_rank = 0;
  new_size = 0;
  gcomm_idx = 0;

  int world_sz = tf.get_comm_size();
  int world_rank = tf.get_comm_rank();

  /* We need at least one proc in each disjoint comm */
  if(world_sz < ncomms){
    return;
  }

  int nprocs_per_comm = world_sz / ncomms;
  bool is_last_comm = (world_rank >= nprocs_per_comm * (ncomms - 1));

  int color = world_rank / nprocs_per_comm;
  /* Make sure that every proc in the last set (all remaining procs are
   * included in the last set/comm) gets the same color */
  if(is_last_comm){
    color = ncomms - 1;
  }
  gcomm_idx = color + 1;

  MPI_Comm_split(tf.get_comm(), color, 0, &new_comm);
  MPI_Comm_size(new_comm, &new_size);
  MPI_Comm_rank(new_comm, &new_rank);
}

/* Create a union comm, ucomm, of every proc in gcomm that passes
 * join_ucomm == true. Mirrors tests/general/pio_async_copy_tests.F90.in's
 * create_ucomm() subroutine. Fortran's MPI_LOGICAL join flags become
 * plain 0/1 MPI_INT flags here (no direct MPI C equivalent of Fortran
 * LOGICAL is needed elsewhere in this codebase). */
static void create_ucomm(MPI_Comm gcomm, bool join_ucomm, MPI_Comm &ucomm)
{
  ucomm = MPI_COMM_NULL;

  int rank = 0, sz = 0;
  MPI_Comm_rank(gcomm, &rank);
  MPI_Comm_size(gcomm, &sz);

  int ljoin = join_ucomm ? 1 : 0;
  std::vector<int> gjoin(sz, 0);
  MPI_Allgather(&ljoin, 1, MPI_INT, gjoin.data(), 1, MPI_INT, gcomm);

  std::vector<int> ucomm_ranks;
  for(int i = 0; i < sz; i++){
    if(gjoin[i]){
      ucomm_ranks.push_back(i);
    }
  }

  MPI_Group gcomm_group, ucomm_group;
  MPI_Comm_group(gcomm, &gcomm_group);
  MPI_Group_incl(gcomm_group, static_cast<int>(ucomm_ranks.size()), ucomm_ranks.data(), &ucomm_group);
  MPI_Comm_create(gcomm, ucomm_group, &ucomm);
  MPI_Group_free(&gcomm_group);
  MPI_Group_free(&ucomm_group);
}

/* Async I/O as a service test with NUM_COMPONENTS compute components and
 * 1 I/O comm. Each compute component copies attributes (global + variable,
 * numeric + char) from a file it creates (ifnames[comp_idx]) to a second
 * file it creates (ofnames[comp_idx]), then verifies the copied attributes
 * are present in the *other* compute component's output file (out-of-order
 * verification, matching the Fortran reference).
 *
 * Mirrors tests/functional/pio_async_file_tests.cpp's async-I/O
 * architecture: manages its own iosystems via PIOc_init_intercomm()/
 * PIOc_finalize() rather than Test_framework::init_iosystem(), so main()
 * below never calls init_iosystem()/finalize_iosystem() for this file.
 *
 * Once ranks diverge into I/O vs. compute roles, the plain (tf.get_comm()
 * scoped) check_ret()/passert() overloads are off-limits for the same
 * deadlock reason as pio_async_file_tests.cpp. Return-code/value checks on
 * PIO calls instead use the comm-scoped check_ret(tf, comm, ...)/
 * passert(tf, comm, ...) overloads from spio_test_check.hpp, scoped to
 * this component's own new_comm - mirroring the Fortran reference's own
 * PIO_TF_CHECK_ERR(ret, new_comm, ...) three-argument form. */
template<typename T>
int test_async_copy_att(SPIO_TF::Test_framework &tf)
{
  const int NUM_COMPONENTS = 2;
  const int NCOMMS = NUM_COMPONENTS + 1;
  const int MIN_NUM_PROCS = NUM_COMPONENTS + 1;
  const int VEC_LOCAL_SZ = 7;
  const int NUM_DIMS = 1;
  const int NUM_REARRANGERS = 2;
  const int ATT_LEN = 32;
  const int rearrs[NUM_REARRANGERS] = {PIO_REARR_SUBSET, PIO_REARR_BOX};
  const std::string rearrs_info[NUM_REARRANGERS] = {"PIO_REARR_SUBSET", "PIO_REARR_BOX"};

  const std::string idimname("PIO_TF_test_idim");
  const std::string odimname("PIO_TF_test_odim");
  const std::string ivname("PIO_TF_test_ivar");
  const std::string ovname("PIO_TF_test_ovar");
  const std::string attname("PIO_TF_test_gatt");
  const std::string cattname("PIO_TF_test_gcatt");

  std::string ifnames[NUM_COMPONENTS], ofnames[NUM_COMPONENTS];
  for(int i = 0; i < NUM_COMPONENTS; i++){
    ifnames[i] = "pio_async_copy_ifile_comp" + std::to_string(i + 1) + ".nc";
    ofnames[i] = "pio_async_copy_ofile_comp" + std::to_string(i + 1) + ".nc";
  }

  T val = static_cast<T>(tf.get_comm_size());
  std::string cval("PIO_TF_test_catt");

  int nerrs = 0;

  if(tf.get_comm_size() < MIN_NUM_PROCS){
    return PIO_NOERR;
  }

  for(int i = 0; i < NUM_REARRANGERS; i++){
    tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
        std::string("Testing rearr : ") + rearrs_info[i] + "\n");

    MPI_Comm new_comm = MPI_COMM_NULL;
    int new_rank = 0, new_size = 0, gcomm_idx = 0;
    split_world_ncomms(tf, NCOMMS, new_comm, new_rank, new_size, gcomm_idx);

    std::vector<MPI_Comm> comp_comms(NUM_COMPONENTS, MPI_COMM_NULL);
    std::vector<int> iosysids(NUM_COMPONENTS, -1);

    bool is_io = (gcomm_idx == 1);
    int comp_idx = gcomm_idx - 2;

    MPI_Comm comp_ucomm = MPI_COMM_NULL;
    create_ucomm(tf.get_comm(), !is_io, comp_ucomm);

    MPI_Comm io_comm = MPI_COMM_NULL;
    int ret = PIO_NOERR;

    if(is_io){
      io_comm = new_comm;
      ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
              io_comm, rearrs[i], iosysids.data());
      if(ret != PIO_NOERR){
        tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_init_intercomm() FAILED on I/O process, ret = " + std::to_string(ret) + "\n");
        nerrs++;
      }
    }
    else{
      comp_comms[comp_idx] = new_comm;
      io_comm = MPI_COMM_NULL;
      ret = PIOc_init_intercomm(NUM_COMPONENTS, tf.get_comm(), comp_comms.data(),
              io_comm, rearrs[i], iosysids.data());
      if(ret != PIO_NOERR){
        tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
            "ERROR: PIOc_init_intercomm() FAILED on compute process, ret = " + std::to_string(ret) + "\n");
        nerrs++;
      }
      else{
        int old_handler = PIO_INTERNAL_ERROR;
        PIOc_set_iosystem_error_handling(iosysids[comp_idx], PIO_BCAST_ERROR, &old_handler);

        int dims[NUM_DIMS];
        dims[0] = VEC_LOCAL_SZ * new_size;

        int num_flavors = 0;
        int flavors[SPIO_TEST_NUM_FLAVORS];
        get_iotypes(&num_flavors, flavors);

        for(int j = 0; j < num_flavors; j++){
          char iotype_name[PIO_MAX_NAME + 1];
          iotype_name[0] = '\0';
          get_iotype_name(flavors[j], iotype_name);
          tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
              std::string("Testing : ") + iotype_name + "\n");

          int pio_ifile = -1, pio_ofile = -1;
          ret = PIOc_createfile(iosysids[comp_idx], &pio_ifile, &flavors[j],
                  ifnames[comp_idx].c_str(), PIO_CLOBBER);
          if(!check_ret(tf, new_comm, ret, "Could not create input file : " + ifnames[comp_idx])){
            nerrs++;
            continue;
          }

          ret = PIOc_createfile(iosysids[comp_idx], &pio_ofile, &flavors[j],
                  ofnames[comp_idx].c_str(), PIO_CLOBBER);
          if(!check_ret(tf, new_comm, ret, "Could not create output file : " + ofnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            continue;
          }

          int pio_idim = -1, pio_odim = -1;
          ret = PIOc_def_dim(pio_ifile, idimname.c_str(), dims[0], &pio_idim);
          if(!check_ret(tf, new_comm, ret, "Could not define dim in input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_def_dim(pio_ofile, odimname.c_str(), dims[0], &pio_odim);
          if(!check_ret(tf, new_comm, ret, "Could not define dim in output file : " + ofnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          int pio_ivarid = -1, pio_ovarid = -1;
          ret = PIOc_def_var(pio_ifile, ivname.c_str(), Pio_type_map<T>::value, NUM_DIMS, &pio_idim, &pio_ivarid);
          if(!check_ret(tf, new_comm, ret, "Could not define var in input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_def_var(pio_ofile, ovname.c_str(), Pio_type_map<T>::value, NUM_DIMS, &pio_odim, &pio_ovarid);
          if(!check_ret(tf, new_comm, ret, "Could not define var in output file : " + ofnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_put_att(pio_ifile, PIO_GLOBAL, attname.c_str(), Pio_type_map<T>::value, 1, &val);
          if(!check_ret(tf, new_comm, ret, "Could not put attribute in the input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_put_att(pio_ifile, PIO_GLOBAL, cattname.c_str(), PIO_CHAR, cval.size(), cval.c_str());
          if(!check_ret(tf, new_comm, ret, "Could not put char attribute in the input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_put_att(pio_ifile, pio_ivarid, attname.c_str(), Pio_type_map<T>::value, 1, &val);
          if(!check_ret(tf, new_comm, ret, "Could not put variable attribute in the input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_put_att(pio_ifile, pio_ivarid, cattname.c_str(), PIO_CHAR, cval.size(), cval.c_str());
          if(!check_ret(tf, new_comm, ret, "Could not put char variable attribute in the input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_copy_att(pio_ifile, PIO_GLOBAL, attname.c_str(), pio_ofile, PIO_GLOBAL);
          if(!check_ret(tf, new_comm, ret, "Could not copy global attribute from input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_copy_att(pio_ifile, PIO_GLOBAL, cattname.c_str(), pio_ofile, PIO_GLOBAL);
          if(!check_ret(tf, new_comm, ret, "Could not copy global char attribute from input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_copy_att(pio_ifile, pio_ivarid, attname.c_str(), pio_ofile, pio_ovarid);
          if(!check_ret(tf, new_comm, ret, "Could not copy variable attribute from input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_copy_att(pio_ifile, pio_ivarid, cattname.c_str(), pio_ofile, pio_ovarid);
          if(!check_ret(tf, new_comm, ret, "Could not copy variable char attribute from input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_enddef(pio_ifile);
          if(!check_ret(tf, new_comm, ret, "Could not enddef in input file : " + ifnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          ret = PIOc_enddef(pio_ofile);
          if(!check_ret(tf, new_comm, ret, "Could not enddef in output file : " + ofnames[comp_idx])){
            nerrs++;
            PIOc_closefile(pio_ifile);
            PIOc_closefile(pio_ofile);
            continue;
          }

          PIOc_closefile(pio_ifile);
          PIOc_closefile(pio_ofile);

          // Since we verify out of order, ensure that all compute comps are done
          MPI_Barrier(comp_ucomm);

          int vidx = NUM_COMPONENTS - comp_idx - 1;
          int pio_vfile = -1;
          ret = PIOc_openfile(iosysids[comp_idx], &pio_vfile, &flavors[j],
                  ofnames[vidx].c_str(), PIO_NOWRITE);
          if(!check_ret(tf, new_comm, ret, "Could not reopen file : " + ofnames[vidx])){
            nerrs++;
            continue;
          }

          ret = PIOc_inq_varid(pio_vfile, ovname.c_str(), &pio_ovarid);
          if(!check_ret(tf, new_comm, ret, "Could not inq var : " + ofnames[vidx])){
            nerrs++;
            PIOc_closefile(pio_vfile);
            continue;
          }

          bool ok = true;

          T rval = static_cast<T>(0);
          ret = PIOc_get_att(pio_vfile, PIO_GLOBAL, attname.c_str(), &rval);
          if(!check_ret(tf, new_comm, ret, "Could not get global att : " + ofnames[vidx])){
            nerrs++;
            PIOc_closefile(pio_vfile);
            continue;
          }
          ok = passert(tf, new_comm, (rval == val), "Global attribute is not the expected value") && ok;

          std::vector<char> rcval(ATT_LEN, '\0');
          ret = PIOc_get_att(pio_vfile, PIO_GLOBAL, cattname.c_str(), rcval.data());
          if(!check_ret(tf, new_comm, ret, "Could not get global char att : " + ofnames[vidx])){
            nerrs++;
            PIOc_closefile(pio_vfile);
            continue;
          }
          ok = passert(tf, new_comm, (std::strncmp(rcval.data(), cval.c_str(), cval.size()) == 0),
              "Global char attribute is not the expected value") && ok;

          rval = static_cast<T>(0);
          ret = PIOc_get_att(pio_vfile, pio_ovarid, attname.c_str(), &rval);
          if(!check_ret(tf, new_comm, ret, "Could not get variable att : " + ofnames[vidx])){
            nerrs++;
            PIOc_closefile(pio_vfile);
            continue;
          }
          ok = passert(tf, new_comm, (rval == val), "Variable attribute is not the expected value") && ok;

          std::fill(rcval.begin(), rcval.end(), '\0');
          ret = PIOc_get_att(pio_vfile, pio_ovarid, cattname.c_str(), rcval.data());
          if(!check_ret(tf, new_comm, ret, "Could not get variable char att : " + ofnames[vidx])){
            nerrs++;
            PIOc_closefile(pio_vfile);
            continue;
          }
          ok = passert(tf, new_comm, (std::strncmp(rcval.data(), cval.c_str(), cval.size()) == 0),
              "Variable char attribute is not the expected value") && ok;

          if(!ok){
            nerrs++;
          }

          PIOc_closefile(pio_vfile);

          // Since we verify out of order, ensure that all compute comps are done
          MPI_Barrier(comp_ucomm);

          PIOc_deletefile(iosysids[comp_idx], ifnames[comp_idx].c_str());
          PIOc_deletefile(iosysids[comp_idx], ofnames[comp_idx].c_str());

          // Since we verify out of order, ensure that all compute comps are done
          MPI_Barrier(comp_ucomm);
        }

        for(int j = 0; j < NUM_COMPONENTS; j++){
          ret = PIOc_finalize(iosysids[j]);
        }
      }
    }

    if(comp_ucomm != MPI_COMM_NULL){
      MPI_Comm_free(&comp_ucomm);
    }
    if(new_comm != MPI_COMM_NULL){
      MPI_Comm_free(&new_comm);
    }
    if(!check_ret(tf, ret, "PIO_init()/Finalize() failed")){
      nerrs++;
    }
  }

  return (nerrs == 0) ? PIO_NOERR : PIO_EINTERNAL;
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
    {"test_async_copy_att_int", test_async_copy_att<int>},
    {"test_async_copy_att_float", test_async_copy_att<float>},
    {"test_async_copy_att_double", test_async_copy_att<double>}
  };

  SPIO_TF::Test_framework tf("pio_async_copy_tests_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  /* This test manages its own async iosystems per registered function
   * (see test_async_copy_att() above), so init_iosystem()/
   * finalize_iosystem() are intentionally not used here. */
  nerrs += tf.run();

  tf.finalize();
  MPI_Finalize();
  return nerrs;
}
