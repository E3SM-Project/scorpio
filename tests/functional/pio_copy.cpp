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

template<typename T>
int test_copy_att(SPIO_TF::Test_framework &tf, int iotype, const std::string &iotype_name)
{
  const int DIM_LEN = 100;
  const std::string ifilename("test_pio_copy.itestfile");
  const std::string ofilename("test_pio_copy.otestfile");
  const std::string ivname("dummy_ival");
  const std::string ovname("dummy_oval");
  const std::string dimname("dummy_dim");
  const std::string attname("dummy_att_val");
  const std::string cattname("dummy_att_cval");

  T init_val = static_cast<T>(tf.get_comm_size());
  std::vector<T> pval(DIM_LEN, init_val);
  std::string pcval("DUMMY_STRING");

  tf.get_logger().log(SPIO_Util::Logger::Log_level::INFO,
      std::string("Testing : pio_copy : ") + iotype_name + "\n");

  int pio_ifile = -1, pio_ofile = -1;
  int ret = PIOc_createfile(tf.get_iosysid(), &pio_ifile, &iotype, ifilename.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open: " + ifilename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_createfile(tf.get_iosysid(), &pio_ofile, &iotype, ofilename.c_str(), PIO_CLOBBER);
  if(!check_ret(tf, ret, "Failed to open: " + ofilename)){
    PIOc_closefile(pio_ifile);
    return PIO_EINTERNAL;
  }

  int pio_idim = -1, pio_odim = -1, pio_ivarid = -1, pio_ovarid = -1;
  ret = PIOc_def_dim(pio_ifile, dimname.c_str(), DIM_LEN, &pio_idim);
  if(!check_ret(tf, ret, "Failed to define dim: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(pio_ifile, ivname.c_str(), Pio_type_map<T>::value, 1, &pio_idim, &pio_ivarid);
  if(!check_ret(tf, ret, "Failed to define var: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_dim(pio_ofile, dimname.c_str(), DIM_LEN, &pio_odim);
  if(!check_ret(tf, ret, "Failed to define dim: " + ofilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_def_var(pio_ofile, ovname.c_str(), Pio_type_map<T>::value, 1, &pio_odim, &pio_ovarid);
  if(!check_ret(tf, ret, "Failed to define var: " + ofilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(pio_ifile, PIO_GLOBAL, attname.c_str(), Pio_type_map<T>::value, DIM_LEN, pval.data());
  if(!check_ret(tf, ret, "Failed to put global attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(pio_ifile, PIO_GLOBAL, cattname.c_str(), PIO_CHAR, pcval.size(), pcval.c_str());
  if(!check_ret(tf, ret, "Failed to put global char attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(pio_ifile, pio_ivarid, attname.c_str(), Pio_type_map<T>::value, DIM_LEN, pval.data());
  if(!check_ret(tf, ret, "Failed to put attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_put_att(pio_ifile, pio_ivarid, cattname.c_str(), PIO_CHAR, pcval.size(), pcval.c_str());
  if(!check_ret(tf, ret, "Failed to put char attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_copy_att(pio_ifile, PIO_GLOBAL, attname.c_str(), pio_ofile, PIO_GLOBAL);
  if(!check_ret(tf, ret, "Failed to copy global attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_copy_att(pio_ifile, PIO_GLOBAL, cattname.c_str(), pio_ofile, PIO_GLOBAL);
  if(!check_ret(tf, ret, "Failed to copy global char attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_copy_att(pio_ifile, pio_ivarid, attname.c_str(), pio_ofile, pio_ovarid);
  if(!check_ret(tf, ret, "Failed to copy attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_copy_att(pio_ifile, pio_ivarid, cattname.c_str(), pio_ofile, pio_ovarid);
  if(!check_ret(tf, ret, "Failed to copy char attribute: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(pio_ifile);
  if(!check_ret(tf, ret, "Failed to enddef: " + ifilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  ret = PIOc_enddef(pio_ofile);
  if(!check_ret(tf, ret, "Failed to enddef: " + ofilename)){
    PIOc_closefile(pio_ifile);
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  PIOc_closefile(pio_ifile);
  PIOc_closefile(pio_ofile);

  ret = PIOc_openfile(tf.get_iosysid(), &pio_ofile, &iotype, ofilename.c_str(), PIO_NOWRITE);
  if(!check_ret(tf, ret, "Could not reopen file " + ofilename)){
    return PIO_EINTERNAL;
  }

  ret = PIOc_inq_varid(pio_ofile, ovname.c_str(), &pio_ovarid);
  if(!check_ret(tf, ret, "Could not inq var: " + ofilename)){
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }

  bool ok = true;

  std::vector<T> gval(DIM_LEN, static_cast<T>(0));
  ret = PIOc_get_att(pio_ofile, PIO_GLOBAL, attname.c_str(), gval.data());
  if(!check_ret(tf, ret, "Failed to get global attribute: " + ofilename)){
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong value") && ok;

  std::vector<char> gcval(DIM_LEN, '\0');
  ret = PIOc_get_att(pio_ofile, PIO_GLOBAL, cattname.c_str(), gcval.data());
  if(!check_ret(tf, ret, "Failed to get global char attribute: " + ofilename)){
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (std::strncmp(gcval.data(), pcval.c_str(), pcval.size()) == 0),
      "Got wrong value") && ok;

  std::fill(gval.begin(), gval.end(), static_cast<T>(0));
  ret = PIOc_get_att(pio_ofile, pio_ovarid, attname.c_str(), gval.data());
  if(!check_ret(tf, ret, "Failed to get attribute: " + ofilename)){
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }
  ok = check_val(tf, gval, pval, "Got wrong value") && ok;

  std::fill(gcval.begin(), gcval.end(), '\0');
  ret = PIOc_get_att(pio_ofile, pio_ovarid, cattname.c_str(), gcval.data());
  if(!check_ret(tf, ret, "Failed to get char attribute: " + ofilename)){
    PIOc_closefile(pio_ofile);
    return PIO_EINTERNAL;
  }
  ok = passert(tf, (std::strncmp(gcval.data(), pcval.c_str(), pcval.size()) == 0),
      "Got wrong value") && ok;

  PIOc_closefile(pio_ofile);

  PIOc_deletefile(tf.get_iosysid(), ifilename.c_str());
  PIOc_deletefile(tf.get_iosysid(), ofilename.c_str());

  return ok ? PIO_NOERR : PIO_EINTERNAL;
}

template<typename T>
int test_copy_att_driver(SPIO_TF::Test_framework &tf)
{
  int num_flavors = 0;
  int flavors[SPIO_TEST_NUM_FLAVORS];
  get_iotypes(&num_flavors, flavors);

  int nerrs = 0;
  for(int i = 0; i < num_flavors; i++){
    char iotype_name[PIO_MAX_NAME + 1];
    iotype_name[0] = '\0';
    get_iotype_name(flavors[i], iotype_name);

    if(test_copy_att<T>(tf, flavors[i], iotype_name) != PIO_NOERR){
      nerrs++;
    }
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
    {"test_copy_att_int", test_copy_att_driver<int>},
    {"test_copy_att_float", test_copy_att_driver<float>},
    {"test_copy_att_double", test_copy_att_driver<double>}
  };

  SPIO_TF::Test_framework tf("pio_copy_cxx", MPI_COMM_WORLD, rfuncs);
  tf.init(argc, argv);

  if(tf.init_iosystem(PIO_REARR_BOX) == PIO_NOERR){
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
