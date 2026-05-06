#include "e3sm_fgi_utils.hpp"
#include <regex>
#include <cstdlib>
#include "argparser.h"

namespace Util{
  namespace GVars{
    /* Types used for parsing user input */
    /* Available I/O types */
    std::unordered_map<std::string, int> iotypes = {
      {"PNETCDF", PIO_IOTYPE_PNETCDF},
      {"NETCDF", PIO_IOTYPE_NETCDF},
      {"NETCDF4C", PIO_IOTYPE_NETCDF4C},
      {"NETCDF4P", PIO_IOTYPE_NETCDF4P},
      {"NCZARR", PIO_IOTYPE_NETCDF4P_NCZARR},
      {"HDF5", PIO_IOTYPE_HDF5},
      {"HDF5C", PIO_IOTYPE_HDF5C}
    };

    /* Available I/O rearrangers */
    std::unordered_map<std::string, int> rearrs = {
      {"SUBSET", PIO_REARR_SUBSET},
      {"BOX", PIO_REARR_BOX},
      {"ANY", PIO_REARR_ANY}
    };

    /* E3SM pseudo test cases */
    std::unordered_map<std::string, E3SM_FGI::Case_Type> cases = {
      {"F", E3SM_FGI::Case_Type::E3SM_F_CASE},
      {"G", E3SM_FGI::Case_Type::E3SM_G_CASE},
      {"I", E3SM_FGI::Case_Type::E3SM_I_CASE}
    };

    /* Log levels */
    std::unordered_map<std::string, Util::Logging::LogLevel> llevels = {
      {"STATUS", Util::Logging::LogLevel::STATUS},
      {"VERBOSE", Util::Logging::LogLevel::VERBOSE},
      {"WARNING", Util::Logging::LogLevel::WARNING},
      {"ERROR", Util::Logging::LogLevel::ERROR}
    };

    /* The global logger */
    std::shared_ptr<Util::Logging::Logger> logger;
  }
}

/* Initialize the argparser options - specify the available user opts */
static void init_user_options(spio_tool_utils::ArgParser &ap)
{
  ap.add_opt("pio-format", "SCORPIO I/O type (for output data). Supported iotypes: " + Util::GVars::iotypes2str())
    .add_opt("nioprocs", "Number of I/O processes")
    .add_opt("rearr", "SCORPIO rearranger. Supported rearrangers: " + Util::GVars::rearrs2str())
    .add_opt("e3sm-case", "Pseudo E3SM Cases to run: " + Util::GVars::cases2str())
    .add_opt("log-level", "Logging level : " + Util::GVars::llevels2str())
    .add_opt("help", "Print help");
}

/* Parse the user options */
static int get_user_options(
              spio_tool_utils::ArgParser &ap,
              int argc, char *argv[],
              std::vector<int> &iotypes,
              std::vector<int> &rearrs,
              std::vector<E3SM_FGI::Case_Type> &cases,
              Util::Logging::LogLevel &llevel,
              int nioprocs, int rank)
{
#ifdef SPIO_NO_CXX_REGEX
  ap.no_regex_parse(argc, argv);
#else
  ap.parse(argc, argv);
#endif

  if(ap.has_arg("help")){
    return -1;
  }

  std::string iotype_str, rearr_str, ctype_str, llevel_str;
  if(ap.has_arg("pio-format")){
    iotype_str = Util::String::toupper(ap.get_arg<std::string>("pio-format"));
    try{
      iotypes.push_back(Util::GVars::str2iotype(iotype_str));
    }
    catch(std::out_of_range &e){
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Invalid I/O format specified : " + iotype_str);
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, e.what());
      ap.print_usage(std::cerr);
      throw;
    }
  }
  else{
    Util::GVars::copy_opt_map(Util::GVars::iotypes, std::back_inserter(iotypes));
  }

  if(ap.has_arg("rearr")){
    rearr_str = Util::String::toupper(ap.get_arg<std::string>("rearr"));
    try{
      rearrs.push_back(Util::GVars::str2rearr(rearr_str));
    }
    catch(std::out_of_range &e){
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Invalid Rearranger specified : " + rearr_str);
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, e.what());
      ap.print_usage(std::cerr);
      throw;
    }
  }
  else{
    Util::GVars::copy_opt_map(Util::GVars::rearrs, std::back_inserter(rearrs));
  }

  if(ap.has_arg("e3sm-case")){
    ctype_str = Util::String::toupper(ap.get_arg<std::string>("e3sm-case"));
    try{
      cases.push_back(Util::GVars::str2case(ctype_str));
    }
    catch(std::out_of_range &e){
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Invalid E3SM case specified : " + ctype_str);
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, e.what());
      ap.print_usage(std::cerr);
      throw;
    } 
  }
  else{
    Util::GVars::copy_opt_map(Util::GVars::cases, std::back_inserter(cases));
  }

  if(ap.has_arg("log-level")){
    llevel_str = Util::String::toupper(ap.get_arg<std::string>("log-level"));
    try{
      llevel = Util::GVars::str2llevel(llevel_str);
    }
    catch(std::out_of_range &e){
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Invalid logging level specified : " + llevel_str);
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, e.what());
      ap.print_usage(std::cerr);
      throw;
    }
  }
  else{
    llevel = Util::Logging::LogLevel::STATUS;
  }

  nioprocs = 1;
  if(ap.has_arg("nioprocs")){
    nioprocs = ap.get_arg<int>("nioprocs");
    if(nioprocs <= 0){
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Invalid number of I/O processes specified : " + std::to_string(nioprocs));
      throw std::runtime_error("Command line argument parse error");
    }
  }

  return 0;
}

int main(int argc, char *argv[])
{
  int ret = 0, rank = 0, tmode;

#if PIO_USE_ASYNC_WR_THREAD
  MPI_Init_thread(&argc, &argv, MPI_THREAD_MULTIPLE, &tmode);
#else
  MPI_Init(&argc, &argv);
#endif

  Util::GVars::logger = Util::Logging::Logger::get_logger(MPI_COMM_WORLD, "STDOUT");
  Util::GVars::logger->set_log_level(Util::Logging::LogLevel::STATUS);

  MPI_Comm comm_in = MPI_COMM_WORLD;
  ret = MPI_Comm_rank(comm_in, &rank); assert(ret == MPI_SUCCESS);

  spio_tool_utils::ArgParser ap(comm_in);

  /* Init the standard user options for the tool */
  init_user_options(ap);

  /* Parse the user options */
  std::vector<int> iotypes, rearrs;
  std::vector<E3SM_FGI::Case_Type> cases;
  Util::Logging::LogLevel log_lvl;
  int nioprocs = 1;
  ret = get_user_options(ap, argc, argv, iotypes, rearrs, cases, log_lvl, nioprocs, rank);
  if(ret != 0){
    MPI_Finalize();
    return ret;
  }

  /* Only log from rank 0 */
  Util::GVars::logger->set_log_rank(0);
  Util::GVars::logger->set_log_level(log_lvl);

  MPI_Barrier(comm_in);
#ifdef SPIO_ENABLE_GPTL_TIMING
  /* Initialize the GPTL timing library. */
  if((ret = GPTLinitialize())){
      return ret;
  }

  GPTLstart("e3sm_case:main");
#endif

  for(std::vector<E3SM_FGI::Case_Type>::const_iterator citer = cases.cbegin();
      citer != cases.end(); ++citer){
    switch(*citer){
      case E3SM_FGI::Case_Type::E3SM_F_CASE : ret = E3SM_FGI::test_e3sm_fcase(comm_in, iotypes, rearrs, nioprocs); break;
      case E3SM_FGI::Case_Type::E3SM_G_CASE : ret = E3SM_FGI::test_e3sm_gcase(comm_in, iotypes, rearrs, nioprocs); break;
      case E3SM_FGI::Case_Type::E3SM_I_CASE : ret = E3SM_FGI::test_e3sm_icase(comm_in, iotypes, rearrs, nioprocs); break;
    }
    if(ret != PIO_NOERR){
      Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Test failed, case=" + Util::GVars::case2str(*citer) + ", ret = " + std::to_string(ret));
    }
  }

  MPI_Barrier(comm_in);
#ifdef SPIO_ENABLE_GPTL_TIMING
  GPTLstop("e3sm_case:main");

  /* Write the GPTL summary/rank_0 output */
  std::string summary_file("spioe3smfgirwgptlsummaryinfo0wrank.dat");
  GPTLpr_summary_file(comm_in, summary_file.c_str());

  if(rank == 0){
    std::string rank0_file("spioe3smfgirwgptlinfo0wrank.dat");
    GPTLpr_file(rank0_file.c_str());
  }

  /* Finalize the GPTL timing library. */
  if((ret = GPTLfinalize())){
    return ret;
  }
#endif

  MPI_Finalize();

  return ret;
}
