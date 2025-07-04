#include <mpi.h>
#ifdef SPIO_ENABLE_GPTL_TIMING
#include <gptl.h>
#endif
#include <iostream>
#include <regex>
#include "adios2pio-nm-lib.h"
#include "argparser.h"

static void init_user_options(spio_tool_utils::ArgParser &ap)
{
  ap.add_opt("bp-file", "input file (or regex of basename for multiple files) in ADIOS BP format")
    .add_opt("rm-bp-file-after-conv", "remove the ADIOS BP file after conversion (or regex to specify which files to delete after conversion)")
    .add_opt("idir", "Directory containing the input files (in ADIOS BP format)")
    .add_opt("nc-file", "output file name after conversion (will be used as prefix when converting multiple files)")
    .add_opt("pio-format", "output SCORPIO I/O type. Supported parameters: \"pnetcdf\",  \"netcdf\",  \"netcdf4c\",  \"netcdf4p\", \"nczarr\"")
    .add_opt("rearr", "SCORPIO rearranger. Supported parameters: \"subset\", \"box\", \"any\". Default \"any\".")
    .add_opt("verbose", "Turn on verbose info messages");
}

/* Convert BP file name (<BP_FILE_NAME>.dir contains the ADIOS BP data)
 * to the corresponding NetCDF file name by stripping off the BP
 * file type extension at the end of the BP file name.
 *
 * BP file names are of the form "^.*([.]nc)?[.]bp$"
 * The returned string is the BP file name stripped off the
 * ".bp" extension
 */
static std::string strip_bp_ext(const std::string &bp_fname)
{
#ifdef SPIO_NO_CXX_REGEX
  const std::string bp_ext(".bp");
  std::size_t ext_sz = bp_ext.size();
  if(bp_fname.size() > ext_sz){
    if(bp_fname.substr(bp_fname.size() - ext_sz, ext_sz) ==  bp_ext){
      return bp_fname.substr(0, bp_fname.size() - ext_sz);
    }
  }
#else
  const std::string BP_FILE_RGX_STR("(.*)[.]bp");
  std::regex bp_file_rgx(BP_FILE_RGX_STR.c_str());
  std::smatch match;
  if(std::regex_search(bp_fname, match, bp_file_rgx) &&
      match.size() == 2){
    return match.str(1);
  }
#endif
  return std::string();
}

static int get_user_options(
              spio_tool_utils::ArgParser &ap,
              int argc, char *argv[],
              std::string &idir,
              std::string &ifname_rgx, std::string &ofile,
              std::string &rm_ifname_rgx,
              std::string &otype,
              std::string &rearr,
              int &debug_lvl, int rank)
{
  const std::string DEFAULT_PIO_FORMAT("pnetcdf");
  const std::string DEFAULT_REARRANGER("any");
  debug_lvl = 0;

#ifdef SPIO_NO_CXX_REGEX
  ap.no_regex_parse(argc, argv);
#else
  ap.parse(argc, argv);
#endif

  if(!ap.has_arg("bp-file") && !ap.has_arg("idir")){
    ap.print_usage(std::cerr);
    return 1;
  }

  if(ap.has_arg("bp-file")){
    ifname_rgx = ap.get_arg<std::string>("bp-file");
    if(ap.has_arg("nc-file")){
      ofile = ap.get_arg<std::string>("nc-file");
    }
  }

  if(ap.has_arg("idir")){
    idir = ap.get_arg<std::string>("idir");
  }

  if(ap.has_arg("rm-bp-file-after-conv")){
    try{
      /* Remove all input files matching the provided regex */
      rm_ifname_rgx = ap.get_arg<std::string>("rm-bp-file-after-conv");
#ifdef SPIO_NO_CXX_REGEX
      if(rank == 0){
        std::cout << "WARNING: No C++ regex support. Ignoring specified regex for deleting files and deleting all input/BP files after conversion...\n";
      }
      rm_ifname_rgx = ".*";
#endif
    }catch(...){
      /* Remove all input files */
      rm_ifname_rgx = ".*";
    }
  }

  if(ap.has_arg("pio-format")){
    otype = ap.get_arg<std::string>("pio-format");
  }
  else{
    otype = DEFAULT_PIO_FORMAT;
  }

  if(ap.has_arg("rearr")){
    rearr = ap.get_arg<std::string>("rearr");
  }
  else{
    rearr = DEFAULT_REARRANGER;
  }

  if(ap.has_arg("verbose")){
    debug_lvl = 1;
  }

  return 0;
}

int main(int argc, char *argv[])
{
  int ret = 0, rank = 0;

  MPI_Init(&argc, &argv);

  MPI_Comm comm_in = MPI_COMM_WORLD;

  ret = MPI_Comm_rank(comm_in, &rank);
  if(ret != MPI_SUCCESS){
    return ret;
  }

  spio_tool_utils::ArgParser ap(comm_in);

  /* Init the standard user options for the tool */
  init_user_options(ap);

  /* Parse the user options */
  string idir, ifname_rgx, outfilename, piotype, rearr;
  string rm_ifname_rgx;
  int debug_lvl = 0;
  ret = get_user_options(ap, argc, argv,
                          idir, ifname_rgx, outfilename,
                          rm_ifname_rgx, piotype, rearr, debug_lvl, rank);

  if(ret != 0){
    return ret;
  }

#ifdef SPIO_ENABLE_GPTL_TIMING
  /* Initialize the GPTL timing library. */
  if((ret = GPTLinitialize())){
      return ret;
  }
#endif

  GPTLstart("adios2pio:main");

  SetDebugOutput(debug_lvl);
  MPI_Barrier(comm_in);
  if(idir.size() == 0){
    /* FIXME: Hopefully ifname_rgx is actually a file name - not regex */
    ret = ConvertBPToNC(ifname_rgx, outfilename, piotype, rearr, comm_in, rm_ifname_rgx);
  }
  else{
    ret = MConvertBPToNC(idir, ifname_rgx, piotype, rearr, comm_in, rm_ifname_rgx);
  }
  MPI_Barrier(comm_in);

  GPTLstop("adios2pio:main");

#ifdef SPIO_ENABLE_GPTL_TIMING
  /* Write the GPTL summary/rank_0 output */
  std::string summary_file("spioconvtoolrwgptlsummaryinfo0wrank.dat");
  GPTLpr_summary_file(comm_in, summary_file.c_str());

  if(rank == 0){
    std::string rank0_file("spioconvtoolrwgptlinfo0wrank.dat");
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
