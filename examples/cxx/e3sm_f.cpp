#include "e3sm_fgi_utils.hpp"
#include "e3sm_fgi_data.hpp"

static inline std::string get_fcase_test_fname(int iotype, int rearr)
{
  const std::string FNAME_PREFIX = "spio_e3sm_fgi_f";
  const std::string FNAME_SUFFIX = ".nc";
  const std::string SEP = "_";

  return FNAME_PREFIX
          + SEP + Util::GVars::iotype2str(iotype)
          + SEP + Util::GVars::rearr2str(rearr)
          + FNAME_SUFFIX;
}

static int test_fcase(MPI_Comm comm, int iosysid, const std::string &fname, int iotype)
{
  int comm_rank = 0, comm_sz = 0;
  int ret = PIO_NOERR;


  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  const int NE = 120;
  const PIO_Offset NCOL_PER_PROC = 4;
  const PIO_Offset NCOL = comm_sz * NCOL_PER_PROC;
  const int NBND = 2;
  const int MAX_CHARS = 8;
  const int NLEV = 3;
  const int NFRAMES = 2;
  const bool is_last_proc = (comm_rank != comm_sz - 1) ? false : true;

  int idx = 0, ilev = 0;
  std::function<double(void )> range_zero_to_inf = [idx] (void ) mutable { return idx++; }; 

  double dlev = 0;
  std::function<double(void )> gen_levels = [dlev] (void ) mutable { double d = dlev; dlev += 0.2; return d; };

  int start_date = 15;
  std::function<std::string(void )> gen_dates = [start_date] (void ) mutable {
    static const std::string month("06");
    static const std::string year("21");
    static const std::string SEP("/");

    if(start_date >= 30) { start_date = 1; }

    return month + SEP + std::to_string(start_date++) + SEP + year;
  };

  std::function<PIO_Offset(void )> decomp_1d_gen =
                          [comm_rank, NCOL, NCOL_PER_PROC](void ){
                            static const PIO_Offset INIT_IDX = 0;
                            static PIO_Offset idx = INIT_IDX;
                            PIO_Offset val = comm_rank * NCOL_PER_PROC + idx++ + 1;

                            if(idx >= NCOL) { idx = INIT_IDX; }
                            
                            return val;
                          };
  
  E3SM_FGI::SPIO_decomp decomp_1d("decomp_1d_ncol", iosysid, PIO_DOUBLE,
                          std::vector<int>({static_cast<int>(NCOL)}), NCOL_PER_PROC, decomp_1d_gen);

  std::function<PIO_Offset(void ) > decomp_2d_gen =
    [comm_rank, is_last_proc, NCOL, NCOL_PER_PROC, NLEV, idx, ilev](void ) mutable {

      PIO_Offset val = ilev * NCOL + comm_rank * NCOL_PER_PROC + idx++ + 1;

      if(!is_last_proc){
        if(idx >= NCOL_PER_PROC) { idx = 0; ilev++; }
      }
      else{
        if(idx >= (NCOL - comm_rank * NCOL_PER_PROC)) { idx = 0; ilev++; }
      }

      return val;
    };

  std::function<double(void) > decomp_2d_val_gen =
    [decomp_2d_gen](void ){
      return static_cast<double>(decomp_2d_gen());
    };
  
  int lsz = NLEV * ((!is_last_proc) ? NCOL_PER_PROC : (NCOL - comm_rank * NCOL_PER_PROC));
  E3SM_FGI::SPIO_decomp decomp_2d("decomp_2d_lev_ncol", iosysid, PIO_DOUBLE,
                          std::vector<int>({NLEV, static_cast<int>(NCOL)}),
                          lsz, decomp_2d_gen);
  std::shared_ptr<const E3SM_FGI::SPIO_decomp> pdecomp_2d =
    std::make_shared<const E3SM_FGI::SPIO_decomp>(std::move(decomp_2d));

  E3SM_FGI::SPIO_file fh(iosysid, fname, iotype, false);

  ret = fh.def({
            /* Global Attributes */
            std::make_shared<E3SM_FGI::SPIO_att>("ne", NE),
            std::make_shared<E3SM_FGI::SPIO_att>("title", "EAM History file information"),
            std::make_shared<E3SM_FGI::SPIO_att>("source", "E3SM Atmosphere Model"),
            std::make_shared<E3SM_FGI::SPIO_att>("empty_string", ""),
            /* Dimensions */
            std::make_shared<E3SM_FGI::SPIO_dim>("time", PIO_UNLIMITED),
            std::make_shared<E3SM_FGI::SPIO_dim>("nbnd", NBND),
            std::make_shared<E3SM_FGI::SPIO_dim>("chars", MAX_CHARS),
            std::make_shared<E3SM_FGI::SPIO_dim>("lev", NLEV),
            std::make_shared<E3SM_FGI::SPIO_dim>("ncol", NCOL),
            /* Variables */
            std::make_shared<E3SM_FGI::SPIO_var<double> >("lat", std::vector<std::string>({"ncol"}), std::vector<E3SM_FGI::SPIO_att>({{"long_name", "latitude"}, {"units", "degrees_north"}}), range_zero_to_inf),
            std::make_shared<E3SM_FGI::SPIO_var<double> >("lev", std::vector<std::string>({"lev"}), std::vector<E3SM_FGI::SPIO_att>({{"long_name", "reference pressure"}, {"units", "Pa"}}), gen_levels),
            std::make_shared<E3SM_FGI::SPIO_var<double> >("P0", std::vector<std::string>({}), std::vector<E3SM_FGI::SPIO_att>({}), std::vector<double>{99999.9999}),
            std::make_shared<E3SM_FGI::SPIO_var<double> >("time", std::vector<std::string>({"time"}), std::vector<E3SM_FGI::SPIO_att>({}), range_zero_to_inf),
            std::make_shared<E3SM_FGI::SPIO_cs_var<double> >("time_bnds", std::vector<std::string>({"time", "nbnd"}), std::vector<E3SM_FGI::SPIO_att>({}), std::vector<std::vector<PIO_Offset> > ({std::vector<PIO_Offset>({0, 0}), std::vector<PIO_Offset>({1, 0})}), std::vector<std::vector<PIO_Offset> > ({std::vector<PIO_Offset>({1, 2}), std::vector<PIO_Offset>({1, 2})}), range_zero_to_inf),
            std::make_shared<E3SM_FGI::SPIO_cs_var<std::string> >("date_written", std::vector<std::string>({"time", "chars"}), std::vector<E3SM_FGI::SPIO_att>({}), std::vector<std::vector<PIO_Offset> > ({std::vector<PIO_Offset>({0, 0}), std::vector<PIO_Offset>({1, 0})}), std::vector<std::vector<PIO_Offset> > ({std::vector<PIO_Offset>({1, MAX_CHARS}), std::vector<PIO_Offset>({1, MAX_CHARS})}), gen_dates),
            std::make_shared<E3SM_FGI::SPIO_unlimited_var<double> >("U", std::vector<std::string>({"time", "lev", "ncol"}), std::vector<E3SM_FGI::SPIO_att>({}), pdecomp_2d, NFRAMES, decomp_2d_val_gen)
          });
  if(ret != PIO_NOERR){
    Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Defining file objects failed");
    return ret;
  }

  ret = fh.put();
  if(ret != PIO_NOERR){
    Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Putting file objects failed");
    return ret;
  }

  return ret;
}

int E3SM_FGI::test_e3sm_fcase(MPI_Comm comm, const std::vector<int> &iotypes,
      const std::vector<int> &rearrs, int nioprocs)
{
  int comm_rank, comm_sz, ret;
  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  assert(nioprocs <= comm_sz);

  int io_stride = comm_sz / nioprocs;

  Util::zip_for_each(iotypes.cbegin(), iotypes.cend(), rearrs.cbegin(), rearrs.cend(),
    [comm, nioprocs, io_stride](int iotype, int rearr){
      const int IOSYS_START_PROC = 0;
      const int SPIO_IOSYSID_INVALID = -2;
      int ret = PIO_NOERR;

      Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing F Case, iotype = " + Util::GVars::iotype2str(iotype) + ", rearr = " + Util::GVars::rearr2str(rearr) + "\n");
      int iosysid = SPIO_IOSYSID_INVALID;
      ret = PIOc_Init_Intracomm(comm, nioprocs, io_stride, IOSYS_START_PROC,
              rearr, &iosysid);
      Util::check_spio_err(ret, "PIOc_Init_Intracomm failed", __FILE__, __LINE__);

      ret = test_fcase(comm, iosysid, get_fcase_test_fname(iotype, rearr), iotype);
      Util::check_spio_err(ret, "Testing F case failed", __FILE__, __LINE__);

      PIOc_finalize(iosysid);
    });

  return PIO_NOERR;
}

