#include "e3sm_fgi_utils.hpp"
#include "e3sm_fgi_data.hpp"

/* Get unique output file name */
static inline std::string get_gcase_test_fname(const std::string &case_type, int iotype, int rearr)
{
  const std::string FNAME_PREFIX = "spio_e3sm_fgi_gcase";
  const std::string FNAME_SUFFIX = ".nc";
  const std::string SEP = "_";

  return FNAME_PREFIX
          + SEP + Util::GVars::iotype2str(iotype)
          + SEP + Util::GVars::rearr2str(rearr)
          + SEP + case_type
          + FNAME_SUFFIX;
}

/* Test pseudo G case */
static int test_gcase(MPI_Comm comm, int iosysid, const std::string &fname, int iotype,
  PIO_Offset ncells, PIO_Offset nedges, PIO_Offset nvertices, PIO_Offset nvertlevels,
  int nframes)
{
  int comm_rank = 0, comm_sz = 0;
  int ret = PIO_NOERR;


  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  const PIO_Offset NEDGES_PER_PROC = std::max(static_cast<unsigned long long>(nedges / comm_sz), static_cast<unsigned long long>(1));
  const int STRLEN = 64;
  const bool is_last_proc = (comm_rank == comm_sz - 1) ? true : false;

  PIO_Offset idx = 0;
  std::function<double(void )> range_zero_to_inf = [idx] (void ) mutable { return static_cast<double>(idx++); }; 

  double dlev = 0;
  std::function<double(void )> gen_levels = [dlev] (void ) mutable { double d = dlev; dlev += 0.2; return d; };

  /* 1D decomp : the edges are evenly partitioned across all MPI processes */
  /* FIXME: This is not how MPAS paritions cells */
  std::function<PIO_Offset(void )> decomp_1d_gen_nedges =
                          [comm_rank, comm_sz, nedges, NEDGES_PER_PROC, idx](void ) mutable {
                            static const PIO_Offset INIT_IDX = 0;
                            const bool is_last_rank = (comm_rank == comm_sz - 1) ? true : false;

                            PIO_Offset val = comm_rank * NEDGES_PER_PROC + idx++ + 1;

                            if(!is_last_rank){
                              if(idx >= NEDGES_PER_PROC) { idx = INIT_IDX; }
                            }
                            else{
                              if(val >= nedges) { idx = INIT_IDX; }
                            }
                            
                            return val;
                          };
  
  E3SM_FGI::SPIO_decomp decomp_1d("decomp_1d_nedges", iosysid, PIO_DOUBLE,
                          std::vector<int>({static_cast<int>(nedges)}), static_cast<int>(NEDGES_PER_PROC), decomp_1d_gen_nedges);

  /* 2D decomp : the edges are evenly partitioned across all MPI processes */
  /* FIXME: This is not how MPAS paritions cells */
  std::function<PIO_Offset(void ) > decomp_2d_gen_nedges =
    [comm_rank, is_last_proc, nedges, NEDGES_PER_PROC, nvertlevels, idx](void ) mutable {
      static const PIO_Offset INIT_IDX = 0;

      PIO_Offset val = comm_rank * (NEDGES_PER_PROC * nvertlevels) + idx++ + 1;

      if(!is_last_proc){
        if(idx >= (comm_rank + 1) * NEDGES_PER_PROC * nvertlevels) { idx = INIT_IDX; }
      }
      else{
        if(idx >= (nedges - comm_rank * NEDGES_PER_PROC) * nvertlevels) { idx = INIT_IDX; }
      }
      
      return val;
    };

  std::function<double(void) > decomp_2d_edges_val_gen =
    [decomp_2d_gen_nedges](void ){
      return static_cast<double>(decomp_2d_gen_nedges());
    };
  
  int lsz = static_cast<int>(nvertlevels *
              ((!is_last_proc) ? NEDGES_PER_PROC : (nedges - comm_rank * NEDGES_PER_PROC)));
  E3SM_FGI::SPIO_decomp decomp_2d_edges("decomp_2d_nedges_nvertlevels", iosysid, PIO_DOUBLE,
                          std::vector<int>({static_cast<int>(nedges), static_cast<int>(nvertlevels)}),
                          lsz, decomp_2d_gen_nedges);
  std::shared_ptr<const E3SM_FGI::SPIO_decomp> pdecomp_2d_edges =
    std::make_shared<const E3SM_FGI::SPIO_decomp>(std::move(decomp_2d_edges));

  E3SM_FGI::SPIO_file fh(iosysid, fname, iotype, false);

  /* Define var/dim/atts */
  ret = fh.def({
            /* Global Attributes */
            std::make_shared<E3SM_FGI::SPIO_att>("title", "MPAS History file information"),
            std::make_shared<E3SM_FGI::SPIO_att>("source", "E3SM Atmosphere Model"),
            /* Dimensions */
            std::make_shared<E3SM_FGI::SPIO_dim>("Time", PIO_UNLIMITED),
            std::make_shared<E3SM_FGI::SPIO_dim>("nCells", ncells),
            std::make_shared<E3SM_FGI::SPIO_dim>("nEdges", nedges),
            std::make_shared<E3SM_FGI::SPIO_dim>("nVertices", nvertices),
            std::make_shared<E3SM_FGI::SPIO_dim>("nVertLevels", nvertlevels),
            std::make_shared<E3SM_FGI::SPIO_dim>("nVertLevelsP1", nvertlevels),
            std::make_shared<E3SM_FGI::SPIO_dim>("StrLen", STRLEN),
            /* Variables */
            std::make_shared<E3SM_FGI::SPIO_unlimited_var<double> >("normalVerlocity", std::vector<std::string>({"Time", "nEdges", "nVertLevels"}), std::vector<E3SM_FGI::SPIO_att>({}), pdecomp_2d_edges, nframes, decomp_2d_edges_val_gen)
          });
  if(ret != PIO_NOERR){
    Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Defining file objects failed");
    return ret;
  }

  /* Write var/atts */
  ret = fh.put();
  if(ret != PIO_NOERR){
    Util::GVars::logger->log(Util::Logging::LogLevel::ERROR, "Putting file objects failed");
    return ret;
  }

  return ret;
}

/* Test small G case */
static int test_gcase_small(MPI_Comm comm, int iosysid, const std::string &fname, int iotype)
{
  int comm_rank = 0, comm_sz = 0;
  int ret = PIO_NOERR;

  Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing small G Case , iotype = " + Util::GVars::iotype2str(iotype) + "\n");

  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  const PIO_Offset NCELLS = 32;
  const PIO_Offset NEDGES = 32;
  const PIO_Offset NVERTICES = 10;
  const PIO_Offset NVERTLEVELS = 11;
  const int NFRAMES = 2;

  return test_gcase(comm, iosysid, fname, iotype, NCELLS, NEDGES, NVERTICES, NVERTLEVELS, NFRAMES);

}

/* Test large G case : this can be too big to run on a single compute node */
static int test_gcase_large(MPI_Comm comm, int iosysid, const std::string &fname, int iotype)
{
  int comm_rank = 0, comm_sz = 0;
  int ret = PIO_NOERR;

  Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing large G Case , iotype = " + Util::GVars::iotype2str(iotype) + "\n");

  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  const PIO_Offset NCELLS = 3693225;
  const PIO_Offset NEDGES = 11135652;
  const PIO_Offset NVERTICES = 7441216;
  const PIO_Offset NVERTLEVELS = 81;
  const int NFRAMES = 2;

  return test_gcase(comm, iosysid, fname, iotype, NCELLS, NEDGES, NVERTICES, NVERTLEVELS, NFRAMES);

}

int E3SM_FGI::test_e3sm_gcase(MPI_Comm comm, const std::vector<int> &iotypes,
      const std::vector<int> &rearrs, int nioprocs)
{
  int comm_rank, comm_sz, ret;
  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  assert(nioprocs <= comm_sz);

  int io_stride = comm_sz / nioprocs;

  /* Run G case for all combinations of I/O types and I/O rearrangers */
  Util::zip_for_each(iotypes.cbegin(), iotypes.cend(), rearrs.cbegin(), rearrs.cend(),
    [comm, nioprocs, io_stride](int iotype, int rearr){
      const int IOSYS_START_PROC = 0;
      const int SPIO_IOSYSID_INVALID = -2;
      int ret = PIO_NOERR;

      Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing G Case, iotype = " + Util::GVars::iotype2str(iotype) + ", rearr = " + Util::GVars::rearr2str(rearr) + "\n");
      int iosysid = SPIO_IOSYSID_INVALID;
      ret = PIOc_Init_Intracomm(comm, nioprocs, io_stride, IOSYS_START_PROC,
              rearr, &iosysid);
      Util::check_spio_err(ret, "PIOc_Init_Intracomm failed", __FILE__, __LINE__);

      ret = test_gcase_small(comm, iosysid, get_gcase_test_fname("small", iotype, rearr), iotype);
      Util::check_spio_err(ret, "Testing G case failed", __FILE__, __LINE__);

      /* FIXME: Uncomment later, and optionally enable it based on user input */
      //ret = test_gcase_large(comm, iosysid, get_gcase_test_fname("large", iotype, rearr), iotype);
      //Util::check_spio_err(ret, "Testing G case failed", __FILE__, __LINE__);

      PIOc_finalize(iosysid);
    });

  return PIO_NOERR;
}

