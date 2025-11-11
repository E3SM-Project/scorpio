#include "e3sm_fgi_utils.hpp"
#include "e3sm_fgi_data.hpp"

static inline std::string get_icase_test_fname(const std::string &case_type, int iotype, int rearr)
{
  const std::string FNAME_PREFIX = "spio_e3sm_fgi_i";
  const std::string FNAME_SUFFIX = ".nc";
  const std::string SEP = "_";

  return FNAME_PREFIX
          + SEP + Util::GVars::iotype2str(iotype)
          + SEP + Util::GVars::rearr2str(rearr)
          + SEP + case_type
          + FNAME_SUFFIX;
}

static int test_icase(MPI_Comm comm, int iosysid, const std::string &fname, int iotype,
                      int nlon, int nlat, int nframes)
{
  int comm_rank = 0, comm_sz = 0;
  int ret = PIO_NOERR;


  ret = MPI_Comm_rank(comm, &comm_rank); assert(ret == MPI_SUCCESS);
  ret = MPI_Comm_size(comm, &comm_sz); assert(ret == MPI_SUCCESS);

  const bool is_last_proc = (comm_rank == comm_sz - 1) ? true : false;
  int nlon_per_proc = std::max(nlon / comm_sz, 1);
  int nlat_per_proc = nlat;

  int idx = 0, ilat = 0, ilon = 0;
  std::function<float(void )> range_zero_to_inf = [idx] (void ) mutable { return static_cast<float>(idx++); }; 

  /* FIXME : Is this the I/O decomp used by the land model ?*/
  std::function<PIO_Offset(void ) > decomp_lon_2d_gen =
    [comm_rank, is_last_proc, nlat, nlat_per_proc, nlon, nlon_per_proc, ilat, ilon](void ) mutable {

      const int ILON_START = comm_rank * nlon_per_proc;

      PIO_Offset val = ilat * nlon + ILON_START + ilon++ + 1;

      if(!is_last_proc){
        if(ilon >= nlon_per_proc) { ilon = 0; ilat++; }
      }
      else{
        if(ilon >= (nlon - (comm_rank * nlon_per_proc))) { ilon = 0; ilat++; }
      }

      return val;
    };

  std::function<float(void) > decomp_lon_2d_val_gen =
    [decomp_lon_2d_gen](void ){
      return static_cast<float>(decomp_lon_2d_gen());
    };
  
  int lsz = (!is_last_proc) ? (nlat_per_proc * nlon_per_proc) :
              (nlat_per_proc * (nlon - comm_rank * nlon_per_proc));
  E3SM_FGI::SPIO_decomp decomp_lon_2d("decomp_lon_2d_lat_lon", iosysid, PIO_FLOAT,
                          std::vector<int>({nlat, nlon}), lsz,
                          decomp_lon_2d_gen);
  std::shared_ptr<const E3SM_FGI::SPIO_decomp> pdecomp_lon_2d =
    std::make_shared<const E3SM_FGI::SPIO_decomp>(std::move(decomp_lon_2d));

  E3SM_FGI::SPIO_file fh(iosysid, fname, iotype, false);

  ret = fh.def({
            /* Global Attributes */
            std::make_shared<E3SM_FGI::SPIO_att>("title", "ELM History file information"),
            std::make_shared<E3SM_FGI::SPIO_att>("source", "E3SM Land Model"),
            std::make_shared<E3SM_FGI::SPIO_att>("ltype_vegetated_or_bare_soil", 1),
            std::make_shared<E3SM_FGI::SPIO_att>("ltype_crop", 1),
            /* Dimensions */
            std::make_shared<E3SM_FGI::SPIO_dim>("time", PIO_UNLIMITED),
            std::make_shared<E3SM_FGI::SPIO_dim>("lat", nlat),
            std::make_shared<E3SM_FGI::SPIO_dim>("lon", nlon),
            /* Variables */
            std::make_shared<E3SM_FGI::SPIO_var<float> >("time", std::vector<std::string>({"time"}), std::vector<E3SM_FGI::SPIO_att>({{"long_name", "time"}, {"units", "days since 0001-01-01 00:00:00"}, {"calendar", "no leap"}, {"bounds", "time_bounds"}}), range_zero_to_inf),
            std::make_shared<E3SM_FGI::SPIO_var<float> >("lat", std::vector<std::string>({"lat"}), std::vector<E3SM_FGI::SPIO_att>({{"long_name", "coordinate latitude"}, {"units", "degrees_north"}, {"_FillValue", (float ) -1.0}, {"missing_value", (float ) -1.0}}), range_zero_to_inf),
            std::make_shared<E3SM_FGI::SPIO_var<float> >("lon", std::vector<std::string>({"lon"}), std::vector<E3SM_FGI::SPIO_att>({{"long_name", "coordinate longitude"}, {"units", "degrees_east"}, {"_FillValue", (float ) -1.0}, {"missing_value", (float ) -1.0}}), range_zero_to_inf),
            std::make_shared<E3SM_FGI::SPIO_unlimited_var<float> >("var_lat_lon", std::vector<std::string>({"time", "lat", "lon"}), std::vector<E3SM_FGI::SPIO_att>({}), pdecomp_lon_2d, nframes, decomp_lon_2d_val_gen)
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

static int test_icase_large(MPI_Comm comm, int iosysid, const std::string &fname, int iotype)
{
  const int NLON = 144;
  const int NLAT = 96;
  const int NFRAMES = 1;

  Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing large I Case\n");
  return test_icase(comm, iosysid, fname, iotype, NLON, NLAT, NFRAMES);
}

static int test_icase_small(MPI_Comm comm, int iosysid, const std::string &fname, int iotype)
{
  const int NLON = 16;
  const int NLAT = 8;
  const int NFRAMES = 1;

  Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing small I Case\n");
  return test_icase(comm, iosysid, fname, iotype, NLON, NLAT, NFRAMES);
}

int E3SM_FGI::test_e3sm_icase(MPI_Comm comm, const std::vector<int> &iotypes,
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

      Util::GVars::logger->log(Util::Logging::LogLevel::STATUS, "Testing I Case, iotype = " + Util::GVars::iotype2str(iotype) + ", rearr = " + Util::GVars::rearr2str(rearr) + "\n");
      int iosysid = SPIO_IOSYSID_INVALID;
      ret = PIOc_Init_Intracomm(comm, nioprocs, io_stride, IOSYS_START_PROC,
              rearr, &iosysid);
      Util::check_spio_err(ret, "PIOc_Init_Intracomm failed", __FILE__, __LINE__);

      ret = test_icase_small(comm, iosysid, get_icase_test_fname("small", iotype, rearr), iotype);
      Util::check_spio_err(ret, "Testing I case (small) failed", __FILE__, __LINE__);

      ret = test_icase_large(comm, iosysid, get_icase_test_fname("large", iotype, rearr), iotype);
      Util::check_spio_err(ret, "Testing I case (large) failed", __FILE__, __LINE__);

      PIOc_finalize(iosysid);
    });

  return PIO_NOERR;
}

