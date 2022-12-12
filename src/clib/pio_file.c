/**
 * @file
 * PIO File Handling
 */
#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#include "spio_io_summary.h"

#ifdef _ADIOS2
#include "../../tools/adios2pio-nm/adios2pio-nm-lib-c.h"
#endif

/**
 * Open an existing file using PIO library.
 *
 * If the open fails, try again as netCDF serial before giving
 * up. Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * Note that the file is opened with default fill mode, NOFILL for
 * pnetcdf, and FILL for netCDF classic and netCDF-4 files.
 *
 * @param iosysid : A defined pio system descriptor (input)
 * @param ncidp : A pio file descriptor (output)
 * @param iotype : A pio output format (input)
 * @param filename : The filename to open
 * @param mode : The netcdf mode for the open operation
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_openfile(int iosysid, int *ncidp, int *iotype, const char *filename,
                  int mode)
{
    /* DEBUG on crusher-gpu start */
    static int counter = 0;
    counter++;
    iosystem_desc_t *ios = pio_get_iosystem_from_id(iosysid);
    if (ios->union_rank == 0)
    {
        printf("[DEBUG] PIOc_openfile, counter = %d\n", counter);
        fflush(stdout);
    }
    if (counter == 1)
    {
      int format = PIO_IOTYPE_PNETCDF;
      int rearranger = PIO_REARR_SUBSET;
      int ncid;

      int ioid_525;
      int ndims_525 = 0;
      int *gdimlen_525 = NULL;
      PIO_Offset fmaplen_525 = 0;
      PIO_Offset *compmap_525 = NULL;
      double buffer_525[27648];

      int ioid_526;
      int ndims_526 = 0;
      int *gdimlen_526 = NULL;
      PIO_Offset fmaplen_526 = 0;
      PIO_Offset *compmap_526 = NULL;
      double buffer_526[13824];

      int ioid_527;
      int ndims_527 = 0;
      int *gdimlen_527 = NULL;
      PIO_Offset fmaplen_527 = 0;
      PIO_Offset *compmap_527 = NULL;
      double buffer_527[14016];

      int ioid_528;
      int ndims_528 = 0;
      int *gdimlen_528 = NULL;
      PIO_Offset fmaplen_528 = 0;
      PIO_Offset *compmap_528 = NULL;
      double buffer_528[192];

      int ioid_529;
      int ndims_529 = 0;
      int *gdimlen_529 = NULL;
      PIO_Offset fmaplen_529 = 0;
      PIO_Offset *compmap_529 = NULL;
      double buffer_529[138240];

      int ioid_530;
      int ndims_530 = 0;
      int *gdimlen_530 = NULL;
      PIO_Offset fmaplen_530 = 0;
      PIO_Offset *compmap_530 = NULL;
      double buffer_530[9576];

      int ioid_531;
      int ndims_531 = 0;
      int *gdimlen_531 = NULL;
      PIO_Offset fmaplen_531 = 0;
      PIO_Offset *compmap_531 = NULL;
      double buffer_531[19152];

      int ioid_532;
      int ndims_532 = 0;
      int *gdimlen_532 = NULL;
      PIO_Offset fmaplen_532 = 0;
      PIO_Offset *compmap_532 = NULL;
      double buffer_532[133];

      int ioid_533;
      int ndims_533 = 0;
      int *gdimlen_533 = NULL;
      PIO_Offset fmaplen_533 = 0;
      PIO_Offset *compmap_533 = NULL;
      double buffer_533[95760];

      int dimids[6];

      int dimid_time;
      int dimid_dim10;
      int dimid_dim2;
      int dimid_elem;
      int dimid_gp;
      int dimid_ilev;
      int dimid_lev;
      int dimid_ncol;

      int varid_time;
      int varid_v_dyn;
      int varid_vtheta_dp_dyn;
      int varid_dp3d_dyn;
      int varid_phi_int_dyn;
      int varid_ps_dyn;
      int varid_Qdp_dyn;
      int varid_w_int_dyn;
      int varid_T_mid;
      int varid_T_prev_micro_step;
      int varid_cldfrac_liq;
      int varid_eddy_diff_mom;
      int varid_horiz_winds;
      int varid_nc_nuceat_tend;
      int varid_ni_activated;
      int varid_o3_volume_mix_ratio;
      int varid_phis;
      int varid_precip_ice_surf_mass;
      int varid_precip_liq_surf_mass;
      int varid_ps;
      int varid_qv;
      int varid_qv_prev_micro_step;
      int varid_sgs_buoy_flux;
      int varid_tracers;

      int *gdimlen = NULL;
      PIO_Offset fmaplen = 0;
      PIO_Offset *compmap = NULL;

      PIOc_readmap("piodecomp_525.dat", &ndims_525, &gdimlen_525, &fmaplen_525, &compmap_525, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 5, gdimlen_525, fmaplen_525, compmap_525, &ioid_525, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_526.dat", &ndims_526, &gdimlen_526, &fmaplen_526, &compmap_526, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 4, gdimlen_526, fmaplen_526, compmap_526, &ioid_526, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_527.dat", &ndims_527, &gdimlen_527, &fmaplen_527, &compmap_527, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 4, gdimlen_527, fmaplen_527, compmap_527, &ioid_527, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_528.dat", &ndims_528, &gdimlen_528, &fmaplen_528, &compmap_528, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 3, gdimlen_528, fmaplen_528, compmap_528, &ioid_528, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_529.dat", &ndims_529, &gdimlen_529, &fmaplen_529, &compmap_529, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 5, gdimlen_529, fmaplen_529, compmap_529, &ioid_529, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_530.dat", &ndims_530, &gdimlen_530, &fmaplen_530, &compmap_530, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 2, gdimlen_530, fmaplen_530, compmap_530, &ioid_530, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_531.dat", &ndims_531, &gdimlen_531, &fmaplen_531, &compmap_531, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 3, gdimlen_531, fmaplen_531, compmap_531, &ioid_531, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_532.dat", &ndims_532, &gdimlen_532, &fmaplen_532, &compmap_532, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 1, gdimlen_532, fmaplen_532, compmap_532, &ioid_532, &rearranger, NULL, NULL);

      PIOc_readmap("piodecomp_533.dat", &ndims_533, &gdimlen_533, &fmaplen_533, &compmap_533, MPI_COMM_WORLD);
      PIOc_InitDecomp(iosysid, PIO_DOUBLE, 3, gdimlen_533, fmaplen_533, compmap_533, &ioid_533, &rearranger, NULL, NULL);

      PIOc_createfile(iosysid, &ncid, &format, "PIOc_openfile_scream.r.nc", PIO_CLOBBER);

      PIOc_def_dim(ncid, "time", PIO_UNLIMITED, &dimid_time);
      PIOc_def_dim(ncid, "dim10", 10, &dimid_dim10);
      PIOc_def_dim(ncid, "dim2", 2, &dimid_dim2);
      PIOc_def_dim(ncid, "elem", 96, &dimid_elem);
      PIOc_def_dim(ncid, "gp", 4, &dimid_gp);
      PIOc_def_dim(ncid, "ilev", 73, &dimid_ilev);
      PIOc_def_dim(ncid, "lev", 72, &dimid_lev);
      PIOc_def_dim(ncid, "ncol", 866, &dimid_ncol);

      PIOc_def_var(ncid, "time", PIO_DOUBLE, 1, &dimid_time, &varid_time);

      dimids[0] = dimid_time;
      dimids[1] = dimid_elem;

      dimids[2] = dimid_dim2;
      dimids[3] = dimid_gp;
      dimids[4] = dimid_gp;
      dimids[5] = dimid_lev;
      PIOc_def_var(ncid, "v_dyn", PIO_DOUBLE, 6, dimids, &varid_v_dyn);

      dimids[2] = dimid_gp;
      dimids[3] = dimid_gp;

      dimids[4] = dimid_lev;
      PIOc_def_var(ncid, "vtheta_dp_dyn", PIO_DOUBLE, 5, dimids, &varid_vtheta_dp_dyn);
      PIOc_def_var(ncid, "dp3d_dyn", PIO_DOUBLE, 5, dimids, &varid_dp3d_dyn);

      dimids[4] = dimid_ilev;
      PIOc_def_var(ncid, "phi_int_dyn", PIO_DOUBLE, 5, dimids, &varid_phi_int_dyn);

      PIOc_def_var(ncid, "ps_dyn", PIO_DOUBLE, 4, dimids, &varid_ps_dyn);

      dimids[2] = dimid_dim10;
      dimids[3] = dimid_gp;
      dimids[4] = dimid_gp;
      dimids[5] = dimid_lev;
      PIOc_def_var(ncid, "Qdp_dyn", PIO_DOUBLE, 6, dimids, &varid_Qdp_dyn);

      dimids[2] = dimid_gp;
      dimids[3] = dimid_gp;
      dimids[4] = dimid_ilev;
      PIOc_def_var(ncid, "w_int_dyn", PIO_DOUBLE, 5, dimids, &varid_w_int_dyn);

      dimids[1] = dimid_ncol;
      dimids[2] = dimid_lev;
      PIOc_def_var(ncid, "T_mid", PIO_DOUBLE, 3, dimids, &varid_T_mid);
      PIOc_def_var(ncid, "T_prev_micro_step", PIO_DOUBLE, 3, dimids, &varid_T_prev_micro_step);
      PIOc_def_var(ncid, "cldfrac_liq", PIO_DOUBLE, 3, dimids, &varid_cldfrac_liq);
      PIOc_def_var(ncid, "eddy_diff_mom", PIO_DOUBLE, 3, dimids, &varid_eddy_diff_mom);

      dimids[2] = dimid_dim2;
      dimids[3] = dimid_lev;
      PIOc_def_var(ncid, "horiz_winds", PIO_DOUBLE, 4, dimids, &varid_horiz_winds);

      dimids[2] = dimid_lev;
      PIOc_def_var(ncid, "nc_nuceat_tend", PIO_DOUBLE, 3, dimids, &varid_nc_nuceat_tend);
      PIOc_def_var(ncid, "ni_activated", PIO_DOUBLE, 3, dimids, &varid_ni_activated);
      PIOc_def_var(ncid, "o3_volume_mix_ratio", PIO_DOUBLE, 3, dimids, &varid_o3_volume_mix_ratio);

      PIOc_def_var(ncid, "phis", PIO_DOUBLE, 2, dimids, &varid_phis);
      PIOc_def_var(ncid, "precip_ice_surf_mass", PIO_DOUBLE, 2, dimids, &varid_precip_ice_surf_mass);
      PIOc_def_var(ncid, "precip_liq_surf_mass", PIO_DOUBLE, 2, dimids, &varid_precip_liq_surf_mass);
      PIOc_def_var(ncid, "ps", PIO_DOUBLE, 2, dimids, &varid_ps);

      PIOc_def_var(ncid, "qv", PIO_DOUBLE, 3, dimids, &varid_qv);
      PIOc_def_var(ncid, "qv_prev_micro_step", PIO_DOUBLE, 3, dimids, &varid_qv_prev_micro_step);
      PIOc_def_var(ncid, "sgs_buoy_flux", PIO_DOUBLE, 3, dimids, &varid_sgs_buoy_flux);

      dimids[2] = dimid_dim10;
      dimids[3] = dimid_lev;
      PIOc_def_var(ncid, "tracers", PIO_DOUBLE, 4, dimids, &varid_tracers);

      PIOc_enddef(ncid);

      PIOc_setframe(ncid, varid_v_dyn, 0);
      PIOc_write_darray(ncid, varid_v_dyn, ioid_525, fmaplen_525, buffer_525, NULL);

      PIOc_setframe(ncid, varid_vtheta_dp_dyn, 0);
      PIOc_write_darray(ncid, varid_vtheta_dp_dyn, ioid_526, fmaplen_526, buffer_526, NULL);

      PIOc_setframe(ncid, varid_dp3d_dyn, 0);
      PIOc_write_darray(ncid, varid_dp3d_dyn, ioid_526, fmaplen_526, buffer_526, NULL);

      PIOc_setframe(ncid, varid_phi_int_dyn, 0);
      PIOc_write_darray(ncid, varid_phi_int_dyn, ioid_527, fmaplen_527, buffer_527, NULL);

      PIOc_setframe(ncid, varid_ps_dyn, 0);
      PIOc_write_darray(ncid, varid_ps_dyn, ioid_528, fmaplen_528, buffer_528, NULL);

      PIOc_setframe(ncid, varid_Qdp_dyn, 0);
      PIOc_write_darray(ncid, varid_Qdp_dyn, ioid_529, fmaplen_529, buffer_529, NULL);

      PIOc_setframe(ncid, varid_w_int_dyn, 0);
      PIOc_write_darray(ncid, varid_w_int_dyn, ioid_527, fmaplen_527, buffer_527, NULL);

      PIOc_setframe(ncid, varid_T_mid, 0);
      PIOc_write_darray(ncid, varid_T_mid, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_T_prev_micro_step, 0);
      PIOc_write_darray(ncid, varid_T_prev_micro_step, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_cldfrac_liq, 0);
      PIOc_write_darray(ncid, varid_cldfrac_liq, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_eddy_diff_mom, 0);
      PIOc_write_darray(ncid, varid_eddy_diff_mom, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_horiz_winds, 0);
      PIOc_write_darray(ncid, varid_horiz_winds, ioid_531, fmaplen_531, buffer_531, NULL);

      PIOc_setframe(ncid, varid_nc_nuceat_tend, 0);
      PIOc_write_darray(ncid, varid_nc_nuceat_tend, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_ni_activated, 0);
      PIOc_write_darray(ncid, varid_ni_activated, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_o3_volume_mix_ratio, 0);
      PIOc_write_darray(ncid, varid_o3_volume_mix_ratio, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_phis, 0);
      PIOc_write_darray(ncid, varid_phis, ioid_532, fmaplen_532, buffer_532, NULL);

      PIOc_setframe(ncid, varid_precip_ice_surf_mass, 0);
      PIOc_write_darray(ncid, varid_precip_ice_surf_mass, ioid_532, fmaplen_532, buffer_532, NULL);

      PIOc_setframe(ncid, varid_precip_liq_surf_mass, 0);
      PIOc_write_darray(ncid, varid_precip_liq_surf_mass, ioid_532, fmaplen_532, buffer_532, NULL);

      PIOc_setframe(ncid, varid_ps, 0);
      PIOc_write_darray(ncid, varid_ps, ioid_532, fmaplen_532, buffer_532, NULL);

      PIOc_setframe(ncid, varid_qv, 0);
      PIOc_write_darray(ncid, varid_qv, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_qv_prev_micro_step, 0);
      PIOc_write_darray(ncid, varid_qv_prev_micro_step, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_sgs_buoy_flux, 0);
      PIOc_write_darray(ncid, varid_sgs_buoy_flux, ioid_530, fmaplen_530, buffer_530, NULL);

      PIOc_setframe(ncid, varid_tracers, 0);
      PIOc_write_darray(ncid, varid_tracers, ioid_533, fmaplen_533, buffer_533, NULL);

      PIOc_closefile(ncid);

      free(compmap_525);
      free(gdimlen_525);
      PIOc_freedecomp(iosysid, ioid_525);

      free(compmap_526);
      free(gdimlen_526);
      PIOc_freedecomp(iosysid, ioid_526);

      free(compmap_527);
      free(gdimlen_527);
      PIOc_freedecomp(iosysid, ioid_527);

      free(compmap_528);
      free(gdimlen_528);
      PIOc_freedecomp(iosysid, ioid_528);

      free(compmap_529);
      free(gdimlen_529);
      PIOc_freedecomp(iosysid, ioid_529);

      free(compmap_530);
      free(gdimlen_530);
      PIOc_freedecomp(iosysid, ioid_530);

      free(compmap_531);
      free(gdimlen_531);
      PIOc_freedecomp(iosysid, ioid_531);

      free(compmap_532);
      free(gdimlen_532);
      PIOc_freedecomp(iosysid, ioid_532);

      free(compmap_533);
      free(gdimlen_533);
      PIOc_freedecomp(iosysid, ioid_533);
    }
    /* DEBUG on crusher-gpu end */

    return openfile_int(iosysid, ncidp, iotype, filename, mode, 1);
}

/**
 * Open an existing file using PIO library.
 *
 * This is like PIOc_openfile(), but if the open fails, this function
 * will not try to open again as netCDF serial before giving
 * up. Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * Note that the file is opened with default fill mode, NOFILL for
 * pnetcdf, and FILL for netCDF classic and netCDF-4 files.
 *
 * @param iosysid : A defined pio system descriptor (input)
 * @param ncidp : A pio file descriptor (output)
 * @param iotype : A pio output format (input)
 * @param filename : The filename to open
 * @param mode : The netcdf mode for the open operation
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Ed Hartnett
 */
int PIOc_openfile2(int iosysid, int *ncidp, int *iotype, const char *filename,
                   int mode)
{
    return openfile_int(iosysid, ncidp, iotype, filename, mode, 0);
}

/**
 * Open an existing file using PIO library.
 *
 * Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid A defined pio system descriptor
 * @param path The filename to open
 * @param mode The netcdf mode for the open operation
 * @param ncidp pointer to int where ncid will go
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Ed Hartnett
 */
int PIOc_open(int iosysid, const char *path, int mode, int *ncidp)
{
    int iotype;

    LOG((1, "PIOc_open iosysid = %d path = %s mode = %x", iosysid, path, mode));

    /* Set the default iotype. */
#ifdef _NETCDF
    iotype = PIO_IOTYPE_NETCDF;
#else /* Assume that _PNETCDF is defined. */
    iotype = PIO_IOTYPE_PNETCDF;
#endif

    /* Figure out the iotype. */
    if (mode & NC_NETCDF4)
    {
#ifdef _NETCDF4
        if (mode & NC_MPIIO || mode & NC_MPIPOSIX)
            iotype = PIO_IOTYPE_NETCDF4P;
        else
            iotype = PIO_IOTYPE_NETCDF4C;
#endif
    }
    else
    {
#ifdef _PNETCDF
        if (mode & NC_PNETCDF || mode & NC_MPIIO)
            iotype = PIO_IOTYPE_PNETCDF;
#endif
    }

    /* Open the file. If the open fails, do not retry as serial
     * netCDF. Just return the error code. */
    return PIOc_openfile_retry(iosysid, ncidp, &iotype, path, mode, 0);
}

/**
 * Create a new file using pio. Input parameters are read on comp task
 * 0 and ignored elsewhere. NOFILL mode will be turned on in all
 * cases.
 *
 * @param iosysid A defined pio system ID, obtained from
 * PIOc_InitIntercomm() or PIOc_InitAsync().
 * @param ncidp A pointer that gets the ncid of the newly created
 * file.
 * @param iotype A pointer to a pio output format. Must be one of
 * PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C, or
 * PIO_IOTYPE_NETCDF4P.
 * @param filename The filename to create.
 * @param mode The netcdf mode for the create operation.
 * @returns 0 for success, error code otherwise.
 * @ingroup PIO_createfile
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_createfile(int iosysid, int *ncidp, const int *iotype, const char *filename,
                    int mode)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    int ret;               /* Return code from function calls. */

    GPTLstart("PIO:PIOc_createfile");
    GPTLstart("PIO:write_total");
    if (*iotype == PIO_IOTYPE_ADIOS)
    {
        GPTLstart("PIO:PIOc_createfile_adios");
        GPTLstart("PIO:write_total_adios");
    }

    /* Get the IO system info from the id. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        GPTLstop("PIO:PIOc_createfile");
        GPTLstop("PIO:write_total");
        if (*iotype == PIO_IOTYPE_ADIOS)
        {
            GPTLstop("PIO:PIOc_createfile_adios");
            GPTLstop("PIO:write_total_adios");
        }
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Unable to create file (%s, mode = %d, iotype=%s). Invalid arguments provided, invalid iosystem id (iosysid = %d)", (filename) ? filename : "NULL", mode, (!iotype) ? "UNKNOWN" : pio_iotype_to_string(*iotype), iosysid);
    }

    spio_ltimer_start(ios->io_fstats->wr_timer_name);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    /* Create the file. */
    if ((ret = PIOc_createfile_int(iosysid, ncidp, iotype, filename, mode)))
    {
        GPTLstop("PIO:PIOc_createfile");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        if (*iotype == PIO_IOTYPE_ADIOS)
        {
            GPTLstop("PIO:PIOc_createfile_adios");
            GPTLstop("PIO:write_total_adios");
        }

        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Unable to create file (%s, mode = %d, iotype=%s) on iosystem (iosystem id = %d). Internal error creating the file", (filename) ? filename : "NULL", mode, (!iotype) ? "UNKNOWN" : pio_iotype_to_string(*iotype), iosysid);
    }

    /* Run this on all tasks if async is not in use, but only on
     * non-IO tasks if async is in use. (Because otherwise, in async
     * mode, set_fill would be called twice by each IO task, since
     * PIOc_createfile() will already be called on each IO task.) */
    if (!ios->async || !ios->ioproc)
    {
        /* Set the fill mode to NOFILL. */
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        if ((ret = PIOc_set_fill(*ncidp, NC_NOFILL, NULL)))
        {
            GPTLstop("PIO:PIOc_createfile");
            GPTLstop("PIO:write_total");
            if (*iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_createfile_adios");
                GPTLstop("PIO:write_total_adios");
            }
            return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                            "Unable to create file (%s, mode = %d, iotype=%s) on iosystem (iosystem id = %d). Setting fill mode to NOFILL failed.", (filename) ? filename : "NULL", mode, (!iotype) ? "UNKNOWN" : pio_iotype_to_string(*iotype), iosysid);
        }
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
    }

    GPTLstop("PIO:PIOc_createfile");
    GPTLstop("PIO:write_total");
    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    if (*iotype == PIO_IOTYPE_ADIOS)
    {
        GPTLstop("PIO:PIOc_createfile_adios");
        GPTLstop("PIO:write_total_adios");
    }

    return ret;
}

/**
 * Open a new file using pio. The default fill mode will be used (FILL
 * for netCDF and netCDF-4 formats, NOFILL for pnetcdf.) Input
 * parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid : A defined pio system descriptor (input)
 * @param cmode : The netcdf mode for the create operation.
 * @param filename : The filename to open
 * @param ncidp : A pio file descriptor (output)
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_create
 * @author Ed Hartnett
 */
int PIOc_create(int iosysid, const char *filename, int cmode, int *ncidp)
{
    int iotype;            /* The PIO IO type. */

    /* Set the default iotype. */
#ifdef _NETCDF
    iotype = PIO_IOTYPE_NETCDF;
#else /* Assume that _PNETCDF is defined. */
    iotype = PIO_IOTYPE_PNETCDF;
#endif

    /* Figure out the iotype. */
    if (cmode & NC_NETCDF4)
    {
#ifdef _NETCDF4
        if (cmode & NC_MPIIO || cmode & NC_MPIPOSIX)
            iotype = PIO_IOTYPE_NETCDF4P;
        else
            iotype = PIO_IOTYPE_NETCDF4C;
#endif
    }
    else
    {
#ifdef _PNETCDF
        if (cmode & NC_PNETCDF || cmode & NC_MPIIO)
            iotype = PIO_IOTYPE_PNETCDF;
#endif
    }

    return PIOc_createfile_int(iosysid, ncidp, &iotype, filename, cmode);
}

/* Internal helper function to perform sync operations
 * ncid : the ncid of the file to sync
 * Returns PIO_NOERR for success, error code otherwise
 */
static int sync_file(int ncid)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file = NULL;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    LOG((1, "sync_file ncid = %d", ncid));

    /* Get the file info from the ncid. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Syncing file (ncid=%d) failed. Invalid file id. Unable to find internal structure associated with the file id", ncid);
    }

    assert(file);
    ios = file->iosystem;
    assert(ios);

    if(file->mode & PIO_WRITE)
    {
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(file->io_fstats->wr_timer_name);
    }
    spio_ltimer_start(ios->io_fstats->tot_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);

#ifdef _ADIOS2
    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        if (file->mode & PIO_WRITE)
        {
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
        }
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);

        return ierr;
    }
#endif

    /* Flush data buffers on computational tasks. */
    if (!ios->async || !ios->ioproc)
    {
        if (file->mode & PIO_WRITE)
        {
            wmulti_buffer *wmb, *twmb;

            LOG((3, "sync_file checking buffers"));
            wmb = &file->buffer;
            while (wmb)
            {
                /* If there are any data arrays waiting in the
                 * multibuffer, flush it to IO tasks. */
                if (wmb->num_arrays > 0)
                {
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    flush_buffer(ncid, wmb, false);
                    spio_ltimer_start(ios->io_fstats->wr_timer_name);
                    spio_ltimer_start(ios->io_fstats->tot_timer_name);
                    spio_ltimer_start(file->io_fstats->wr_timer_name);
                    spio_ltimer_start(file->io_fstats->tot_timer_name);
                }
                twmb = wmb;
                wmb = wmb->next;
                if (twmb == &file->buffer)
                {
                    twmb->ioid = -1;
                    twmb->next = NULL;
                }
                else
                {
                    free(twmb);
                }
            }
        }
    }

    /* If async is in use, send message to IO master tasks. */
    if (ios->async)
    {
        int msg = PIO_MSG_SYNC;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid);
        if (ierr != PIO_NOERR)
        {
            if(file->mode & PIO_WRITE)
            {
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
            }
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Syncing file %s (ncid=%d) failed. Unable to send asynchronous message, PIO_MSG_SYNC, on iosystem (iosysid=%d)", pio_get_fname_from_file(file), ncid, ios->iosysid);
        }
    }

    /* Call the sync function on IO tasks.

       We choose not to call ncmpi_sync() for PIO_IOTYPE_PNETCDF,
       as it has been confirmed to have a very high cost on some
       systems. ncmpi_sync() itself does nothing but simply calls
       MPI_File_sync(), which usually incurs a huge performance
       penalty by calling POSIX sync internally. It is designed
       to ensure the data is safely stored on the disk hardware,
       before the function returns. People use it for extremely
       cautious behavior only.
     */
    if (file->mode & PIO_WRITE)
    {
        if (ios->ioproc)
        {
            switch (file->iotype)
            {
#ifdef _NETCDF4
            case PIO_IOTYPE_NETCDF4P:
                ierr = nc_sync(file->fh);
                break;
            case PIO_IOTYPE_NETCDF4C:
#endif
#ifdef _NETCDF
            case PIO_IOTYPE_NETCDF:
                if (ios->io_rank == 0)
                    ierr = nc_sync(file->fh);
                break;
#endif
#ifdef _PNETCDF
            case PIO_IOTYPE_PNETCDF:
                ierr = flush_output_buffer(file, true, 0);
                break;
#endif
#ifdef _HDF5
            case PIO_IOTYPE_HDF5:
                ierr = PIO_NOERR;
                break;
#endif
            default:
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__,
                                "Syncing file %s (ncid=%d) failed. Invalid/Unsupported iotype (%s:%d) provided", pio_get_fname_from_file(file), ncid, pio_iotype_to_string(file->iotype), file->iotype);
            }
        }
        LOG((2, "sync_file ierr = %d", ierr));
    }

    ierr = check_netcdf(ios, NULL, ierr, __FILE__, __LINE__);
    if (ierr != PIO_NOERR)
    {
        LOG((1, "nc*_sync failed, ierr = %d", ierr));
        if(file->mode & PIO_WRITE)
        {
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
        }
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return ierr;
    }

    if(file->mode & PIO_WRITE)
    {
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
    }
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return PIO_NOERR;
}

/**
 * Close a file previously opened with PIO.
 *
 * @param ncid: the file pointer
 * @returns PIO_NOERR for success, error code otherwise.
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_closefile(int ncid)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file = NULL;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
#ifdef _ADIOS2
    char outfilename[PIO_MAX_NAME + 1];
    size_t len = 0;
#endif

    LOG((1, "PIOc_closefile ncid = %d", ncid));

    /* Find the info about this file. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Closing file failed. Invalid file id (ncid=%d) provided", ncid);
    }
    assert(file);
    ios = file->iosystem;
    assert(ios);

    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        GPTLstart("PIO:PIOc_closefile_adios");
        GPTLstart("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
        GPTLstart("PIO:write_total");
#endif
    }
    else
    {
        GPTLstart("PIO:PIOc_closefile");

        if (file->mode & PIO_WRITE)
        {
            GPTLstart("PIO:PIOc_closefile_write_mode");
            GPTLstart("PIO:write_total");
        }
    }

    /* Sync changes before closing on all tasks if async is not in
     * use, but only on non-IO tasks if async is in use. */
    if (!ios->async || !ios->ioproc)
        if (file->mode & PIO_WRITE)
        {
            sync_file(ncid);
        }

    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
#ifndef _ADIOS_BP2NC_TEST
        spio_ltimer_start(ios->io_fstats->wr_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
        spio_ltimer_start(file->io_fstats->wr_timer_name);
        spio_ltimer_start(file->io_fstats->tot_timer_name);
#endif
    }
    else
    {
        if (file->mode & PIO_WRITE)
        {
            spio_ltimer_start(ios->io_fstats->wr_timer_name);
            spio_ltimer_start(file->io_fstats->wr_timer_name);
        }
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
        spio_ltimer_start(file->io_fstats->tot_timer_name);
    }

    /* If async is in use and this is a comp tasks, then the compmaster
     * sends a msg to the pio_msg_handler running on the IO master and
     * waiting for a message. Then broadcast the ncid over the intercomm
     * to the IO tasks. */
    if (ios->async)
    {
        int msg = PIO_MSG_CLOSE_FILE;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid);
        if(ierr != PIO_NOERR)
        {
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_closefile_adios");
                GPTLstop("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
                GPTLstop("PIO:write_total");
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
#endif
            }
            else
            {
                GPTLstop("PIO:PIOc_closefile");

                if (file->mode & PIO_WRITE)
                {
                    GPTLstop("PIO:PIOc_closefile_write_mode");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                }
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
            }
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                            "Closing file (%s, ncid=%d) failed. Error sending async msg PIO_MSG_CLOSE_FILE", pio_get_fname_from_file(file), ncid);
        }
    }

    /* ADIOS: assume all procs are also IO tasks */
#ifdef _ADIOS2
    if (file->iotype == PIO_IOTYPE_ADIOS)
    {
        if (file->adios_io_process == 1 && file->engineH != NULL)
        {
            LOG((2, "ADIOS close file %s", file->filename));

            ierr = begin_adios2_step(file, NULL);
            if (ierr != PIO_NOERR)
            {
                if (file->iotype == PIO_IOTYPE_ADIOS)
                {
                    GPTLstop("PIO:PIOc_closefile_adios");
                    GPTLstop("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
#endif
                }
                else
                {
                    GPTLstop("PIO:PIOc_closefile");

                    if (file->mode & PIO_WRITE)
                    {
                        GPTLstop("PIO:PIOc_closefile_write_mode");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    }
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                }
                return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                               "adios2_begin_step failed for file (%s)", pio_get_fname_from_file(file));
            }

            adios2_attribute *attributeH = adios2_inquire_attribute(file->ioH, "/__pio__/fillmode");
            if (attributeH == NULL)
            {
                attributeH = adios2_define_attribute(file->ioH, "/__pio__/fillmode", adios2_type_int32_t, &file->fillmode);
                if (attributeH == NULL)
                {
                    if (file->iotype == PIO_IOTYPE_ADIOS)
                    {
                        GPTLstop("PIO:PIOc_closefile_adios");
                        GPTLstop("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
#endif
                    }
                    else
                    {
                        GPTLstop("PIO:PIOc_closefile");

                        if (file->mode & PIO_WRITE)
                        {
                            GPTLstop("PIO:PIOc_closefile_write_mode");
                            GPTLstop("PIO:write_total");
                            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                            spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        }
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    }
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Defining (ADIOS) attribute (name=/__pio__/fillmode) failed for file (%s, ncid=%d)",
                                   pio_get_fname_from_file(file), file->pio_ncid);
                }
            }

            /* This is needed to write out the attribute /__pio__/fillmode */
            {
                adios2_variable *variableH = adios2_inquire_variable(file->ioH, "/__pio__/info/testing");
                if (variableH == NULL)
                {
                    variableH = adios2_define_variable(file->ioH,
                                                       "/__pio__/info/testing", adios2_type_int32_t,
                                                       0, NULL, NULL, NULL,
                                                       adios2_constant_dims_true);
                    if (variableH == NULL)
                    {
                        return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Defining (ADIOS) variable (name=/__pio__/info/testing) failed for file (%s)",
                                       pio_get_fname_from_file(file));
                    }
                }

                adios2_error adiosErr = adios2_put(file->engineH, variableH, &ios->num_uniontasks, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=/__pio__/info/testing) failed (adios2_error=%s) for file (%s)",
                                   convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
                }
            }

            GPTLstart("end_adios2_step_PIOc_closefile");
            ierr = end_adios2_step(file, ios);
            GPTLstop("end_adios2_step_PIOc_closefile");
            if (ierr != PIO_NOERR)
            {
                if (file->iotype == PIO_IOTYPE_ADIOS)
                {
                    GPTLstop("PIO:PIOc_closefile_adios");
                    GPTLstop("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
#endif
                }
                else
                {
                    GPTLstop("PIO:PIOc_closefile");

                    if (file->mode & PIO_WRITE)
                    {
                        GPTLstop("PIO:PIOc_closefile_write_mode");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    }
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                }
                return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                               "adios2_end_step failed for file (%s)", pio_get_fname_from_file(file));
            }

            adios2_error adiosErr = adios2_close(file->engineH);
            if (adiosErr != adios2_error_none)
            {
                if (file->iotype == PIO_IOTYPE_ADIOS)
                {
                    GPTLstop("PIO:PIOc_closefile_adios");
                    GPTLstop("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
#endif
                }
                else
                {
                    GPTLstop("PIO:PIOc_closefile");
                    if (file->mode & PIO_WRITE)
                    {
                        GPTLstop("PIO:PIOc_closefile_write_mode");
                        GPTLstop("PIO:write_total");
                        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    }
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                }
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__, "Closing (ADIOS) file (%s, ncid=%d) failed (adios2_error=%s)",
                               pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
            }

            file->engineH = NULL;
        }

        for (int i = 0; i < file->num_dim_vars; i++)
        {
            if (file->dim_names[i] != NULL)
            {
                free(file->dim_names[i]);
                file->dim_names[i] = NULL;
            }
        }

        file->num_dim_vars = 0;

        for (int i = 0; i < file->num_vars; i++)
        {
            if (file->adios_vars[i].name != NULL)
            {
                free(file->adios_vars[i].name);
                file->adios_vars[i].name = NULL;
            }
            if (file->adios_vars[i].gdimids != NULL)
            {
                free(file->adios_vars[i].gdimids);
                file->adios_vars[i].gdimids = NULL;
            }
            file->adios_vars[i].adios_varid = NULL;
            file->adios_vars[i].decomp_varid = NULL;
            file->adios_vars[i].frame_varid = NULL;
            file->adios_vars[i].fillval_varid = NULL;

            if (file->adios_vars[i].fillval_buffer != NULL)
            {
                free(file->adios_vars[i].fillval_buffer);
                file->adios_vars[i].fillval_buffer = NULL;
                file->adios_vars[i].fillval_cnt = 0;
            }
            if (file->adios_vars[i].decomp_buffer != NULL)
            {
                free(file->adios_vars[i].decomp_buffer);
                file->adios_vars[i].decomp_buffer = NULL;
                file->adios_vars[i].decomp_cnt = 0;
            }
            if (file->adios_vars[i].frame_buffer != NULL)
            {
                free(file->adios_vars[i].frame_buffer);
                file->adios_vars[i].frame_buffer = NULL;
                file->adios_vars[i].frame_cnt = 0;
            }
            if (file->adios_vars[i].num_wb_buffer != NULL)
            {
                free(file->adios_vars[i].num_wb_buffer);
                file->adios_vars[i].num_wb_buffer = NULL;
                file->adios_vars[i].num_wb_cnt = 0;
            }
        }

        file->num_vars = 0;

        /* Track attributes */
        for (int i = 0; i < file->num_attrs; i++)
        {
            if (file->adios_attrs[i].att_name != NULL)
            {
                free(file->adios_attrs[i].att_name);
                file->adios_attrs[i].att_name = NULL;
            }
        }

        file->num_attrs = 0;

        /* Block merging */
        if (file->block_myrank == 0)
        {
            if (file->block_array != NULL)
            {
                free(file->block_array);
                file->block_array = NULL;
                file->block_array_size = 0;
            }
            if (file->array_counts != NULL)
            {
                free(file->array_counts);
                file->array_counts = NULL;
                file->array_counts_size = 0;
            }
            if (file->array_disp != NULL)
            {
                free(file->array_disp);
                file->array_disp = NULL;
                file->array_disp_size = 0;
            }
            if (file->block_list != NULL)
            {
                free(file->block_list);
                file->block_list = NULL;
            }
        }

#ifdef _ADIOS_BP2NC_TEST /* Comment out for large scale run */
#ifdef _PNETCDF
        char conv_iotype[] = "pnetcdf";
#else
        char conv_iotype[] = "netcdf";
#endif

        int rearr_type = PIO_REARR_SUBSET;

        /* Convert XXXX.nc.bp to XXXX.nc */
        len = strlen(file->filename);
        assert(len > 6 && len <= PIO_MAX_NAME);
        strncpy(outfilename, file->filename, len - 3);
        outfilename[len - 3] = '\0';
        LOG((1, "CONVERTING: %s", file->filename));
        MPI_Barrier(ios->union_comm);
        ierr = C_API_ConvertBPToNC(file->filename, outfilename, conv_iotype, rearr_type, ios->union_comm);
        MPI_Barrier(ios->union_comm);
        LOG((1, "DONE CONVERTING: %s", file->filename));
        if (ierr != PIO_NOERR)
        {
            if (file->iotype == PIO_IOTYPE_ADIOS)
            {
                GPTLstop("PIO:PIOc_closefile_adios");
                GPTLstop("PIO:write_total_adios");
            }
            else
            {
                GPTLstop("PIO:PIOc_closefile");

                if (file->mode & PIO_WRITE)
                {
                    GPTLstop("PIO:PIOc_closefile_write_mode");
                    GPTLstop("PIO:write_total");
                    spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                }
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
            }
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                            "C_API_ConvertBPToNC(infile = %s, outfile = %s, piotype = %s) failed", file->filename, outfilename, conv_iotype);
        }
#endif
        if (file->filename != NULL)
        {
            free(file->filename);
            file->filename = NULL;
        }

        if (file->iotype == PIO_IOTYPE_ADIOS)
        {
            GPTLstop("PIO:PIOc_closefile_adios");
            GPTLstop("PIO:write_total_adios");
#ifndef _ADIOS_BP2NC_TEST
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
#endif
        }
        else
        {
            GPTLstop("PIO:PIOc_closefile");

            if (file->mode & PIO_WRITE)
            {
                GPTLstop("PIO:PIOc_closefile_write_mode");
                GPTLstop("PIO:write_total");
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
            }
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
        }

        spio_write_file_io_summary(file);

        /* Delete file from our list of open files. */
        pio_delete_file_from_list(ncid);

        return PIO_NOERR;
    }
#endif

    assert(file->iotype != PIO_IOTYPE_ADIOS);

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        switch (file->iotype)
        {
#ifdef _NETCDF4
        case PIO_IOTYPE_NETCDF4P:
            ierr = nc_close(file->fh);
            break;
        case PIO_IOTYPE_NETCDF4C:
#endif
#ifdef _NETCDF
        case PIO_IOTYPE_NETCDF:
            if (ios->io_rank == 0)
                ierr = nc_close(file->fh);
            break;
#endif
#ifdef _PNETCDF
        case PIO_IOTYPE_PNETCDF:
            if ((file->mode & PIO_WRITE)){
                ierr = ncmpi_buffer_detach(file->fh);
            }
            ierr = ncmpi_close(file->fh);
            break;
#endif
#ifdef _HDF5
        case PIO_IOTYPE_HDF5:
            ierr = PIO_NOERR;

            H5Pclose(file->dxplid_coll);
            H5Pclose(file->dxplid_indep);

            for (int i = 0; i < file->hdf5_num_dims; i++)
            {
                if (!file->hdf5_dims[i].has_coord_var)
                    H5Dclose(file->hdf5_dims[i].hdf5_dataset_id);
            }

            for (int i = 0; i < file->hdf5_num_vars; i++)
            {
                H5Dclose(file->hdf5_vars[i].hdf5_dataset_id);

                if (file->hdf5_vars[i].nc_type == NC_CHAR)
                    H5Tclose(file->hdf5_vars[i].hdf5_type);
            }

            H5Fclose(file->hdf5_file_id);

            break;
#endif
        default:
            GPTLstop("PIO:PIOc_closefile");
            if (file->mode & PIO_WRITE)
            {
                GPTLstop("PIO:PIOc_closefile_write_mode");
                GPTLstop("PIO:write_total");
                spio_ltimer_stop(ios->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
            }
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, file, PIO_EBADIOTYPE, __FILE__, __LINE__,
                            "Closing file (%s, ncid=%d) failed. Unsupported iotype (%d) specified", pio_get_fname_from_file(file), file->pio_ncid, file->iotype);
        }
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        LOG((1, "nc*_close failed, ierr = %d", ierr));
        GPTLstop("PIO:PIOc_closefile");
        if (file->mode & PIO_WRITE)
        {
            GPTLstop("PIO:PIOc_closefile_write_mode");
            GPTLstop("PIO:write_total");
            spio_ltimer_stop(ios->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
        }
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return pio_err(NULL, file, ierr, __FILE__, __LINE__,
                        "Closing file (%s, ncid=%d) failed. Underlying I/O library (iotype=%s) call failed", pio_get_fname_from_file(file), file->pio_ncid, pio_iotype_to_string(file->iotype));
    }

#ifdef _HDF5
    if (file->iotype == PIO_IOTYPE_HDF5)
    {
        for (int i = 0; i < file->hdf5_num_dims; i++)
        {
            free(file->hdf5_dims[i].name);
            file->hdf5_dims[i].name = NULL;
        }

        file->hdf5_num_dims = 0;

        for (int i = 0; i < file->hdf5_num_vars; i++)
        {
            free(file->hdf5_vars[i].name);
            file->hdf5_vars[i].name = NULL;

            free(file->hdf5_vars[i].alt_name);
            file->hdf5_vars[i].alt_name = NULL;

            free(file->hdf5_vars[i].hdf5_dimids);
            file->hdf5_vars[i].hdf5_dimids = NULL;
        }

        file->hdf5_num_vars = 0;
    }
#endif

    if (file->mode & PIO_WRITE)
    {
        GPTLstop("PIO:PIOc_closefile_write_mode");
        GPTLstop("PIO:write_total");
        spio_ltimer_stop(ios->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
    }
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);

    spio_write_file_io_summary(file);

    /* Delete file from our list of open files. */
    pio_delete_file_from_list(ncid);

    GPTLstop("PIO:PIOc_closefile");
    return ierr;
}

/**
 * Delete a file.
 *
 * @param iosysid a pio system handle.
 * @param filename a filename.
 * @returns PIO_NOERR for success, error code otherwise.
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_deletefile(int iosysid, const char *filename)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function codes. */
     int msg = PIO_MSG_DELETE_FILE;
    size_t len;

    GPTLstart("PIO:PIOc_deletefile");
    LOG((1, "PIOc_deletefile iosysid = %d filename = %s", iosysid, filename));

    /* Get the IO system info from the id. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        GPTLstop("PIO:PIOc_deletefile");
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Deleting file (%s) failed. Invalid I/O system id (iosysid=%d) specified.", (filename) ? filename : "NULL", iosysid);
    }

    assert(ios);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    /* If async is in use, send message to IO master task. */
    if (ios->async)
    {
        len = strlen(filename) + 1;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, len, filename);
        if(ierr != PIO_NOERR)
        {
            GPTLstop("PIO:PIOc_deletefile");
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                        "Deleting file (%s) failed. Sending async message, PIO_MSG_DELETE_FILE, failed", (filename) ? filename : "NULL");
        }
    }

    /* If this is an IO task, then call the netCDF function. The
     * barriers are needed to assure that no task is trying to operate
     * on the file while it is being deleted. IOTYPE is not known, but
     * nc_delete() will delete any type of file. */
    if (ios->ioproc)
    {
        mpierr = MPI_Barrier(ios->io_comm);

        if (!mpierr && ios->io_rank == 0)
#ifdef _NETCDF
             ierr = nc_delete(filename);
#else /* Assume that _PNETCDF is defined. */
             ierr = ncmpi_delete(filename, MPI_INFO_NULL);
#endif

        if (!mpierr)
            mpierr = MPI_Barrier(ios->io_comm);
    }
    LOG((2, "PIOc_deletefile ierr = %d", ierr));

    ierr = check_netcdf(ios, NULL, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
        GPTLstop("PIO:PIOc_deletefile");
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                    "Deleting file (%s) failed. Internal I/O library call failed.", (filename) ? filename : "NULL");
    }

    GPTLstop("PIO:PIOc_deletefile");
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    return ierr;
}

/**
 * PIO interface to nc_sync This routine is called collectively by all
 * tasks in the communicator ios.union_comm.
 *
 * Refer to the <A
 * HREF="http://www.unidata.ucar.edu/software/netcdf/docs/modules.html"
 * target="_blank"> netcdf </A> documentation.
 *
 * @param ncid the ncid of the file to sync.
 * @returns PIO_NOERR for success, error code otherwise.
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_sync(int ncid)
{
    file_desc_t *file = NULL;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    GPTLstart("PIO:PIOc_sync");
    LOG((1, "PIOc_sync ncid = %d", ncid));

    /* Get the file info from the ncid. */
    if ((ierr = pio_get_file(ncid, &file)))
    {
        GPTLstop("PIO:PIOc_sync");
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Syncing file (ncid=%d) failed. Invalid file id. Unable to find internal structure associated with the file id", ncid);
    }
    assert(file);

    if (file->mode & PIO_WRITE)
    {
        GPTLstart("PIO:write_total");
        if (file->iotype == PIO_IOTYPE_ADIOS)
            GPTLstart("PIO:write_total_adios");
    }

    ierr = sync_file(ncid);

    if (file->mode & PIO_WRITE)
    {
        GPTLstop("PIO:write_total");
        if (file->iotype == PIO_IOTYPE_ADIOS)
            GPTLstop("PIO:write_total_adios");
    }

    GPTLstop("PIO:PIOc_sync");

    return ierr;
}
