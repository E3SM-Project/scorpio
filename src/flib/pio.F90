!>
!! @file 
!! @brief User interface Module for PIO, this is the only file a user program should 'use'
!! 
!<

module pio
! Package all exposed variables and functions under one roof

! only pio_offset is intended for export from kinds

  use pio_kinds, only :  pio_offset_kind

  use spio_decomp, only : pio_initdecomp, pio_freedecomp, pio_get_local_array_size,&
                          pio_readdof, pio_writedof
  use spio_init, only : pio_init, pio_finalize, pio_iosystem_is_active,&
                        pio_iotask_rank, pio_iam_iotask, pio_getnumiotasks, pio_get_numiotasks,&
                        pio_set_hint, pio_set_rearr_opts, pio_set_blocksize
  use spio_file, only : pio_openfile, pio_createfile, pio_closefile,&
                        pio_deletefile, pio_setframe, pio_advanceframe,&
                        pio_syncfile, pio_file_is_open, pio_set_fill
  use spio_misc_api, only : pio_set_buffer_size_limit, pio_iotype_available
  use spio_err, only : pio_setdebuglevel, pio_seterrorhandling, pio_strerror, pio_set_log_level

  use pio_types, only : io_desc_t, file_desc_t, var_desc_t, iosystem_desc_t, &
       pio_rearr_opt_t, pio_rearr_comm_fc_opt_t, pio_rearr_comm_fc_2d_enable,&
       pio_rearr_comm_fc_1d_comp2io, pio_rearr_comm_fc_1d_io2comp,&
       pio_rearr_comm_fc_2d_disable, pio_rearr_comm_unlimited_pend_req,&
       pio_rearr_comm_p2p, pio_rearr_comm_coll,&
       pio_int, pio_real, pio_double, pio_noerr, iotype_netcdf, &
       iotype_pnetcdf,  pio_iotype_netcdf4p, pio_iotype_netcdf4c, &
       pio_iotype_pnetcdf,pio_iotype_netcdf, pio_iotype_adios, pio_iotype_hdf5, &
       pio_global, pio_char, pio_write, pio_nowrite, pio_clobber, pio_noclobber, &
       pio_max_name, pio_max_var_dims, pio_rearr_subset, pio_rearr_box, &
#if defined(_NETCDF) || defined(_PNETCDF)
       pio_fill, pio_nofill, pio_unlimited, pio_fill_char, pio_fill_int, pio_fill_double, pio_fill_float, &
#endif
       pio_64bit_offset, pio_64bit_data, &
       pio_internal_error, pio_bcast_error, pio_reduce_error,&
          pio_return_error, pio_default,&
       pio_contiguous, pio_chunked, pio_compact

  use spio_misc_api, only : pio_set_buffer_size_limit,  pio_iotype_available,&
                            pio_set_chunk_cache, pio_get_chunk_cache,&
                            pio_copy_att
  use spio_darray, only : pio_read_darray, pio_write_darray

  use spio_def_file, only : pio_enddef, pio_redef
  use spio_inq_file, only : pio_inq_nvars, pio_inq_ndims, pio_inq_natts, pio_inq_unlimdim,&
                            pio_inquire
  use spio_def_var, only :  pio_def_var, pio_def_var_deflate, pio_def_var_chunking,&
                            pio_set_var_chunk_cache, pio_get_var_chunk_cache
  use spio_inq_var, only :  pio_inq_varid, pio_inq_varname, pio_inq_vartype, pio_inq_varndims,&
                            pio_inq_varnatts, pio_inq_vardimid, pio_inq_var_deflate,&
                            pio_inquire_variable
  use spio_def_dim, only :  pio_def_dim
  use spio_inq_dim, only :  pio_inq_dimid, pio_inq_dimname, pio_inq_dimlen,&
                            pio_inquire_dimension
  use spio_inq_att, only :  pio_inq_attid, pio_inq_attname, pio_inq_atttype, pio_inq_attlen,&
                            pio_inq_att

  use spio_put_att, only : PIO_put_att
  use spio_get_att, only : PIO_get_att
  use spio_put_var, only : PIO_put_var
  use spio_get_var, only : PIO_get_var
  use iso_c_binding

  implicit none
  public

end module pio

