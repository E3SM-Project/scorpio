#ifndef __PIO_API_H__
#define __PIO_API_H__

/* This file contains the C user API declarations */
#if defined(__cplusplus)
extern "C" {
#endif

/* ========== Error handling APIs =========== */
int PIOc_strerror(int pioerr, char *errmsg, size_t errmsg_sz);
int PIOc_set_log_level(int level);

/* ========== APIs to handle I/O Decomposition =============== */

/* Init decomposition with 1-based compmap array. */
int PIOc_InitDecomp(int iosysid, int pio_type, int ndims, const int *gdimlen, int maplen,
                    const PIO_Offset *compmap, int *ioidp, const int *rearr,
                    const PIO_Offset *iostart, const PIO_Offset *iocount);
int PIOc_InitDecomp_bc(int iosysid, int basetype, int ndims, const int *gdimlen,
                       const long int *start, const long int *count, int *ioidp);

/* Init decomposition with 0-based compmap array. */
int PIOc_init_decomp(int iosysid, int pio_type, int ndims, const int *gdimlen, int maplen,
                     const PIO_Offset *compmap, int *ioidp, int rearranger,
                     const PIO_Offset *iostart, const PIO_Offset *iocount);

/* Free resources associated with a decomposition. */
int PIOc_freedecomp(int iosysid, int ioid);

/* Write/Read I/O decompositions */
int PIOc_readmap(const char *file, int *ndims, int **gdims, PIO_Offset *fmaplen,
                 PIO_Offset **map, MPI_Comm comm);
int PIOc_readmap_from_f90(const char *file,int *ndims, int **gdims, PIO_Offset *maplen,
                          PIO_Offset **map, int f90_comm);
int PIOc_writemap(const char *file, int ioid, int ndims, const int *gdims, PIO_Offset maplen,
                  const PIO_Offset *map, MPI_Comm comm);
int PIOc_writemap_from_f90(const char *file, int ioid, int ndims, const int *gdims,
                           PIO_Offset maplen, const PIO_Offset *map, int f90_comm);

/* Write a decomposition file. */
int PIOc_write_decomp(const char *file, int iosysid, int ioid, MPI_Comm comm);

/* Write a decomposition file using netCDF. */
int PIOc_write_nc_decomp(int iosysid, const char *filename, int cmode, int ioid,
                         const char *title, const char *history, int fortran_order);

/* Read a netCDF decomposition file. */
int PIOc_read_nc_decomp(int iosysid, const char *filename, int *ioid, MPI_Comm comm,
                        int pio_type, char *title, char *history, int *fortran_order);

/* ============= APIs for an I/O system (like MPI communicators) =================== */
/* Initializing I/O system for asynchronous I/O */
int PIOc_init_async(MPI_Comm world, int num_io_procs, const int *io_proc_list, int component_count,
                    const int *num_procs_per_comp, const int **proc_list, MPI_Comm *io_comm, MPI_Comm *comp_comm,
                    int rearranger, int *iosysidp);

/* Initializing I/O system for asynchronous I/O */
int PIOc_init_intercomm(int component_count, const MPI_Comm peer_comm,
                        const MPI_Comm *ucomp_comms, const MPI_Comm uio_comm,
                        int rearranger, int *iosysidps);
int PIOc_Init_Intercomm_from_F90(int component_count, int f90_peer_comm,
                                  const int *f90_comp_comms, int f90_io_comm,
                                  int rearranger, int *iosysidps);
int PIOc_get_numiotasks(int iosysid, int *numiotasks);
/* Initialize an I/O system */
int PIOc_Init_Intracomm(MPI_Comm comp_comm, int num_iotasks, int stride, int base, int rearr,
                        int *iosysidp);
int PIOc_Init_Intracomm_from_F90(int f90_comp_comm,
                                 const int num_iotasks, const int stride,
                                 const int base, const int rearr,
                                 rearr_opt_t *rearr_opts, int *iosysidp);
/* Finalize an I/O system */
int PIOc_finalize(int iosysid);

/* Set error handling for entire io system. */
int PIOc_Set_IOSystem_Error_Handling(int iosysid, int method);

/* Set error handling for entire io system. */
int PIOc_set_iosystem_error_handling(int iosysid, int method, int *old_method);

int PIOc_iam_iotask(int iosysid, bool *ioproc);
int PIOc_iotask_rank(int iosysid, int *iorank);
int PIOc_iosystem_is_active(int iosysid, bool *active);
int PIOc_iotype_available(int iotype);
int PIOc_set_rearr_opts(int iosysid, int comm_type, int fcd,
                        bool enable_hs_c2i, bool enable_isend_c2i,
                        int max_pend_req_c2i,
                        bool enable_hs_i2c, bool enable_isend_i2c,
                        int max_pend_req_i2c);
int PIOc_set_hint(int iosysid, const char *hint, const char *hintval);
int PIOc_set_chunk_cache(int iosysid, int iotype, PIO_Offset size, PIO_Offset nelems,
                         float preemption);
int PIOc_get_chunk_cache(int iosysid, int iotype, PIO_Offset *sizep, PIO_Offset *nelemsp,
                         float *preemptionp);
int PIOc_set_blocksize(int newblocksize);
/* Set the IO node data buffer size limit. */
PIO_Offset PIOc_set_buffer_size_limit(PIO_Offset limit);

/* ================= Read/Write APIs for distributed data ================ */
int PIOc_advanceframe(int ncid, int varid);
int PIOc_setframe(int ncid, int varid, int frame);
int PIOc_write_darray(int ncid, int varid, int ioid, PIO_Offset arraylen, const void *array,
                      const void *fillvalue);
int PIOc_write_darray_multi(int ncid, const int *varids, int ioid, int nvars, PIO_Offset arraylen,
                            const void *array, const int *frame, const void **fillvalue, bool flushtodisk);
int PIOc_read_darray(int ncid, int varid, int ioid, PIO_Offset arraylen, void *array);
int PIOc_get_local_array_size(int ioid);

/* ================= File APIs ================= */
int PIOc_deletefile(int iosysid, const char *filename);
int PIOc_createfile(int iosysid, int *ncidp, const int *iotype, const char *fname, int mode);
int PIOc_create(int iosysid, const char *path, int cmode, int *ncidp);
int PIOc_openfile(int iosysid, int *ncidp, int *iotype, const char *fname, int mode);
int PIOc_openfile2(int iosysid, int *ncidp, int *iotype, const char *fname, int mode);
int PIOc_openfile_retry(int iosysid, int *ncidp, int *iotype,
                        const char *filename, int mode, int retry);
int PIOc_open(int iosysid, const char *path, int mode, int *ncidp);
int PIOc_closefile(int ncid);
int PIOc_File_is_Open(int ncid);
/* Set the error hanlding for a file. */
int PIOc_Set_File_Error_Handling(int ncid, int method);
int PIOc_sync(int ncid);

/* ================= File meta-data APIs ============== */
int PIOc_redef(int ncid);
int PIOc_enddef(int ncid);
int PIOc_inq_format(int ncid, int *formatp);
int PIOc_inq(int ncid, int *ndimsp, int *nvarsp, int *ngattsp, int *unlimdimidp);
int PIOc_inq_ndims(int ncid, int *ndimsp);
int PIOc_inq_nvars(int ncid, int *nvarsp);
int PIOc_inq_natts(int ncid, int *ngattsp);
int PIOc_inq_unlimdim(int ncid, int *unlimdimidp);
int PIOc_inq_unlimdims(int ncid, int *nunlimdimsp, int *unlimdimidsp);
int PIOc_inq_type(int ncid, nc_type xtype, char *name, PIO_Offset *sizep);

/* APIs for variable dimensions */
int PIOc_inq_dim(int ncid, int dimid, char *name, PIO_Offset *lenp);
int PIOc_inq_dimid(int ncid, const char *name, int *idp);
int PIOc_inq_dimname(int ncid, int dimid, char *name);
int PIOc_inq_dimlen(int ncid, int dimid, PIO_Offset *lenp);
int PIOc_rename_dim(int ncid, int dimid, const char *name);
int PIOc_def_dim(int ncid, const char *name, PIO_Offset len, int *idp);

/* APIs for file variables */
int PIOc_inq_varid(int ncid, const char *name, int *varidp);
int PIOc_inq_var(int ncid, int varid, char *name, int namelen, nc_type *xtypep, int *ndimsp,
                 int *dimidsp, int *nattsp);
int PIOc_inq_varname(int ncid, int varid, char *name, int namelen);
int PIOc_inq_vartype(int ncid, int varid, nc_type *xtypep);
int PIOc_inq_varndims(int ncid, int varid, int *ndimsp);
int PIOc_inq_vardimid(int ncid, int varid, int *dimidsp);
int PIOc_inq_varnatts(int ncid, int varid, int *nattsp);
int PIOc_def_var(int ncid, const char *name, nc_type xtype,  int ndims,
                 const int *dimidsp, int *varidp);
int PIOc_set_fill(int ncid, int fillmode, int *old_modep);
int PIOc_def_var_fill(int ncid, int varid, int no_fill, const void *fill_value);
int PIOc_inq_var_fill(int ncid, int varid, int *no_fill, void *fill_valuep);
int PIOc_rename_var(int ncid, int varid, const char *name);

/* APIs for data/variable compression. These settings only apply to netCDF-4 files for now */
int PIOc_def_var_deflate(int ncid, int varid, int shuffle, int deflate,
                         int deflate_level);
int PIOc_inq_var_deflate(int ncid, int varid, int *shufflep, int *deflatep,
                         int *deflate_levelp);
/* PIOc_inq_var_szip() is not implemented
int PIOc_inq_var_szip(int ncid, int varid, int *options_maskp, int *pixels_per_blockp);
*/
int PIOc_def_var_chunking(int ncid, int varid, int storage, const PIO_Offset *chunksizesp);
int PIOc_inq_var_chunking(int ncid, int varid, int *storagep, PIO_Offset *chunksizesp);
int PIOc_def_var_endian(int ncid, int varid, int endian);
int PIOc_inq_var_endian(int ncid, int varid, int *endianp);
int PIOc_set_var_chunk_cache(int ncid, int varid, PIO_Offset size, PIO_Offset nelems,
                             float preemption);
int PIOc_get_var_chunk_cache(int ncid, int varid, PIO_Offset *sizep, PIO_Offset *nelemsp,
                             float *preemptionp);

/* APIs for file/variable attributes */
int PIOc_rename_att(int ncid, int varid, const char *name, const char *newname);
int PIOc_del_att(int ncid, int varid, const char *name);
int PIOc_inq_att(int ncid, int varid, const char *name, nc_type *xtypep,
                 PIO_Offset *lenp);
int PIOc_inq_attid(int ncid, int varid, const char *name, int *idp);
int PIOc_inq_attlen(int ncid, int varid, const char *name, PIO_Offset *lenp);
int PIOc_inq_atttype(int ncid, int varid, const char *name, nc_type *xtypep);
int PIOc_inq_attname(int ncid, int varid, int attnum, char *name);

int PIOc_copy_att(int incid, int ivarid, const char *name, int oncid, int ovarid);

/* Write APIs for file/variable attributes */
int PIOc_put_att(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len, const void *op);
int PIOc_put_att_text(int ncid, int varid, const char *name, PIO_Offset len, const char *op);
int PIOc_put_att_schar(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const signed char *op);
int PIOc_put_att_short(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const short *op);
int PIOc_put_att_int(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                     const int *op);
int PIOc_put_att_long(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                      const long *op);
int PIOc_put_att_float(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const float *op);
int PIOc_put_att_double(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                        const double *op);
int PIOc_put_att_uchar(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                       const unsigned char *op);
int PIOc_put_att_ushort(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                        const unsigned short *op);
int PIOc_put_att_uint(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                      const unsigned int *op);
int PIOc_put_att_longlong(int ncid, int varid, const char *name, nc_type xtype, PIO_Offset len,
                          const long long *op);
int PIOc_put_att_ulonglong(int ncid, int varid, const char *name, nc_type xtype,
                           PIO_Offset len, const unsigned long long *op);

/* APIs for reading file/variable attributes */
int PIOc_get_att(int ncid, int varid, const char *name, void *ip);
int PIOc_get_att_text(int ncid, int varid, const char *name, char *ip);
int PIOc_get_att_schar(int ncid, int varid, const char *name, signed char *ip);
int PIOc_get_att_short(int ncid, int varid, const char *name, short *ip);
int PIOc_get_att_int(int ncid, int varid, const char *name, int *ip);
int PIOc_get_att_long(int ncid, int varid, const char *name, long *ip);
int PIOc_get_att_float(int ncid, int varid, const char *name, float *ip);
int PIOc_get_att_double(int ncid, int varid, const char *name, double *ip);
int PIOc_get_att_uchar(int ncid, int varid, const char *name, unsigned char *ip);
int PIOc_get_att_ushort(int ncid, int varid, const char *name, unsigned short *ip);
int PIOc_get_att_uint(int ncid, int varid, const char *name, unsigned int *ip);
int PIOc_get_att_longlong(int ncid, int varid, const char *name, long long *ip);
int PIOc_get_att_ulonglong(int ncid, int varid, const char *name, unsigned long long *ip);

/* APIs for reading entire non-distributed data/variable */
int PIOc_get_var(int ncid, int varid, void *buf);
int PIOc_get_var_text(int ncid, int varid, char *buf);
int PIOc_get_var_schar(int ncid, int varid, signed char *buf);
int PIOc_get_var_short(int ncid, int varid, short *buf);
int PIOc_get_var_int(int ncid, int varid, int *buf);
int PIOc_get_var_long(int ncid, int varid, long *buf);
int PIOc_get_var_float(int ncid, int varid, float *buf);
int PIOc_get_var_double(int ncid, int varid, double *buf);
int PIOc_get_var_uchar(int ncid, int varid, unsigned char *buf);
int PIOc_get_var_ushort(int ncid, int varid, unsigned short *buf);
int PIOc_get_var_uint(int ncid, int varid, unsigned int *buf);
int PIOc_get_var_longlong(int ncid, int varid, long long *buf);
int PIOc_get_var_ulonglong(int ncid, int varid, unsigned long long *buf);

/* APIs for writing entire non-distributed data/variable */
int PIOc_put_var(int ncid, int varid, const void *buf);
int PIOc_put_var_text(int ncid, int varid, const char *op);
int PIOc_put_var_schar(int ncid, int varid, const signed char *op);
int PIOc_put_var_short(int ncid, int varid, const short *op);
int PIOc_put_var_int(int ncid, int varid, const int *op);
int PIOc_put_var_long(int ncid, int varid, const long *op);
int PIOc_put_var_float(int ncid, int varid, const float *op);
int PIOc_put_var_double(int ncid, int varid, const double *op);
int PIOc_put_var_uchar(int ncid, int varid, const unsigned char *op);
int PIOc_put_var_ushort(int ncid, int varid, const unsigned short *op);
int PIOc_put_var_uint(int ncid, int varid, const unsigned int *op);
int PIOc_put_var_longlong(int ncid, int varid, const long long *op);
int PIOc_put_var_ulonglong(int ncid, int varid, const unsigned long long *op);

/* APIs for reading non-distributed data/variable at a specified index */
int PIOc_get_var1(int ncid, int varid, const PIO_Offset *index, void *buf);
int PIOc_get_var1_text(int ncid, int varid, const PIO_Offset *index, char *buf);
int PIOc_get_var1_schar(int ncid, int varid, const PIO_Offset *index, signed char *buf);
int PIOc_get_var1_short(int ncid, int varid, const PIO_Offset *index, short *buf);
int PIOc_get_var1_int(int ncid, int varid, const PIO_Offset *index, int *buf);
int PIOc_get_var1_long(int ncid, int varid, const PIO_Offset *index, long *buf);
int PIOc_get_var1_float(int ncid, int varid, const PIO_Offset *index, float *buf);
int PIOc_get_var1_double(int ncid, int varid, const PIO_Offset *index, double *buf);
int PIOc_get_var1_uchar(int ncid, int varid, const PIO_Offset *index, unsigned char *buf);
int PIOc_get_var1_ushort(int ncid, int varid, const PIO_Offset *index, unsigned short *buf);
int PIOc_get_var1_uint(int ncid, int varid, const PIO_Offset *index, unsigned int *buf);
int PIOc_get_var1_longlong(int ncid, int varid, const PIO_Offset *index, long long *buf);
int PIOc_get_var1_ulonglong(int ncid, int varid, const PIO_Offset *index, unsigned long long *buf);

/* APIs for writing non-distributed data/variable at a specified index */
int PIOc_put_var1(int ncid, int varid, const PIO_Offset *index, const void *buf);
int PIOc_put_var1_text(int ncid, int varid, const PIO_Offset *index, const char *op);
int PIOc_put_var1_schar(int ncid, int varid, const PIO_Offset *index, const signed char *op);
int PIOc_put_var1_short(int ncid, int varid, const PIO_Offset *index, const short *op);
int PIOc_put_var1_int(int ncid, int varid, const PIO_Offset *index, const int *op);
int PIOc_put_var1_long(int ncid, int varid, const PIO_Offset *index, const long *ip);
int PIOc_put_var1_float(int ncid, int varid, const PIO_Offset *index, const float *op);
int PIOc_put_var1_double(int ncid, int varid, const PIO_Offset *index, const double *op);
int PIOc_put_var1_uchar(int ncid, int varid, const PIO_Offset *index,
                        const unsigned char *op);
int PIOc_put_var1_ushort(int ncid, int varid, const PIO_Offset *index,
                         const unsigned short *op);
int PIOc_put_var1_uint(int ncid, int varid, const PIO_Offset *index,
                       const unsigned int *op);
int PIOc_put_var1_longlong(int ncid, int varid, const PIO_Offset *index, const long long *op);
int PIOc_put_var1_ulonglong(int ncid, int varid, const PIO_Offset *index,
                            const unsigned long long *op);

/* APIs for reading a hyperslab of non-distributed data/variable */
int PIOc_get_vara(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count, void *buf);
int PIOc_get_vara_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       char *buf);
int PIOc_get_vara_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        signed char *buf);
int PIOc_get_vara_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        short *buf);
int PIOc_get_vara_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      int *buf);
int PIOc_get_vara_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        float *buf);
int PIOc_get_vara_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       long *buf);
int PIOc_get_vara_double(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, double *buf);
int PIOc_get_vara_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        unsigned char *buf);
int PIOc_get_vara_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         unsigned short *buf);
int PIOc_get_vara_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       unsigned int *buf);
int PIOc_get_vara_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           long long *buf);
int PIOc_get_vara_ulonglong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                            unsigned long long *buf);

/* APIs for writing a hyperslab of non-distributed data/variable */
int PIOc_put_vara(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const void *buf);
int PIOc_put_vara_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const char *op);
int PIOc_put_vara_schar(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const signed char *op);
int PIOc_put_vara_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const short *op);
int PIOc_put_vara_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const int *op);
int PIOc_put_vara_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const long *op);
int PIOc_put_vara_float(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const float *op);
int PIOc_put_vara_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const double *op);
int PIOc_put_vara_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const unsigned char *op);
int PIOc_put_vara_ushort(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, const unsigned short *op);
int PIOc_put_vara_uint(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const unsigned int *op);
int PIOc_put_vara_longlong(int ncid, int varid, const PIO_Offset *start,
                           const PIO_Offset *count, const long long *op);
int PIOc_put_vara_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const unsigned long long *op);

/* APIs for reading a hyperslab of strided non-distributed data/variable */
int PIOc_get_vars(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, void *buf);
int PIOc_get_vars_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, char *buf);
int PIOc_get_vars_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, signed char *buf);
int PIOc_get_vars_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, short *buf);
int PIOc_get_vars_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, int *buf);
int PIOc_get_vars_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, long *buf);
int PIOc_get_vars_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, float *buf);
int PIOc_get_vars_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, double *buf);
int PIOc_get_vars_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, unsigned char *buf);
int PIOc_get_vars_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, unsigned short *buf);
int PIOc_get_vars_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, unsigned int *buf);
int PIOc_get_vars_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, long long *buf);
int PIOc_get_vars_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            unsigned long long *buf);

/* APIs for writing a hyperslab of strided non-distributed data/variable */
int PIOc_put_vars(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const void *buf);
int PIOc_put_vars_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride, const char *op);
int PIOc_put_vars_schar(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride,
                        const signed char *op);
int PIOc_put_vars_short(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride, const short *op);
int PIOc_put_vars_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, const int *op);
int PIOc_put_vars_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const float *op);
int PIOc_put_vars_double(int ncid, int varid, const PIO_Offset *start,
                         const PIO_Offset *count, const PIO_Offset *stride, const double *op);
int PIOc_put_vars_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const long *op);
int PIOc_put_vars_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const unsigned char *op);
int PIOc_put_vars_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const unsigned short *op);
int PIOc_put_vars_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const unsigned int *op);
int PIOc_put_vars_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const long long *op);
int PIOc_put_vars_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            const unsigned long long *op);

/* APIs for reading/writing a hyperslab of strided non-distributed data/variable
 * with a mapped array. The mapped array maps between memory and variable data
 */
/* Varm functions are deprecated and should be used with extreme
 * caution or not at all. Varm functions are not supported in
 * async mode. */
int PIOc_get_varm_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, signed char *buf);
int PIOc_get_varm_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, short *buf);
int PIOc_get_varm_ulonglong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                            const PIO_Offset *stride, const PIO_Offset *imap, unsigned long long *buf);
int PIOc_get_varm_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, unsigned short *buf);
int PIOc_get_varm_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const PIO_Offset *imap, long long *buf);
int PIOc_get_varm_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, double *buf);
int PIOc_get_varm_text(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, char *buf);
int PIOc_get_varm_int(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, const PIO_Offset *imap, int *buf);
int PIOc_get_varm_uint(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, unsigned int *buf);
int PIOc_get_varm(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const PIO_Offset *imap, void *buf,
                  PIO_Offset bufcount, MPI_Datatype buftype);
int PIOc_get_varm_float(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, float *buf);
int PIOc_get_varm_long(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                       const PIO_Offset *stride, const PIO_Offset *imap, long *buf);
int PIOc_put_varm(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                  const PIO_Offset *stride, const PIO_Offset *imap, const void *buf,
                  PIO_Offset bufcount, MPI_Datatype buftype);
int PIOc_put_varm_uchar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap,
                        const unsigned char *op);
int PIOc_put_varm_short(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, const short *op);
int PIOc_put_varm_text(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride,
                       const PIO_Offset *imap, const char *op);
int PIOc_put_varm_ushort(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, const unsigned short *op);
int PIOc_put_varm_ulonglong(int ncid, int varid, const PIO_Offset *start,
                            const PIO_Offset *count, const PIO_Offset *stride,
                            const PIO_Offset *imap, const unsigned long long *op);
int PIOc_put_varm_int(int ncid, int varid, const PIO_Offset *start,
                      const PIO_Offset *count, const PIO_Offset *stride,
                      const PIO_Offset *imap, const int *op);
int PIOc_put_varm_float(int ncid, int varid, const PIO_Offset *start,
                        const PIO_Offset *count, const PIO_Offset *stride,
                        const PIO_Offset *imap, const float *op);
int PIOc_put_varm_long(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride,
                       const PIO_Offset *imap, const long *op);
int PIOc_put_varm_uint(int ncid, int varid, const PIO_Offset *start,
                       const PIO_Offset *count, const PIO_Offset *stride,
                       const PIO_Offset *imap, const unsigned int *op);
int PIOc_put_varm_double(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                         const PIO_Offset *stride, const PIO_Offset *imap, const double *op);
int PIOc_put_varm_schar(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                        const PIO_Offset *stride, const PIO_Offset *imap, const signed char *op);
int PIOc_put_varm_longlong(int ncid, int varid, const PIO_Offset *start, const PIO_Offset *count,
                           const PIO_Offset *stride, const PIO_Offset *imap, const long long *op);

#if defined(__cplusplus)
}
#endif

#endif /* __PIO_API_H__ */
