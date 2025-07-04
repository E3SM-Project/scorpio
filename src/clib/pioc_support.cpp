/** @file
 * Support functions for the PIO library.
 */
#include <pio_config.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdarg.h>
#include <pio.h>
#include <pio_internal.h>

#include <execinfo.h>

#include <dirent.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <libgen.h>
#include <string>
#include <regex>
#include "spio_io_summary.h"
#include "spio_file_mvcache.h"
#include "spio_hash.h"

#define VERSNO 2001

#if defined(__cplusplus)
extern "C"{
#endif

void PIOc_warn(int iosysid, int ncid, const char *fname, int line, const char *uwarn_msg);
int PIOc_error(int iosysid, int ncid, int err_num, const char *fname, int line, const char *uerr_msg);

#ifdef TIMING_INTERNAL
/* GPTL does not export this function via the header file. So adding the func decl here for
 * portability acorss multiple versions of GPTL
 * FIXME: Modify all versions of GPTL that we use to export this function
 */
extern int GPTLis_initialized (void);
#endif

#if defined(__cplusplus)
}
#endif

/* Some logging constants. */
#if PIO_ENABLE_LOGGING
#define MAX_LOG_MSG 1024
#define MAX_RANK_STR 12
#define ERROR_PREFIX "ERROR: "
#define NC_LEVEL_DIFF 3
int pio_log_level = 0;
int pio_log_ref_cnt = 0;
int my_rank;
FILE *LOG_FILE = NULL;
#endif /* PIO_ENABLE_LOGGING */
int pio_timer_ref_cnt = 0;

/**
 * The PIO library maintains its own set of ncids. This is the next
 * ncid number that will be assigned.
 */
extern int pio_next_ncid;

/** The default error handler used when iosystem cannot be located. */
extern int default_error_handler;

extern bool fortran_order;

static std::string spio_get_nczarr_fname(const char *fname)
{
  std::string nczarr_fname;
  const std::string nczarr_fname_prefix = "file://";
  const std::string nczarr_fname_suffix = "#mode=nczarr,file";
  const std::string fname_sep = "/";

  if(!fname){
    return nczarr_fname;
  }

  char dname[PIO_MAX_NAME + 1];
  char real_dname[PIO_MAX_NAME + 1];
  char bname[PIO_MAX_NAME + 1];

  strncpy(dname, fname, PIO_MAX_NAME); dname[PIO_MAX_NAME] = '\0';
  strncpy(bname, fname, PIO_MAX_NAME); bname[PIO_MAX_NAME] = '\0';

  char *dname_ptr = dirname(dname);
  char *bname_ptr = basename(bname);

  char *real_dname_ptr = realpath(dname_ptr, real_dname);
  if(real_dname_ptr){
    nczarr_fname =  nczarr_fname_prefix +
                    real_dname_ptr + fname_sep + bname_ptr +
                    nczarr_fname_suffix;
  }

  return nczarr_fname;
}

/**
 * Utility function to remove a directory and all its contents.
 */
int spio_remove_directory(const char *path)
{
    DIR *d = opendir(path);
    size_t path_len = strlen(path);
    int r = -1;

    if (d)
    {
        struct dirent *p;

        r = 0;

        while (!r && (p = readdir(d)))
        {
            int r2 = -1;
            char *buf;
            size_t len;

            /* Skip the names "." and ".." as we don't want to recurse on them. */
            if (!strncmp(p->d_name, ".", 1) || !strncmp(p->d_name, "..", 2))
                continue;

            len = path_len + strlen(p->d_name) + 2;
            buf = (char *) malloc(len);

            if (buf)
            {
                struct stat statbuf;

                snprintf(buf, len, "%s/%s", path, p->d_name);

                if (!stat(buf, &statbuf))
                {
                    if (S_ISDIR(statbuf.st_mode))
                        r2 = spio_remove_directory(buf);
                    else
                        r2 = unlink(buf);
                }

                free(buf);
            }

            r = r2;
        }

        closedir(d);
    }

    if (!r)
        r = rmdir(path);

    return r;
}

#ifdef _ADIOS2
static int flush_adios2_tracking_data(file_desc_t *file)
{
    if (file->adios_io_process != 1)
    {
        return PIO_NOERR;
    }

    GPTLstart("PIO:flush_adios2_tracking_data");
    adios2_error adiosErr = adios2_error_none;
    for (int i = 0; i < file->num_vars; i++)
    {
        adios_var_desc_t *av = &(file->adios_vars[i]);
        if (av->decomp_cnt > 0)
        {
            if (file->adios_rank == file->write_decomp_id)
            {
                size_t count_val = (size_t)av->decomp_cnt;
                adiosErr = adios2_set_selection(av->decomp_varid, 1, NULL, &count_val);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Setting (ADIOS) selection to variable (name=decomp_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }

                #if defined(_SPIO_ADIOS_USE_LOSSY_COMPRESSION) && defined(ADIOS2_HAVE_ZFP)
                if (file->iosystem->adios_lossy_compression_method == ADIOS_COMPRESSION_METHOD_ZFP)
                {
                    if (count_val == 1)
                    {
                        adiosErr = adios2_remove_operations(av->decomp_varid);
                        if (adiosErr != adios2_error_none)
                        {
                            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                           "Removing all current operations associated with variable (name=decomp_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                           av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                        }
                    }
                }
                #endif

                adiosErr = adios2_put(file->engineH, av->decomp_varid, av->decomp_buffer, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=decomp_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }
                av->decomp_cnt = 0;
                file->num_written_blocks += 1;
            }
        }

        if (av->frame_cnt > 0)
        {
            if (file->adios_rank == file->write_frame_id)
            {
                size_t count_val = (size_t)av->frame_cnt;
                adiosErr = adios2_set_selection(av->frame_varid, 1, NULL, &count_val);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Setting (ADIOS) selection to variable (name=frame_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }

                #if defined(_SPIO_ADIOS_USE_LOSSY_COMPRESSION) && defined(ADIOS2_HAVE_ZFP)
                if (file->iosystem->adios_lossy_compression_method == ADIOS_COMPRESSION_METHOD_ZFP)
                {
                    if (count_val == 1)
                    {
                        adiosErr = adios2_remove_operations(av->frame_varid);
                        if (adiosErr != adios2_error_none)
                        {
                            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                           "Removing all current operations associated with variable (name=frame_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                           av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                        }
                    }
                }
                #endif

                adiosErr = adios2_put(file->engineH, av->frame_varid, av->frame_buffer, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=frame_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }
                av->frame_cnt = 0;
                file->num_written_blocks += 1;
            }
        }

        if (av->fillval_cnt > 0)
        {
            if (file->adios_rank == file->write_fillval_id)
            {
                size_t count_val = (size_t)av->fillval_cnt;
                adiosErr = adios2_set_selection(av->fillval_varid, 1, NULL, &count_val);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Setting (ADIOS) selection to variable (name=fillval_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }

                #if defined(_SPIO_ADIOS_USE_LOSSY_COMPRESSION) && defined(ADIOS2_HAVE_ZFP)
                if (file->iosystem->adios_lossy_compression_method == ADIOS_COMPRESSION_METHOD_ZFP)
                {
                    if (count_val == 1)
                    {
                        adiosErr = adios2_remove_operations(av->fillval_varid);
                        if (adiosErr != adios2_error_none)
                        {
                            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                           "Removing all current operations associated with variable (name=fillval_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                           av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                        }
                    }
                }
                #endif

                adiosErr = adios2_put(file->engineH, av->fillval_varid, av->fillval_buffer, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=fillval_id/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }
                av->fillval_cnt = 0;
                file->num_written_blocks += 1;
            }
        }

        if (av->num_wb_cnt > 0)
        {
            if (file->block_myrank == 0)
            {
                size_t count_val = (size_t)av->num_wb_cnt;
                adiosErr = adios2_set_selection(av->num_block_writers_varid, 1, NULL, &count_val);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Setting (ADIOS) selection to variable (name=num_data_block_writers/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }

                #if defined(_SPIO_ADIOS_USE_LOSSY_COMPRESSION) && defined(ADIOS2_HAVE_ZFP)
                if (file->iosystem->adios_lossy_compression_method == ADIOS_COMPRESSION_METHOD_ZFP)
                {
                    if (count_val == 1)
                    {
                        adiosErr = adios2_remove_operations(av->num_block_writers_varid);
                        if (adiosErr != adios2_error_none)
                        {
                            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                           "Removing all current operations associated with variable (num_data_block_writers/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                           av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                        }
                    }
                }
                #endif

                adiosErr = adios2_put(file->engineH, av->num_block_writers_varid, av->num_wb_buffer, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    GPTLstop("PIO:flush_adios2_tracking_data");
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=num_data_block_writers/%s) failed (adios2_error=%s) for file (%s, ncid=%d)",
                                   av->name, convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file), file->pio_ncid);
                }
                av->num_wb_cnt = 0;
                file->num_written_blocks += 1;
            }
        }
    }

    GPTLstop("PIO:flush_adios2_tracking_data");

    return PIO_NOERR;
}

static int initialize_adios2_for_block_merging(iosystem_desc_t *ios, file_desc_t *file)
{
    GPTLstart("PIO:initialize_adios2_for_block_merging");

    int mpierr = MPI_SUCCESS;

    file->block_comm   = ios->block_comm;
    file->block_myrank = ios->block_myrank;
    file->block_nprocs = ios->block_nprocs;

    /* Gather the list of processes in each block group */
    file->block_list = NULL;
    if (file->block_myrank == 0)
    {
        file->block_list = (int*)calloc(file->block_nprocs, sizeof(int));
        if (file->block_list == NULL)
        {
            GPTLstop("PIO:initialize_adios2_for_block_merging");
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                           "Initializing (ADIOS) for block merging on file (%s, ncid=%d) failed. Out of memory allocating %lld bytes for block list",
                           pio_get_fname_from_file(file), file->pio_ncid, (long long) (file->block_nprocs * sizeof(int)));
        }
    }

    mpierr = MPI_Gather(&(file->all_rank), 1, MPI_INT, file->block_list, 1, MPI_INT, 0, file->block_comm);
    if (mpierr != MPI_SUCCESS)
    {
        GPTLstop("PIO:initialize_adios2_for_block_merging");
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    }

    /* Arrays to be used for merging blocks */
    file->block_array = NULL;
    file->block_array_size = 0;
    file->array_counts = NULL;
    file->array_counts_size = 0;
    file->array_disp = NULL;
    file->array_disp_size = 0;
    if (file->block_myrank == 0)
    {
        file->block_array = (char*)calloc(BLOCK_MAX_BUFFER, sizeof(char));
        if (file->block_array == NULL)
        {
            GPTLstop("PIO:initialize_adios2_for_block_merging");
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                           "Initializing (ADIOS) for block merging on file (%s, ncid=%d) failed. Out of memory allocating %lld bytes for block array",
                           pio_get_fname_from_file(file), file->pio_ncid, (long long) (BLOCK_MAX_BUFFER * sizeof(char)));
        }

        file->block_array_size = BLOCK_MAX_BUFFER;

        adios2_error adiosErr = adios2_error_none;

        /* BP5 */
        adiosErr = adios2_set_parameter(file->ioH, "BufferVType", "chunk");
        if (adiosErr != adios2_error_none)
        {
            GPTLstop("PIO:initialize_adios2_for_block_merging");
            return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Setting (ADIOS) parameter (InitialBufferSize) failed (adios2_error=%s) for file (%s)",
                           convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
        }

        adiosErr = adios2_set_parameter(file->ioH, "BufferChunkSize", "1Gb");
        if (adiosErr != adios2_error_none)
        {
            GPTLstop("PIO:initialize_adios2_for_block_merging");
            return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Setting (ADIOS) parameter (InitialBufferSize) failed (adios2_error=%s) for file (%s)",
                           convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
        }
        /* BP5 */

#ifndef _ADIOS_BP2NC_TEST /* Initializing buffer to 1Gb takes about 1 sec. Don't do it for unit tests */
        adiosErr = adios2_set_parameter(file->ioH, "InitialBufferSize", "1Gb");
        if (adiosErr != adios2_error_none)
        {
            GPTLstop("PIO:initialize_adios2_for_block_merging");
            return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Setting (ADIOS) parameter (InitialBufferSize) failed (adios2_error=%s) for file (%s)",
                           convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
        }
#endif

        file->array_counts = (unsigned int*)calloc(file->block_nprocs, sizeof(unsigned int));
        file->array_disp   = (unsigned int*)calloc(file->block_nprocs, sizeof(unsigned int));
        if (file->array_counts == NULL || file->array_disp == NULL)
        {
            GPTLstop("PIO:initialize_adios2_for_block_merging");
            return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                           "Initializing (ADIOS) for block merging on file (%s, ncid=%d) failed. Out of memory allocating %lld bytes for internal buffers used for merging distributed data blocks",
                           pio_get_fname_from_file(file), file->pio_ncid, (long long) (file->block_nprocs * sizeof(unsigned int)));
        }
        file->array_counts_size = file->block_nprocs * sizeof(unsigned int);
        file->array_disp_size   = file->block_nprocs * sizeof(unsigned int);
    }

    GPTLstop("PIO:initialize_adios2_for_block_merging");

    return PIO_NOERR;
}

static int initialize_adios2_variables(iosystem_desc_t *ios, file_desc_t *file)
{
    GPTLstart("PIO:initialize_adios2_variables");

    if (file->adios_io_process == 1)
    {
        adios2_error adiosErr = adios2_error_none;
        adiosErr = adios2_set_parameter(file->ioH, "CollectiveMetadata", "ON");
        if (adiosErr != adios2_error_none)
        {
            GPTLstop("PIO:initialize_adios2_variables");
            return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Setting (ADIOS) parameter (CollectiveMetadata=ON) failed (adios2_error=%s) for file (%s)",
                           convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
        }

        adiosErr = adios2_set_parameter(file->ioH, "StatsLevel", "0");
        if (adiosErr != adios2_error_none)
        {
            GPTLstop("PIO:initialize_adios2_variables");
            return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Setting (ADIOS) parameter (StatsLevel=0) failed (adios2_error=%s) for file (%s)",
                           convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
        }
    }

    /* Call adios end step in PIOc_setframe(), if num_step_calls>max_step_calls */
    file->max_step_calls = PIO_MAX_CACHED_STEPS_FOR_ADIOS;
    file->num_step_calls = 0;
    file->current_frame = -1;
    file->begin_step_called = 0;
    file->num_written_blocks = 0;

    memset(file->dim_names, 0, sizeof(file->dim_names));

    file->num_dim_vars = 0;
    file->num_vars = 0;
    file->num_attrs = 0;
    file->num_gattrs = 0;
    file->fillmode = NC_NOFILL;
    file->n_written_ioids = 0;

    /* Handle PIO_Offset size and corresponding adios type */
    file->pio_offset_size = (int) sizeof(PIO_Offset);
    if (file->pio_offset_size == 1)
    {
        file->pio_offset_type = adios2_type_int8_t;
    }
    else if (file->pio_offset_size == 2)
    {
        file->pio_offset_type = adios2_type_int16_t;
    }
    else if (file->pio_offset_size == 4)
    {
        file->pio_offset_type = adios2_type_int32_t;
    }
    else if (file->pio_offset_size == 8)
    {
        file->pio_offset_type = adios2_type_int64_t;
    }
    else
    {
        GPTLstop("PIO:initialize_adios2_variables");
        return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                       "PIO_Offet size (%d) is not supported for file (%s)",
                       file->pio_offset_size, pio_get_fname_from_file(file));
    }

    /* Set communicator for all adios processes, process rank, and I/O master node */
    file->write_decomp_id  = -1;
    file->write_frame_id   = -1;
    file->write_fillval_id = -1;
    if (ios->adios_io_process == 1)
    {
        /* Writers for specific variables in pio_write_darray */
        file->write_decomp_id  = file->num_adiostasks - 1;
        file->write_frame_id   = (file->write_decomp_id * 2) / 3;
        file->write_fillval_id = file->write_decomp_id / 3;
    }

    GPTLstop("PIO:initialize_adios2_variables");

    return PIO_NOERR;
}

adios2_type spio_get_adios_type(nc_type xtype)
{
    adios2_type t;
    switch (xtype)
    {
    case NC_BYTE:
        t = adios2_type_int8_t;
        break;
    case NC_CHAR:
        t = adios2_type_int8_t;
        break;
    case NC_SHORT:
        t = adios2_type_int16_t;
        break;
    case NC_INT:
        t = adios2_type_int32_t;
        break;
    case NC_FLOAT:
        t = adios2_type_float;
        break;
    case NC_DOUBLE:
        t = adios2_type_double;
        break;
    case NC_UBYTE:
        t = adios2_type_uint8_t;
        break;
    case NC_USHORT:
        t = adios2_type_uint16_t;
        break;
    case NC_UINT:
        t = adios2_type_uint32_t;
        break;
    case NC_INT64:
        t = adios2_type_int64_t;
        break;
    case NC_UINT64:
        t = adios2_type_uint64_t;
        break;
    case NC_STRING:
        t = adios2_type_string;
        break;
    default:
        t = adios2_type_int8_t;
        break;
    }

    return t;
}

const char *convert_adios2_error_to_string(adios2_error error)
{
    switch (error)
    {
        case adios2_error_none:
            return "adios2_error_none (success)";
        case adios2_error_invalid_argument:
            return "adios2_error_invalid_argument (user input error)";
        case adios2_error_system_error:
            return "adios2_error_system_error (low-level system error, e.g. system IO error)";
        case adios2_error_runtime_error:
            return "adios2_error_runtime_error (runtime errors other than system errors, e.g. memory overflow)";
        case adios2_error_exception:
            return "adios2_error_exception (any other error exception)";
        default:
            return "UNKNOWN adios2_error";
    }
}

int begin_adios2_step(file_desc_t *file, iosystem_desc_t *ios)
{
    if (file->adios_io_process == 1 && file->begin_step_called == 0)
    {
        adios2_step_status step_status;
        adios2_error adiosStepErr = adios2_begin_step(file->engineH, adios2_step_mode_append, 100.0, &step_status);
        if (adiosStepErr != adios2_error_none)
        {
            return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "adios2_begin_step failed (adios2_error=%s) for file (%s)",
                           convert_adios2_error_to_string(adiosStepErr), pio_get_fname_from_file(file));
        }
        file->begin_step_called = 1;
    }

    return PIO_NOERR;
}

int end_adios2_step(file_desc_t *file, iosystem_desc_t *ios)
{
    if (file->adios_io_process == 1 && file->begin_step_called == 1)
    {
        flush_adios2_tracking_data(file);
        adios2_error adiosStepErr = adios2_end_step(file->engineH);
        if (adiosStepErr != adios2_error_none)
        {
            return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "adios2_end_step failed (adios2_error=%s) for file (%s)",
                           convert_adios2_error_to_string(adiosStepErr), pio_get_fname_from_file(file));
        }
        file->begin_step_called = 0;
        file->num_step_calls = 0;
        file->num_written_blocks = 0;
    }

    return PIO_NOERR;
}

adios2_variable* spio_define_adios2_variable(iosystem_desc_t *ios, file_desc_t *file, adios2_io *io,
                                             const char *name, const adios2_type type, const size_t ndims,
                                             const size_t *shape, const size_t *start, const size_t *count,
                                             const adios2_constant_dims constant_dims)
{
    /* Define the variable in ADIOS2 */
    adios2_variable* variable = adios2_define_variable(io, name, type, ndims, shape, start, count, constant_dims);

#ifdef _SPIO_ADIOS_USE_COMPRESSION
    assert(ios != NULL && file != NULL);
    /* Skip compression for scalar variables (ndims == 0), see https://github.com/ornladios/ADIOS2/issues/4390 */
    if (variable != NULL && ndims > 0 && (file->iotype == PIO_IOTYPE_ADIOSC))
    {
        size_t operation_index = 0;
        adios2_error adiosErr = adios2_error_none;

        /* Use lossless compression method by default */
        int variable_compression_method = ios->adios_lossless_compression_method;

        #ifdef _SPIO_ADIOS_USE_LOSSY_COMPRESSION
        /* Switch to lossy compression method */
        variable_compression_method = ios->adios_lossy_compression_method;

        /* SZ or MGARD compressor only supports float or double types */
        if (variable_compression_method == ADIOS_COMPRESSION_METHOD_SZ || variable_compression_method == ADIOS_COMPRESSION_METHOD_MGARD)
        {
            /* Switch back to lossless compression method for non floating point type */
            if (type != adios2_type_float && type != adios2_type_double)
                variable_compression_method = ios->adios_lossless_compression_method;
        }
        /* ZFP compressor does not support 8-bit or 16-bit integer types */
        else if (variable_compression_method == ADIOS_COMPRESSION_METHOD_ZFP)
        {
            /* Switch back to lossless compression method for unsupported integer types */
            if (type == adios2_type_int8_t || type == adios2_type_uint8_t ||
                type == adios2_type_int16_t || type == adios2_type_uint16_t)
                variable_compression_method = ios->adios_lossless_compression_method;
        }
        #endif

        if (variable_compression_method == ADIOS_COMPRESSION_METHOD_BLOSC2)
        {
            if (ios->lossless_compression_operator != NULL)
            {
                /* Add a compression operation (Blosc2) to this variable */
                adiosErr = adios2_add_operation(&operation_index, variable, ios->lossless_compression_operator, "compressor", "zstd"); /* Compressor type (e.g., zstd) */
                if (adiosErr != adios2_error_none)
                {
                    pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                            "Failed to add Blosc2 compression operation (compressor=zstd) to variable %s (adios2_error=%s)",
                            name, convert_adios2_error_to_string(adiosErr));
                }

                /* Compression level is an integer between 0 (no compression) and 9 (more compression, more memory consumption) inclusive */
                adiosErr = adios2_set_operation_parameter(variable, operation_index, "clevel", "1");
                if (adiosErr != adios2_error_none)
                {
                    pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                            "Failed to set Blosc2 compression level (clevel=5) for variable %s (adios2_error=%s)",
                            name, convert_adios2_error_to_string(adiosErr));
                }

                /* Shuffle option (BLOSC_SHUFFLE/BLOSC_NOSHUFFLE/BLOSC_BITSHUFFLE) */
                adiosErr = adios2_set_operation_parameter(variable, operation_index, "doshuffle", "BLOSC_BITSHUFFLE");
                if (adiosErr != adios2_error_none)
                {
                    pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                            "Failed to set Blosc2 shuffle (doshuffle=BLOSC_BITSHUFFLE) for variable %s (adios2_error=%s)",
                            name, convert_adios2_error_to_string(adiosErr));
                }
            }
        }
        else if (variable_compression_method == ADIOS_COMPRESSION_METHOD_BZIP2)
        {
            if (ios->lossless_compression_operator != NULL)
            {
                /* Add a compression operation (BZip2) to this variable */
                adiosErr = adios2_add_operation(&operation_index, variable, ios->lossless_compression_operator, "", "");
                if (adiosErr != adios2_error_none)
                {
                    pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                            "Failed to add BZip2 compression operation to variable %s (adios2_error=%s)",
                            name, convert_adios2_error_to_string(adiosErr));
                }
            }
        }
        #ifdef _SPIO_ADIOS_USE_LOSSY_COMPRESSION
        else if (variable_compression_method == ADIOS_COMPRESSION_METHOD_MGARD)
        {
            if (ios->lossy_compression_operator != NULL)
            {
                /* Add a lossy compression operation (MGARD) to this variable */
                adiosErr = adios2_add_operation(&operation_index, variable, ios->lossy_compression_operator, "accuracy", "0.001");
                if (adiosErr != adios2_error_none)
                {
                    pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                            "Failed to add MGARD compression operation to variable %s (adios2_error=%s)",
                            name, convert_adios2_error_to_string(adiosErr));
                }
            }
        }
        else if (variable_compression_method == ADIOS_COMPRESSION_METHOD_SZ)
        {
            if (ios->lossy_compression_operator != NULL)
            {
                /* Add a lossy compression operation (SZ) to this variable */
                adiosErr = adios2_add_operation(&operation_index, variable, ios->lossy_compression_operator, "accuracy", "0.001");
                if (adiosErr != adios2_error_none)
                {
                    pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                            "Failed to add SZ compression operation to variable %s (adios2_error=%s)",
                            name, convert_adios2_error_to_string(adiosErr));
                }
            }
        }
        else if (variable_compression_method == ADIOS_COMPRESSION_METHOD_ZFP)
        {
            if (ios->lossy_compression_operator != NULL)
            {
                /* Add a lossy compression operation (ZFP) to this variable */
                adiosErr = adios2_add_operation(&operation_index, variable, ios->lossy_compression_operator, "accuracy", "0.001");
                if (adiosErr != adios2_error_none)
                {
                    pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                            "Failed to add ZFP compression operation to variable %s (adios2_error=%s)",
                            name, convert_adios2_error_to_string(adiosErr));
                }
            }
        }
        #endif
    }
#endif /* _SPIO_ADIOS_USE_COMPRESSION */

    return variable;
}
#endif /* _ADIOS2 */

/**
 * Return a string description of an error code.
 *
 * @param pioerr the error code returned by a PIO function call.
 * @param errmsg Pointer that will get the error message.
 * @return 0 on success.
 */
int PIOc_strerror_impl(int pioerr, char *errmsg, size_t errmsg_sz)
{
    LOG((1, "PIOc_strerror pioerr = %d", pioerr));

    /* Caller must provide this. */
    pioassert(errmsg, "pointer to errmsg string must be provided", __FILE__, __LINE__);

    /* System error, i.e., pioerr == errno */
    if (pioerr > 0)
    {
        const char *cp = (const char *)strerror(pioerr);
        if (cp)
        {
            strncpy(errmsg, cp, errmsg_sz);
        }
        else
        {
            snprintf(errmsg, errmsg_sz, "Unknown system error (err = %d)", pioerr);
        }
    }
    else if (pioerr == PIO_NOERR)
    {
        strncpy(errmsg, "No error", errmsg_sz);
    }
#if defined(_NETCDF)
    else if (pioerr <= NC2_ERR && pioerr >= NC4_LAST_ERROR)     /* NetCDF error */
    {
        strncpy(errmsg, nc_strerror(pioerr), errmsg_sz);
    }
#endif /* endif defined(_NETCDF) */
#if defined(_PNETCDF)
    else if (pioerr > PIO_FIRST_ERROR_CODE)     /* PnetCDF error */
    {
        strncpy(errmsg, ncmpi_strerror(pioerr), errmsg_sz);
    }
#endif /* defined( _PNETCDF) */
    else
    {
        /* Handle PIO errors. */
        switch(pioerr)
        {
        case PIO_EBADIOTYPE:
            strncpy(errmsg, "Bad IO type", errmsg_sz);
            break;
#ifdef _ADIOS2
        case PIO_EADIOSREAD:
            strncpy(errmsg, "ADIOS Read failed. ADIOS IO type does not support read operations", errmsg_sz);
            break;
        case PIO_EADIOS2ERR:
            strncpy(errmsg, "ADIOS2 API failed. Unknown error occured when calling an ADIOS2 API", errmsg_sz);
            break;
#endif
#ifdef _HDF5
        case PIO_EHDF5ERR:
            strncpy(errmsg, "HDF5 API failed. Unknown error occurred when calling an HDF5 API", errmsg_sz);
            break;
#endif
        default:
            snprintf(errmsg, errmsg_sz, "Unknown Error: Unrecognized error code (err = %d)", pioerr);
        }
    }

    return PIO_NOERR;
}

/**
 * Set the logging level if PIO was built with
 * PIO_ENABLE_LOGGING. Set to -1 for nothing, 0 for errors only, 1 for
 * important logging, and so on. Log levels below 1 are only printed
 * on the io/component root.
 *
 * A log file is also produced for each task. The file is called
 * pio_log_X.txt, where X is the (0-based) task number.
 *
 * If the library is not built with logging, this function does
 * nothing.
 *
 * @param level the logging level, 0 for errors only, 5 for max
 * verbosity.
 * @returns 0 on success, error code otherwise.
 */
int PIOc_set_log_level_impl(int level)
{

#if PIO_ENABLE_LOGGING
    /* Set the log level. */
    pio_log_level = level;

#ifdef NETCDF_C_LOGGING_ENABLED
    int ret;
    
    /* If netcdf logging is available turn it on starting at level = 4. */
    if (level > NC_LEVEL_DIFF)
        if ((ret = nc_set_log_level(level - NC_LEVEL_DIFF)))
        {
            return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                            "Setting NetCDF log level (level=%d) failed", level);
        }
#endif /* NETCDF_C_LOGGING_ENABLED */
#endif /* PIO_ENABLE_LOGGING */

    return PIO_NOERR;
}

/**
 * Initialize logging.  Open log file, if not opened yet, or increment
 * ref count if already open.
 */
void pio_init_logging(void)
{
#if PIO_ENABLE_LOGGING
    char log_filename[PIO_MAX_NAME];

    if (!LOG_FILE)
    {
        /* Create a filename with the rank in it. */
        MPI_Comm_rank(MPI_COMM_WORLD, &my_rank);
        sprintf(log_filename, "pio_log_%d.txt", my_rank);

        /* Open a file for this rank to log messages. */
        LOG_FILE = fopen(log_filename, "w");

        pio_log_ref_cnt = 1;
    }
    else
    {
        pio_log_ref_cnt++;
    }
#endif /* PIO_ENABLE_LOGGING */
}

/**
 * Finalize logging - close log files, if open.
 */
void pio_finalize_logging(void)
{
#if PIO_ENABLE_LOGGING
    pio_log_ref_cnt -= 1;
    if (LOG_FILE)
    {
        if (pio_log_ref_cnt == 0)
        {
            fclose(LOG_FILE);
            LOG_FILE = NULL;
        }
        else
            LOG((2, "pio_finalize_logging, postpone close, ref_cnt = %d",
                 pio_log_ref_cnt));
    }
#endif /* PIO_ENABLE_LOGGING */
}

/**
 * Initialize GPTL timer library, if needed
 * The library is only initialized if the timing is internal
 */
/* Indicate if GPTL was initialized inside the library */
static bool GPTL_was_init_in_lib = false;
bool spio_gptl_was_init_in_lib(void )
{
  return GPTL_was_init_in_lib;
}

void pio_init_gptl(void )
{
#ifdef TIMING_INTERNAL
    pio_timer_ref_cnt += 1;
    if((pio_timer_ref_cnt == 1) && (!GPTLis_initialized()))
    {
        GPTLinitialize();
        GPTL_was_init_in_lib = true;
    }
#endif
}

/**
 * Finalize GPTL timer library, if needed
 * The library is only finalized if the timing is internal
 */
void pio_finalize_gptl(void)
{
#ifdef TIMING_INTERNAL
    pio_timer_ref_cnt -= 1;
    if((pio_timer_ref_cnt == 0) && (spio_gptl_was_init_in_lib()))
    {
        GPTLfinalize();
    }
#endif
}

#if PIO_ENABLE_LOGGING
/**
 * This function prints out a message, if the severity of the message
 * is lower than the global pio_log_level. To use it, do something
 * like this:
 *
 * pio_log(0, "this computer will explode in %d seconds", i);
 *
 * After the first arg (the severity), use the rest like a normal
 * printf statement. Output will appear on stdout.
 * This function is heavily based on the function in section 15.5 of
 * the C FAQ.
 *
 * In code this functions should be wrapped in the LOG(()) macro.
 *
 * @param severity the severity of the message, 0 for error messages,
 * then increasing levels of verbosity.
 * @param fmt the format string.
 * @param ... the arguments used in format string.
 */
void pio_log(int severity, const char *fmt, ...)
{
    va_list argp;
    int t;
    int rem_len = MAX_LOG_MSG;
    char msg[MAX_LOG_MSG];
    char *ptr = msg;
    char rank_str[MAX_RANK_STR];

    /* If the severity is greater than the log level, we don't print
       this message. */
    if (severity > pio_log_level)
        return;

    /* If the severity is 0, only print on rank 0. */
    if (severity < 1 && my_rank != 0)
        return;

    /* If the severity is zero, this is an error. Otherwise insert that
       many tabs before the message. */
    if (!severity)
    {
        strncpy(ptr, ERROR_PREFIX, (rem_len > 0) ? rem_len : 0);
        ptr += strlen(ERROR_PREFIX);
        rem_len -= strlen(ERROR_PREFIX);
    }
    for (t = 0; t < severity; t++)
    {
        strncpy(ptr++, "\t", (rem_len > 0) ? rem_len : 0);
        rem_len--;
    }

    /* Show the rank. */
    snprintf(rank_str, MAX_RANK_STR, "%d ", my_rank);
    strncpy(ptr, rank_str, (rem_len > 0) ? rem_len : 0);
    ptr += strlen(rank_str);
    rem_len -= strlen(rank_str);

    /* Print out the variable list of args with vprintf. */
    va_start(argp, fmt);
    vsnprintf(ptr, ((rem_len > 0) ? rem_len : 0), fmt, argp);
    va_end(argp);

    /* Put on a final linefeed. */
    ptr = msg + strlen(msg);
    rem_len = MAX_LOG_MSG - strlen(msg);
    strncpy(ptr, "\n\0", (rem_len > 0) ? rem_len : 0);

    /* Add string delimiter to handle the case where the buffer
     * is completely filled up
     */
    msg[MAX_LOG_MSG - 1] = '\0';

    /* Send message to log file. */
    if (LOG_FILE)
    {
        fprintf(LOG_FILE, "%s", msg);
        fflush(LOG_FILE);
    }
    else
    {
        /* Send message to stdout. */
        fprintf(stdout, "%s", msg);
        /* Ensure an immediate flush of stdout. */
        fflush(stdout);
    }
}
#endif /* PIO_ENABLE_LOGGING */

/**
 * Obtain a backtrace and print it to stderr. This is appended to the
 * text decomposition file.
 *
 * Note from Jim:
 *
 * The stack trace can be used to identify the usage in
 * the model code of the particular decomposition in question and so
 * if using the pio performance tool leads to tuning that could be
 * applied in the model you know more or less where to do it.
 *
 * It's also useful if you have a model bug - then you have 20 or so
 * decomp files and you need to identify the one that was problematic.
 * So it's used as an add to the developer and not used at all by any
 * automated process or tools.
 *
 * @param fp file pointer to send output to
 */
void print_trace(FILE *fp)
{
    void *array[10];
    size_t size;
    char **strings;
    size_t i;

    /* Note that this won't actually work. */
    if (fp == NULL)
        fp = stderr;

    size = backtrace(array, 10);
    strings = backtrace_symbols(array, size);

    fprintf(fp,"Obtained %zd stack frames.\n", size);

    for (i = 0; i < size; i++)
        fprintf(fp,"%s\n", strings[i]);

    free(strings);
}

/**
 * Abort program and call MPI_Abort().
 *
 * @param msg an error message
 * @param fname name of code file where error occured
 * @param line the line of code where the error occurred.
 */
void piodie(const char *fname, int line, const char *fmt, ...)
{
    assert(fname && fmt);

    va_list argp;
    va_start(argp, fmt);
    fprintf(stderr, "PIO: FATAL ERROR: Aborting... ");
    vfprintf(stderr, fmt, argp);
    fprintf(stderr, " (%s: %d)\n", (fname) ? fname : "_", line);
    va_end(argp);

    print_trace(stderr);
#ifdef MPI_SERIAL
    abort();
#else
    MPI_Abort(MPI_COMM_WORLD, -1);
#endif
}

/**
 * Perform an assert. Note that this function does nothing if NDEBUG
 * is defined.
 *
 * @param expression the expression to be evaluated
 * @param msg an error message
 * @param fname name of code file where error occured
 * @param line the line of code where the error occurred.
 */
void pioassert(bool expression, const char *msg, const char *fname, int line)
{
#ifndef NDEBUG
    if (!expression)
    {
        fprintf(stderr, "Assertion failed...\n");
        piodie(fname, line, msg);
    }
#endif
}

/**
 * Handle MPI errors. An error message is sent to stderr, then the
 * check_netcdf() function is called with PIO_EIO. This version of the
 * function accepts an ios parameter, for the (rare) occasions where
 * we have an ios but not a file.
 * (Not a collective call)
 *
 * @param ios pointer to the iosystem_info_t. May be NULL.
 * @param file pointer to the file_desc_t info. May be NULL.
 * @param mpierr the MPI return code to handle
 * @param filename the name of the code file where error occured.
 * @param line the line of code where error occured.
 * @return PIO_NOERR for no error, otherwise PIO_EIO.
 */
int check_mpi(iosystem_desc_t *ios, file_desc_t *file, int mpierr,
               const char *filename, int line)
{
    if (mpierr)
    {
        char errstring[MPI_MAX_ERROR_STRING];
        int errstrlen;

        /* If we can get an error string from MPI, print it to stderr. */
        if (!MPI_Error_string(mpierr, errstring, &errstrlen))
            fprintf(stderr, "PIO: MPI ERROR: %s in file %s at line %d\n",
                    errstring, filename ? filename : "_", line);

        /* Handle all MPI errors as PIO_EIO. */
        return pio_err(ios, file, PIO_EIO, filename, line,
                        "An MPI error (mpierr=%d) occured", mpierr);
    }
    return PIO_NOERR;
}

/**
 * Check the result of a netCDF API call.
 * (Collective call for file/ios with error handler != PIO_RETURN_ERROR)
 *
 * PIO_INTERNAL_ERROR : Abort (inside PIO) on error from any MPI process
 * PIO_RETURN_ERROR : Return error back to the user (Allow the user to
 * handle the error. Each MPI process just returns the error code back
 * to the user)
 * PIO_BCAST_ERROR : Broadcast error code from I/O process with rank 0
 * (in the I/O communicator) to all processes.
 * PIO_REDUCE_ERROR : Reduce error codes across all processes (and log
 * the error codes from each process). This error handler detects error
 * in any process.
 *
 * @param ios pointer to the iosystem description struct. Ignored if NULL.
 * @param file pointer to the PIO structure describing this file. Ignored if NULL.
 * @param status the return value from the netCDF call.
 * @param fname the name of the code file.
 * @param line the line number of the netCDF call in the code.
 * @return the error code
 */
int check_netcdf(iosystem_desc_t *ios, file_desc_t *file, int status,
                  const char *fname, int line)
{
    int eh = default_error_handler; /* Error handler that will be used. */
    char errmsg[PIO_MAX_NAME + 1];  /* Error message. */
    int ioroot;
    MPI_Comm comm;
    int mpierr = MPI_SUCCESS;

    /* User must provide this. */
    assert(ios || file);
    assert(fname);

    LOG((1, "check_netcdf status = %d fname = %s line = %d", status, fname, line));

    /* Find the error handler. Error handlers associated with file has
     * priority over ios error handlers.
     */
    if(file){
        eh = file->iosystem->error_handler;
        ioroot = file->iosystem->ioroot;
        comm = file->iosystem->my_comm;
    }
    else{
        assert(ios);
        eh = ios->error_handler;
        ioroot = ios->ioroot;
        comm = ios->my_comm;
    }

    assert( (eh == PIO_INTERNAL_ERROR) ||
            (eh == PIO_BCAST_ERROR) ||
            (eh == PIO_RETURN_ERROR) ||
            (eh == PIO_REDUCE_ERROR) );
    LOG((2, "check_netcdf chose error handler = %d", eh));

    /* Get an error message. */
    if(status != PIO_NOERR){
        if(eh == PIO_INTERNAL_ERROR){
            int ret = PIOc_strerror_impl(status, errmsg, PIO_MAX_NAME);
            assert(ret == PIO_NOERR);
            LOG((1, "check_netcdf errmsg = %s", errmsg));
            piodie(fname, line, "FATAL ERROR: %s (file = %s)", errmsg, (file)?(file->fname):"UNKNOWN");
        }
    }

    if(eh == PIO_BCAST_ERROR){
        mpierr = MPI_Bcast(&status, 1, MPI_INT, ioroot, comm);
        if(mpierr != MPI_SUCCESS){
            return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
        }
    }
    else if(eh == PIO_REDUCE_ERROR){
        /* We assume that error codes are all negative */
        int lstatus = status;
        mpierr = MPI_Allreduce(&lstatus, &status, 1, MPI_INT, MPI_MIN, comm);
        if(mpierr != MPI_SUCCESS){
            return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
        }

        /* If we have a global error, get information on ranks with the
         * error
         */
        if(status != PIO_NOERR){
            int comm_sz, comm_rank;
            mpierr = MPI_Comm_rank(comm, &comm_rank);
            if(mpierr != MPI_SUCCESS){
                return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
            }
            mpierr = MPI_Comm_size(comm, &comm_sz);
            if(mpierr != MPI_SUCCESS){
                return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
            }
            /* Gather the error code to rank 0 */
            int *err_info = NULL;
            int err_info_sz = comm_sz;
            const int COMM_ROOT = 0;
            if(comm_rank == COMM_ROOT){
                err_info = (int *)malloc(err_info_sz * sizeof(int));
                if(!err_info){
                    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                    "Internal error while handling NetCDF error. Out of memory allocating %lld bytes gathering error codes", (unsigned long long) (err_info_sz * sizeof(int)));  
                }
            }
            mpierr = MPI_Gather(&lstatus, 1, MPI_INT, err_info,
                                1, MPI_INT, COMM_ROOT, comm);
            if(mpierr != MPI_SUCCESS){
                return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
            }
            /* Group in ranges of ranks with same error and log */
            if(comm_rank == COMM_ROOT){
                int prev_err = err_info[0];
                int start_rank = 0;
                int end_rank = 0;
                for(int i=1; i<err_info_sz; i++){
                    if(err_info[i] != prev_err){
                        /* Log prev range error and start new range */
                        int ret = PIOc_strerror_impl(prev_err, errmsg, PIO_MAX_NAME);
                        assert(ret == PIO_NOERR);
                        LOG((1, "ERROR: ranks[%d-%d] = %d (%s)",
                              start_rank, end_rank, prev_err, errmsg));
                        prev_err = err_info[i];
                        start_rank = i;
                    }
                    end_rank++;
                }
                /* The last range */
                if(err_info[end_rank] == prev_err){
                    int ret = PIOc_strerror_impl(prev_err, errmsg, PIO_MAX_NAME);
                    assert(ret == PIO_NOERR);
                    LOG((1, "ERROR: ranks[%d-%d] = %d (%s)",
                          start_rank, end_rank, prev_err, errmsg));
                }
                free(err_info);
            }
        }
    }

    /* For PIO_RETURN_ERROR, just return the error. */
    return status;
}

/**
 * Handle an error in PIO. This will consult the error handler
 * settings and either call MPI_Abort() or return an error code.
 * (Not a collective call)
 *
 * If the error handler is set to PIO_INTERNAL_ERROR an error
 * results in an internal abort. For all other error handlers
 * the function returns a PIO error code back to the caller.
 *
 * @param ios pointer to the IO system info. Ignored if NULL.
 * @param file pointer to the file description data. Ignored if
 * NULL. If provided file->iosystem is used as ios pointer.
 * @param err_num the error code
 * @param fname name of code file where error occured.
 * @param line the line of code where the error occurred.
 * @param uerr_msg_fmt User error message format
 * @param ... User error message arguments
 * @returns err_num if abort is not called.
 */
int pio_err(iosystem_desc_t *ios, file_desc_t *file,
            int err_num, const char *fname, int line,
            const char *uerr_msg_fmt, ...)
{
    char err_msg[PIO_MAX_NAME + 1];
    int err_handler = default_error_handler; /* Default error handler. */
    int ret;

    /* User must provide this. */
    pioassert(fname, "file name must be provided", __FILE__, __LINE__);

    /* No harm, no foul. */
    if (err_num == PIO_NOERR)
        return PIO_NOERR;

    /* Get the error message. */
    if ((ret = PIOc_strerror_impl(err_num, err_msg, PIO_MAX_NAME)))
        return ret;

    /* PIO_MAX_NAME may not be enough to store the entire user error
     * message, especially when long filenames are provided by the
     * user in the message
     */
    const int UERR_MSG_MAX_LEN = 8192;
    char uerr_msg[UERR_MSG_MAX_LEN + 1];
    va_list argp;
    va_start(argp, uerr_msg_fmt);
    vsnprintf(uerr_msg, UERR_MSG_MAX_LEN, uerr_msg_fmt, argp);
    va_end(argp);

    /* If logging is in use, log an error message. */
    LOG((1, "ERROR: %s. %s err_num = %d fname = %s line = %d", uerr_msg, err_msg, err_num, fname ? fname : "\0", line));

    /* What error handler should we use? */
    if (file)
    {
        ios = file->iosystem;
        err_handler = ios->error_handler;
    }
    else if (ios)
        err_handler = ios->error_handler;

    LOG((2, "pio_err chose error handler = %d", err_handler));

    /* Should we abort? */
    if (err_handler == PIO_INTERNAL_ERROR)
    {
        /* For debugging only, this will print a traceback of the call tree.  */
        piodie(fname, line, "An error occured, %s. %s (err=%d). Aborting since the error handler was set to PIO_INTERNAL_ERROR...", uerr_msg, err_msg, err_num);
    }
    else if (err_handler != PIO_RETURN_ERROR)
    {
        /* If the user does not explicitly ask to return error, print
         * the error message in stderr on the root IO proc
         */
        bool print_err_msg = (ios) ? (ios->union_rank == ios->ioroot) : true;

        if (print_err_msg)
        {
            fprintf(stderr, "PIO: ERROR: %s. %s (error num=%d), (%s:%d)\n", uerr_msg, err_msg, err_num, (fname) ? fname : "\0", line);
            fflush(stderr);
        }
    }

    /* For PIO_BCAST_ERROR and PIO_RETURN_ERROR error handlers
     * just return the error code back to the caller
     */
    return err_num;
}

/**
 * Handle an error in SCORPIO.
 * (Not a collective call)
 *
 * If the error handler is set to PIO_INTERNAL_ERROR an error
 * results in an internal abort. For all other error handlers
 * the function returns a PIO error code back to the caller.
 *
 * The error handler used for handling the error is
 * 1) The error handler associated with the file if @p ncid is valid
 * 2) If @p ncid is not valid, the error handler associated with
 *    the I/O system if @p iosysid is valid
 * 3) If the I/O system id (@p iosysid) and the file id (@p ncid)
 * are both invalid the default error handler (PIO_INTERNAL_ERROR)
 * is used to handle the error code
 *
 * @param iosysid The I/O system id associated with the error
 * @param ncid The file id of the file associated with the error
 * @param err_num The error code (corresponding to the error that
 *  needs to be handled)
 * @param fname The name of source code file where the error occured.
 * @param line The line number of source code where the error occurred.
 * @param uerr_msg The user error message
 * @returns The error code, PIO_NOERR on no error and an error
 * code otherwise
 */
int PIOc_error(int iosysid, int ncid,
            int err_num, const char *fname, int line,
            const char *uerr_msg)
{
  iosystem_desc_t *ios = NULL;
  file_desc_t *file = NULL;
  int ierr = PIO_NOERR;

  /* Note that calls to get the I/O system and
   * the file can (is allowed to) fail
   */
  ios = pio_get_iosystem_from_id(iosysid);
  ierr = pio_get_file(ncid, &file);

  return pio_err(ios, file, err_num, fname, line, "%s", uerr_msg);
}

/**
 * Handle a warning in SCORPIO.
 * (Not a collective call)
 *
 * @param iosysid The I/O system id associated with the warning
 * @param ncid The file id of the file associated with the warning
 * @param fname The name of source code file where the warning occured.
 * @param line The line number of source code where the warning occurred.
 * @param uwarn_msg The user warning message
 */
void PIOc_warn(int iosysid, int ncid,
                const char *fname, int line,
                const char *uwarn_msg)
{
  iosystem_desc_t *ios = NULL;
  file_desc_t *file = NULL;
  int ret;

  /* Note that calls to get the I/O system and
   * the file can (is allowed to) fail
   */
  ret = pio_get_file(ncid, &file);

  if(file){
    ios = file->iosystem;
  }
  else{
    ios = pio_get_iosystem_from_id(iosysid);
  }

  bool print_warn_msg = (ios) ? (ios->union_rank == ios->ioroot) : true;
  if(print_warn_msg){
    fprintf(stderr, "PIO: WARNING: %s, (%s:%d)\n", uwarn_msg, (fname) ? fname : "\0", line);
    fflush(stderr);
  }
}

/**
 * Allocate a region struct, and initialize it.
 *
 * @param ios pointer to the IO system info, used for error
 * handling. Ignored if NULL.
 * @param ndims the number of dimensions for the data in this region.
 * @param a pointer that gets a pointer to the newly allocated
 * io_region struct.
 * @returns 0 for success, error code otherwise.
 */
int alloc_region2(iosystem_desc_t *ios, int ndims, io_region **regionp)
{
    io_region *region;

    /* Check inputs. */
    pioassert(ndims >= 0 && regionp, "invalid input", __FILE__, __LINE__);
    LOG((1, "alloc_region2 ndims = %d sizeof(io_region) = %d", ndims,
         sizeof(io_region)));
    
    /* Allocate memory for the io_region struct. */
    if (!(region = (io_region *) calloc(1, sizeof(io_region))))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while allocating region. Out of memory allocating %lld bytes I/O region", (unsigned long long) sizeof(io_region));
    }

    /* Allocate memory for the array of start indicies. */
    if (!(region->start = (PIO_Offset *) calloc(ndims, sizeof(PIO_Offset))))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while allocating region. Out of memory allocating %lld bytes  for start array in the I/O region", (unsigned long long) (ndims * sizeof(PIO_Offset)));
    }

    /* Allocate memory for the array of counts. */
    if (!(region->count = (PIO_Offset *) calloc(ndims, sizeof(PIO_Offset))))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while allocating region. Out of memory allocating %lld bytes  for count array in the I/O region", (unsigned long long) (ndims * sizeof(PIO_Offset)));
    }

    /* Return pointer to new region to caller. */
    *regionp = region;
    
    return PIO_NOERR;
}

/**
 * Given a PIO type, find the MPI type and the type size.
 *
 * @param pio_type a PIO type, PIO_INT, PIO_FLOAT, etc.
 * @param mpi_type a pointer to MPI_Datatype that will get the MPI
 * type that coresponds to the PIO type. Ignored if NULL.
 * @param type_size a pointer to int that will get the size of the
 * type, in bytes. (For example, 4 for PIO_INT). Ignored if NULL.
 * @returns 0 for success, error code otherwise.
 */
int find_mpi_type(int pio_type, MPI_Datatype *mpi_type, int *type_size)
{
    MPI_Datatype my_mpi_type;
    int my_type_size;

    /* Decide on the base type. */
    switch(pio_type)
    {
    case PIO_BYTE:
        my_mpi_type = MPI_BYTE;
        my_type_size = NETCDF_CHAR_SIZE;
        break;
    case PIO_CHAR:
        my_mpi_type = MPI_CHAR;
        my_type_size = NETCDF_CHAR_SIZE;
        break;
    case PIO_SHORT:
        my_mpi_type = MPI_SHORT;
        my_type_size = NETCDF_SHORT_SIZE;
        break;
    case PIO_INT:
        my_mpi_type = MPI_INT;
        my_type_size = NETCDF_INT_FLOAT_SIZE;
        break;
    case PIO_FLOAT:
        my_mpi_type = MPI_FLOAT;
        my_type_size = NETCDF_INT_FLOAT_SIZE;
        break;
    case PIO_DOUBLE:
        my_mpi_type = MPI_DOUBLE;
        my_type_size = NETCDF_DOUBLE_INT64_SIZE;
        break;
#ifdef _NETCDF4
    case PIO_UBYTE:
        my_mpi_type = MPI_UNSIGNED_CHAR;
        my_type_size = NETCDF_CHAR_SIZE;
        break;
    case PIO_USHORT:
        my_mpi_type = MPI_UNSIGNED_SHORT;
        my_type_size = NETCDF_SHORT_SIZE;
        break;
    case PIO_UINT:
        my_mpi_type = MPI_UNSIGNED;
        my_type_size = NETCDF_INT_FLOAT_SIZE;
        break;
    case PIO_INT64:
        my_mpi_type = MPI_LONG_LONG;
        my_type_size = NETCDF_DOUBLE_INT64_SIZE;
        break;
    case PIO_UINT64:
        my_mpi_type = MPI_UNSIGNED_LONG_LONG;
        my_type_size = NETCDF_DOUBLE_INT64_SIZE;
        break;
    case PIO_STRING:
        my_mpi_type = MPI_CHAR;
        my_type_size = NETCDF_CHAR_SIZE;
        break;
#endif /* _NETCDF4 */
    default:
        return PIO_EBADTYPE;
    }

    /* If caller wants MPI type, set it. */
    if (mpi_type)
        *mpi_type = my_mpi_type;

    /* If caller wants type size, set it. */
    if (type_size)
        *type_size = my_type_size;

    return PIO_NOERR;
}

/**
 * Allocate space for an IO description struct, and initialize it.
 *
 * @param ios pointer to the IO system info, used for error
 * handling.
 * @param piotype the PIO data type (ex. PIO_FLOAT, PIO_INT, etc.).
 * @param ndims the number of dimensions.
 * @param iodesc pointer that gets the newly allocated io_desc_t.
 * @returns 0 for success, error code otherwise.
 */
int malloc_iodesc(iosystem_desc_t *ios, int piotype, int ndims,
                  io_desc_t **iodesc)
{
    MPI_Datatype mpi_type;
    PIO_Offset type_size;
    int mpierr = MPI_SUCCESS;
    int ret;

    /* Check input. */
    pioassert(ios && piotype > 0 && ndims >= 0 && iodesc,
              "invalid input", __FILE__, __LINE__);

    LOG((1, "malloc_iodesc piotype = %d ndims = %d", piotype, ndims));

    /* Get the MPI type corresponding with the PIO type. */
    if ((ret = find_mpi_type(piotype, &mpi_type, NULL)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Internal error while allocating memory for iodesc. Unable to find MPI type corresponding to PIO type (%d)", piotype);
    }

    /* What is the size of the pio type? */
    if ((ret = spio_pnetcdf_inq_type(0, piotype, NULL, &type_size)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Internal error while allocating memory for iodesc. Finding the size of PIO type (%d) failed", piotype);
    }

    /* Allocate space for the io_desc_t struct. */
    if (!(*iodesc = (io_desc_t *) calloc(1, sizeof(io_desc_t))))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Internal error while allocating memory for iodesc. Out of memory allocating %lld bytes for the I/O descriptor", (unsigned long long) sizeof(io_desc_t));
    }

    /* Remember the pio type and its size. */
    (*iodesc)->piotype = piotype;
    (*iodesc)->piotype_size = type_size;

    /* Remember the MPI type. */
    (*iodesc)->mpitype = mpi_type;

    /* Get the size of the type. */
    if ((mpierr = MPI_Type_size((*iodesc)->mpitype, &(*iodesc)->mpitype_size)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);

    /* Initialize some values in the struct. */
    (*iodesc)->maxregions = 1;
    (*iodesc)->ioid = -1;
    (*iodesc)->ndims = ndims;

    /* Allocate space for, and initialize, the first region. */
    if ((ret = alloc_region2(ios, ndims, &((*iodesc)->firstregion))))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Internal error while allocating memory for iodesc. Allocating memory for 1st region failed. Out of memory allocating memory for I/O region in the I/O descriptor");
    }

    /* Set the swap memory settings to defaults for this IO system. */
    (*iodesc)->rearr_opts = ios->rearr_opts;

#if PIO_SAVE_DECOMPS
    /* The descriptor is not yet saved to disk */
    (*iodesc)->is_saved = false;
#endif

    return PIO_NOERR;
}

/**
 * Free a region list.
 *
 * top a pointer to the start of the list to free.
 */
void free_region_list(io_region *top)
{
    io_region *ptr, *tptr;

    ptr = top;
    while (ptr)
    {
        if (ptr->start)
            free(ptr->start);
        if (ptr->count)
            free(ptr->count);
        tptr = ptr;
        ptr = ptr->next;
        free(tptr);
    }
}

/**
 * Free a decomposition map.
 *
 * @param iosysid the IO system ID.
 * @param ioid the ID of the decomposition map to free.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_freedecomp_impl(int iosysid, int ioid)
{
    iosystem_desc_t *ios;
    io_desc_t *iodesc;
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function calls. */
    int ret = 0;

    GPTLstart("PIO:PIOc_freedecomp");
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        GPTLstop("PIO:PIOc_freedecomp");
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Freeing PIO decomposition failed. Invalid iosystem id (%d) provided", iosysid);
    }
    assert(ios);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
    {
        GPTLstop("PIO:PIOc_freedecomp");
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Freeing PIO decomposition failed. Invalid io decomposition id (%d) provided", ioid);
    }

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_FREEDECOMP; /* Message for async notification. */

        PIO_SEND_ASYNC_MSG(ios, msg, &ret, iosysid, ioid);
        if(ret != PIO_NOERR)
        {
            GPTLstop("PIO:PIOc_freedecomp");
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                            "Freeing PIO decomposition failed (iosysid = %d, iodesc id=%d). Error sending asynchronous message, PIO_MSG_FREEDECOMP, on iosystem", iosysid, ioid);
        }
    }

    /* Free the map. */
    free(iodesc->map);

    /* Free the dimlens. */
    free(iodesc->dimlen);

    if (iodesc->rfrom)
        free(iodesc->rfrom);

    if (iodesc->rtype)
    {
        for (int i = 0; i < iodesc->nrecvs; i++)
            if (iodesc->rtype[i] != MPI_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(&iodesc->rtype[i])))
                {
                    GPTLstop("PIO:PIOc_freedecomp");
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
                }

        free(iodesc->rtype);
    }

    if (iodesc->stype)
    {
        for (int i = 0; i < iodesc->num_stypes; i++)
            if (iodesc->stype[i] != MPI_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(iodesc->stype + i)))
                {
                    GPTLstop("PIO:PIOc_freedecomp");
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
                }

        iodesc->num_stypes = 0;
        free(iodesc->stype);
    }

    if (iodesc->scount)
        free(iodesc->scount);

    if (iodesc->rcount)
        free(iodesc->rcount);

    if (iodesc->sindex)
        free(iodesc->sindex);

    if (iodesc->rindex)
        free(iodesc->rindex);

    if (iodesc->firstregion)
        free_region_list(iodesc->firstregion);

    if (iodesc->fillregion)
        free_region_list(iodesc->fillregion);

    if (iodesc->rearranger == PIO_REARR_SUBSET)
        if ((mpierr = MPI_Comm_free(&iodesc->subset_comm)))
        {
            GPTLstop("PIO:PIOc_freedecomp");
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
        }

    ret = pio_delete_iodesc_from_list(ioid);
    if (ret != PIO_NOERR)
    {
        GPTLstop("PIO:PIOc_freedecomp");
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Freeing PIO decomposition failed (iosysid = %d, ioid=%d). Error while trying to delete I/O descriptor from internal list", iosysid, ioid); 
    }
    GPTLstop("PIO:PIOc_freedecomp");
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);

    return ret;
}

/**
 * Read a decomposition map from a file. The decomp file is only read
 * by task 0 in the communicator.
 *
 * @param file the filename
 * @param ndims pointer to an int with the number of dims.
 * @param gdims pointer to an array of dimension ids.
 * @param fmaplen
 * @param map
 * @param comm
 * @returns 0 for success, error code otherwise.
 */
int PIOc_readmap_impl(const char *file, int *ndims, int **gdims, PIO_Offset *fmaplen,
                 PIO_Offset **map, MPI_Comm comm)
{
    int npes, myrank;
    int rnpes, rversno;
    char rversstr[PIO_MAX_NAME], rnpesstr[PIO_MAX_NAME], rndimsstr[PIO_MAX_NAME];
    int j;
    int *tdims;
    PIO_Offset *tmap;
    MPI_Status status;
    PIO_Offset maplen;
    int mpierr = MPI_SUCCESS; /* Return code for MPI calls. */

    /* Check inputs. */
    if (!file || !ndims || !gdims || !fmaplen || !map)
    {
        return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__,
                        "Reading I/O decomposition failed. Invalid arguments provided, file is %s (expected not NULL), ndims is %s (expected not NULL), gdims is %s (expected not NULL), fmaplen is %s (expected not NULL), map is %s (expected not NULL)", PIO_IS_NULL(file), PIO_IS_NULL(ndims), PIO_IS_NULL(gdims), PIO_IS_NULL(fmaplen), PIO_IS_NULL(map));
    }

    if ((mpierr = MPI_Comm_size(comm, &npes)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    if ((mpierr = MPI_Comm_rank(comm, &myrank)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

    if (myrank == 0)
    {
        FILE *fp = fopen(file, "r");
        if (!fp)
        {
            pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__,
                    "Reading I/O decomposition from file (%s) failed. Opening the file failed", file);
        }

        fscanf(fp,"%s%d%s%d%s%d\n",rversstr, &rversno, rnpesstr, &rnpes, rndimsstr, ndims);

        if (rversno != VERSNO)
        {
            return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__,
                            "Reading I/O decomposition from file (%s) failed. Version mismatch. The version read from file, %d, does not match the expected/supported version (%d)", file, rversno, VERSNO);
        }

        if (rnpes < 1 || rnpes > npes)
        {
            return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__,
                            "Reading I/O decomposition from file (%s) failed. Corrupt/Invalid entries in file. Number of PEs = %d (expected >= 1 && <= npes, %d)", file, rnpes, npes);
        }

        if ((mpierr = MPI_Bcast(&rnpes, 1, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(ndims, 1, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if (!(tdims = (int *) calloc(*ndims, sizeof(int))))
        {
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Reading I/O decomposition from file (%s) failed. Out of memory allocating %lld bytes for temp buffer to store dimension ids", file, (unsigned long long) ((*ndims) * sizeof(int)));
        }
        for (int i = 0; i < *ndims; i++)
            fscanf(fp,"%d ", tdims + i);

        if ((mpierr = MPI_Bcast(tdims, *ndims, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

        for (int i = 0; i < rnpes; i++)
        {
            fscanf(fp, "%d %lld", &j, &maplen);
            if (j != i)  // Not sure how this could be possible
            {
                return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__,
                                "Reading I/O decomposition from file (%s) failed. Corrupt/invalid entries in file. Expected decomposition info of process %d but read decomposition info of process %d instead", file, i, j);
            }
            if (!(tmap = (PIO_Offset *) malloc(maplen * sizeof(PIO_Offset))))
            {
                return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                                "Reading I/O decomposition from file (%s) failed. Out of memory allocating %lld bytes for storing I/O decomposition map", file, (unsigned long long) (maplen * sizeof(PIO_Offset)));
            }
            for (j = 0; j < maplen; j++)
                fscanf(fp, "%lld ", tmap+j);

            if (i > 0)
            {
                if ((mpierr = MPI_Send(&maplen, 1, PIO_OFFSET, i, i + npes, comm)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                if ((mpierr = MPI_Send(tmap, maplen, PIO_OFFSET, i, i, comm)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                free(tmap);
            }
            else
            {
                *map = tmap;
                *fmaplen = maplen;
            }
        }
        fclose(fp);
    }
    else
    {
        if ((mpierr = MPI_Bcast(&rnpes, 1, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(ndims, 1, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if (!(tdims = (int *) calloc(*ndims, sizeof(int))))
        {
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Reading I/O decomposition from file (%s) failed. Out of memory allocating %lld bytes for temp buffer to store dimension ids", file, (unsigned long long)((*ndims) * sizeof(int)));
        }
        if ((mpierr = MPI_Bcast(tdims, *ndims, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

        if (myrank < rnpes)
        {
            if ((mpierr = MPI_Recv(&maplen, 1, PIO_OFFSET, 0, myrank + npes, comm, &status)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            if (!(tmap = (PIO_Offset *) malloc(maplen * sizeof(PIO_Offset))))
            {
                return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                                "Reading I/O decomposition from file (%s) failed. Out of memory allocating %lld bytes to store I/O decomposition map", file, (unsigned long long) (maplen * sizeof(PIO_Offset)));
            }
            if ((mpierr = MPI_Recv(tmap, maplen, PIO_OFFSET, 0, myrank, comm, &status)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            *map = tmap;
        }
        else
        {
            tmap = NULL;
            maplen = 0;
        }
        *fmaplen = maplen;
    }
    *gdims = tdims;
    return PIO_NOERR;
}

/**
 * Read a decomposition map from file.
 *
 * @param file the filename
 * @param ndims pointer to the number of dimensions
 * @param gdims pointer to an array of dimension ids
 * @param maplen pointer to the length of the map
 * @param map pointer to the map array
 * @param f90_comm
 * @returns 0 for success, error code otherwise.
 */
int PIOc_readmap_from_f90_impl(const char *file, int *ndims, int **gdims, PIO_Offset *maplen,
                          PIO_Offset **map, int f90_comm)
{
    return PIOc_readmap_impl(file, ndims, gdims, maplen, map, MPI_Comm_f2c(f90_comm));
}

/**
 * Write the decomposition map to a file using netCDF, everyones
 * favorite data format.
 *
 * @param iosysid the IO system ID.
 * @param filename the filename to be used.
 * @param cmode for PIOc_create(). Will be bitwise or'd with NC_WRITE.
 * @param ioid the ID of the IO description.
 * @param title optial title attribute for the file. Must be less than
 * PIO_MAX_NAME + 1 if provided. Ignored if NULL.
 * @param history optial history attribute for the file. Must be less
 * than PIO_MAX_NAME + 1 if provided. Ignored if NULL.
 * @param fortran_order set to non-zero if fortran array ordering is
 * used, or to zero if C array ordering is used.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_write_nc_decomp_impl(int iosysid, const char *filename, int cmode, int ioid,
                         const char *title, const char *history, int fortran_order)
{
    iosystem_desc_t *ios; /* IO system info. */
    io_desc_t *iodesc;    /* Decomposition info. */
    int max_maplen;       /* The maximum maplen used for any task. */
    int mpierr = MPI_SUCCESS;
    int ret;

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Writing I/O decomposition (%d) to NetCDF file(%s) failed. Invalid io system id (%d) provided", ioid, (filename) ? filename : "UNKNOWN", iosysid);
    }

    /* Check inputs. */
    if (!filename)
    {
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                        "Writing I/O decomposition (%d) to NetCDF file failed. Invalid filename (NULL) provided", ioid);
    }
    if (title)
        if (strlen(title) > PIO_MAX_NAME)
        {
            return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                            "Writing I/O decomposition (%d) to NetCDF file(%s) failed. The title (%s) exceeds the maximum length (%d) allowed", ioid, filename, title, (int ) PIO_MAX_NAME);
        }
    if (history)
        if (strlen(history) > PIO_MAX_NAME)
        {
            return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                            "Writing I/O decomposition (%d) to NetCDF file(%s) failed. The history (%s) exceeds the maximum length (%d) allowed", ioid, filename, history, (int ) PIO_MAX_NAME);
        }

    LOG((1, "PIOc_write_nc_decomp filename = %s iosysid = %d ioid = %d "
         "ios->num_comptasks = %d", filename, iosysid, ioid, ios->num_comptasks));

    /* Get the IO desc, which describes the decomposition. */
    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
    {
        return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file(%s) failed. Invalid I/O decomposition id (%d) provided", filename, ioid);
    }

    /* Allocate memory for array which will contain the length of the
     * map on each task, for all computation tasks. */
    int task_maplen[ios->num_comptasks];
    LOG((3, "ios->num_comptasks = %d", ios->num_comptasks));

    /* Gather maplens from all computation tasks and fill the
     * task_maplen array on all tasks. */
    if ((mpierr = MPI_Allgather(&iodesc->maplen, 1, MPI_INT, task_maplen, 1, MPI_INT,
                                ios->comp_comm)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);

    /* Find the max maxplen. */
    if ((mpierr = MPI_Allreduce(&iodesc->maplen, &max_maplen, 1, MPI_INT, MPI_MAX,
                                ios->comp_comm)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    LOG((3, "max_maplen = %d", max_maplen));

    /* 2D array that will hold all the map information for all
     * tasks. */
    int full_map[ios->num_comptasks][max_maplen];

    /* Fill local array with my map. Use the fill value for unused */
    /* elements at the end if max_maplen is longer than maplen. Also
     * subtract 1 because the iodesc->map is 1-based. */
    int my_map[max_maplen];
    for (int e = 0; e < max_maplen; e++)
    {
        my_map[e] = e < iodesc->maplen ? iodesc->map[e] - 1 : NC_FILL_INT;
        LOG((3, "my_map[%d] = %d", e, my_map[e]));
    }
    
    /* Gather my_map from all computation tasks and fill the full_map array. */
    if ((mpierr = MPI_Allgather(&my_map, max_maplen, MPI_INT, full_map, max_maplen,
                                MPI_INT, ios->comp_comm)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    
    for (int p = 0; p < ios->num_comptasks; p++)
        for (int e = 0; e < max_maplen; e++)
            LOG((3, "full_map[%d][%d] = %d", p, e, full_map[p][e]));

    /* Write the netCDF decomp file. */
    if ((ret = spio_write_nc_decomp_int(ios, filename, cmode, iodesc->ndims, iodesc->dimlen,
                                        ios->num_comptasks, task_maplen, (int *)full_map, title,
                                        history, fortran_order)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition (%d) to NetCDF file(%s) failed. Internal error", ioid, filename);
    }

    return PIO_NOERR;
}

/**
 * Read the decomposition map from a netCDF decomp file produced by
 * PIOc_write_nc_decomp().
 *
 * @param iosysid the IO system ID.
 * @param filename the name of the decomp file.
 * @param ioid pointer that will get the newly-assigned ID of the IO
 * description. The ioid is needed to later free the decomposition.
 * @param comm an MPI communicator.
 * @param pio_type the PIO type to be used as the type for the data.
 * @param title pointer that will get optial title attribute for the
 * file. Will be less than PIO_MAX_NAME + 1 if provided. Ignored if
 * NULL.
 * @param history pointer that will get optial history attribute for
 * the file. Will be less than PIO_MAX_NAME + 1 if provided. Ignored if
 * NULL.
 * @param fortran_order pointer that gets set to 1 if fortran array
 * ordering is used, or to zero if C array ordering is used.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_read_nc_decomp_impl(int iosysid, const char *filename, int *ioidp, MPI_Comm comm,
                        int pio_type, char *title, char *history, int *fortran_order)
{
    iosystem_desc_t *ios; /* Pointer to the IO system info. */
    int ndims;            /* The number of data dims (except unlim). */
    int max_maplen;       /* The max maplen of any task. */
    int *global_dimlen;   /* An array with sizes of global dimensions. */
    int *task_maplen;     /* A map of one tasks mapping to global data. */
    int *full_map;        /* A map with the task maps of every task. */
    int num_tasks_decomp; /* The number of tasks for this decomp. */
    int size;             /* Size of comm. */
    int my_rank;          /* Task rank in comm. */
    char source_in[PIO_MAX_NAME + 1];  /* Text metadata in decomp file. */
    char version_in[PIO_MAX_NAME + 1]; /* Text metadata in decomp file. */
    int mpierr = MPI_SUCCESS;
    int ret;

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Reading I/O decompsition from NetCDF file (%s) failed. Invalid io system id (%d) provided", (filename) ? filename : "UNKNOWN", iosysid);
    }

    /* Check inputs. */
    if (!filename || !ioidp)
    {
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file failed. Invalid arguments provided, filename=%s (expected not UNKNOWN), ioidp is %s (expected not NULL)", (filename) ? filename : "UNKNOWN", PIO_IS_NULL(ioidp));
    }

    LOG((1, "PIOc_read_nc_decomp filename = %s iosysid = %d pio_type = %d",
         filename, iosysid, pio_type));

    /* Get the communicator size and task rank. */
    if ((mpierr = MPI_Comm_size(comm, &size)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    if ((mpierr = MPI_Comm_rank(comm, &my_rank)))
        return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);
    LOG((2, "size = %d my_rank = %d", size, my_rank));

    /* Read the file. This allocates three arrays that we have to
     * free. */
    if ((ret = spio_read_nc_decomp_int(iosysid, filename, &ndims, &global_dimlen, &num_tasks_decomp,
                                       &task_maplen, &max_maplen, &full_map, title, history,
                                       source_in, version_in, fortran_order)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Internal error reading the file", filename);
    }
    LOG((2, "ndims = %d num_tasks_decomp = %d max_maplen = %d", ndims, num_tasks_decomp,
         max_maplen));

    /* If the size does not match the number of tasks in the decomp,
     * that's an error. */
    if (size != num_tasks_decomp)
        ret = PIO_EINVAL;

    /* Now initialize the iodesc on each task for this decomposition. */
    if (!ret)
    {
        PIO_Offset compmap[task_maplen[my_rank]];

        /* Copy array into PIO_Offset array. Make it 1 based. */
        for (int e = 0; e < task_maplen[my_rank]; e++)
            compmap[e] = full_map[my_rank * max_maplen + e] + 1;

        /* Initialize the decomposition. */
        ret = PIOc_InitDecomp_impl(iosysid, pio_type, ndims, global_dimlen, task_maplen[my_rank],
                              compmap, ioidp, NULL, NULL, NULL);
        if (ret != PIO_NOERR)
        {
            return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                            "Reading I/O decomposition from NetCDF file (%s) failed. Error while initializing the I/O decomposition", filename);
        }
    }

    /* Free resources. */
    free(global_dimlen);
    free(task_maplen);
    free(full_map);

    return ret;
}

/* Write the decomp information in netCDF. This is an internal
 * function.
 *
 * @param ios pointer to io system info.
 * @param filename the name the decomp file will have.
 * @param cmode for PIOc_create(). Will be bitwise or'd with NC_WRITE.
 * @param ndims number of dims in the data being described.
 * @param global_dimlen an array, of size ndims, with the size of the
 * global array in each dimension.
 * @param num_tasks the number of tasks the data are decomposed over.
 * @param task_maplen array of size num_tasks with the length of the
 * map for each task.
 * @param map pointer to a 2D array of size [num_tasks][max_maplen]
 * with the 0-based mapping from local to global array elements.
 * @param title null-terminated string that will be written as an
 * attribute. If provided, length must be < PIO_MAX_NAME + 1. Ignored
 * if NULL.
 * @param history null-terminated string that will be written as an
 * attribute. If provided, length must be < PIO_MAX_NAME + 1. Ignored
 * if NULL.
 * @param fortran_order set to non-zero if using fortran array
 * ordering, 0 for C array ordering.
 * @returns 0 for success, error code otherwise.
 */
int spio_write_nc_decomp_int(iosystem_desc_t *ios, const char *filename, int cmode, int ndims,
                             const int *global_dimlen, int num_tasks, const int *task_maplen, const int *map,
                             const char *title, const char *history, int fortran_order)
{
    int max_maplen = 0;
    int ncid;
    int ret;

    /* Check inputs. */
    pioassert(ios && filename && global_dimlen && task_maplen &&
              (!title || strlen(title) <= PIO_MAX_NAME) &&
              (!history || strlen(history) <= PIO_MAX_NAME), "invalid input",
              __FILE__, __LINE__);

    LOG((2, "spio_write_nc_decomp_int filename = %s ndims = %d num_tasks = %d", filename,
         ndims, num_tasks));

    /* Find the maximum maplen. */
    for (int t = 0; t < num_tasks; t++)
        if (task_maplen[t] > max_maplen)
            max_maplen = task_maplen[t];
    LOG((3, "max_maplen = %d", max_maplen));

    /* Create the netCDF decomp file. */
    if ((ret = PIOc_create_impl(ios->iosysid, filename, cmode | NC_WRITE, &ncid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Create the file failed", filename);
    }

    /* Write an attribute with the version of this file. */
    char version[PIO_MAX_NAME + 1];
    sprintf(version, "%d.%d.%d", PIO_VERSION_MAJOR, PIO_VERSION_MINOR, PIO_VERSION_PATCH);
    if ((ret = PIOc_put_att_text_impl(ncid, NC_GLOBAL, DECOMP_VERSION_ATT_NAME,
                                 strlen(version) + 1, version)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to put an attribute (%s) with the PIO Version info", filename, DECOMP_VERSION_ATT_NAME);
    }

    /* Write an attribute with the max map len. */
    if ((ret = PIOc_put_att_int_impl(ncid, NC_GLOBAL, DECOMP_MAX_MAPLEN_ATT_NAME,
                                PIO_INT, 1, &max_maplen)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to put an attribute (%s) with the max map length (%d)", filename, DECOMP_MAX_MAPLEN_ATT_NAME, max_maplen);
    }

    /* Write title attribute, if the user provided one. */
    if (title)
        if ((ret = PIOc_put_att_text_impl(ncid, NC_GLOBAL, DECOMP_TITLE_ATT_NAME,
                                     strlen(title) + 1, title)))
        {
            return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                            "Writing I/O decomposition to NetCDF file (%s) failed. Unable to put an attribute (%s) for the title (%s)", filename, DECOMP_TITLE_ATT_NAME, title);
        }

    /* Write history attribute, if the user provided one. */
    if (history)
        if ((ret = PIOc_put_att_text_impl(ncid, NC_GLOBAL, DECOMP_HISTORY_ATT_NAME,
                                     strlen(history) + 1, history)))
        {
            return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                            "Writing I/O decomposition to NetCDF file (%s) failed. Unable to put an attribute (%s) for the history (%s)", filename, DECOMP_HISTORY_ATT_NAME, history);
        }

    /* Write a source attribute. */
    char source[] = "Decomposition file produced by PIO library.";
    if ((ret = PIOc_put_att_text_impl(ncid, NC_GLOBAL, DECOMP_SOURCE_ATT_NAME,
                                 strlen(source) + 1, source)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to put an attribute (%s) with source information (%s)", filename, DECOMP_SOURCE_ATT_NAME, source);
    }

    /* Write an attribute with array ordering (C or Fortran). */
    char c_order_str[] = DECOMP_C_ORDER_STR;
    char fortran_order_str[] = DECOMP_FORTRAN_ORDER_STR;
    char *my_order_str = fortran_order ? fortran_order_str : c_order_str;
    if ((ret = PIOc_put_att_text_impl(ncid, NC_GLOBAL, DECOMP_ORDER_ATT_NAME,
                                 strlen(my_order_str) + 1, my_order_str)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to put an attribute (%s) for the array ordering (%s)", filename, DECOMP_ORDER_ATT_NAME, my_order_str);
    }

    /* Write an attribute with the stack trace. This can be helpful
     * for debugging. */
    #define MAX_BACKTRACE 10
    void *bt[MAX_BACKTRACE];
    size_t bt_size;
    char **bt_strings;
    bt_size = backtrace(bt, MAX_BACKTRACE);
    bt_strings = backtrace_symbols(bt, bt_size);

    /* Find the max size. */
    int max_bt_size = 0;
    for (int b = 0; b < bt_size; b++)
        if (strlen(bt_strings[b]) > max_bt_size)
            max_bt_size = strlen(bt_strings[b]);
    if (max_bt_size > PIO_MAX_NAME)
        max_bt_size = PIO_MAX_NAME;

    /* Copy the backtrace into one long string. */
    char full_bt[max_bt_size * bt_size + bt_size + 1];
    full_bt[0] = '\0';
    for (int b = 0; b < bt_size; b++)
    {
        strncat(full_bt, bt_strings[b], max_bt_size);
        strcat(full_bt, "\n");
    }
    free(bt_strings);
    printf("full_bt = %s", full_bt);

    /* Write the stack trace as an attribute. */
    if ((ret = PIOc_put_att_text_impl(ncid, NC_GLOBAL, DECOMP_BACKTRACE_ATT_NAME,
                                 strlen(full_bt) + 1, full_bt)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to put an attribute (%s) with the back trace of the call (%s)", filename, DECOMP_BACKTRACE_ATT_NAME, full_bt);
    }

    /* We need a dimension for the dimensions in the data. (Example:
     * for 4D data we will need to store 4 dimension IDs.) */
    int dim_dimid;
    if ((ret = PIOc_def_dim_impl(ncid, DECOMP_DIM_DIM, ndims, &dim_dimid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to define dim (%s) for the array representing dims in data", filename, DECOMP_DIM_DIM);
    }

    /* We need a dimension for tasks. If we have 4 tasks, we need to
     * store an array of length 4 with the size of the local array on
     * each task. */
    int task_dimid;
    if ((ret = PIOc_def_dim_impl(ncid, DECOMP_TASK_DIM_NAME, num_tasks, &task_dimid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to define dim (%s) for the array representing local size of array/data on each process", filename, DECOMP_TASK_DIM_NAME);
    }

    /* We need a dimension for the map. It's length may vary, we will
     * use the max_maplen for the dimension size. */
    int mapelem_dimid;
    if ((ret = PIOc_def_dim_impl(ncid, DECOMP_MAPELEM_DIM_NAME, max_maplen,
                            &mapelem_dimid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to define dim (%s) for the dimension of the map", filename, DECOMP_MAPELEM_DIM_NAME);
    }

    /* Define a var to hold the global size of the array for each
     * dimension. */
    int gsize_varid;
    if ((ret = PIOc_def_var_impl(ncid, DECOMP_GLOBAL_SIZE_VAR_NAME, NC_INT, 1,
                            &dim_dimid, &gsize_varid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to define variable (%s) to store the global dimensions", filename, DECOMP_GLOBAL_SIZE_VAR_NAME);
    }

    /* Define a var to hold the length of the local array on each task. */
    int maplen_varid;
    if ((ret = PIOc_def_var_impl(ncid, DECOMP_MAPLEN_VAR_NAME, NC_INT, 1, &task_dimid,
                            &maplen_varid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to define variable (%s) to store the local size of the array on each task", filename, DECOMP_MAPLEN_VAR_NAME);
    }

    /* Define a 2D var to hold the length of the local array on each task. */
    int map_varid;
    int map_dimids[2] = {task_dimid, mapelem_dimid};
    if ((ret = PIOc_def_var_impl(ncid, DECOMP_MAP_VAR_NAME, NC_INT, 2, map_dimids,
                            &map_varid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to define variable (%s) to store the I/O decomposition map", filename, DECOMP_MAP_VAR_NAME);
    }

    /* End define mode, to write data. */
    if ((ret = PIOc_enddef_impl(ncid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Ending the define mode failed", filename);
    }

    /* Write the global dimension sizes. */
    if ((PIOc_put_var_int_impl(ncid, gsize_varid, global_dimlen)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to write variable (%s) that contains the global dimensions", filename, DECOMP_GLOBAL_SIZE_VAR_NAME);
    }

    /* Write the size of the local array on each task. */
    if ((PIOc_put_var_int_impl(ncid, maplen_varid, task_maplen)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to write variable (%s) that contains the local size of the array on each task", filename, DECOMP_MAPLEN_VAR_NAME);
    }

    /* Write the map. */
    if ((PIOc_put_var_int_impl(ncid, map_varid, map)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Unable to write variable (%s) that contains the I/O decomposition map", filename, DECOMP_MAP_VAR_NAME);
    }

    /* Close the netCDF decomp file. */
    if ((ret = PIOc_closefile_impl(ncid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Writing I/O decomposition to NetCDF file (%s) failed. Closing the file failed", filename);
    }

    return PIO_NOERR;
}

/* Read the decomp information from a netCDF decomp file. This is an
 * internal function.
 *
 * @param iosysid the IO system ID.
 * @param filename the name the decomp file will have.
 * @param ndims pointer to int that will get number of dims in the
 * data being described. Ignored if NULL.
 * @param global_dimlen a pointer that gets an array, of size ndims,
 * that will have the size of the global array in each
 * dimension. Ignored if NULL, otherwise must be freed by caller.
 * @param num_tasks pointer to int that gets the number of tasks the
 * data are decomposed over. Ignored if NULL.
 * @param task_maplen pointer that gets array of size num_tasks that
 * gets the length of the map for each task. Ignored if NULL,
 * otherwise must be freed by caller.
 * @param max_maplen pointer to int that gets the maximum maplen for
 * any task. Ignored if NULL.
 * @param map pointer that gets a 2D array of size [num_tasks][max_maplen]
 * that will have the 0-based mapping from local to global array
 * elements. Ignored if NULL, otherwise must be freed by caller.
 * @param title pointer that will get the contents of title attribute,
 * if present. If present, title will be < PIO_MAX_NAME + 1 in
 * length. Ignored if NULL.
 * @param history pointer that will get the contents of history attribute,
 * if present. If present, history will be < PIO_MAX_NAME + 1 in
 * length. Ignored if NULL.
 * @param source pointer that will get the contents of source
 * attribute. Source will be < PIO_MAX_NAME + 1 in length. Ignored if
 * NULL.
 * @param version pointer that will get the contents of version
 * attribute. It will be < PIO_MAX_NAME + 1 in length. Ignored if
 * NULL.
 * @param fortran_order int pointer that will get a 0 if this
 * decomposition file uses C array ordering, 1 if it uses Fortran
 * array ordering.
 * @returns 0 for success, error code otherwise.
 */
int spio_read_nc_decomp_int(int iosysid, const char *filename, int *ndims, int **global_dimlen,
                            int *num_tasks, int **task_maplen, int *max_maplen, int **map, char *title,
                            char *history, char *source, char *version, int *fortran_order)
{
    iosystem_desc_t *ios;
    int ncid;
    int ret;

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Invalid iosystem id (%d) provided", (filename) ? filename : "UNKNOWN", iosysid);
    }

    /* Check inputs. */
    if (!filename)
    {
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file failed. Invalid filename (NULL) provided");
    }

    LOG((1, "spio_read_nc_decomp_int iosysid = %d filename = %s", iosysid, filename));

    /* Open the netCDF decomp file. */
    if ((ret = PIOc_open_impl(iosysid, filename, NC_WRITE, &ncid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error opening the file", filename);
    }

    /* Read version attribute. */
    char version_in[PIO_MAX_NAME + 1];
    if ((ret = PIOc_get_att_text_impl(ncid, NC_GLOBAL, DECOMP_VERSION_ATT_NAME, version_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading attribute (%s) containing version information", filename, DECOMP_VERSION_ATT_NAME);
    }
    LOG((3, "version_in = %s", version_in));
    if (version)
        strncpy(version, version_in, PIO_MAX_NAME + 1);

    /* Read order attribute. */
    char order_in[PIO_MAX_NAME + 1];
    if ((ret = PIOc_get_att_text_impl(ncid, NC_GLOBAL, DECOMP_ORDER_ATT_NAME, order_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading attribute (%s) containing order information", filename, DECOMP_ORDER_ATT_NAME);
    }
    LOG((3, "order_in = %s", order_in));
    if (fortran_order)
    {
        if (!strncmp(order_in, DECOMP_C_ORDER_STR, PIO_MAX_NAME + 1))
            *fortran_order = 0;
        else if (!strncmp(order_in, DECOMP_FORTRAN_ORDER_STR, PIO_MAX_NAME + 1))
            *fortran_order = 1;
        else
        {
            return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                            "Reading I/O decomposition from NetCDF file (%s) failed. Invalid/corrupt entry (%s) in attribute (%s) containing order information", filename, order_in, DECOMP_ORDER_ATT_NAME);
        }
    }

    /* Read attribute with the max map len. */
    int max_maplen_in;
    if ((ret = PIOc_get_att_int_impl(ncid, NC_GLOBAL, DECOMP_MAX_MAPLEN_ATT_NAME, &max_maplen_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading attribute (%s) containing max map lengths", filename, DECOMP_MAX_MAPLEN_ATT_NAME);
    }
    LOG((3, "max_maplen_in = %d", max_maplen_in));
    if (max_maplen)
        *max_maplen = max_maplen_in;

    /* Read title attribute, if it is in the file. */
    char title_in[PIO_MAX_NAME + 1];
    ret = PIOc_get_att_text_impl(ncid, NC_GLOBAL, DECOMP_TITLE_ATT_NAME, title_in);
    if (ret == PIO_NOERR)
    {
        /* If the caller wants it, copy the title for them. */
        if (title)
            strncpy(title, title_in, PIO_MAX_NAME + 1);
    }
    else if (ret == PIO_ENOTATT)
    {
        /* No title attribute. */
        if (title)
            title[0] = '\0';
    }
    else
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading attribute (%s) containing the title", filename, DECOMP_TITLE_ATT_NAME);
    }

    /* Read history attribute, if it is in the file. */
    char history_in[PIO_MAX_NAME + 1];
    ret = PIOc_get_att_text_impl(ncid, NC_GLOBAL, DECOMP_HISTORY_ATT_NAME, history_in);
    if (ret == PIO_NOERR)
    {
        /* If the caller wants it, copy the history for them. */
        if (history)
            strncpy(history, history_in, PIO_MAX_NAME + 1);
    }
    else if (ret == PIO_ENOTATT)
    {
        /* No history attribute. */
        if (history)
            history[0] = '\0';
    }
    else
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading attribute (%s) containing the history information", filename, DECOMP_HISTORY_ATT_NAME);
    }

    /* Read source attribute. */
    char source_in[PIO_MAX_NAME + 1];
    if ((ret = PIOc_get_att_text_impl(ncid, NC_GLOBAL, DECOMP_SOURCE_ATT_NAME, source_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading attribute (%s) containing the source information", filename, DECOMP_SOURCE_ATT_NAME);
    }
    if (source)
        strncpy(source, source_in, PIO_MAX_NAME + 1);

    /* Read dimension for the dimensions in the data. (Example: for 4D
     * data we will need to store 4 dimension IDs.) */
    int dim_dimid;
    PIO_Offset ndims_in;
    if ((ret = PIOc_inq_dimid_impl(ncid, DECOMP_DIM_DIM, &dim_dimid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error inquiring id for attribute (%s) containing dims ", filename, DECOMP_DIM_DIM);
    }
    if ((ret = PIOc_inq_dim_impl(ncid, dim_dimid, NULL, &ndims_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading attribute (%s) containing dims ", filename, DECOMP_DIM_DIM);
    }
    if (ndims)
        *ndims = ndims_in;

    /* Read the global sizes of the array. */
    int gsize_varid;
    int global_dimlen_in[ndims_in];
    if ((ret = PIOc_inq_varid_impl(ncid, DECOMP_GLOBAL_SIZE_VAR_NAME, &gsize_varid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error inquiring id for variable (%s) containing global dimensions", filename, DECOMP_GLOBAL_SIZE_VAR_NAME);
    }
    if ((ret = PIOc_get_var_int_impl(ncid, gsize_varid, global_dimlen_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading variable (%s) containing global dimensions", filename, DECOMP_GLOBAL_SIZE_VAR_NAME);
    }
    if (global_dimlen)
    {
        if (!(*global_dimlen = (int *) malloc(ndims_in * sizeof(int))))
        {
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Reading I/O decomposition from NetCDF file (%s) failed. Out of memory allocating %lld bytes for global dimension length array", filename, (unsigned long long) (ndims_in * sizeof(int)));
        }
        for (int d = 0; d < ndims_in; d++)
            (*global_dimlen)[d] = global_dimlen_in[d];
    }

    /* Read dimension for tasks. If we have 4 tasks, we need to store
     * an array of length 4 with the size of the local array on each
     * task. */
    int task_dimid;
    PIO_Offset num_tasks_in;
    if ((ret = PIOc_inq_dimid_impl(ncid, DECOMP_TASK_DIM_NAME, &task_dimid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error inquiring id for dimension (%s) for tasks", filename, DECOMP_TASK_DIM_NAME);
    }
    if ((ret = PIOc_inq_dim_impl(ncid, task_dimid, NULL, &num_tasks_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error inquiring dimension (%s) for tasks", filename, DECOMP_TASK_DIM_NAME);
    }
    if (num_tasks)
        *num_tasks = num_tasks_in;

    /* Read the length if the local array on each task. */
    int maplen_varid;
    int task_maplen_in[num_tasks_in];
    if ((ret = PIOc_inq_varid_impl(ncid, DECOMP_MAPLEN_VAR_NAME, &maplen_varid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error inquiring id for variable (%s) containing local size of array", filename, DECOMP_MAPLEN_VAR_NAME);
    }
    if ((ret = PIOc_get_var_int_impl(ncid, maplen_varid, task_maplen_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading variable (%s) containing local size of array", filename, DECOMP_MAPLEN_VAR_NAME);
    }
    if (task_maplen)
    {
        if (!(*task_maplen = (int *) malloc(num_tasks_in * sizeof(int))))
        {
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Reading I/O decomposition from NetCDF file (%s) failed. Out of memory allocating %lld bytes to store length of I/O decomposition maps for each task", filename, (unsigned long long) (num_tasks_in * sizeof(int)));
        }
        for (int t = 0; t < num_tasks_in; t++)
            (*task_maplen)[t] = task_maplen_in[t];
    }

    /* Read the map. */
    int map_varid;
    int map_in[num_tasks_in][max_maplen_in];
    if ((ret = PIOc_inq_varid_impl(ncid, DECOMP_MAP_VAR_NAME, &map_varid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error inquiring id for variable (%s) containing the map", filename, DECOMP_MAP_VAR_NAME);
    }
    if ((ret = PIOc_get_var_int_impl(ncid, map_varid, (int *)map_in)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Error reading variable (%s) containing the decomposition map", filename, DECOMP_MAP_VAR_NAME);
    }
    if (map)
    {
        if (!(*map = (int *) malloc(num_tasks_in * max_maplen_in * sizeof(int))))
        {
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Reading I/O decomposition from NetCDF file (%s) failed. Out of memory allocating %lld bytes for the I/O decomposition map", filename, (unsigned long long) (num_tasks_in * max_maplen_in * sizeof(int)));
        }
        for (int t = 0; t < num_tasks_in; t++)
            for (int l = 0; l < max_maplen_in; l++)
                (*map)[t * max_maplen_in + l] = map_in[t][l];
    }

    /* Close the netCDF decomp file. */
    if ((ret = PIOc_closefile_impl(ncid)))
    {
        return pio_err(ios, NULL, ret, __FILE__, __LINE__,
                        "Reading I/O decomposition from NetCDF file (%s) failed. Closing the file failed", filename);
    }

    return PIO_NOERR;
}

/**
 * Write the decomposition map to a file.
 *
 * @param file the filename to be used.
 * @param iosysid the IO system ID.
 * @param ioid the ID of the IO description.
 * @param comm an MPI communicator.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_write_decomp_impl(const char *file, int iosysid, int ioid, MPI_Comm comm)
{
    iosystem_desc_t *ios;
    io_desc_t *iodesc;

    LOG((1, "PIOc_write_decomp file = %s iosysid = %d ioid = %d", file, iosysid, ioid));

    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Write I/O decomposition to file (%s) failed. Invalid iosystem id (%d) provided", (file) ? file : "UNKNOWN", iosysid);
    }

    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
    {
        return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Write I/O decomposition to file (%s) failed. Invalid io descriptor id (%d) provided (iosysid=%d)", (file) ? file : "UNKNOWN", ioid, iosysid);
    }

    return PIOc_writemap_impl(file, iodesc->ioid, iodesc->ndims, iodesc->dimlen, iodesc->maplen, iodesc->map,
                         comm);
}

/**
 * Write the decomposition map to a file.
 *
 * @param file the filename
 * @param ioid id of the decomposition
 * @param ndims the number of dimensions
 * @param gdims an array of dimension ids
 * @param maplen the length of the map
 * @param map the map array
 * @param comm an MPI communicator.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_writemap_impl(const char *file, int ioid, int ndims, const int *gdims, PIO_Offset maplen,
                  const PIO_Offset *map, MPI_Comm comm)
{
    int npes, myrank;
    PIO_Offset *nmaplen = NULL;
    MPI_Status status;
    int i;
    PIO_Offset *nmap;
    int gdims_reversed[ndims];
    int mpierr = MPI_SUCCESS; /* Return code for MPI calls. */

    LOG((1, "PIOc_writemap file = %s ioid = %d ndims = %d maplen = %d", file, ioid, ndims, maplen));

    if (!file || !gdims || !map)
    {
        return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__,
                        "Writing I/O decomposition to file failed. Invalid arguments, file is %s (expected not NULL), gdims is %s (expected not NULL), map is %s (expected not NULL)", PIO_IS_NULL(file), PIO_IS_NULL(gdims), PIO_IS_NULL(map));
    }

    if ((mpierr = MPI_Comm_size(comm, &npes)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    if ((mpierr = MPI_Comm_rank(comm, &myrank)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    LOG((2, "npes = %d myrank = %d", npes, myrank));

    /* Allocate memory for the nmaplen. */
    if (myrank == 0)
        if (!(nmaplen = (PIO_Offset *) malloc(npes * sizeof(PIO_Offset))))
        {
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Writing I/O decomposition to file (%s) failed. Out of memory allocating %lld bytes for array of map lengths", file, (unsigned long long) (npes * sizeof(PIO_Offset)));
        }

    if ((mpierr = MPI_Gather(&maplen, 1, PIO_OFFSET, nmaplen, 1, PIO_OFFSET, 0, comm)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

    /* Only rank 0 writes the file. */
    if (myrank == 0)
    {
        FILE *fp;

        /* Open the file to write. */
        if (!(fp = fopen(file, "w")))
        {
            return pio_err(NULL, NULL, PIO_EIO, __FILE__, __LINE__,
                            "Writing I/O decomposition to file (%s) failed. Error opening the file", file);
        }

        /* Write the version and dimension info. */
        fprintf(fp,"version %d npes %d ndims %d \n", VERSNO, npes, ndims);
        if(fortran_order)
        {
            for(int i=0; i<ndims; i++)
            {
                gdims_reversed[i] = gdims[ndims - 1 - i];
            }
            gdims = gdims_reversed;
        }
        for (i = 0; i < ndims; i++)
            fprintf(fp, "%d ", gdims[i]);
        fprintf(fp, "\n");

        /* Write the map. */
        fprintf(fp, "0 %lld\n", nmaplen[0]);
        for (i = 0; i < nmaplen[0]; i++)
            fprintf(fp, "%lld ", map[i]);
        fprintf(fp,"\n");

        for (i = 1; i < npes; i++)
        {
            LOG((2, "creating nmap for i = %d", i));
            nmap = (PIO_Offset *)malloc(nmaplen[i] * sizeof(PIO_Offset));
            if (!nmap)
            {
                return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                                "Writing I/O decomposition to file (%s) failed. Out of memory allocating %lld bytes for I/O decomposition map (map length = %lld) for process %d", file, (unsigned long long) (nmaplen[i] * sizeof(PIO_Offset)), (unsigned long long) nmaplen[i], i);
            }

            if ((mpierr = MPI_Send(&i, 1, MPI_INT, i, npes + i, comm)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            if ((mpierr = MPI_Recv(nmap, nmaplen[i], PIO_OFFSET, i, i, comm, &status)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            LOG((2,"MPI_Recv map complete"));

            fprintf(fp, "%d %lld\n", i, nmaplen[i]);
            for (int j = 0; j < nmaplen[i]; j++)
                fprintf(fp, "%lld ", nmap[j]);
            fprintf(fp, "\n");

            free(nmap);
        }
        /* Free memory for the nmaplen. */
        free(nmaplen);
        fprintf(fp, "\n");
        print_trace(fp);

        /* Print the decomposition id */
        fprintf(fp, "ioid\t%d\n", ioid);
        /* Close the file. */
        fclose(fp);
        LOG((2,"decomp file closed."));
    }
    else
    {
        LOG((2,"ready to MPI_Recv..."));
        if ((mpierr = MPI_Recv(&i, 1, MPI_INT, 0, npes+myrank, comm, &status)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        LOG((2,"MPI_Recv got %d", i));
        if ((mpierr = MPI_Send(map, maplen, PIO_OFFSET, 0, myrank, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        LOG((2,"MPI_Send map complete"));
    }

    return PIO_NOERR;
}

/**
 * Write the decomposition map to a file for F90.
 *
 * @param file the filename
 * @param ndims the number of dimensions
 * @param gdims an array of dimension ids
 * @param maplen the length of the map
 * @param map the map array
 * @param comm an MPI communicator.
 * @returns 0 for success, error code otherwise.
 */
int PIOc_writemap_from_f90_impl(const char *file, int ioid, int ndims, const int *gdims,
                           PIO_Offset maplen, const PIO_Offset *map, int f90_comm)
{
    return PIOc_writemap_impl(file, ioid, ndims, gdims, maplen, (PIO_Offset *)map,
                         MPI_Comm_f2c(f90_comm));
}

int PIO_get_avail_iotypes(char *buf, size_t sz)
{
    int ret = PIO_NOERR;
    size_t max_sz = sz;
    char *cbuf = buf;

    assert(buf && (sz > 0));
#ifdef _NETCDF
    snprintf(cbuf, sz, "%s (%d)", pio_iotype_to_string(PIO_IOTYPE_NETCDF), PIO_IOTYPE_NETCDF);
    sz = max_sz - strlen(buf);
    cbuf = buf + strlen(buf);
#endif /* _NETCDF */

#ifdef _NETCDF4
    assert(sz > 0);
    snprintf(cbuf, sz, ", %s (%d), %s (%d)",
              pio_iotype_to_string(PIO_IOTYPE_NETCDF4C), PIO_IOTYPE_NETCDF4C,
              pio_iotype_to_string(PIO_IOTYPE_NETCDF4P), PIO_IOTYPE_NETCDF4P);
    sz = max_sz - strlen(buf);
    cbuf = buf + strlen(buf);
#ifdef PIO_USE_NETCDF4_NCZARR
    assert(sz > 0);
    snprintf(cbuf, sz, ", %s (%d)",
              pio_iotype_to_string(PIO_IOTYPE_NETCDF4P_NCZARR), PIO_IOTYPE_NETCDF4P_NCZARR);
    sz = max_sz - strlen(buf);
    cbuf = buf + strlen(buf);
#endif
#endif /* _NETCDF4 */

#ifdef _PNETCDF
    assert(sz > 0);
    snprintf(cbuf, sz, ", %s (%d)", pio_iotype_to_string(PIO_IOTYPE_PNETCDF), PIO_IOTYPE_PNETCDF);
    sz = max_sz - strlen(buf);
    cbuf = buf + strlen(buf);
#endif /* _PNETCDF */

#ifdef _ADIOS2
    assert(sz > 0);
    snprintf(cbuf, sz, ", %s (%d), %s (%d)",
              pio_iotype_to_string(PIO_IOTYPE_ADIOS), PIO_IOTYPE_ADIOS,
              pio_iotype_to_string(PIO_IOTYPE_ADIOSC), PIO_IOTYPE_ADIOSC);
    sz = max_sz - strlen(buf);
    cbuf = buf + strlen(buf);
#endif

    return ret;
}

/**
 * Create a new file using pio. This is an internal function that is
 * called by both PIOc_create() and PIOc_createfile(). Input
 * parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid A defined pio system ID, obtained from
 * PIOc_InitIntercomm() or PIOc_InitAsync().
 * @param ncidp A pointer that gets the ncid of the newly created
 * file.
 * @param iotype A pointer to a pio output format. Must be one of
 * PIO_IOTYPE_PNETCDF, PIO_IOTYPE_NETCDF, PIO_IOTYPE_NETCDF4C,
 * PIO_IOTYPE_NETCDF4P or PIO_IOTYPE_NETCDF4P_NCZARR.
 * @param filename The filename to create.
 * @param mode The netcdf mode for the create operation.
 * @returns 0 for success, error code otherwise.
 * @ingroup PIO_createfile
 */
int spio_createfile_int(int iosysid, int *ncidp, const int *iotype, const char *filename,
                        int mode)
{
    char tname[SPIO_TIMER_MAX_NAME];
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int mpierr = MPI_SUCCESS;  /* Return code from MPI function codes. */
    int ierr = PIO_NOERR;              /* Return code from function calls. */

    /* Get the IO system info from the iosysid. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Creating file (%s) failed. Invalid iosystem id (%d) provided", (filename) ? filename : "UNKNOWN", iosysid);
    }

    /* User must provide valid input for these parameters. */
    if (!ncidp || !iotype || !filename || strlen(filename) > PIO_MAX_NAME)
    {
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                        "Creating file (%s) failed. Invalid arguments provided, ncidp is %s (expected not NULL), iotype is %s (expected not NULL), filename is %s (expected not NULL), filename length = %lld (expected <= %d)", (filename) ? filename : "UNKNOWN", PIO_IS_NULL(ncidp), PIO_IS_NULL(iotype), PIO_IS_NULL(filename), (filename) ? ((unsigned long long )strlen(filename)) : 0, (int )PIO_MAX_NAME);
    }

    /* A valid iotype must be specified. */
    if (!iotype_is_valid(*iotype))
    {
        char avail_iotypes[PIO_MAX_NAME + 1];
        PIO_get_avail_iotypes(avail_iotypes, PIO_MAX_NAME);
        return pio_err(ios, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__,
                        "Creating file (%s) failed. Invalid iotype (%s:%d) specified. Available iotypes are : %s", filename, pio_iotype_to_string(*iotype), *iotype, avail_iotypes);
    }

    LOG((1, "PIOc_createfile iosysid = %d iotype = %d filename = %s mode = %d",
         iosysid, *iotype, filename, mode));

    /* Allocate space for the file info. */
    if (!(file = (file_desc_t *) calloc(sizeof(file_desc_t), 1)))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Creating file (%s) failed. Out of memory allocating %lld bytes for the file descriptor", filename, (unsigned long long) (sizeof(file_desc_t)));
    }

    file->io_fstats = (spio_io_fstats_summary_t *) calloc(sizeof(spio_io_fstats_summary_t), 1);
    if(!(file->io_fstats))
    {
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Creating file (%s) failed. Out of memory allocating %lld bytes for caching file I/O statistics", filename, (unsigned long long) (sizeof(spio_io_fstats_summary_t)));
    }

    /* Fill in some file values. */
    file->fh = -1;
    file->reserve_extra_header_space = true; /* Set to true for creating output NetCDF files only. */
    file->is_reopened = false;
    strncpy(file->fname, filename, PIO_MAX_NAME);
    ierr = pio_create_uniq_str(ios, NULL, tname, SPIO_TIMER_MAX_NAME, "tmp_", "_file");
    if(ierr != PIO_NOERR)
    {
        /* Not a fatal error */
        LOG((0, "Creating a unique name for the write timer for file (%s, ncid=%d) failed, ret = %d", file->fname, file->pio_ncid, ierr));
        tname[0] = '\0';
    }

    snprintf(file->io_fstats->wr_timer_name, SPIO_TIMER_MAX_NAME, "PIO:wr_%s", tname);
    snprintf(file->io_fstats->rd_timer_name, SPIO_TIMER_MAX_NAME, "PIO:rd_%s", tname);
    snprintf(file->io_fstats->tot_timer_name, SPIO_TIMER_MAX_NAME, "PIO:tot_%s", tname);

    spio_ltimer_start(file->io_fstats->wr_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);

    file->iosystem = ios;
    file->iotype = *iotype;
    file->buffer.ioid = -1;
#ifdef _HDF5
    file->hdf5_num_dims = 0;
    file->hdf5_num_vars = 0;
    file->hdf5_num_attrs = 0;
    file->hdf5_num_gattrs = 0;
#endif
    /*
    file->num_unlim_dimids = 0;
    file->unlim_dimids = NULL;
    */
    for (int i = 0; i < PIO_MAX_VARS; i++)
    {
        file->varlist[i].varid = -1;
        file->varlist[i].vname[0] = '\0';
        file->varlist[i].vdesc[0] = '\0';
        file->varlist[i].rec_var = 0;
        file->varlist[i].record = -1;
        file->varlist[i].request = NULL;
        file->varlist[i].request_sz = NULL;
        file->varlist[i].nreqs = 0;
        file->varlist[i].fillvalue = NULL;
        file->varlist[i].pio_type = PIO_NAT;
        file->varlist[i].type_size = 0;
        file->varlist[i].vrsize = 0;
        file->varlist[i].rb_pend = 0;
        file->varlist[i].vrsize = 0;
        file->varlist[i].wb_pend = 0;
        file->varlist[i].use_fill = 0;
        file->varlist[i].fillbuf = NULL;
    }
    file->mode = mode;

    /* Set to true if this task should participate in IO (only true for
     * one task with netcdf serial files. */
    if (file->iotype == PIO_IOTYPE_NETCDF4P || file->iotype == PIO_IOTYPE_NETCDF4P_NCZARR ||
        file->iotype == PIO_IOTYPE_PNETCDF ||
        ios->io_rank == 0)
        file->do_io = 1;

    LOG((2, "file->do_io = %d ios->async = %d", file->do_io, ios->async));

    /* If async is in use, and this is not an IO task, bcast the
     * parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_CREATE_FILE;
        size_t len = strlen(filename) + 1;

        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, len, filename, file->iotype, file->mode);
        if(ierr != PIO_NOERR)
        {
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Creating file (%s) failed. Error sending asynchronous message, PIO_MSG_CREATE_FILE, to create the file on iosystem (iosysid=%d)", filename, ios->iosysid);
        }
    }

#ifdef _ADIOS2
    /* Append ".bp" to output filename for the corresponding ADIOS BP filename */
    static const char adios_bp_filename_extn[] = ".bp";
    size_t adios_bp_filename_len = strlen(filename) + sizeof(adios_bp_filename_extn) + 1;
    if (ios->ioproc)
    {
        if (!(file->mode & PIO_NOCLOBBER))
        {
            /* If PIO_NOCLOBBER is not set, i.e., CLOBBER mode, delete any ADIOS BP files with the same name */
            if (ios->io_rank == 0)
            {
                char *adios_bp_filename = (char *) calloc(adios_bp_filename_len, sizeof(char));
                if (adios_bp_filename == NULL)
                {
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                                   "Allocating memory for adios filename (%s) failed. Out of memory allocating %lld bytes for the file name",
                                   adios_bp_filename, (unsigned long long) (adios_bp_filename_len));
                }
                snprintf(adios_bp_filename, adios_bp_filename_len, "%s%s", filename, adios_bp_filename_extn);

                struct stat fsd, adios_bp_fsd;
                if (0 == stat(filename, &fsd))
                {
                    if(0 == stat(adios_bp_filename, &adios_bp_fsd))
                    {
                        /* Check if foo.nc points to foo.nc.bp */
                        if(S_ISDIR(fsd.st_mode) && (fsd.st_dev == adios_bp_fsd.st_dev)
                            && (fsd.st_ino == adios_bp_fsd.st_ino))
                        {
                            /* Delete the BP file */
                            spio_remove_directory(adios_bp_filename);
                        }
                    }

                    /* Delete file foo.nc (could be a symbolic link to foo.nc.bp) */
                    unlink(filename);
                }

                free(adios_bp_filename);
            }
        }
    }

    /* ADIOS: assume all procs are also IO tasks */
    /* FIXME: Frequently due to application error different MPI processes have different
     * file mode flags, so although ideally we need this barrier inside if(!(file->mode & PIO_NOCLOBBER))
     * block above adding it here to avoid a potential hang */
    /* Make sure that no task is trying to operate on the ADIOS BP file
     * while it is being deleted */
    if ((mpierr = MPI_Barrier(ios->union_comm)))
    {
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
    }

    if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
    {
#ifdef SPIO_NO_CXX_REGEX
        if (ios->io_rank == 0)
        {
            printf("PIO: WARNING: C++11 regex support is not available; skipping override of ADIOS with PnetCDF for creating file %s\n", filename);
        }
#else /* SPIO_NO_CXX_REGEX */
        std::regex rgx(SPIO_OVERRIDE_ADIOS_WITH_PNETCDF_FNAME_REGEX);
        if (std::regex_match(filename, rgx))
        {
#ifdef _PNETCDF
            if (ios->io_rank == 0)
            {
                printf("PIO: WARNING: Creating file %s with PnetCDF instead of ADIOS (matched regex: %s)\n",
                       filename, SPIO_OVERRIDE_ADIOS_WITH_PNETCDF_FNAME_REGEX);
            }

            file->iotype = PIO_IOTYPE_PNETCDF;
#else
            if (ios->io_rank == 0)
            {
                printf("PIO: WARNING: PnetCDF is not available; skipping override of ADIOS with PnetCDF for creating file %s (matched regex: %s)\n",
                       filename, SPIO_OVERRIDE_ADIOS_WITH_PNETCDF_FNAME_REGEX);
            }
#endif
        }
#endif /* SPIO_NO_CXX_REGEX */
    }

    if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
    {
        LOG((2, "Calling adios_open mode = %d", file->mode));

        file->filename = (char*)calloc(adios_bp_filename_len, sizeof(char));
        if (file->filename == NULL)
        {
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                           "Creating file (%s) using ADIOS iotype failed. Out of memory allocating %lld bytes for the file name",
                           filename, (unsigned long long) (adios_bp_filename_len));
        }
        snprintf(file->filename, adios_bp_filename_len, "%s%s", filename, adios_bp_filename_extn);

        if (file->mode & PIO_NOCLOBBER) /* Check whether BP directory filename.bp exists */
        {
            struct stat sd;
            if (0 == stat(file->filename, &sd))
            {
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EEXIST, __FILE__, __LINE__,
                               "Creating file (%s) using ADIOS iotype and PIO_NOCLOBBER mode failed. BP directory (%s) already exists",
                               filename, file->filename);
            }
        }

        /* Set communicator for all adios processes, process rank, and I/O master node     */
        file->all_comm = ios->union_comm;
        file->all_rank = ios->union_rank;
        file->num_alltasks = ios->num_uniontasks;

        /* NOTE: If adios_io_process == 1, the process is an adios process and will participate in adios operations */
        file->adios_comm = ios->adios_comm;
        file->adios_rank = ios->adios_rank;
        file->num_adiostasks = ios->num_adiostasks;
        file->adios_io_process = ios->adios_io_process;

        file->adios_reader_num_decomp_blocks = 0;

#ifdef _SPIO_ADIOS_NO_DECOMPS
        file->store_adios_decomp = false;
#else
        file->store_adios_decomp = true;
#endif

        /* Create a new ADIOS group */
        char declare_name[PIO_MAX_NAME];
        snprintf(declare_name, PIO_MAX_NAME, "%s%lu", file->filename, get_adios2_io_cnt());

        adios2_error adiosErr = adios2_error_none;
        if (file->adios_io_process == 1)
        {
            file->ioH = adios2_declare_io(ios->adiosH, (const char*)(declare_name));
            if (file->ioH == NULL)
            {
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Declaring (ADIOS) IO (name=%s) failed for file (%s)",
                               declare_name, pio_get_fname_from_file(file));
            }

            adiosErr = adios2_set_engine(file->ioH, "BP5"); /* was BP4 */
            if (adiosErr != adios2_error_none)
            {
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Setting (ADIOS) engine (type=BP4) failed (adios2_error=%s) for file (%s)",
                               convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
            }
        }

        /* Initialize adios I/O related parameters */
        ierr = initialize_adios2_variables(ios, file);
        if (ierr != PIO_NOERR)
        {
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Initializing parameters failed for file (%s)",
                           pio_get_fname_from_file(file));
        }

        /* Initialize for block merging in pio_write_darray */
        ierr = initialize_adios2_for_block_merging(ios, file);
        if (ierr != PIO_NOERR)
        {
            spio_ltimer_stop(file->io_fstats->wr_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Initializing block merge failed for file (%s)",
                           pio_get_fname_from_file(file));
        }

        if (file->adios_io_process == 1)
        {
            GPTLstart("PIO:adios2_open_call");
            file->engineH = adios2_open(file->ioH, file->filename, adios2_mode_write);
            if (file->engineH == NULL)
            {
                GPTLstop("PIO:adios2_open_call");
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening (ADIOS) file (%s) failed",
                               pio_get_fname_from_file(file));
            }
            GPTLstop("PIO:adios2_open_call");

            if(file->adios_rank == 0)
            {
                const std::string adios_bp_md_filename(std::string(file->filename) + "/md.0");
                ierr = symlink(adios_bp_md_filename.c_str(), filename);
                if(ierr != 0)
                {
                    fprintf(stdout, "PIO: WARNING: Creating symlink for ADIOS BP mdata file (%s) failed, ierr = %d", adios_bp_md_filename.c_str(), ierr);
                }
            }

            ierr = begin_adios2_step(file, ios);
            if (ierr != PIO_NOERR)
            {
                spio_ltimer_stop(file->io_fstats->wr_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "ADIOS begin step failed for file (%s)",
                               pio_get_fname_from_file(file));
            }

            if (file->adios_rank == 0)
            {
                adios2_variable *variableH = adios2_inquire_variable(file->ioH, "/__pio__/info/nproc");
                if (variableH == NULL)
                {
                    variableH = spio_define_adios2_variable(ios, file, file->ioH,
                                                            "/__pio__/info/nproc", adios2_type_int32_t,
                                                            0, NULL, NULL, NULL,
                                                            adios2_constant_dims_true);
                    if (variableH == NULL)
                    {
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Defining (ADIOS) variable (name=/__pio__/info/nproc) failed for file (%s)",
                                       pio_get_fname_from_file(file));
                    }
                }

                adiosErr = adios2_put(file->engineH, variableH, &ios->num_uniontasks, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=/__pio__/info/nproc) failed (adios2_error=%s) for file (%s)",
                                   convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
                }

                variableH = adios2_inquire_variable(file->ioH, "/__pio__/info/decomp_stored");
                if (variableH == NULL)
                {
                    variableH = spio_define_adios2_variable(ios, file, file->ioH,
                                                            "/__pio__/info/decomp_stored", adios2_type_int32_t,
                                                            0, NULL, NULL, NULL,
                                                            adios2_constant_dims_true);
                    if (variableH == NULL)
                    {
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Defining (ADIOS) variable (name=/__pio__/info/decomp_stored) failed for file (%s)",
                                       pio_get_fname_from_file(file));
                    }
                }

                int info_decomp_stored = (file->store_adios_decomp)? 1 : 0;
                adiosErr = adios2_put(file->engineH, variableH, &info_decomp_stored, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=/__pio__/info/decomp_stored) failed (adios2_error=%s) for file (%s)",
                                   convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
                }
                file->num_written_blocks += 2;
            }

            /* Write the number and list of processes in block merges */
            if (file->block_myrank == 0)
            {
                /* Write the number of processes in block merges */
                adios2_variable *variableH_blocks = adios2_inquire_variable(file->ioH, "/__pio__/info/block_nprocs");
                if (variableH_blocks == NULL)
                {
                    variableH_blocks = spio_define_adios2_variable(ios, file, file->ioH,
                                                                   "/__pio__/info/block_nprocs", adios2_type_int32_t,
                                                                   0, NULL, NULL, NULL,
                                                                   adios2_constant_dims_true);
                    if (variableH_blocks == NULL)
                    {
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Defining (ADIOS) variable (name=/__pio__/info/block_nprocs) failed for file (%s)",
                                       pio_get_fname_from_file(file));
                    }
                }

                adiosErr = adios2_put(file->engineH, variableH_blocks, &file->block_nprocs, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=/__pio__/info/block_nprocs) failed (adios2_error=%s) for file (%s)",
                                   convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
                }

                /* Write the list of processes in each block */
                adios2_variable *variableH_list = adios2_inquire_variable(file->ioH, "/__pio__/info/block_list");
                if (variableH_list == NULL)
                {
                    size_t v_count = file->block_nprocs;
                    variableH_list = spio_define_adios2_variable(ios, file, file->ioH,
                                                                 "/__pio__/info/block_list", adios2_type_int32_t,
                                                                 1, NULL, NULL, &v_count,
                                                                 adios2_constant_dims_true);
                    if (variableH_list == NULL)
                    {
                        spio_ltimer_stop(file->io_fstats->wr_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Defining (ADIOS) variable (name=/__pio__/info/block_list) failed for file (%s)",
                                       pio_get_fname_from_file(file));
                    }
                }
                adiosErr = adios2_put(file->engineH, variableH_list, file->block_list, adios2_mode_sync);
                if (adiosErr != adios2_error_none)
                {
                    spio_ltimer_stop(file->io_fstats->wr_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Putting (ADIOS) variable (name=/__pio__/info/block_list) failed (adios2_error=%s) for file (%s)",
                                   convert_adios2_error_to_string(adiosErr), pio_get_fname_from_file(file));
                }
                file->num_written_blocks += 2;
            }
        }
    }
#endif

    /* If this task is in the IO component, do the IO. */
    if (ios->ioproc)
    {
#ifdef _NETCDF4
        /* All NetCDF4 files use the CDF5 file format by default,
         * - 64bit offset, 64bit data
         * However the NetCDF library does not allow setting the 
         * NC_64BIT_OFFSET or NC_64BIT_DATA flags for NetCDF4 types 
         * - this internal reset of flags is for user convenience */
        if ((file->iotype == PIO_IOTYPE_NETCDF4P) ||
            (file->iotype == PIO_IOTYPE_NETCDF4C))
        {
            LOG((2, "File create mode (before change) = %x",file->mode));
            if ((file->mode & NC_64BIT_OFFSET) == NC_64BIT_OFFSET)
            {
                file->mode &= ~NC_64BIT_OFFSET;
            }
            if ((file->mode & NC_64BIT_DATA) == NC_64BIT_DATA)
            {
                file->mode &= ~NC_64BIT_DATA;
            }
            LOG((2, "File create mode (after change) = %x",file->mode));
        }
#endif

        switch (file->iotype)
        {
#ifdef _NETCDF4
        case PIO_IOTYPE_NETCDF4P:
            file->mode = file->mode |  NC_MPIIO | NC_NETCDF4;
#ifdef NC_NODIMSCALE_ATTACH
            /* If NC_NODIMSCALE_ATTACH flag is available (introduced by NetCDF 4.9.0), apply it
               when creating HDF5-based NetCDF4 files to skip attaching dimscale to variables.

               This workaround is required to bypass possible HDF5 errors (reproducible with
               NetCDF 4.8.1 or 4.9.0) caused by the High-level DS API H5DSattach_scale called
               multiple times inside a loop in the NetCDF library.
             */
            file->mode |= NC_NODIMSCALE_ATTACH;
#endif
            LOG((2, "Calling nc_create_par io_comm = %d mode = %d fh = %d",
                 ios->io_comm, file->mode, file->fh));
            ierr = nc_create_par(filename, file->mode, ios->io_comm, ios->info, &file->fh);
            LOG((2, "nc_create_par returned %d file->fh = %d", ierr, file->fh));
            break;
        case PIO_IOTYPE_NETCDF4P_NCZARR:
            file->mode = file->mode |  NC_NETCDF4;
            LOG((2, "Calling nc_create_par (nczarr) io_comm = %d mode = %d fh = %d",
                 ios->io_comm, file->mode, file->fh));
            ierr = PIO_ENOTBUILT;
#if PIO_USE_NETCDF4_NCZARR
            {
              std::string filename_nczarr = spio_get_nczarr_fname(filename);
              if(!filename_nczarr.empty()){
                ierr = nc_create_par(filename_nczarr.c_str(), file->mode,
                                      ios->io_comm, ios->info, &file->fh);
              }
            }
            LOG((2, "nc_create_par returned %d file->fh = %d", ierr, file->fh));
#else
            LOG((2, "nc_create_par returned %d, NCZarr support not built/enabled", ierr));
#endif
            break;
        case PIO_IOTYPE_NETCDF4C:
            file->mode = file->mode | NC_NETCDF4;
#endif
#ifdef _NETCDF
        case PIO_IOTYPE_NETCDF:
            if (!ios->io_rank)
            {
                LOG((2, "Calling nc_create mode = %d", file->mode));
                ierr = nc_create(filename, file->mode, &file->fh);
            }
            break;
#endif
#ifdef _PNETCDF
        case PIO_IOTYPE_PNETCDF:
            LOG((2, "Calling ncmpi_create mode = %d", file->mode));
            if (ios->info == MPI_INFO_NULL)
                MPI_Info_create(&ios->info);

            /* Set some MPI-IO hints below */

            /* ROMIO will not perform data sieving for writes. Data sieving is
               designed for I/O patterns that read or write small, noncontiguous
               file regions. It does not help if the aggregated writes are always
               contiguous, covering the entire variables.
             */
            MPI_Info_set(ios->info, "romio_ds_write", "disable");

            /* Enable ROMIO's collective buffering for writes. Collective
               buffering, also called two-phase collective I/O, reorganizes
               data across processes to match data layout in file.
             */
            MPI_Info_set(ios->info, "romio_cb_write", "enable"); 

            /* Disable independent file operations. ROMIO will make an effort to
               avoid performing any file operation on non-aggregator processes.
               When writing, ROMIO selects a subset of processes as "I/O aggregators".
               These aggregators are the MPI processes that actually write data to the
               file. The non-aggregator processes never touch the file. Under this
               deferred open strategy for non-aggregators, not all processes open the
               file and determine the file distribution parameters.

               For lustre file system, there is a confirmed bug with MPICH 3.2.1:
               The file striping hints are not sync-ed for all processes, which might
               cause a division-by-zero SIGFPE inside ROMIO's lustre driver on some
               non-aggregator processes with a zero stripe count.

               For some MPI implementations based on MPICH 3.2 (MPICH_VERSION defined),
               this bug is not reproducible with Cray MPICH and Intel MPI, but it is
               reproducible with MVAPICH2.

               For Open MPI (MPICH_VERSION not defined) that uses ROMIO 3.2.1, this bug
               is also not reproducible.

               This bug has been fixed in MPICH 3.3 or higher versions, see
               https://github.com/pmodels/mpich/commit/a33abfa

               We choose not to set this hint for MPICH 3.2.1 or lower versions, except
               for Cray MPICH and Intel MPI.
               Note: OSU MVAPICH2 2.3.3 (01/09/20) is still based on MPICH 3.2.1
                     Cray MPICH 7.7.12 is still based on MPICH 3.2
                     Intel MPI 2018.0.3 is still based on MPICH 3.2
                     Intel MPI 2019.0.3 is based on MPICH 3.3b3
             */
#if defined(MPICH_NUMVERSION) && (MPICH_NUMVERSION < 30300000) && !defined(CRAY_MPICH_VERSION) && !defined(I_MPI_VERSION)
            MPI_Info_set(ios->info, "romio_no_indep_rw", "false");
#else
            MPI_Info_set(ios->info, "romio_no_indep_rw", "true");
#endif

            /* Users can set striping_factor and striping_unit hints for files created with
               MPI-IO. If they are not explicitly set with PIO_set_hint or PIOc_set_hint,
               use some default values below.
             */

#if PIO_MAX_LUSTRE_OSTS
            /* A Lustre file system has a limited number of available OSTs to use. For example,
               this number is 248 on Cori and 56 on Theta currently.

               We should not go to full as Lustre will bypass assigning slow OSTs to the file
               at file creation time, going to a recommended smaller number (e.g. 72 on Cori
               and 48 on Theta) leaves room for this.

               We also do not want to create high metadata requests which might negatively
               impact other users.
             */
            char info_striping_factor[PIO_MAX_NAME];
            int flag_striping_factor;

            /* Number of Object Storage Targets (OSTs) a file exists on */
            MPI_Info_get(ios->info, "striping_factor", PIO_MAX_NAME, info_striping_factor, &flag_striping_factor);
            if (!flag_striping_factor)
            {
                int striping_factor_size = (ios->num_iotasks < PIO_MAX_LUSTRE_OSTS)? ios->num_iotasks : PIO_MAX_LUSTRE_OSTS;
                char striping_factor_str[PIO_MAX_NAME];
                snprintf(striping_factor_str, PIO_MAX_NAME, "%d", striping_factor_size);

                MPI_Info_set(ios->info, "striping_factor", striping_factor_str);
            }
#endif

#if PIO_STRIPING_UNIT
            /* In a Lustre file system, striping_unit (stripe-size) is the number of bytes write on one
               OST before cycling to the next. Default (1MB) has been most successful.

               One optimization we need for GPFS is to align writes to the file system block size.
               We can do this by setting the "striping_unit" hint to GPFS block size (16MB on Summit).

               For a block-based parallel file system such as GPFS, an application write becomes a block
               write at the file system layer. If a write straddles two blocks, locks must be acquired
               for both blocks. Aligning the start of a variable to a block boundary, combined with
               collective I/O optimizations in the MPI-IO library can often eliminate all unaligned
               file system accesses.
             */
            char info_striping_unit[PIO_MAX_NAME];
            int flag_striping_unit;

            MPI_Info_get(ios->info, "striping_unit", PIO_MAX_NAME, info_striping_unit, &flag_striping_unit);
            if (!flag_striping_unit)
            {
                char striping_unit_str[PIO_MAX_NAME];
                snprintf(striping_unit_str, PIO_MAX_NAME, "%d", PIO_STRIPING_UNIT);

                MPI_Info_set(ios->info, "striping_unit", striping_unit_str);
            }
#endif

            /* Set some PnetCDF I/O hints below */

            /* Do not align the starting file offsets of individual fixed-size
               variables. If applications use PnetCDF nonblocking APIs to
               aggregate write requests to multiple variables, then the best
               practice is to disable the variable alignment. This will prevent
               creating gaps in file space between two consecutive fixed-size
               variables and thus the writes to file system can be contiguous.
             */
            MPI_Info_set(ios->info, "nc_var_align_size", "1");

            /* Enable in-place byte swap on Little Endian architectures. With
               this option, PnetCDF performs byte swap on user I/O buffers
               whenever possible. This results in the least amount of internal
               memory usage. However, if an immutable user buffer is used,
               segmentation fault may occur when byte swap is performed on
               user buffer in place.
             */
            MPI_Info_set(ios->info, "nc_in_place_swap", "enable");

            /* Set the size of a temporal buffer to be allocated by PnetCDF
               internally to pack noncontiguous user write buffers supplied
               to the nonblocking requests into a contiguous space. On some
               systems, using noncontiguous user buffers in MPI collective
               write functions performs significantly worse than using
               contiguous buffers. This hint is supported by latest PnetCDF
               (version 1.11.0 and later).
               
               [More information]
               Noncontiguous write buffers are almost unavoidable:
               1) Each IO decomposition has its own writer buffer for a file
               2) PnetCDF might use noncontiguous helper buffers to perform
                  data type conversion

               Without this hint, we have seen hanging issues on Cori and
               Titan for some E3SM cases run with SUBSET rearranger. This
               hint is optional if BOX rearranger is used. 

               The default buffer size is 16 MiB in PnetCDF and we tentatively
               set it to 64 MiB. For E3SM production runs, if SUBSET rearranger
               is used, we might need an even larger buffer size in PnetCDF. For
               example, if 150 IO tasks are used to write a file of size 80 GiB,
               we should try a buffer size larger than 546 MiB.
             */
            MPI_Info_set(ios->info, "nc_ibuf_size", "67108864");

            ierr = ncmpi_create(ios->io_comm, filename, file->mode, ios->info, &file->fh);
            if (!ierr)
            {
                /* When using buffered put APIs in PnetCDF, we must explicitly specify the size of the internal buffer.
                   To avoid potential failures, we ensure a minimum buffer size (16 MiB) when pio_buffer_size_limit is
                   0 or too small. If pio_buffer_size_limit is set to 0, this workaround prevents a direct failure of
                   the ncmpi_buffer_attach call (NULL buffer error). Additionally, if pio_buffer_size_limit is too small,
                   using the larger minimum size reduces the likelihood of potential NC_EINSUFFBUF errors. As recommended
                   by PnetCDF documentation, users should make sure that the internal buffer size is sufficiently large
                   for buffered put operations.
                 */
                const PIO_Offset min_bufsize = 16777216; /* Minimum buffer size (16 MiB) for buffered put APIs in PnetCDF */
                ierr = ncmpi_buffer_attach(file->fh, (pio_buffer_size_limit > min_bufsize)? pio_buffer_size_limit : min_bufsize);
            }
            break;
#endif
#ifdef _HDF5
        case PIO_IOTYPE_HDF5:
            ierr = spio_hdf5_create(ios, file, filename);
            break;
#endif
        }
    }

    /* Broadcast mode to all tasks. */
    if ((mpierr = MPI_Bcast(&file->mode, 1, MPI_INT, ios->ioroot, ios->union_comm)))
    {
#ifdef _ADIOS2
        if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
        {
            if (file->filename != NULL)
            {
                free(file->filename);
                file->filename = NULL;
            }
        }
#endif
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        free(file->io_fstats);
        free(file);
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    ierr = check_netcdf(ios, file, ierr, __FILE__, __LINE__);
    /* If there was an error, free the memory we allocated and handle error. */
    if (ierr != PIO_NOERR)
    {
#ifdef _ADIOS2
        if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
        {
            if (file->filename != NULL)
            {
                free(file->filename);
                file->filename = NULL;
            }
        }
#endif
        spio_ltimer_stop(file->io_fstats->wr_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        free(file->io_fstats);
        free(file);
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                        "Creating file (%s) failed. Internal error", filename);
    }

    /* This flag is implied by netcdf create functions but we need
       to know if its set. */
    file->mode = file->mode | PIO_WRITE;

    /* Add the struct with this files info to the global list of
     * open files. */
    MPI_Comm comm = MPI_COMM_NULL;
    if(ios->async)
    {
        /* For asynchronous I/O service, since file ids are passed across
         * disjoint comms we need it to be unique across the union comm
         */
        comm = ios->union_comm;
    }

    /* Initialize the multi-variable cache in the file that is used to cache
     * data from multiple variables before writing it out to the output file
     * Note: The MVCache is not used by ADIOS
     */
    spio_file_mvcache_init(file);

    *ncidp = pio_add_to_file_list(file, comm);

    LOG((2, "Created file %s file->fh = %d file->pio_ncid = %d", filename,
         file->fh, file->pio_ncid));

    spio_ltimer_stop(file->io_fstats->wr_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return ierr;
}

/**
 * Check that a file meets PIO requirements for use of unlimited
 * dimensions. This function is only called on netCDF-4 files. If the
 * file is found to violate PIO requirements it is closed.
 * 
 * @param ncid the file->fh for this file (the real netCDF ncid, not
 * the pio_ncid).
 * @returns 0 if file is OK, error code otherwise.
 * @author Ed Hartnett
 */
int check_unlim_use(int ncid)
{
#ifdef _NETCDF4
    int nunlimdims; /* Number of unlimited dims in file. */
    int nvars;       /* Number of vars in file. */
    int ierr;        /* Return code. */

    /* Are there 2 or more unlimited dims in this file? */
    if ((ierr = nc_inq_unlimdims(ncid, &nunlimdims, NULL)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Checking unlimited use failed on file %s (ncid = %d). Querying the number of unlimited dimensions in the file failed", pio_get_fname_from_file_id(ncid), ncid);
    }
    if (nunlimdims < 2)
        return PIO_NOERR;

    /* How many vars in file? */
    if ((ierr = nc_inq_nvars(ncid, &nvars)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Checking unlimited use failed on file %s (ncid = %d). Querying the number of variables in the file failed", pio_get_fname_from_file_id(ncid), ncid);
    }

    /* Check each var. */
    for (int v = 0; v < nvars && !ierr; v++)
    {
        int nvardims;
        if ((ierr = nc_inq_varndims(ncid, v, &nvardims)))
        {
            return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                            "Checking unlimited use failed on file %s (ncid = %d) for variable %s (varid = %d). Querying the number of dimensions for the variable failed", pio_get_fname_from_file_id(ncid), ncid, pio_get_vname_from_file_id(ncid, v), v);
        }
        int vardimid[nvardims];
        if ((ierr = nc_inq_vardimid(ncid, v, vardimid)))
        {
            return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                            "Checking unlimited use failed on file %s (ncid = %d) for variable %s (varid = %d). Querying the dimension ids for the variable failed", pio_get_fname_from_file_id(ncid), ncid, pio_get_vname_from_file_id(ncid, v), v);
        }

        /* Check all var dimensions, except the first. If we find
         * unlimited, that's a problem. */
        for (int vd = 1; vd < nvardims; vd++)
        {
            size_t dimlen;
            if ((ierr = nc_inq_dimlen(ncid, vardimid[vd], &dimlen)))
            {
                return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                                "Checking unlimited use failed on file %s (ncid = %d) for variable %s (varid = %d). Querying dimension length of a dimension (dim id = %d) failed", pio_get_fname_from_file_id(ncid), ncid, pio_get_vname_from_file_id(ncid, v), v, vardimid[vd]);
            }
            if (dimlen == NC_UNLIMITED)
            {
                nc_close(ncid);
                return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__,
                                "Checking unlimited use failed on file %s (ncid = %d) for variable %s (varid = %d). Dimension length of dimension %d (dim id = %d) is unlimited. Only the first (0th) dimension is expected to be unlimited", pio_get_fname_from_file_id(ncid), ncid, pio_get_vname_from_file_id(ncid, v), v, vd, vardimid[vd]);
            }
        }
    }
#endif /* _NETCDF4 */
    
    return PIO_NOERR;
}

#ifdef _ADIOS2
char str_buf[PIO_MAX_NAME] = {'\0'};
char attr_data_buf[64 * PIO_MAX_NAME] = {'\0'};
char const adios_pio_var_prefix[] = "/__pio__/var/";
char const adios_pio_dim_prefix[] = "/__pio__/dim/";
char const adios_def_nctype_suffix[] = "/def/nctype";
char const adios_def_adiostype_suffix[] = "/def/adiostype";
char const adios_def_ndims_suffix[] = "/def/ndims";
char const adios_def_ncop_suffix[] = "/def/ncop";
char const adios_def_dims_suffix[] = "/def/dims";
char const adios_def_decomp_suffix[] = "/def/decomp";
char const adios_pio_global_prefix[] = "/__pio__/global/";
char const adios_pio_track_frame_id_prefix[] = "/__pio__/track/frame_id/";
char const adios_pio_decomp_prefix[] = "/__pio__/decomp/";

static int adios_get_nc_type(file_desc_t *file, int);
static int adios_get_adios_type(file_desc_t *file, int);
static int adios_get_ndims(file_desc_t *file, int);
static int adios_get_step_info(file_desc_t *file, int, size_t, size_t);
static int adios_get_dim_ids(file_desc_t *file, int);
static int adios_get_nc_op_tag(file_desc_t *file, int);
static int adios_get_attr(file_desc_t *file, int current_var_cnt, char *const *attr_names, size_t);
static int adios_get_num_decomp_blocks(file_desc_t *file);
static int adios_get_decomp_storage_info(file_desc_t *file);
static size_t adios_read_vars_vars(file_desc_t *file, size_t var_size, char *const *var_names);
static size_t adios_read_vars_attrs(file_desc_t *file, size_t attr_size, char *const *attr_names);

/**
 *  Map frame id to adios2 step
 */
int adios_reader_get_step(file_desc_t *file, int var_id, int frame_id)
{
    int search_frame_id;
    if (frame_id == -1)
        search_frame_id = 0;
    else
        search_frame_id = frame_id;

    for (int step = 0; step < file->adios_vars[var_id].interval_map->n_adios_steps; step++)
    {
        if (search_frame_id >= file->adios_vars[var_id].interval_map->map[step][0] &&
            search_frame_id <= file->adios_vars[var_id].interval_map->map[step][1])
            return step;
    }

    return -1;
}

static int get_dim_id(file_desc_t *file, char *dim_name)
{
    for (int i = 0; i <  file->num_dim_vars; i++)
    {
        if (strncmp(file->dim_names[i], dim_name, PIO_MAX_NAME) == 0)
            return i;
    }

    return -1;
}

static int adios_read_global_dimensions(iosystem_desc_t *ios, file_desc_t *file, char **var_names, size_t var_size)
{
    adios2_error adiosErr = adios2_error_none;

    for (size_t i = 0; i < var_size; i++)
    {
        if (strncmp(var_names[i], adios_pio_dim_prefix, strlen(adios_pio_dim_prefix)) == 0)
        {
            int sub_length = strlen(var_names[i]) - strlen(adios_pio_dim_prefix);
            int full_length = strlen(var_names[i]);
            int prefix_length = strlen(adios_pio_var_prefix);

            if (file->dim_names[file->num_dim_vars] == NULL)
            {
                file->dim_names[file->num_dim_vars] = (char *) calloc(sub_length + 1, 1);
                if (file->dim_names[file->num_dim_vars] == NULL)
                {
                    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                   "Reading global dimensions in file (%s, ncid=%d) using ADIOS iotype failed. "
                                   "Out of memory allocating %lld bytes for a dimension name",
                                   pio_get_fname_from_file(file), file->pio_ncid, (long long int)(sub_length + 1));
                }
            }

            if (file->dim_names[file->num_dim_vars])
                strncpy(file->dim_names[file->num_dim_vars], &var_names[i][prefix_length], sub_length);

            adios2_variable *variableH = adios2_inquire_variable(file->ioH, var_names[i]);
            if (variableH != NULL)
            {
                adios2_type type;
                adiosErr = adios2_variable_type(&type, variableH);
                if (adiosErr != adios2_error_none)
                {
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Reading global dimensions in file (%s, ncid=%d) using ADIOS iotype failed. "
                                   "The low level (ADIOS) I/O library call failed to retrieve variable type (adios2_error=%s)",
                                   pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
                }

                if (type == adios2_type_uint64_t)
                {
                    uint64_t len = 0;
                    adiosErr = adios2_get(file->engineH, variableH, &len, adios2_mode_sync);
                    if (adiosErr != adios2_error_none)
                    {
                        return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Reading global dimensions in file (%s, ncid=%d) using ADIOS iotype failed. "
                                       "The low level (ADIOS) I/O library call failed to get data associated with a variable from an engine (adios2_error=%s)",
                                       pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
                    }

                    file->dim_values[file->num_dim_vars] = len;
                }
                else
                {
                    return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Reading global dimensions in file (%s, ncid=%d) using ADIOS iotype failed. "
                                   "Unsupported ADIOS type (%x)",
                                   pio_get_fname_from_file(file), file->pio_ncid, type);
                }
            }

            file->num_dim_vars++;
        }
    }

    return 0;
}

static char* adios_name(const char* prefix, const char* root, const char *suffix)
{
    assert(prefix != NULL);
    assert(root != NULL);
    assert(suffix != NULL);

    char *name = str_buf;
    size_t len_prefix = strlen(prefix);
    memcpy(name, prefix, len_prefix);

    size_t len_root = strlen(root);
    memcpy(name + len_prefix, root, len_root);

    size_t len_suffix = strlen(suffix);
    memcpy(name + len_prefix + len_root, suffix, len_suffix);

    name[len_prefix + len_root + len_suffix] = '\0';

    return name;
}

static int adios_get_attr(file_desc_t *file, int attr_id, char *const *attr_names, size_t i)
{
    /* Get an attribute */
    adios2_attribute *attr = adios2_inquire_attribute(file->ioH, attr_names[i]);
    if (attr != NULL)
    {
        adios2_type type;
        adios2_error adiosErr = adios2_attribute_type(&type, attr);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting attribute (id = %d, name = %s) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to retrieve attribute type (adios2_error=%s)",
                           attr_id, file->adios_attrs[attr_id].att_name, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        if (type == adios2_type_int8_t)
        {
            file->adios_attrs[attr_id].att_type = NC_BYTE;
            file->adios_attrs[attr_id].adios_type = adios2_type_int8_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_int16_t)
        {
            file->adios_attrs[attr_id].att_type = NC_SHORT;
            file->adios_attrs[attr_id].adios_type = adios2_type_int16_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_int32_t)
        {
            file->adios_attrs[attr_id].att_type = NC_INT;
            file->adios_attrs[attr_id].adios_type = adios2_type_int32_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_float)
        {
            file->adios_attrs[attr_id].att_type = NC_FLOAT;
            file->adios_attrs[attr_id].adios_type = adios2_type_float;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_double)
        {
            file->adios_attrs[attr_id].att_type = NC_DOUBLE;
            file->adios_attrs[attr_id].adios_type = adios2_type_double;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_uint8_t)
        {
            file->adios_attrs[attr_id].att_type = NC_UBYTE;
            file->adios_attrs[attr_id].adios_type = adios2_type_uint8_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_uint16_t)
        {
            file->adios_attrs[attr_id].att_type = NC_USHORT;
            file->adios_attrs[attr_id].adios_type = adios2_type_uint16_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_uint32_t)
        {
            file->adios_attrs[attr_id].att_type = NC_UINT;
            file->adios_attrs[attr_id].adios_type = adios2_type_uint32_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_int64_t)
        {
            file->adios_attrs[attr_id].att_type = NC_INT64;
            file->adios_attrs[attr_id].adios_type = adios2_type_int64_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_uint64_t)
        {
            file->adios_attrs[attr_id].att_type = NC_UINT64;
            file->adios_attrs[attr_id].adios_type = adios2_type_uint64_t;
            file->adios_attrs[attr_id].att_len = (PIO_Offset) 1;
        }
        else if (type == adios2_type_string)
        {
            file->adios_attrs[attr_id].att_type = NC_CHAR;
            file->adios_attrs[attr_id].adios_type = adios2_type_string;

            size_t size_attr = 0;
            char *attr_data = attr_data_buf;

            adiosErr = adios2_attribute_data(attr_data, &size_attr, attr);
            if (adiosErr != adios2_error_none)
            {
                return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Getting attribute (id = %d, name = %s) in file (%s, ncid=%d) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to retrieve attribute data pointer (adios2_error=%s)",
                               attr_id, file->adios_attrs[attr_id].att_name, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
            }

            file->adios_attrs[attr_id].att_len = (PIO_Offset)(strlen(attr_data));
        }
        else
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting attribute (id = %d, name = %s) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "Unsupported ADIOS type (%x)",
                           attr_id, file->adios_attrs[attr_id].att_name, pio_get_fname_from_file(file), file->pio_ncid, type);
        }
    }

    return 0;
}

static int adios_get_dim_ids(file_desc_t *file, int varid)
{
    /* Get dimension IDs */
    char *dims_attr_name = adios_name(adios_pio_var_prefix,
                                      file->adios_vars[varid].name,
                                      adios_def_dims_suffix);

    adios2_attribute *attr = adios2_inquire_attribute(file->ioH, dims_attr_name);
    if (attr != NULL)
    {
        size_t size_attr = 0;
        adios2_error adiosErr = adios2_attribute_size(&size_attr, attr);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting dimension IDs for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to return the number of elements in attribute (adios2_error=%s)",
                           file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        if (size_attr > 0)
        {
            char **attr_data = (char **) calloc(size_attr, sizeof(char *));
            if (attr_data == NULL)
            {
                return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                               "Getting dimension IDs for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                               "Out of memory allocating %lld bytes for the attribute data array",
                               file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)(size_attr * sizeof(char *)));
            }

            for (size_t a = 0; a < size_attr; a++)
            {
                attr_data[a] = (char *) calloc(PIO_MAX_NAME, 1);
                if (attr_data[a] == NULL)
                {
                    return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                                   "Getting dimension IDs for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                                   "Out of memory allocating %lld bytes for an entry of the attribute data array",
                                   file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)PIO_MAX_NAME);
                }
            }

            adiosErr = adios2_attribute_data(attr_data, &size_attr, attr);
            if (adiosErr != adios2_error_none)
            {
                return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Getting dimension IDs for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to retrieve attribute data pointer (adios2_error=%s)",
                               file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
            }

            if (file->adios_vars[varid].gdimids == NULL)
            {
                file->adios_vars[varid].gdimids = (int *) calloc(file->adios_vars[varid].ndims, sizeof(int));
                if (file->adios_vars[varid].gdimids == NULL)
                {
                    return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                                   "Getting dimension IDs for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                                   "Out of memory allocating %lld bytes for the dimension IDs of the variable",
                                   file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)(file->adios_vars[varid].ndims * sizeof(int)));
                }
            }

            for (size_t a = 0; a < size_attr; a++)
            {
                int gdimid = get_dim_id(file, attr_data[a]);
                if (gdimid < 0)
                {
                    return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                   "Getting dimension IDs for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                                   "Cannot determine ID for dimension %s",
                                   file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, attr_data[a]);
                }

                free(attr_data[a]);
                file->adios_vars[varid].gdimids[a] = gdimid;
            }

            free(attr_data);
        }
    }

    return 0;
}

static int adios_get_nc_op_tag(file_desc_t *file, int varid)
{
    /* Get darray or put_var tag for a variable */
    char *dims_attr_name = adios_name(adios_pio_var_prefix,
                                      file->adios_vars[varid].name,
                                      adios_def_ncop_suffix);

    adios2_attribute *attr = adios2_inquire_attribute(file->ioH, dims_attr_name);
    if (attr != NULL)
    {
        size_t size_attr = 0;
        adios2_error adiosErr = adios2_attribute_size(&size_attr, attr);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting darray/put_var tag for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to return the number of elements in attribute (adios2_error=%s)",
                           file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        if (size_attr > 0)
        {
            char* attr_data = (char *) calloc(NC_OP_TAG_MAX_LEN, 1);
            if (attr_data == NULL)
            {
                return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                               "Getting darray/put_var tag for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                               "Out of memory allocating %lld bytes for attribute data",
                               file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)NC_OP_TAG_MAX_LEN);
            }

            adiosErr = adios2_attribute_data(attr_data, &size_attr, attr);
            if (adiosErr != adios2_error_none)
            {
                return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Getting darray/put_var tag for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to retrieve attribute data pointer (adios2_error=%s)",
                               file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
            }

            strncpy(file->adios_vars[varid].nc_op_tag, attr_data, NC_OP_TAG_MAX_LEN);
            free(attr_data);
        }
    }

    return 0;
}

static int adios_get_ndims(file_desc_t *file, int varid)
{
    /* Get number of dimensions */
    char *ndims_attr_name = adios_name(adios_pio_var_prefix,
                                       file->adios_vars[varid].name,
                                       adios_def_ndims_suffix);

    adios2_attribute *ndims_attr = adios2_inquire_attribute(file->ioH, ndims_attr_name);
    if (ndims_attr != NULL)
    {
        int32_t attr_data = 0;
        size_t size_attr = 0;
        adios2_error adiosErr = adios2_attribute_data(&attr_data, &size_attr, ndims_attr);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting number of dimensions for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to retrieve attribute data pointer (adios2_error=%s)",
                           file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        file->adios_vars[varid].ndims = attr_data;
    }

    return 0;
}

static int adios_get_adios_type(file_desc_t *file, int varid)
{
    /* Get adios type */
    char *attr_name = adios_name(adios_pio_var_prefix,
                                 file->adios_vars[varid].name,
                                 adios_def_adiostype_suffix);

    adios2_attribute *attr = adios2_inquire_attribute(file->ioH, attr_name);
    if (attr != NULL)
    {
        adios2_type attr_data = adios2_type_unknown;
        size_t size_attr = 0;
        adios2_error adiosErr = adios2_attribute_data(&attr_data, &size_attr, attr);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting ADIOS type for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to retrieve attribute data pointer (adios2_error=%s)",
                           file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        file->adios_vars[varid].adios_type = attr_data;
    }

    return 0;
}

static int adios_get_nc_type(file_desc_t *file, int varid)
{
    /* Get nc type */
    char *attr_name = adios_name(adios_pio_var_prefix,
                                 file->adios_vars[varid].name,
                                 adios_def_nctype_suffix);

    adios2_attribute *attr = adios2_inquire_attribute(file->ioH, attr_name);
    if (attr != NULL)
    {
        int32_t attr_data = 0;
        size_t size_attr = 0;
        adios2_error adiosErr = adios2_attribute_data(&attr_data, &size_attr, attr);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting NC type for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to retrieve attribute data pointer (adios2_error=%s)",
                           file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        file->adios_vars[varid].nc_type = attr_data;
    }

    return 0;
}

static void update_interval_map(adios_interval_map_t *map, short min_frame_id, short max_frame_id, short adios_step)
{
    map->map[adios_step][0] = min_frame_id;
    map->map[adios_step][1] = max_frame_id;
}

static adios_interval_map_t* init_interval_map(file_desc_t *file, int varid, size_t n_steps)
{
    adios_interval_map_t* map = (adios_interval_map_t*)calloc(1, sizeof(adios_interval_map_t));
    if (map == NULL)
    {
        pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                "Initializing interval map for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                "Out of memory allocating %lld bytes for the map",
                file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)(sizeof(adios_interval_map_t)));

        return NULL;
    }

    map->n_adios_steps = n_steps;
    map->map = (short **)(calloc(n_steps, sizeof(short*)));
    if (map->map == NULL)
    {
        pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                "Initializing interval map for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                "Out of memory allocating %lld bytes for the map data",
                file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)(n_steps * sizeof(short*)));

        return NULL;
    }

    for (size_t i = 0; i < map->n_adios_steps; i++)
    {
        map->map[i] = (short *)calloc(2, sizeof(short));
        if (map->map[i] == NULL)
        {
            pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                    "Initializing interval map for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                    "Out of memory allocating %lld bytes for an entry (index = %ld) of the map data",
                    file->adios_vars[varid].name, varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)(2 * sizeof(short*)), i);

            return NULL;
        }

        map->map[i][0] = -1;
        map->map[i][1] = -1;
    }

    return map;
}

static int adios_get_step_info(file_desc_t *file, int varid, size_t adios_step, size_t n_adios_steps)
{
    /* Get relevant adios step */
    assert(file->adios_vars[varid].interval_map == NULL);
    file->adios_vars[varid].interval_map = init_interval_map(file, varid, n_adios_steps);

    /* Try to look up the frame_id */
    char const *frame_id_name = adios_name(adios_pio_track_frame_id_prefix, file->adios_vars[varid].name, "");
    adios2_variable *variableH = adios2_inquire_variable(file->ioH, frame_id_name);
    if (variableH)
    {
        size_t block_size = 0;
        adios2_error adiosErr = adios2_selection_size(&block_size, variableH);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting step info for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to return the minimum required allocation for the current selection (adios2_error=%s)",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        adios2_type type;
        adiosErr = adios2_variable_type(&type, variableH);
        if (adiosErr != adios2_error_none)
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting step info for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to retrieve variable type (adios2_error=%s)",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
        }

        if (type == adios2_type_int32_t)
        {
            int32_t *frame = (int32_t *)calloc(block_size, sizeof(int32_t));
            if (frame == NULL)
            {
                return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                               "Getting step info for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                               "Out of memory allocating %lld bytes for frame",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, (long long int)(block_size * sizeof(int32_t)));
            }

            adiosErr = adios2_get(file->engineH, variableH, frame, adios2_mode_sync);
            if (adiosErr != adios2_error_none)
            {
                return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Getting step info for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to get data associated with a variable from an engine (adios2_error=%s)",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
            }

            if (block_size == 1 && frame[0] == -1) /* Special case */
                update_interval_map(file->adios_vars[varid].interval_map, 0, 0, adios_step);
            else
                update_interval_map(file->adios_vars[varid].interval_map, frame[0], frame[block_size - 1], adios_step);

            free(frame);
        }
        else
        {
            return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Getting step info for variable (%s, varid = %d) in file (%s, ncid=%d) using ADIOS iotype failed. "
                           "Unsupported ADIOS type (%x)",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, type);
        }
    }
    else
    {
        char const *var_name = adios_name(adios_pio_var_prefix, file->adios_vars[varid].name, "");
        variableH = adios2_inquire_variable(file->ioH, var_name);
        if (variableH)
            update_interval_map(file->adios_vars[varid].interval_map, 0, 0, adios_step);
    }

    return 0;
}

static int adios_get_num_decomp_blocks(file_desc_t *file)
{
    adios2_variable *variableH = adios2_inquire_variable(file->ioH, "/__pio__/info/block_list");
    if (variableH == NULL)
    {
        return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                       "Getting number of decomposition blocks in file (%s, ncid=%d) using ADIOS iotype failed. "
                       "/__pio__/info/block_list is missing",
                       pio_get_fname_from_file(file), file->pio_ncid);
    }

    adios2_varinfo *info_block_list = adios2_inquire_blockinfo(file->engineH, variableH, 0);
    if (info_block_list == NULL)
    {
        return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                       "Getting number of decomposition blocks (%s, ncid=%d) using ADIOS iotype failed. "
                       "The low level (ADIOS) I/O library call failed to get the list of blocks for a variable in a given step (NULL pointer returned)",
                       pio_get_fname_from_file(file), file->pio_ncid);
    }

    file->adios_reader_num_decomp_blocks = info_block_list->nblocks;

    /* Free adios2_varinfo structure (this ADIOS2 API returns void) */
    adios2_free_blockinfo(info_block_list);

    return PIO_NOERR;
}

static int adios_get_decomp_storage_info(file_desc_t *file)
{
    adios2_variable *variableH = adios2_inquire_variable(file->ioH, "/__pio__/info/decomp_stored");

    /* Maintains backward compatibility with earlier generated BP files where decomposition storage
     * information is absent. */
    if (variableH == NULL)
    {
        file->store_adios_decomp = true;
        return PIO_NOERR;
    }

    int info_decomp_stored = -1;
    adios2_error adiosErr = adios2_get(file->engineH, variableH, &info_decomp_stored, adios2_mode_sync);
    if (adiosErr != adios2_error_none)
    {
        return pio_err(NULL, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                       "Getting decomposition storage info in file (%s, ncid=%d) using ADIOS iotype failed. "
                       "The low level (ADIOS) I/O library call failed to get data associated with a variable from an engine (adios2_error=%s)",
                       pio_get_fname_from_file(file), file->pio_ncid, convert_adios2_error_to_string(adiosErr));
    }
    assert(info_decomp_stored == 0 || info_decomp_stored == 1);

    file->store_adios_decomp = (info_decomp_stored == 0)? false : true;

    return PIO_NOERR;
}

static size_t adios_read_vars_attrs(file_desc_t *file, size_t attr_size, char *const *attr_names)
{
    size_t current_var_cnt = 0;
    for (size_t i = 0; i < attr_size; i++)
    {
        if (strstr(attr_names[i], adios_pio_var_prefix) != NULL && strstr(attr_names[i], adios_def_ndims_suffix))
        {
            int full_length = strlen(attr_names[i]);
            int prefix_length = strlen(adios_pio_var_prefix);
            int suffix_length = strlen(adios_def_ndims_suffix);
            int sub_length = full_length - prefix_length - suffix_length;

            char *name_tmp = (char *) calloc(sub_length + 1, 1);
            if (name_tmp == NULL)
            {
                return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                               "Reading ADIOS variables from attributes in file (%s, ncid=%d) failed. "
                               "Out of memory allocating %lld bytes for a temporary name",
                               pio_get_fname_from_file(file), file->pio_ncid, (long long int)(sub_length + 1));
            }

            char *pos = attr_names[i] + prefix_length;
            strncpy(name_tmp, pos, sub_length);

            /* Check that we do not have it already */
            bool found = false;
            for (int varid = 0; varid < file->num_vars; varid++)
            {
                if (strncmp(name_tmp, file->adios_vars[varid].name, sub_length) == 0)
                {
                    found = true;
                    break;
                }
            }

            if (found)
            {
                free(name_tmp);
                continue;
            }

            file->adios_vars[file->num_vars].name = (char *) calloc(sub_length + 1, 1);
            if (file->adios_vars[file->num_vars].name == NULL)
            {
                return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                               "Reading ADIOS variables from attributes in file (%s, ncid=%d) failed. "
                               "Out of memory allocating %lld bytes for variable (id = %d) name",
                               pio_get_fname_from_file(file), file->pio_ncid, (long long int)(sub_length + 1), file->num_vars);
            }

            strncpy(file->adios_vars[file->num_vars].name, name_tmp, sub_length);
            free(name_tmp);

            file->num_vars++;
            current_var_cnt++;
        }
    }

    return current_var_cnt;
}

static size_t adios_read_vars_vars(file_desc_t *file, size_t var_size, char *const *var_names)
{
    size_t current_var_cnt = 0;
    for (size_t i = 0; i < var_size; i++)
    {
        /* Strings that start with /__pio__/var/ are variables */
        if (strstr(var_names[i], adios_pio_var_prefix) != NULL)
        {
            int sub_length = strlen(var_names[i]) - strlen(adios_pio_var_prefix);
            int full_length = strlen(var_names[i]);
            int prefix_length = strlen(adios_pio_var_prefix);

            /* Check that we do not have it already */
            char *name_tmp = (char *) calloc(sub_length + 1, 1);
            if (name_tmp == NULL)
            {
                return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                               "Reading ADIOS variables from variables in file (%s, ncid=%d) failed. "
                               "Out of memory allocating %lld bytes for a temporary name",
                               pio_get_fname_from_file(file), file->pio_ncid, (long long int)(sub_length + 1));
            }

            char *pos = var_names[i] + prefix_length;
            strncpy(name_tmp, pos, sub_length);

            bool found = false;
            for (int varid = 0; varid < file->num_vars; varid++)
            {
                if (strncmp(name_tmp, file->adios_vars[varid].name, sub_length) == 0)
                {
                    found = true;
                    break;
                }
            }

            if (found)
            {
                free(name_tmp);
                continue;
            }

            file->adios_vars[file->num_vars].name = (char *) calloc(sub_length + 1, 1);
            if (file->adios_vars[file->num_vars].name == NULL)
            {
                return pio_err(NULL, file, PIO_ENOMEM, __FILE__, __LINE__,
                               "Reading ADIOS variables from variables in file (%s, ncid=%d) failed. "
                               "Out of memory allocating %lld bytes for variable (id = %d) name",
                               pio_get_fname_from_file(file), file->pio_ncid, (long long int)(sub_length + 1), file->num_vars);
            }

            strncpy(file->adios_vars[file->num_vars].name, name_tmp, sub_length);
            free(name_tmp);

            file->num_vars++;
            current_var_cnt++;
        }
    }

    return current_var_cnt;
}
#endif

/**
 * Open an existing file using PIO library. This is an internal
 * function. Depending on the value of the retry parameter, a failed
 * open operation will be handled differently. If retry is non-zero,
 * then a failed attempt to open a file with netCDF-4 (serial or
 * parallel), or parallel-netcdf will be followed by an attempt to
 * open the file as a serial classic netCDF file. This is an important
 * feature to some NCAR users. The functionality is exposed to the
 * user as PIOc_openfile() (which does the retry), and PIOc_open()
 * (which does not do the retry).
 *
 * Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid: A defined pio system descriptor (input)
 * @param ncidp: A pio file descriptor (output)
 * @param iotype: A pio output format (input)
 * @param filename: The filename to open
 * @param mode: The netcdf mode for the open operation
 * @param retry: non-zero to automatically retry with netCDF serial
 * classic.
 *
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Jim Edwards, Ed Hartnett
 */
int PIOc_openfile_retry_impl(int iosysid, int *ncidp, int *iotype, const char *filename,
                        int mode, int retry)
{
    char tname[SPIO_TIMER_MAX_NAME];
    iosystem_desc_t *ios;      /* Pointer to io system information. */
    file_desc_t *file;         /* Pointer to file information. */
    int imode;                 /* Internal mode val for netcdf4 file open. */
    int mpierr = MPI_SUCCESS;  /** Return code from MPI function codes. */
    int ierr = PIO_NOERR;      /* Return code from function calls. */
    int ierr2 = PIO_NOERR;      /* Return code from function calls. */

    /* Get the IO system info from the iosysid. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Opening file (%s) failed. Invalid iosystem id (%d) provided", (filename) ? filename : "UNKNOWN", iosysid);
    }

    /* User must provide valid input for these parameters. */
    if (!ncidp || !iotype || !filename || strlen(filename) > PIO_MAX_NAME)
    {
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__,
                        "Opening file (%s) failed. Invalid arguments provided. ncidp is %s (expected not NULL), iotype is %s (expected not NULL), filename is %s (expected not NULL), filename length = %lld (expected <= %d)", (filename) ? filename : "UNKNOWN", PIO_IS_NULL(ncidp), PIO_IS_NULL(iotype), PIO_IS_NULL(filename), (filename) ? ((unsigned long long )strlen(filename)) : 0, (int )PIO_MAX_NAME);
    }

    /* A valid iotype must be specified. */
    if (!iotype_is_valid(*iotype))
    {
        char avail_iotypes[PIO_MAX_NAME + 1];
        PIO_get_avail_iotypes(avail_iotypes, PIO_MAX_NAME);
        return pio_err(ios, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__,
                        "Opening file (%s) failed. Invalid iotype (%s:%d) specified. Available iotypes are : %s", filename, pio_iotype_to_string(*iotype), *iotype, avail_iotypes);
    }

    spio_ltimer_start(ios->io_fstats->rd_timer_name);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    LOG((2, "PIOc_openfile_retry iosysid = %d iotype = %d filename = %s mode = %d retry = %d",
         iosysid, *iotype, filename, mode, retry));

    /* Allocate space for the file info. */
    if (!(file = (file_desc_t *) calloc(sizeof(file_desc_t), 1)))
    {
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Opening file (%s) failed. Out of memory allocating %lld bytes for the file structure", filename, (unsigned long long) (sizeof(*file)));
    }

    file->io_fstats = (spio_io_fstats_summary_t *) calloc(sizeof(spio_io_fstats_summary_t), 1);
    if(!(file->io_fstats))
    {
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Opening file (%s) failed. Out of memory allocating %lld bytes for caching file I/O statistics", filename, (unsigned long long) (sizeof(spio_io_fstats_summary_t)));
    }

    /* Fill in some file values. */
    file->fh = -1;
    file->reserve_extra_header_space = false; /* Set to true for creating output NetCDF files only. */
    file->is_reopened = true;
    strncpy(file->fname, filename, PIO_MAX_NAME);
    ierr = pio_create_uniq_str(ios, NULL, tname, SPIO_TIMER_MAX_NAME, "tmp_", "_file");
    if(ierr != PIO_NOERR)
    {
        /* Not a fatal error */
        LOG((0, "Creating a unique name for the write timer for file (%s, ncid=%d) failed, ret = %d", file->fname, file->pio_ncid, ierr));
        tname[0] = '\0';
    }

    snprintf(file->io_fstats->wr_timer_name, SPIO_TIMER_MAX_NAME, "PIO:wr_%s", tname);
    snprintf(file->io_fstats->rd_timer_name, SPIO_TIMER_MAX_NAME, "PIO:rd_%s", tname);
    snprintf(file->io_fstats->tot_timer_name, SPIO_TIMER_MAX_NAME, "PIO:tot_%s", tname);

    /* FIXME: Files can be opened for rds and writes */
    spio_ltimer_start(file->io_fstats->rd_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);

    file->iotype = *iotype;
#ifdef _ADIOS2
    if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
    {
        /* Trying to open a file with adios unless ADIOS_BP2NC_TEST option is enabled for unit tests */
        bool adios2_file_exist = false;

#ifndef _ADIOS_BP2NC_TEST
        adios2_error adiosErr = adios2_error_none;
        char declare_name[PIO_MAX_NAME] = {'\0'};
        char bpname[PIO_MAX_NAME] = {'\0'};
        struct stat sd;

        strcat(bpname, filename);
        strcat(bpname, ".bp");

        if (0 == stat(bpname, &sd))
        {
            strncpy(file->fname, bpname, PIO_MAX_NAME);
            snprintf(declare_name, PIO_MAX_NAME, "%s%lu", file->fname, get_adios2_io_cnt());
            strncpy(file->io_name_reader, declare_name, PIO_MAX_NAME);

            file->ioH = adios2_declare_io(ios->adios_readerH, file->io_name_reader);
            if (file->ioH == NULL)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to declare a new io handler",
                               filename);
            }

            adiosErr = adios2_set_parameter(file->ioH, "BufferVType", "chunk");
            if (adiosErr != adios2_error_none)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to set a single parameter (adios2_error=%s)",
                               filename, convert_adios2_error_to_string(adiosErr));
            }

            adiosErr = adios2_set_parameter(file->ioH, "BufferChunkSize", "1Gb");
            if (adiosErr != adios2_error_none)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to set a single parameter (adios2_error=%s)",
                               filename, convert_adios2_error_to_string(adiosErr));
            }

            adiosErr = adios2_set_parameter(file->ioH, "InitialBufferSize", "1Gb");
            if (adiosErr != adios2_error_none)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to set a single parameter (adios2_error=%s)",
                               filename, convert_adios2_error_to_string(adiosErr));
            }

            adiosErr = adios2_set_parameter(file->ioH, "OpenTimeoutSecs", "1");
            if (adiosErr != adios2_error_none)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to set a single parameter (adios2_error=%s)",
                               filename, convert_adios2_error_to_string(adiosErr));
            }

            adiosErr = adios2_set_engine(file->ioH, "BP5");
            if (adiosErr != adios2_error_none)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, NULL, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to set the engine type for current io handler (adios2_error=%s)",
                               filename, convert_adios2_error_to_string(adiosErr));
            }

            adios2_file_exist = true;

            file->engineH = adios2_open(file->ioH, file->fname, adios2_mode_read);
            if (file->engineH == NULL)
                adios2_file_exist = false; /* Failed to open with adios2 trying pnetcdf */
            else
                strncpy(file->fname, bpname, PIO_MAX_NAME);
        }
#endif

        if (!adios2_file_exist)
        {
            /* Fall back to open as pnetcdf */
#ifdef _PNETCDF
            file->iotype = PIO_IOTYPE_PNETCDF;
#else
#ifdef _NETCDF4
#ifdef _MPISERIAL
            file->iotype = PIO_IOTYPE_NETCDF4C;
#else
            file->iotype = PIO_IOTYPE_NETCDF4P;
#endif
#endif
#endif
        }
    }
#endif

#ifdef _HDF5
    if (file->iotype == PIO_IOTYPE_HDF5)
    {
        if (H5Fis_hdf5(filename) > 0)
        {
#ifdef _NETCDF4
            /* Use NETCDF4 IO type to read files generated with HDF5 IO type so far */
#ifdef _MPISERIAL
            file->iotype = PIO_IOTYPE_NETCDF4C;
#else
            file->iotype = PIO_IOTYPE_NETCDF4P;
#endif
#else
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__,
                            "Opening file (%s) with PIO_IOTYPE_HDF5 failed. HDF5 IO type currently requires NETCDF4 (not configured for SCORPIO) to support read operations", filename);
#endif
        }
        else
        {
#ifdef _PNETCDF
            file->iotype = PIO_IOTYPE_PNETCDF;
#else
#ifdef _NETCDF
            file->iotype = PIO_IOTYPE_NETCDF;
#endif
#endif
        }
    }
#endif

    file->iosystem = ios;
    file->mode = mode;
    /*
    file->num_unlim_dimids = 0;
    file->unlim_dimids = NULL;
    */

#ifdef _ADIOS2
    file->num_dim_vars = 0;
#endif

    for (int i = 0; i < PIO_MAX_VARS; i++)
    {
        file->varlist[i].varid = -1;
        file->varlist[i].vname[0] = '\0';
        file->varlist[i].vdesc[0] = '\0';
        file->varlist[i].rec_var = 0;
        file->varlist[i].record = -1;
        file->varlist[i].request = NULL;
        file->varlist[i].request_sz = NULL;
        file->varlist[i].nreqs = 0;
        file->varlist[i].fillvalue = NULL;
        file->varlist[i].pio_type = PIO_NAT;
        file->varlist[i].type_size = 0;
        file->varlist[i].vrsize = 0;
        file->varlist[i].rb_pend = 0;
        file->varlist[i].vrsize = 0;
        file->varlist[i].wb_pend = 0;
        file->varlist[i].use_fill = 0;
        file->varlist[i].fillbuf = NULL;
    }

#ifdef _ADIOS2
    /* Set communicator for all adios processes, process rank, and I/O master node */
    file->all_comm = ios->union_comm;
    file->all_rank = ios->union_rank;
    file->num_alltasks = ios->num_uniontasks;
#endif

    /* Set to true if this task should participate in IO (only true
     * for one task with netcdf serial files. */
    if (file->iotype == PIO_IOTYPE_NETCDF4P || file->iotype == PIO_IOTYPE_NETCDF4P_NCZARR ||
        file->iotype == PIO_IOTYPE_PNETCDF ||
        ios->io_rank == 0)
        file->do_io = 1;

    /* If async is in use, bcast the parameters from compute to I/O procs. */
    if(ios->async)
    {
        int len = strlen(filename) + 1;
        PIO_SEND_ASYNC_MSG(ios, PIO_MSG_OPEN_FILE, &ierr, len, filename, file->iotype, file->mode, retry);
        if(ierr != PIO_NOERR)
        {
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                            "Opening file (%s) failed. Sending asynchronous message, PIO_MSG_OPEN_FILE, failed on iosystem (iosysid=%d)", filename, ios->iosysid);
        }
    }

#ifdef _ADIOS2
    if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
    {
        /* Get available variables and set structures. Restrict to the first step only */
        adios2_step_status status;
        size_t nsteps = 0, step = 0;
        adios2_error err_steps = adios2_steps(&nsteps, file->engineH);
        adios2_error adiosErr = adios2_error_none;

        /* Initialize adios structure */
        assert(file->num_vars < PIO_MAX_VARS);
        for (size_t var = 0; var < PIO_MAX_VARS; var++)
        {
            file->adios_vars[var].nc_type = PIO_NAT;
            file->adios_vars[var].adios_type = adios2_type_unknown;
            file->adios_vars[var].adios_type_size = 0;
            file->adios_vars[var].nattrs = 0;
            file->adios_vars[var].ndims = 0;
            file->adios_vars[var].adios_varid = 0;
            file->adios_vars[var].decomp_varid = 0;
            file->adios_vars[var].frame_varid = 0;
            file->adios_vars[var].fillval_varid = 0;
            file->adios_vars[var].gdimids = NULL;
            file->adios_vars[var].interval_map = NULL;
        }

        file->cache_data_blocks = spio_hash(10000);
        file->cache_block_sizes = spio_hash(10000);
        file->cache_darray_info = spio_hash(10000);

        file->adios_reader_num_decomp_blocks = 0;

        file->store_adios_decomp = true;

        while (step < nsteps && adios2_begin_step(file->engineH, adios2_step_mode_read, -1.0, &status) == adios2_error_none)
        {
            file->begin_step_called = 1;
            if (status == adios2_step_status_end_of_stream)
                break;

            size_t var_size = 0;
            char **var_names = adios2_available_variables(file->ioH, &var_size);
            if (var_names == NULL)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to return an array or c strings for names of available variables",
                               filename);
            }

            adios_read_global_dimensions(ios, file, var_names, var_size);

            /* Read names of variables from variables */
            size_t nvars = adios_read_vars_vars(file, var_size, var_names);

            /* Free memory */
            for (size_t i = 0; i < var_size; i++)
                free(var_names[i]);

            free(var_names);

            /* Additionally read variables from attributes like
             * /__pio__/var/dummy_scalar_var_int/def/ndims */
            size_t attr_size = 0;
            char **attr_names = adios2_available_attributes(file->ioH, &attr_size);
            if (attr_names == NULL)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to return an array or c strings for names of available attributes",
                               filename);
            }

            nvars = adios_read_vars_attrs(file, attr_size, attr_names);

            for (size_t i = 0; i < attr_size; i++)
                free(attr_names[i]);

            free(attr_names);

            /* Initialize variables */
            for (int var_id = 0; var_id < file->num_vars; var_id++)
            {
                adios_get_nc_type(file, var_id);
                adios_get_adios_type(file, var_id);
                adios_get_ndims(file, var_id);
                adios_get_nc_op_tag(file, var_id);
                adios_get_dim_ids(file, var_id);
                adios_get_step_info(file, var_id, step, nsteps);
            }

            if (step == 0)
            {
                adios_get_num_decomp_blocks(file);
                adios_get_decomp_storage_info(file);
            }

            adiosErr = adios2_end_step(file->engineH);
            if (adiosErr != adios2_error_none)
            {
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                               "Opening file (%s) using ADIOS iotype failed. "
                               "The low level (ADIOS) I/O library call failed to terminate interaction with current step (adios2_error=%s)",
                               filename, convert_adios2_error_to_string(adiosErr));
            }

            file->begin_step_called = 0;
            step++;
        }

        int attr_id = 0;
        size_t attr_size = 0;
        char **attr_names = adios2_available_attributes(file->ioH, &attr_size);
        if (attr_names == NULL)
        {
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                           "Opening file (%s) using ADIOS iotype failed. "
                           "The low level (ADIOS) I/O library call failed to return an array or c strings for names of available attributes",
                           filename);
        }

        /** Parse global attributes */
        for (size_t i = 0; i < attr_size; i++)
        {
            /* Get global attributes */
            /* Strings that start with /__pio__/var/ are variables */
            if (strncmp(attr_names[i], adios_pio_global_prefix, strlen(adios_pio_global_prefix)) == 0)
            {
                /* Get substring from string for name */
                int sub_length = strlen(attr_names[i]) - strlen(adios_pio_global_prefix);
                int full_length = strlen(attr_names[i]);
                int prefix_length = strlen(adios_pio_global_prefix);

                file->adios_attrs[attr_id].att_name = (char *) calloc(sub_length + 1, 1);
                if (file->adios_attrs[attr_id].att_name == NULL)
                {
                    spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                    spio_ltimer_stop(file->io_fstats->rd_timer_name);
                    spio_ltimer_stop(file->io_fstats->tot_timer_name);
                    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                   "Opening file (%s) using ADIOS iotype failed. "
                                   "Out of memory allocating %lld bytes for global attribute %s",
                                   filename, (long long int)(sub_length + 1), attr_names[i]);
                }

                strncpy(file->adios_attrs[attr_id].att_name, &attr_names[i][prefix_length], sub_length);
                file->adios_attrs[attr_id].att_varid = -1;

                adios_get_attr(file, attr_id, attr_names, i);

                attr_id++;
                assert(attr_id < PIO_MAX_ATTRS);
            }

            /* Cache decomp attributes */
            if (strncmp(attr_names[i], adios_pio_var_prefix, strlen(adios_pio_var_prefix)) == 0 &&
                strstr(attr_names[i], adios_def_decomp_suffix) != NULL)
            {
                adios2_attribute const *attributeH = adios2_inquire_attribute(file->ioH, attr_names[i]);
                if (attributeH != NULL)
                {
                    size_t size_attr = 0;
                    char *attr_data = (char *) calloc(PIO_MAX_NAME, 1);
                    if (attr_data == NULL)
                    {
                        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->rd_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                       "Opening file (%s) using ADIOS iotype failed. "
                                       "Out of memory allocating %lld bytes for decomposition attribute (%s)",
                                       filename, (long long int)PIO_MAX_NAME, attr_names[i]);
                    }

                    adiosErr = adios2_attribute_data(attr_data, &size_attr, attributeH);
                    if (adiosErr != adios2_error_none)
                    {
                        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->rd_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, file, PIO_EADIOS2ERR, __FILE__, __LINE__,
                                       "Opening file (%s) using ADIOS iotype failed. "
                                       "The low level (ADIOS) I/O library call failed to retrieve attribute data pointer (adios2_error=%s)",
                                       filename, convert_adios2_error_to_string(adiosErr));
                    }

                    /* attr_data will be deleted in the cache delete operation */
                    file->cache_darray_info->put(file->cache_darray_info, attr_names[i], attr_data);
                }
            }
        }
        file->num_gattrs += attr_id;

        /* Parse variable attributes */
        for (size_t i = 0; i < attr_size; i++)
        {
            /* Get variable attribute */
            char suffix_att_name[] = "/";

            for (size_t var_cnt = 0; var_cnt < file->num_vars; var_cnt++)
            {
                char *attr_full_prefix = adios_name(adios_pio_var_prefix, file->adios_vars[var_cnt].name,
                                                    suffix_att_name);
                if (strncmp(attr_names[i], attr_full_prefix, strlen(attr_full_prefix)) == 0)
                {
                    int sub_length = strlen(attr_names[i]) - strlen(attr_full_prefix);
                    int full_length = strlen(attr_names[i]);
                    int prefix_length = strlen(attr_full_prefix);
                    if (strrchr(attr_names[i], '/') > &attr_names[i][prefix_length])
                        continue;

                    file->adios_attrs[attr_id].att_name = (char *) calloc(sub_length + 1, 1);
                    if (file->adios_attrs[attr_id].att_name == NULL)
                    {
                        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                        spio_ltimer_stop(file->io_fstats->rd_timer_name);
                        spio_ltimer_stop(file->io_fstats->tot_timer_name);
                        return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                       "Opening file (%s) using ADIOS iotype failed. "
                                       "Out of memory allocating %lld bytes for attribute (id = %d) name",
                                       filename, (long long int)(sub_length + 1), attr_id);
                    }

                    strncpy(file->adios_attrs[attr_id].att_name, &attr_names[i][prefix_length], sub_length);
                    file->adios_attrs[attr_id].att_varid = var_cnt;
                    adios_get_attr(file, attr_id, attr_names, i);

                    assert(attr_id < PIO_MAX_ATTRS);
                    attr_id++;
                }
            }

            free(attr_names[i]);
        }

        free(attr_names);
        file->num_attrs += attr_id;
    }
#endif

    /* If this is an IO task, then call the netCDF function. */
    if (ios->ioproc)
    {
        switch (file->iotype)
        {
#ifdef _NETCDF4

        case PIO_IOTYPE_NETCDF4P:
#ifdef _MPISERIAL
            ierr = nc_open(filename, file->mode, &file->fh);
#else
            imode = file->mode |  NC_MPIIO;
            if ((ierr = nc_open_par(filename, imode, ios->io_comm, ios->info, &file->fh)))
                break;
            file->mode = imode;

            /* Check the vars for valid use of unlim dims. */
            if ((ierr = check_unlim_use(file->fh)))
                break;
            LOG((2, "PIOc_openfile_retry:nc_open_par filename = %s mode = %d imode = %d ierr = %d",
                 filename, file->mode, imode, ierr));
#endif
            break;

        case PIO_IOTYPE_NETCDF4P_NCZARR:
            file->mode = file->mode |  NC_NETCDF4;
            LOG((2, "Calling nc_open_par (nczarr) io_comm = %d mode = %d fh = %d",
                 ios->io_comm, file->mode, file->fh));
            ierr = PIO_ENOTBUILT;
#ifndef _MPISERIAL
#if PIO_USE_NETCDF4_NCZARR
            {
              std::string filename_nczarr = spio_get_nczarr_fname(filename);
              if(!filename_nczarr.empty()){
                ierr = nc_open_par(filename_nczarr.c_str(), file->mode,
                                      ios->io_comm, ios->info, &file->fh);
                if(ierr != NC_NOERR){
                  /* The file being opened may not be NCZarr, for now retry with NetCDF4 */
                  file->iotype = PIO_IOTYPE_NETCDF4P;
                  ierr = nc_open_par(filename, file->mode, ios->io_comm, ios->info, &file->fh);
                }
              }
            }
            LOG((2, "nc_open_par returned %d file->fh = %d", ierr, file->fh));
#else
            LOG((2, "nc_open_par returned %d, NCZarr support not built/enabled", ierr));
#endif
#endif
            break;
        case PIO_IOTYPE_NETCDF4C:
            if (ios->io_rank == 0)
            {
                imode = file->mode | NC_NETCDF4;
                if ((ierr = nc_open(filename, imode, &file->fh)))
                    break;
                file->mode = imode;
                /* Check the vars for valid use of unlim dims. */
                if ((ierr = check_unlim_use(file->fh)))
                    break;                    
            }
            break;
#endif /* _NETCDF4 */

#ifdef _NETCDF
        case PIO_IOTYPE_NETCDF:
            if (ios->io_rank == 0)
                ierr = nc_open(filename, file->mode, &file->fh);
            break;
#endif /* _NETCDF */

#ifdef _PNETCDF
        case PIO_IOTYPE_PNETCDF:
            ierr = ncmpi_open(ios->io_comm, filename, file->mode, ios->info, &file->fh);

            // This should only be done with a file opened to append
            if (ierr == PIO_NOERR && (file->mode & PIO_WRITE))
            {
                if (ios->iomaster == MPI_ROOT)
                    LOG((2, "%d Setting IO buffer %ld", __LINE__, pio_buffer_size_limit));

                /* Please refer to a previous related comment for why we ensure a minimum buffer size (16 MiB) for
                   buffered put APIs in PnetCDF.
                 */
                const PIO_Offset min_bufsize = 16777216; /* Minimum buffer size (16 MiB) for buffered put APIs in PnetCDF */
                ierr = ncmpi_buffer_attach(file->fh, (pio_buffer_size_limit > min_bufsize)? pio_buffer_size_limit : min_bufsize);
            }
            LOG((2, "ncmpi_open(%s) : fd = %d", filename, file->fh));
            break;
#endif
#ifdef _ADIOS2
        case PIO_IOTYPE_ADIOS:
        case PIO_IOTYPE_ADIOSC:
            break; /* This case has been handled above */
#endif
        default:
            {
                char avail_iotypes[PIO_MAX_NAME + 1];
                int tmp_iotype = file->iotype;

                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);

                free(file->io_fstats);
                free(file);
                PIO_get_avail_iotypes(avail_iotypes, PIO_MAX_NAME);
                return pio_err(ios, NULL, PIO_EBADIOTYPE, __FILE__, __LINE__,
                                "Opening file (%s) failed. Invalid iotype (%s:%d) specified. Available iotypes are : %s", filename, pio_iotype_to_string(tmp_iotype), tmp_iotype, avail_iotypes);
            }
        }

        /* If the caller requested a retry, and we failed to open a
           file due to an incompatible type of NetCDF, try it once
           with just plain old basic NetCDF. */
        if (retry)
        {
#ifdef _NETCDF
            LOG((2, "retry error code ierr = %d io_rank %d", ierr, ios->io_rank));
            /* Bcast error code from io rank 0 to all io procs */
            mpierr = MPI_Bcast(&ierr, 1, MPI_INT, 0, ios->io_comm);
            if(mpierr != MPI_SUCCESS){
                spio_ltimer_stop(ios->io_fstats->rd_timer_name);
                spio_ltimer_stop(ios->io_fstats->tot_timer_name);
                spio_ltimer_stop(file->io_fstats->rd_timer_name);
                spio_ltimer_stop(file->io_fstats->tot_timer_name);
                return check_mpi(NULL, file, ierr, __FILE__, __LINE__);
            }
            if ((ierr != NC_NOERR) && (file->iotype != PIO_IOTYPE_NETCDF))
            {
                /* reset file markers for NETCDF on all IO tasks */
                file->iotype = PIO_IOTYPE_NETCDF;

                /* FIXME: The changes below to modify the user-specified
                 * iotype needs to be propogated to all tasks. */
                /* modify the user-specified iotype on all tasks */
                /* Modifying the user-specified iotype unfortunately
                 * causes some E3SM model components to reset the iotype
                 * to NetCDF for all subsequent reads and writes
                 * (eventhough only some files read are in the NetCDF4
                 * format)
                 * Commenting this out for now, until we introduce
                 * PIO_IOTYPE_ANY that would allow users to explicitly
                 * specify that the lib can internally modify iotypes
                 */
                /* *iotype = PIO_IOTYPE_NETCDF; */

                /* open netcdf file serially on main task */
                if (ios->io_rank == 0)
                {
                    char errmsg[PIO_MAX_NAME];

                    ierr2 = PIOc_strerror_impl(ierr, errmsg, PIO_MAX_NAME);
                    printf("PIO: WARNING: Opening file (%s) with iotype=%d (%s) failed (ierr=%d, %s). Retrying with iotype=PIO_IOTYPE_NETCDF\n", filename, *iotype, pio_iotype_to_string(*iotype), ierr, ((ierr2 == PIO_NOERR) ? errmsg : ""));
                    ierr = nc_open(filename, file->mode, &file->fh);
                    if(ierr == NC_NOERR)
                    {
                        printf("PIO: WARNING: Opening file (%s) with iotype=%d (%s) failed. But retrying with iotype PIO_IOTYPE_NETCDF was successful. Switching iotype to PIO_IOTYPE_NETCDF for this file\n", filename, *iotype, pio_iotype_to_string(*iotype));
                    }
                }
                else
                {
                    /* reset ierr */
                    ierr = PIO_NOERR;

                    file->do_io = 0;
                }
            }
            LOG((2, "retry nc_open(%s) : fd = %d, iotype = %d, do_io = %d, ierr = %d",
                 filename, file->fh, file->iotype, file->do_io, ierr));
#endif /* _NETCDF */
        }
    }

    /* Broadcast open mode to all tasks. */
    if ((mpierr = MPI_Bcast(&file->mode, 1, MPI_INT, ios->ioroot, ios->my_comm)))
    {
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        free(file->io_fstats);
        free(file);
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }

    if (retry)
    {
        /* Broadcast IO type (might be switched on IO tasks) to all tasks. */
        if ((mpierr = MPI_Bcast(&file->iotype, 1, MPI_INT, ios->ioroot, ios->my_comm)))
        {
            spio_ltimer_stop(ios->io_fstats->rd_timer_name);
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->rd_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            free(file->io_fstats);
            free(file);
            return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
        }
    }

    ierr = check_netcdf(ios, file, ierr, __FILE__, __LINE__);
    /* If there was an error, free allocated memory and deal with the error. */
    if(ierr != PIO_NOERR){
        int tmp_iotype = file->iotype;
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        free(file->io_fstats);
        free(file);
        LOG((1, "PIOc_openfile_retry failed, ierr = %d", ierr));
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                        "Opening file (%s) with iotype %d (%s) failed. The low level I/O library call failed", filename, tmp_iotype, pio_iotype_to_string(tmp_iotype));;
    }

    /* Add this file to the list of currently open files. */
    MPI_Comm comm = MPI_COMM_NULL;
    if(ios->async)
    {
        /* For asynchronous I/O service, since file ids are passed across
         * disjoint comms we need it to be unique across the union comm
         */
        comm = ios->union_comm;
    }

    /* Initialize the multi-variable cache used to cache data for
     * multiple variables before writing it to the output file
     * Note: The MVCache is not used by ADIOS or a file opened
     *       as "read only"
     */
    spio_file_mvcache_init(file);

    *ncidp = pio_add_to_file_list(file, comm);

    LOG((2, "Opened file %s file->pio_ncid = %d file->fh = %d ierr = %d",
         filename, file->pio_ncid, file->fh, ierr));

    /* Set ncid to ADIOS attributes */
    if ((file->iotype == PIO_IOTYPE_ADIOS) || (file->iotype == PIO_IOTYPE_ADIOSC))
    {
#ifdef _ADIOS2
        for (int i = 0; i < file->num_attrs; i++)
            file->adios_attrs[i].att_ncid = *ncidp;
#endif
    }

    /* Check if the file has unlimited dimensions */
    if(!ios->async || !ios->ioproc)
    {
        spio_ltimer_stop(ios->io_fstats->rd_timer_name);
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        spio_ltimer_stop(file->io_fstats->rd_timer_name);
        spio_ltimer_stop(file->io_fstats->tot_timer_name);
        ierr = PIOc_inq_unlimdims_impl(*ncidp, &(file->num_unlim_dimids), NULL);
        if(ierr != PIO_NOERR)
        {
            spio_file_mvcache_finalize(file);
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                              "Opening file (%s) failed. Although the file was opened successfully, querying the number of unlimited dimensions in the file failed", filename);
        }
        if(file->num_unlim_dimids > 0)
        {
            file->unlim_dimids = (int *)malloc(file->num_unlim_dimids * sizeof(int));
            if(!file->unlim_dimids)
            {
                spio_file_mvcache_finalize(file);
                return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__,
                                "Opening file (%s) failed. Out of memory allocating %lld bytes for caching the unlimited dimension ids", filename, (unsigned long long) (file->num_unlim_dimids * sizeof(int)));
            }
            ierr = PIOc_inq_unlimdims_impl(*ncidp, NULL, file->unlim_dimids);
            if(ierr != PIO_NOERR)
            {
                spio_file_mvcache_finalize(file);
                return pio_err(ios, file, ierr, __FILE__, __LINE__,
                                "Opening file (%s) failed. Although the file was opened successfully, querying the unlimited dimensions in the file failed", filename);
            }
        }
        spio_ltimer_start(ios->io_fstats->rd_timer_name);
        spio_ltimer_start(ios->io_fstats->tot_timer_name);
        spio_ltimer_start(file->io_fstats->rd_timer_name);
        spio_ltimer_start(file->io_fstats->tot_timer_name);
        LOG((3, "File has %d unlimited dimensions", file->num_unlim_dimids));
    }

    spio_ltimer_stop(ios->io_fstats->rd_timer_name);
    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->rd_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return ierr;
}

/**
 * Internal function used when opening an existing file. This function
 * is called by PIOc_openfile() and PIOc_openfile2(). It opens the
 * file and then learns some things about the metadata in that file.
 *
 * Input parameters are read on comp task 0 and ignored elsewhere.
 *
 * @param iosysid: A defined pio system descriptor (input)
 * @param ncidp: A pio file descriptor (output)
 * @param iotype: A pio output format (input)
 * @param filename: The filename to open
 * @param mode: The netcdf mode for the open operation
 * @param retry: non-zero to automatically retry with netCDF serial
 * classic.
 *
 * @return 0 for success, error code otherwise.
 * @ingroup PIO_openfile
 * @author Ed Hartnett
 */
int openfile_int(int iosysid, int *ncidp, int *iotype, const char *filename,
                 int mode, int retry)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    /* Get the IO system info from the iosysid. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Opening file (%s) failed. Invalid iosystem id (%d) provided", (filename) ? filename : "UNKNOWN", iosysid);
    }
    
    /* Open the file. */
    if ((ierr = PIOc_openfile_retry_impl(iosysid, ncidp, iotype, filename, mode, retry)))
    {
        return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                        "Opening file (%s) failed. Internal error", (filename) ? filename : "UNKNOWN");
    }

    return PIO_NOERR;
}

/**
 * Internal function to provide inq_type function for pnetcdf.
 *
 * @param ncid ignored because pnetcdf does not have user-defined
 * types.
 * @param xtype type to learn about.
 * @param name pointer that gets name of type. Ignored if NULL.
 * @param sizep pointer that gets size of type. Ignored if NULL.
 * @returns 0 on success, error code otherwise.
 */
int spio_pnetcdf_inq_type(int ncid, nc_type xtype, char *name,
                          PIO_Offset *sizep)
{
    int typelen;

    switch (xtype)
    {
    case NC_UBYTE:
    case NC_BYTE:
    case NC_CHAR:
        typelen = 1;
        break;
    case NC_SHORT:
    case NC_USHORT:
        typelen = 2;
        break;
    case NC_UINT:
    case NC_INT:
    case NC_FLOAT:
        typelen = 4;
        break;
    case NC_UINT64:
    case NC_INT64:
    case NC_DOUBLE:
        typelen = 8;
        break;
    default:
        return PIO_EBADTYPE;
    }

    /* If pointers were supplied, copy results. */
    if (sizep)
        *sizep = typelen;
    if (name)
        strcpy(name, "some type");

    return PIO_NOERR;
}

/**
 * This is an internal function that handles both PIOc_enddef and
 * PIOc_redef.
 *
 * @param ncid the ncid of the file to enddef or redef
 * @param is_enddef set to non-zero for enddef, 0 for redef.
 * @returns PIO_NOERR on success, error code on failure. */
int spio_change_def(int ncid, int is_enddef)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ierr = PIO_NOERR;  /* Return code from function calls. */

    LOG((2, "spio_change_def ncid = %d is_enddef = %d", ncid, is_enddef));

    /* Find the info about this file. When I check the return code
     * here, some tests fail. ???*/
    if ((ierr = pio_get_file(ncid, &file)))
    {
        return pio_err(NULL, NULL, ierr, __FILE__, __LINE__,
                        "Changing the define mode for file (ncid = %d) failed. Invalid file id", ncid);
    }
    assert(file);
    ios = file->iosystem;
    assert(ios);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);
    spio_ltimer_start(file->io_fstats->tot_timer_name);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = is_enddef ? PIO_MSG_ENDDEF : PIO_MSG_REDEF;
        
        PIO_SEND_ASYNC_MSG(ios, msg, &ierr, ncid);
        if(ierr != PIO_NOERR)
        {
            LOG((1, "Error sending async msg for PIO_MSG_ENDDEF/PIO_MSG_REDEF"));
            spio_ltimer_stop(ios->io_fstats->tot_timer_name);
            spio_ltimer_stop(file->io_fstats->tot_timer_name);
            return pio_err(ios, NULL, ierr, __FILE__, __LINE__,
                            "Changing the define mode for file (%s) failed. Error sending async msg, PIO_MSG_ENDDEF/PIO_MSG_REDEF, on iosystem (iosysid=%d)", pio_get_fname_from_file(file), ios->iosysid);
        }
    }

    /* If this is an IO task, then call the netCDF function. */
    LOG((3, "spio_change_def ios->ioproc = %d", ios->ioproc));
    if (ios->ioproc)
    {
        LOG((3, "spio_change_def calling netcdf function file->fh = %d file->do_io = %d iotype = %d",
             file->fh, file->do_io, file->iotype));

        /* NetCDF and PnetCDF by default do not reserve any extra space in
         * the NetCDF file headers (compactness etc). However this can be a
         * problem (performance related) when renaming variables etc since
         * this information is stored in the NetCDF header. Adding more
         * information into the header requires the libraries to resize the
         * file, extend the header space and copy the contents of the file
         * (similar to realloc).
         *
         * The solution for the performance issue is to reserve some extra
         * space in the header when creating NetCDF files. The current calls
         * to nc_enddef()/ncmpi_enddef() need to be replaced with nc__enddef
         * ()/ncmpi__enddef() (note the double underscore).
         *
         * This reservation should be made only once: after the header section
         * is expanded, a new reservation may involve moving (shifting) data,
         * which can be very expensive if the data sections are huge.
         */
#ifdef _PNETCDF
        if (file->iotype == PIO_IOTYPE_PNETCDF)
        {
            if (is_enddef)
            {
                /* ncmpi__enddef has been available since PnetCDF 1.5.0 (PNETCDF_MIN_VER_REQD is currently 1.8.1):
                   int ncmpi__enddef(int ncid,
                                     MPI_Offset h_minfree,
                                     MPI_Offset v_align,
                                     MPI_Offset v_minfree,
                                     MPI_Offset r_align);

                   The usual call of ncmpi_enddef(ncid) is equivalent to ncmpi__enddef(ncid, 0, 0, 0, 0).

                   Call ncmpi__enddef with appropriate arguments instead of ncmpi_enddef:
                    - To reserve a sufficiently large space for the file header if it is expected
                      to expand (set h_minfree > 0 explicitly).
                      See https://github.com/E3SM-Project/scorpio/issues/436

                    - To prevent PnetCDF (versions prior to 1.13.0) from implicitly selecting the
                      file striping size to align the header extent (set v_align > 0 explicitly).
                      See https://github.com/E3SM-Project/scorpio/issues/566
                 */

                /* Sets the pad at the end of the "header" section. */
                MPI_Offset h_minfree = 0; /* Unless extra header space is requested, this can be left as default (0) */

                /* Controls the alignment of the beginning of the data section for fixed-size/record variables. */
                const MPI_Offset v_align = 4; /* For fixed-size variables, needs to be left as the default (4 bytes) */
                const MPI_Offset r_align = 4; /* For record variables, needs to be left as the default (4 bytes) */

                /* Sets the pad at the end of the data section for fixed-size variables. */
                const MPI_Offset v_minfree = 0; /* This can be left as default (0) */

                if (file->reserve_extra_header_space)
                {
                    /* The recommended size by Charlie Zender (NCO developer) is 10 KB */
                    assert(PIO_RESERVED_FILE_HEADER_SIZE >= 0);
                    h_minfree = PIO_RESERVED_FILE_HEADER_SIZE;

                    file->reserve_extra_header_space = false;
                }

                ierr = ncmpi__enddef(file->fh, h_minfree, v_align, v_minfree, r_align);
            }
            else
                ierr = ncmpi_redef(file->fh);
        }
#endif /* _PNETCDF */
#ifdef _NETCDF
        if (file->iotype != PIO_IOTYPE_PNETCDF && file->iotype != PIO_IOTYPE_ADIOS && file->iotype != PIO_IOTYPE_ADIOSC && file->iotype != PIO_IOTYPE_HDF5 && file->do_io)
        {
            if (is_enddef)
            {
                LOG((3, "spio_change_def calling nc_enddef file->fh = %d", file->fh));
                if (file->iotype == PIO_IOTYPE_NETCDF)
                {
#ifdef NETCDF_C_NC__ENDDEF_EXISTS
                    if (file->reserve_extra_header_space)
                    {
                        /* Sets the pad at the end of the "header" section.
                         * The recommended size by Charlie Zender (NCO developer) is 10 KB */
                        assert(PIO_RESERVED_FILE_HEADER_SIZE >= 0);
                        const MPI_Offset h_minfree = PIO_RESERVED_FILE_HEADER_SIZE;

                        /* Controls the alignment of the beginning of the data section for fixed-size/record variables. */
                        const size_t v_align = 4; /* For fixed-size variables, needs to be left as the default (4 bytes) */
                        const size_t r_align = 4; /* For record variables, needs to be left as the default (4 bytes) */

                        /* Sets the pad at the end of the data section for fixed-size variables. */
                        const size_t v_minfree = 0; /* This can be left as default (0) */

                        /* nc__enddef has been available since NetCDF 3.x (NETCDF_C_MIN_VER_REQD is currently 4.3.3) */
                        ierr = nc__enddef(file->fh, h_minfree, v_align, v_minfree, r_align);

                        file->reserve_extra_header_space = false;
                    }
                    else
                        ierr = nc_enddef(file->fh);
#else
                    /* CAUTION: nc__enddef may not be available on future NetCDF implementations, switch back to nc_enddef. */
                    ierr = nc_enddef(file->fh);
#endif
                }
                else
                {
                    /* We still call nc_enddef for NetCDF4 type. According to NCO user guide, nc__enddef will improve speed of
                     * future metadata expansion with classic and 64bit NetCDF files, though not necessarily with NetCDF4 files.
                     */
                    ierr = nc_enddef(file->fh);
                }
            }
            else
                ierr = nc_redef(file->fh);
        }
#endif /* _NETCDF */
#ifdef _HDF5
        if (file->iotype == PIO_IOTYPE_HDF5)
        {
            if (is_enddef)
            {
                ierr = spio_hdf5_enddef(ios, file);
            }
        }
#endif /* _HDF5 */
    }

    ierr = check_netcdf(NULL, file, ierr, __FILE__, __LINE__);
    if(ierr != PIO_NOERR){
      spio_ltimer_stop(ios->io_fstats->tot_timer_name);
      spio_ltimer_stop(file->io_fstats->tot_timer_name);
      return pio_err(ios, file, ierr, __FILE__, __LINE__,
                      "Changing the define mode for file (%s) failed. Low-level I/O library API failed", pio_get_fname_from_file(file));
    }
    LOG((3, "spio_change_def succeeded"));

    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    spio_ltimer_stop(file->io_fstats->tot_timer_name);
    return ierr;
}

/**
 * Check whether an IO type is valid for the build.
 *
 * @param iotype the IO type to check
 * @returns 0 if not valid, non-zero otherwise.
 */
int iotype_is_valid(int iotype)
{
    /* Assume it's not valid. */
    int ret = 0;

    /* Some builds include netCDF. */
#ifdef _NETCDF
    if (iotype == PIO_IOTYPE_NETCDF)
        ret++;
#endif /* _NETCDF */

    /* Some builds include netCDF-4. */
#ifdef _NETCDF4
    if (iotype == PIO_IOTYPE_NETCDF4C || iotype == PIO_IOTYPE_NETCDF4P)
        ret++;
#ifdef PIO_USE_NETCDF4_NCZARR
    if (iotype == PIO_IOTYPE_NETCDF4P_NCZARR)
        ret++;
#endif
#endif /* _NETCDF4 */

    /* Some builds include pnetcdf. */
#ifdef _PNETCDF
    if (iotype == PIO_IOTYPE_PNETCDF)
        ret++;
#endif /* _PNETCDF */

#ifdef _ADIOS2
    if ((iotype == PIO_IOTYPE_ADIOS) || (iotype == PIO_IOTYPE_ADIOSC))
        ret++;
#endif

    /* Some builds include hdf5. */
#ifdef _HDF5
    if (iotype == PIO_IOTYPE_HDF5)
        ret++;
#endif /* _HDF5 */

    return ret;
}

/**
 * Internal function to compare rearranger flow control options.
 *
 * @param opt pointer to rearranger flow control options to compare.
 * @param exp_opt pointer to rearranger flow control options with
 * expected values.
 * @return true if values in opt == values in exp_opt, false
 * otherwise.
 */
bool cmp_rearr_comm_fc_opts(const rearr_comm_fc_opt_t *opt,
                            const rearr_comm_fc_opt_t *exp_opt)
{
    bool is_same = true;

    assert(opt && exp_opt);

    if (opt->hs != exp_opt->hs)
    {
        LOG((1, "Warning rearranger hs = %s, expected = %s",
             opt->hs ? "TRUE" : "FALSE", exp_opt->hs ? "TRUE" : "FALSE"));
        is_same = false;
    }

    if (opt->isend != exp_opt->isend)
    {
        LOG((1, "Warning rearranger isend = %s, expected = %s",
             opt->isend ? "TRUE" : "FALSE", exp_opt->isend ? "TRUE" : "FALSE"));
        is_same = false;
    }

    if (opt->max_pend_req != exp_opt->max_pend_req)
    {
        LOG((1, "Warning rearranger max_pend_req = %d, expected = %d",
             opt->max_pend_req, exp_opt->max_pend_req));
        is_same = false;
    }

    return is_same;
}

/**
 * Internal function to compare rearranger options.
 *
 * @param rearr_opts pointer to rearranger options to compare
 * @param exp_rearr_opts pointer to rearranger options with the
 * expected value
 * @return true if values in rearr_opts == values in exp_rearr_opts
 * false otherwise
 */
bool cmp_rearr_opts(const rearr_opt_t *rearr_opts, const rearr_opt_t *exp_rearr_opts)
{
    bool is_same = true;

    assert(rearr_opts && exp_rearr_opts);

    if (rearr_opts->comm_type != exp_rearr_opts->comm_type)
    {
        LOG((1, "Warning rearranger comm_type = %d, expected = %d. ", rearr_opts->comm_type,
             exp_rearr_opts->comm_type));
        is_same = false;
    }

    if (rearr_opts->fcd != exp_rearr_opts->fcd)
    {
        LOG((1, "Warning rearranger fcd = %d, expected = %d. ", rearr_opts->fcd,
             exp_rearr_opts->fcd));
        is_same = false;
    }

    is_same = is_same && cmp_rearr_comm_fc_opts(&(rearr_opts->comp2io),
                                                &(exp_rearr_opts->comp2io));
    is_same = is_same && cmp_rearr_comm_fc_opts(&(rearr_opts->io2comp),
                                                &(exp_rearr_opts->io2comp));

    return is_same;
}

/**
 * Internal function to reset rearranger opts in iosystem to valid values.
 * The only values reset here are options that are not set (or of interest)
 * to the user. e.g. Setting the io2comp/comp2io settings to defaults when
 * user chooses coll for rearrangement.
 * The old default for max pending requests was DEF_P2P_MAXREQ = 64.
 *
 * @param rearr_opt pointer to rearranger options
 * @return return error if the rearr_opt is invalid
 */
int check_and_reset_rearr_opts(rearr_opt_t *rearr_opt)
{
    /* Disable handshake/isend and set max_pend_req to unlimited */
    const rearr_comm_fc_opt_t def_comm_nofc_opts =
        { false, false, PIO_REARR_COMM_UNLIMITED_PEND_REQ };
    /* Disable handshake /isend and set max_pend_req = 0 to turn off throttling */
    const rearr_comm_fc_opt_t def_coll_comm_fc_opts = { false, false, 0 };
    const rearr_opt_t def_coll_rearr_opts = {
        PIO_REARR_COMM_COLL,
        PIO_REARR_COMM_FC_2D_DISABLE,
        def_coll_comm_fc_opts,
        def_coll_comm_fc_opts
    };

    assert(rearr_opt);

    /* Reset to defaults, if needed (user did not set it correctly) */
    if (rearr_opt->comm_type == PIO_REARR_COMM_COLL)
    {
        /* Compare and log the user and default rearr opts for coll. */
        cmp_rearr_opts(rearr_opt, &def_coll_rearr_opts);
        /* Hard reset flow control options. */
        *rearr_opt = def_coll_rearr_opts;
    }
    else if (rearr_opt->comm_type == PIO_REARR_COMM_P2P)
    {
        if (rearr_opt->fcd == PIO_REARR_COMM_FC_2D_DISABLE)
        {
            /* Compare and log user and default opts. */
            cmp_rearr_comm_fc_opts(&(rearr_opt->comp2io),
                                   &def_comm_nofc_opts);
            cmp_rearr_comm_fc_opts(&(rearr_opt->io2comp),
                                   &def_comm_nofc_opts);
            /* Hard reset flow control opts to defaults. */
            rearr_opt->comp2io = def_comm_nofc_opts;
            rearr_opt->io2comp = def_comm_nofc_opts;
        }
        else if (rearr_opt->fcd == PIO_REARR_COMM_FC_1D_COMP2IO)
        {
            /* Compare and log user and default opts. */
            cmp_rearr_comm_fc_opts(&(rearr_opt->io2comp),
                                   &def_comm_nofc_opts);
            /* Hard reset io2comp dir to defaults. */
            rearr_opt->io2comp = def_comm_nofc_opts;
        }
        else if (rearr_opt->fcd == PIO_REARR_COMM_FC_1D_IO2COMP)
        {
            /* Compare and log user and default opts. */
            cmp_rearr_comm_fc_opts(&(rearr_opt->comp2io),
                                   &def_comm_nofc_opts);
            /* Hard reset comp2io dir to defaults. */
            rearr_opt->comp2io = def_comm_nofc_opts;
        }
        else
        {
            if (rearr_opt->fcd != PIO_REARR_COMM_FC_2D_ENABLE)
                return PIO_EINVAL;

            /* Don't reset if flow control is enabled in both directions
             * by user. */
        }
    }
    else
    {
        return PIO_EINVAL;
    }

    if (( (rearr_opt->comp2io.max_pend_req !=
            PIO_REARR_COMM_UNLIMITED_PEND_REQ) &&
          (rearr_opt->comp2io.max_pend_req < 0)  ) ||
        ( (rearr_opt->io2comp.max_pend_req !=
            PIO_REARR_COMM_UNLIMITED_PEND_REQ) &&
          (rearr_opt->io2comp.max_pend_req < 0)  ))
        return PIO_EINVAL;

    return PIO_NOERR;
}

/**
 * Set the rearranger options associated with an iosystem
 *
 * @param comm_type Type of communication (pt2pt/coll) used
 * by the rearranger. See PIO_REARR_COMM_TYPE for more detail.
 * Possible values are :
 * PIO_REARR_COMM_P2P (Point to point communication)
 * PIO_REARR_COMM_COLL (Collective communication)
 * @param fcd Flow control direction for the rearranger.
 * See PIO_REARR_COMM_FC_DIR for more detail.
 * Possible values are :
 * PIO_REARR_COMM_FC_2D_ENABLE : Enable flow control from
 * compute processes to io processes and vice versa
 * PIO_REARR_COMM_FC_1D_COMP2IO : Enable flow control from
 * compute processes to io processes (only)
 * PIO_REARR_COMM_FC_1D_IO2COMP : Enable flow control from
 * io processes to compute processes (only)
 * PIO_REARR_COMM_FC_2D_DISABLE : Disable flow control from
 * compute processes to io processes and vice versa.
 * @param enable_hs_c2i Enable handshake while rearranging
 * data, from compute to io processes
 * @param enable_isend_c2i Enable isends while rearranging
 * data, from compute to io processes
 * @param max_pend_req_c2i Maximum pending requests during
 * data rearragment from compute processes to io processes
 * @param enable_hs_i2c Enable handshake while rearranging
 * data, from io to compute processes
 * @param enable_isend_i2c Enable isends while rearranging
 * data, from io to compute processes
 * @param max_pend_req_i2c Maximum pending requests during
 * data rearragment from io processes to compute processes
 * @param iosysidp index of the defined system descriptor
 * @return 0 on success, otherwise a PIO error code.
 */
int PIOc_set_rearr_opts_impl(int iosysid, int comm_type, int fcd, bool enable_hs_c2i,
                        bool enable_isend_c2i, int max_pend_req_c2i,
                        bool enable_hs_i2c, bool enable_isend_i2c,
                        int max_pend_req_i2c)
{
    iosystem_desc_t *ios;
    int ret = PIO_NOERR;
    rearr_opt_t user_rearr_opts = {
        comm_type, fcd,
        {enable_hs_c2i,enable_isend_c2i, max_pend_req_c2i},
        {enable_hs_i2c, enable_isend_i2c, max_pend_req_i2c}
    };

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Setting rearranger options failed. Invalid iosystem id (%d) provided", iosysid);
    }
    assert(ios);
    spio_ltimer_start(ios->io_fstats->tot_timer_name);

    /* Perform sanity checks on the user supplied values and reset 
     * values not set (or of no interest) by the user 
     */
    ret = check_and_reset_rearr_opts(&user_rearr_opts);
    if (ret != PIO_NOERR)
    {
        spio_ltimer_stop(ios->io_fstats->tot_timer_name);
        return ret;
    }

    /* Set the options. */
    ios->rearr_opts = user_rearr_opts;

    spio_ltimer_stop(ios->io_fstats->tot_timer_name);
    return ret;
}

/* Calculate and cache the variable record size 
 * for the variable corresponding to varid
 * Note: Since this function calls many PIOc_* functions
 * only compute procs should call this function for async
 * i/o calls
 * */
int calc_var_rec_sz(int ncid, int varid)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    int ndims;
    nc_type vtype;
    PIO_Offset vtype_sz = 0;
    int ierr = PIO_NOERR, mpierr = MPI_SUCCESS;

    ierr = pio_get_file(ncid, &file);
    if(ierr != PIO_NOERR)
    {
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                        "Internal error while calculating variable record size. Invalid file id (ncid = %d) provided, unable to query the associated file", ncid);
    }
    ios = file->iosystem;
    assert(ios != NULL);

    /* Async io is still under development and write/read darrays need to
     * be implemented correctly before we remove the check below
     */
    if(ios->async)
    {
        LOG((1, "WARNING: Cannot calculate record size (not supported for async)"));
        return PIO_NOERR;
    }

    /* Calculate and cache the size of a single record/timestep */
    ierr = PIOc_inq_var_impl(ncid, varid, NULL, 0, &vtype, &ndims, NULL, NULL);
    if(ierr != PIO_NOERR)
    {
        return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Internal error while calculating the variable %s (varid=%d) record size in file %s (ncid=%d). Unable to query the number of dimensions and type of the variable.", pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), ncid);
    }
    if(ndims > 0)
    {
        int dimids[ndims];
        PIO_Offset dimlen[ndims];

        ierr = PIOc_inq_type_impl(ncid, vtype, NULL, &vtype_sz);
        if(ierr != PIO_NOERR)
        {
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Internal error while calculating the variable (%s) record size in file (%s). Unable to query type info for the variable.", pio_get_vname_from_file(file, varid), pio_get_fname_from_file(file));
        }

        ierr = PIOc_inq_vardimid_impl(ncid, varid, dimids);
        if(ierr != PIO_NOERR)
        {
            return pio_err(ios, file, ierr, __FILE__, __LINE__,
                        "Internal error while calculating the variable (%s) record size in file (%s). Unable to query the variable dimension ids.", pio_get_vname_from_file(file, varid), pio_get_fname_from_file(file));
        }

        for(int i=0; i<ndims; i++)
        {
            bool is_rec_dim = false;
            /* For record variables check if dim is an unlimited
              * dimension. For record dims set dimlen = 1
              */ 
            if(file->varlist[varid].rec_var)
            {
                for(int j=0; j<file->num_unlim_dimids; j++)
                {
                    if(dimids[i] == file->unlim_dimids[j])
                    {
                        is_rec_dim = true;
                        dimlen[i] = 1;
                        break;
                    }
                }
            }
            if(!is_rec_dim)
            {
                ierr = PIOc_inq_dim_impl(ncid, dimids[i], NULL, &(dimlen[i]));
                if(ierr != PIO_NOERR)
                {
                    return pio_err(ios, file, ierr, __FILE__, __LINE__,
                                    "Internal error while calculating the variable (%s) record size in file (%s). Unable to query the length of dimension %d.", pio_get_vname_from_file(file, varid), pio_get_fname_from_file(file), i);
                }
            }
            file->varlist[varid].vrsize = 
              ((file->varlist[varid].vrsize) ? file->varlist[varid].vrsize : 1)
              * dimlen[i]; 
        }
    }
    mpierr = MPI_Bcast(&(file->varlist[varid].vrsize), 1, MPI_OFFSET,
                        ios->ioroot, ios->my_comm);
    if(mpierr != MPI_SUCCESS)
    {
        LOG((1, "Unable to bcast vrsize"));
        return check_mpi(NULL, file, mpierr, __FILE__, __LINE__);
    }
    return ierr;
}

/* Get a description of the variable 
 * @param ncid PIO id for the file
 * @param varid PIO id for the variable
 * @param Any user string that needs to be prepended to the variable
 * description, can optionally be NULL
 * Returns a string that describes the variable associated with varid
 * - The returned string should be copied by the user since the
 *   contents of the buffer returned can change in the next call
 *   to this function.
 * */
const char *get_var_desc_str(int ncid, int varid, const char *desc_prefix)
{
    iosystem_desc_t *ios;  /* Pointer to io system information. */
    file_desc_t *file;     /* Pointer to file information. */
    static const char EMPTY_STR[] = "";
    int ierr = PIO_NOERR;

    ierr = pio_get_file(ncid, &file);
    if(ierr != PIO_NOERR)
    {
        LOG((1, "Unable to get file corresponding to ncid = %d", ncid));
        return EMPTY_STR;
    }
    ios = file->iosystem;
    assert(ios != NULL);

    snprintf(file->varlist[varid].vdesc, PIO_MAX_NAME,
              "%s %s %s %llu %llu %llu %llu %llu",
              (desc_prefix)?desc_prefix:"",
              file->varlist[varid].vname,
              file->fname,
              (unsigned long long int)file->varlist[varid].vrsize,
              (unsigned long long int)file->varlist[varid].rb_pend,
              (unsigned long long int)file->varlist[varid].wb_pend,
              (unsigned long long int)file->rb_pend,
              (unsigned long long int)file->wb_pend
              );

    return file->varlist[varid].vdesc;
}

/* A ROMIO patch from PnetCDF's E3SM-IO benchmark program
 * https://github.com/Parallel-NetCDF/E3SM-IO/blob/master/romio_patch.c */
#if defined(MPICH_NUMVERSION) && (MPICH_NUMVERSION < 30300000)
/*
 *
 *   Copyright (C) 2014 UChicgo/Argonne, LLC.
 *   See COPYRIGHT notice in top-level directory.
 */

/* utility function for creating large contiguous types: algorithim from BigMPI
 * https://github.com/jeffhammond/BigMPI */

static int type_create_contiguous_x(MPI_Count count, MPI_Datatype oldtype, MPI_Datatype * newtype)
{
    /* to make 'count' fit MPI-3 type processing routines (which take integer
     * counts), we construct a type consisting of N INT_MAX chunks followed by
     * a remainder.  e.g for a count of 4000000000 bytes you would end up with
     * one 2147483647-byte chunk followed immediately by a 1852516353-byte
     * chunk */
    MPI_Datatype chunks, remainder;
    MPI_Aint lb, extent, disps[2];
    int blocklens[2];
    MPI_Datatype types[2];

    /* truly stupendously large counts will overflow an integer with this math,
     * but that is a problem for a few decades from now.  Sorry, few decades
     * from now! */
    assert(count / INT_MAX == (int) (count / INT_MAX));
    int c = (int) (count / INT_MAX);    /* OK to cast until 'count' is 256 bits */
    int r = count % INT_MAX;

    MPI_Type_vector(c, INT_MAX, INT_MAX, oldtype, &chunks);
    MPI_Type_contiguous(r, oldtype, &remainder);

    MPI_Type_get_extent(oldtype, &lb, &extent);

    blocklens[0] = 1;
    blocklens[1] = 1;
    disps[0] = 0;
    disps[1] = c * extent * INT_MAX;
    types[0] = chunks;
    types[1] = remainder;

    MPI_Type_create_struct(2, blocklens, disps, types, newtype);

    MPI_Type_free(&chunks);
    MPI_Type_free(&remainder);

    return MPI_SUCCESS;
}

/* like MPI_Type_create_hindexed, except array_of_lengths can be a larger datatype.
 *
 * Hindexed provides 'count' pairs of (displacement, length), but what if
 * length is longer than an integer?  We will create 'count' types, using
 * contig if length is small enough, or something more complex if not */

int __wrap_ADIOI_Type_create_hindexed_x(int count,
                                 const MPI_Count array_of_blocklengths[],
                                 const MPI_Aint array_of_displacements[],
                                 MPI_Datatype oldtype, MPI_Datatype * newtype)
{
    int i, ret;
    MPI_Datatype *types;
    int *blocklens;
    int is_big = 0;

    types = (MPI_Datatype*) malloc(count * sizeof(MPI_Datatype));
    blocklens = (int*) malloc(count * sizeof(int));

    /* squashing two loops into one.
     * - Look in the array_of_blocklengths for any large values
     * - convert MPI_Count items (if they are not too big) into int-sized items
     * after this loop we will know if we can use MPI_type_hindexed or if we
     * need a more complicated BigMPI-style struct-of-chunks.
     *
     * Why not use the struct-of-chunks in all cases?  HDF5 reported a bug,
     * which I have not yet precicesly nailed down, but appears to have
     * something to do with struct-of-chunks when the chunks are small */

#ifdef USE_ORIGINAL_MPICH_3_2
    for(i=0; i<count; i++) {
        if (array_of_blocklengths[i] > INT_MAX) {
            blocklens[i] = 1;
            is_big=1;
            type_create_contiguous_x(array_of_blocklengths[i], oldtype,  &(types[i]));
        } else {
            /* OK to cast: checked for "bigness" above */
            blocklens[i] = (int)array_of_blocklengths[i];
            MPI_Type_contiguous(blocklens[i], oldtype, &(types[i]));
        }
    }

    if (is_big) {
        ret = MPI_Type_create_struct(count, blocklens, array_of_displacements,
                types, newtype);
    } else {
        ret = MPI_Type_create_hindexed(count, blocklens, array_of_displacements, oldtype, newtype);
    }
    for (i=0; i< count; i++)
        MPI_Type_free(&(types[i]));
#else
    /* See https://github.com/pmodels/mpich/pull/3089 */
    for (i = 0; i < count; i++) {
        if (array_of_blocklengths[i] > INT_MAX) {
            blocklens[i] = 1;
            is_big = 1;
            type_create_contiguous_x(array_of_blocklengths[i], oldtype, &(types[i]));
        } else {
            /* OK to cast: checked for "bigness" above */
            blocklens[i] = (int) array_of_blocklengths[i];
            types[i] = oldtype;
        }
    }

    if (is_big) {
        ret = MPI_Type_create_struct(count, blocklens, array_of_displacements, types, newtype);
        for (i = 0; i < count; i++)
            if (types[i] != oldtype)
                MPI_Type_free(&(types[i]));
    } else {
        ret = MPI_Type_create_hindexed(count, blocklens, array_of_displacements, oldtype, newtype);
    }
#endif
    free(types);
    free(blocklens);

    return ret;
}
#endif

char *spio_strdup(const char *str)
{
    if (str == NULL)
        return NULL;

    size_t n = strlen(str) + 1;
    char *dup = (char*)malloc(n);
    if (dup)
    {
        strcpy(dup, str);
    }

    return dup;
}

PIO_Offset spio_get_nc_type_size(nc_type xtype)
{
    switch (xtype)
    {
    case NC_UBYTE:
    case NC_BYTE:
    case NC_CHAR:
        return 1;
    case NC_SHORT:
    case NC_USHORT:
        return 2;
    case NC_UINT:
    case NC_INT:
    case NC_FLOAT:
        return 4;
    case NC_UINT64:
    case NC_INT64:
    case NC_DOUBLE:
        return 8;
    default:
        return -1;
    }

    return -1;
}

#ifdef _HDF5
hid_t spio_nc_type_to_hdf5_type(nc_type xtype)
{
    switch (xtype)
    {
        case NC_BYTE:   return H5T_NATIVE_UINT8;
        case NC_UBYTE:  return H5T_NATIVE_UCHAR;
        case NC_CHAR:   return H5T_NATIVE_CHAR;
        case NC_SHORT:  return H5T_NATIVE_SHORT;
        case NC_USHORT: return H5T_NATIVE_USHORT;
        case NC_INT:    return H5T_NATIVE_INT;
        case NC_UINT:   return H5T_NATIVE_UINT;
        case NC_FLOAT : return H5T_NATIVE_FLOAT;
        case NC_DOUBLE: return H5T_NATIVE_DOUBLE;
        case NC_INT64:  return H5T_NATIVE_INT64;
        case NC_UINT64: return H5T_NATIVE_UINT64;
        default: return H5I_INVALID_HID;
    }

    return H5I_INVALID_HID;
}

int spio_hdf5_create(iosystem_desc_t *ios, file_desc_t *file, const char *filename)
{
    int mpierr = MPI_SUCCESS;

    assert(ios && file && filename);
    assert(file->iotype == PIO_IOTYPE_HDF5);
    assert(ios->ioproc);

    if (file->mode & PIO_NOCLOBBER) /* Check whether HDF5 file exists */
    {
        struct stat sd;
        if (0 == stat(filename, &sd))
        {
            return pio_err(ios, NULL, PIO_EEXIST, __FILE__, __LINE__,
                           "Creating file (%s) using HDF5 iotype failed. "
                           "The HDF5 file already exists and PIO_NOCLOBBER mode is specified",
                           filename);
        }
    }
    else
    {
        /* Delete HDF5 file if it exists */
        if (ios->io_rank == 0)
        {
            struct stat sd;
            if (0 == stat(filename, &sd))
                unlink(filename);
        }

        /* Make sure that no task is trying to operate on the
         * HDF5 file while it is being deleted */
        if ((mpierr = MPI_Barrier(ios->io_comm)))
        {
            return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
        }
    }

    if (ios->info == MPI_INFO_NULL)
    {
        if ((mpierr = MPI_Info_create(&ios->info)))
        {
            return check_mpi(ios, file, mpierr, __FILE__, __LINE__);
        }
    }

    hid_t fcpl_id = H5Pcreate(H5P_FILE_CREATE);
    if (fcpl_id == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new file creation property list",
                       filename);
    }

    if (H5Pset_link_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set tracking and indexing of link creation order",
                       filename);
    }

    /* H5DSattach_scale calls (even with MPI_Barrier) might fail or hang if attribute creation
     * order is tracked or indexed. Before we have a better workaround, temporarily disable
     * tracking and indexing of attribute creation order. */
#if 0
    if (H5Pset_attr_creation_order(fcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set tracking and indexing of attribute creation order",
                       filename);
    }
#endif

    hid_t fapl_id = H5Pcreate(H5P_FILE_ACCESS);
    if (fapl_id == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new file access property list",
                       filename);
    }

    if (H5Pset_fapl_mpio(fapl_id, ios->io_comm, ios->info) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to store the user-supplied MPI IO parameters",
                       filename);
    }

    file->hdf5_file_id = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl_id, fapl_id);
    if (file->hdf5_file_id == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new HDF5 file",
                       filename);
    }

    if (H5Pclose(fcpl_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close a file creation property list",
                       filename);
    }

    if (H5Pclose(fapl_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close a file access property list",
                       filename);
    }

    /* Set up collective dataset transfer property list */
    file->dxplid_coll = H5Pcreate(H5P_DATASET_XFER);
    if (file->dxplid_coll == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new dataset transfer property list (collective data transfer)",
                       filename);
    }

    if (H5Pset_dxpl_mpio(file->dxplid_coll, H5FD_MPIO_COLLECTIVE) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set data transfer mode to collective",
                       filename);
    }

    /* Set up independent dataset transfer property list */
    file->dxplid_indep = H5Pcreate(H5P_DATASET_XFER);
    if (file->dxplid_indep == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new dataset transfer property list (independent data transfer)",
                       filename);
    }

    if (H5Pset_dxpl_mpio(file->dxplid_indep, H5FD_MPIO_INDEPENDENT) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set data transfer mode to independent",
                       filename);
    }

    /* Writing _NCProperties attribute */
    const char* attr_name = "_NCProperties";
    char nc_properties[PIO_MAX_NAME];
    unsigned int major, minor, release;

    if (H5get_libversion(&major, &minor, &release) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to retrieve the the version of the HDF5 library",
                       filename);
    }

    snprintf(nc_properties, PIO_MAX_NAME,
             "version=2,scorpio=%d.%d.%d,hdf5=%1u.%1u.%1u",
             PIO_VERSION_MAJOR, PIO_VERSION_MINOR, PIO_VERSION_PATCH,
             major, minor, release);

    hid_t attr_id;
    hsize_t asize = strlen(nc_properties);
    hid_t loc_id = file->hdf5_file_id;

    hid_t space_id = H5Screate(H5S_SCALAR);
    if (space_id == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                       filename);
    }

    hid_t h5_string_type = H5Tcopy(H5T_C_S1);
    if (h5_string_type == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to make a copy of the predefined string datatype in C",
                       filename);
    }

    if (H5Tset_size(h5_string_type, asize + 1) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set the total size (%ld bytes) for a derived C-style string datatype",
                       filename, asize + 1);
    }

    if (H5Tset_strpad(h5_string_type, H5T_STR_NULLTERM) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to define the type of padding (NULL-terminated) used for a derived C-style string datatype",
                       filename);
    }

    if (H5Tset_cset(h5_string_type, H5T_CSET_ASCII) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set the character set (US ASCII) to be used in a derived C-style string datatype",
                       filename);
    }

    /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
    htri_t att_exists = H5Aexists(loc_id, attr_name);
    if (att_exists > 0)
    {
        assert(0);
    }
    else if (att_exists == 0)
    {
        attr_id = H5Acreate2(loc_id, attr_name, h5_string_type, space_id, H5P_DEFAULT, H5P_DEFAULT);
        if (attr_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Creating file (%s) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to the file",
                           filename, attr_name);
        }
    }
    else
    {
        /* Error determining whether an attribute with a given name exists on an object */
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to determine whether an attribute (%s) exists on the file",
                       filename, attr_name);
    }

    if (H5Awrite(attr_id, h5_string_type, nc_properties) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to the file",
                       filename, attr_name);
    }

    if (H5Sclose(space_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a scalar dataspace",
                       filename);
    }

    if (H5Aclose(attr_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to the file",
                       filename, attr_name);
    }

    if (H5Tclose(h5_string_type) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Creating file (%s) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a derived C-style string datatype "
                       "used by an attribute (%s) attached to the file",
                       filename, attr_name);
    }

    return PIO_NOERR;
}

int spio_hdf5_def_var(iosystem_desc_t *ios, file_desc_t *file, const char *name,
                      nc_type xtype, int ndims, const int *dimidsp, int varid)
{
    hid_t h5_xtype;
    hid_t sid;
    hid_t dcpl_id;
    hsize_t cdim[H5S_MAX_RANK], dims[H5S_MAX_RANK], mdims[H5S_MAX_RANK];
    int i;

    assert(ios && file && name && ndims >= 0 && varid >= 0);
    assert(file->iotype == PIO_IOTYPE_HDF5);
    assert(ios->ioproc);

    for (i = 0; i < ndims; i++)
        dims[i] = mdims[i] = file->hdf5_dims[dimidsp[i]].len;

    dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
    if (dcpl_id == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new dataset creation property list",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    /* H5DSattach_scale calls (even with MPI_Barrier) might fail or hang if attribute creation
     * order is tracked or indexed. Before we have a better workaround, temporarily disable
     * tracking and indexing of attribute creation order. */
#if 0
    if (H5Pset_attr_creation_order(dcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to set tracking and indexing of attribute creation order",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }
#endif

    if (xtype == NC_CHAR)
    {
        /* String type */
        h5_xtype = H5Tcopy(H5T_C_S1);
        if (h5_xtype == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to make a copy of the predefined string datatype in C",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (H5Tset_strpad(h5_xtype, H5T_STR_NULLTERM) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to define the type of padding (NULL-terminated) used for a derived C-style string datatype",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (H5Tset_cset(h5_xtype, H5T_CSET_ASCII) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to set the character set (US ASCII) to be used in a derived C-style string datatype",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }
    }
    else
    {
        h5_xtype = spio_nc_type_to_hdf5_type(xtype);
        if (h5_xtype == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "Unsupported variable type (type=%x)",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid, xtype);
        }
    }

    file->hdf5_vars[varid].hdf5_type = h5_xtype;

    if (ndims > 0 && dims[0] == PIO_UNLIMITED)
    {
        mdims[0] = H5S_UNLIMITED;

        /* Chunk size along rec dim is always 1 */
        cdim[0] = 1;
        for (i = 1; i < ndims; i++)
            cdim[i] = mdims[i];

        if (H5Pset_chunk(dcpl_id, ndims, cdim) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to set the size of the chunks used to store a chunked layout dataset",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }
    }

    sid = H5Screate_simple(ndims, dims, mdims);
    if (sid == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    const char* dataset_name = (file->hdf5_vars[varid].alt_name == NULL)? name : file->hdf5_vars[varid].alt_name;
    file->hdf5_vars[varid].hdf5_dataset_id = H5Dcreate2(file->hdf5_file_id, dataset_name, h5_xtype, sid, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    if (file->hdf5_vars[varid].hdf5_dataset_id < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to create a new dataset (%s) for the variable",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid, dataset_name);
    }

    if (H5Sclose(sid) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a simple dataspace",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if (H5Pclose(dcpl_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close a dataset creation property list",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    /* Write a hidden coordinates attribute (_Netcdf4Coordinates), which lists the dimids of this variable. */
    if (ndims > 0)
    {
        hsize_t coords_len[1];
        hid_t coords_space_id, coords_att_id;
        htri_t attr_exists;

        coords_len[0] = ndims;
        coords_space_id = H5Screate_simple(1, coords_len, coords_len);
        if (coords_space_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        /* Writing _Netcdf4Coordinates attribute */
        const char* attr_name = "_Netcdf4Coordinates";

        /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
        attr_exists = H5Aexists(file->hdf5_vars[varid].hdf5_dataset_id, attr_name);
        if (attr_exists > 0)
        {
            assert(0);
        }
        else if (attr_exists == 0)
        {
            coords_att_id = H5Acreate2(file->hdf5_vars[varid].hdf5_dataset_id, attr_name,
                                       H5T_NATIVE_INT, coords_space_id, H5P_DEFAULT, H5P_DEFAULT);
            if (coords_att_id == H5I_INVALID_HID)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to the variable",
                               name, varid, pio_get_fname_from_file(file), file->pio_ncid, attr_name);
            }
        }
        else
        {
            /* Error determining whether an attribute with a given name exists on an object */
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to determine whether an attribute (%s) exists on the variable",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid, attr_name);
        }

        assert(dimidsp);
        if (H5Awrite(coords_att_id, H5T_NATIVE_INT, dimidsp) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to the variable",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid, attr_name);
        }

        if (H5Sclose(coords_space_id) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to release a simple dataspace",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (H5Aclose(coords_att_id) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Defining variable (%s, varid = %d) in file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to the variable",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid, attr_name);
        }
    }

    return PIO_NOERR;
}

int spio_hdf5_enddef(iosystem_desc_t *ios, file_desc_t *file)
{
    int i;

    assert(ios && file);
    assert(file->iotype == PIO_IOTYPE_HDF5);
    assert(ios->ioproc);

    for (i = 0; i < file->hdf5_num_dims; i++)
    {
        if (!file->hdf5_dims[i].has_coord_var)
        {
            hid_t space_id, dcpl_id, dimscale_id;
            hsize_t dims[1], max_dims[1], chunk_dims[1] = {1};

            dcpl_id = H5Pcreate(H5P_DATASET_CREATE);
            if (dcpl_id == H5I_INVALID_HID)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to create a new dataset creation property list",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }

            /* H5DSattach_scale calls (even with MPI_Barrier) might fail or hang if attribute creation
             * order is tracked or indexed. Before we have a better workaround, temporarily disable
             * tracking and indexing of attribute creation order. */
#if 0
            if (H5Pset_attr_creation_order(dcpl_id, H5P_CRT_ORDER_TRACKED | H5P_CRT_ORDER_INDEXED) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to set tracking and indexing of attribute creation order",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }
#endif

            /* Set size of dataset to size of dimension. */
            max_dims[0] = dims[0] = file->hdf5_dims[i].len;

            /* If this dimension scale is unlimited, set up chunking with a chunksize of 1. */
            if (max_dims[0] == PIO_UNLIMITED)
            {
                max_dims[0] = H5S_UNLIMITED;

                if (H5Pset_chunk(dcpl_id, 1, chunk_dims) < 0)
                {
                    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                                   "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                                   "The low level (HDF5) I/O library call failed to set the size of the chunks used to store a chunked layout dataset",
                                   pio_get_fname_from_file(file), file->pio_ncid);
                }
            }

            space_id = H5Screate_simple(1, dims, max_dims);
            if (space_id == H5I_INVALID_HID)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }

            /* Create a dataset that will be converted to a dimension scale. */
            dimscale_id = H5Dcreate2(file->hdf5_file_id, file->hdf5_dims[i].name, H5T_IEEE_F32BE,
                                     space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
            if (dimscale_id < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to create a new dataset (%s) that will be converted to a dimension scale",
                               pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name);
            }

            char dimscale_name[PIO_MAX_NAME];
            snprintf(dimscale_name, PIO_MAX_NAME, "%s%10d", "This is a netCDF dimension but not a netCDF variable.", (int)dims[0]);

            if (H5DSset_scale(dimscale_id, dimscale_name) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to convert a dataset (for dimension %s) to a dimension scale",
                               pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name);
            }

            file->hdf5_dims[i].hdf5_dataset_id = dimscale_id;

            /* Write a special attribute (_Netcdf4Dimid) for the netCDF-4 dimension ID. */
            hid_t dimid_att_id;
            htri_t attr_exists;

            hid_t dimid_space_id = H5Screate(H5S_SCALAR);
            if (dimid_space_id == H5I_INVALID_HID)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }

            /* Writing _Netcdf4Dimid attribute */
            const char* attr_name = "_Netcdf4Dimid";

            /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
            attr_exists = H5Aexists(dimscale_id, attr_name);
            if (attr_exists > 0)
            {
                assert(0);
            }
            else if (attr_exists == 0)
            {
                dimid_att_id = H5Acreate2(dimscale_id, attr_name,
                                          H5T_NATIVE_INT, dimid_space_id, H5P_DEFAULT, H5P_DEFAULT);
                if (dimid_att_id == H5I_INVALID_HID)
                {
                    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                                   "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                                   "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to a dimension scale (%s)",
                                   pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
                }
            }
            else
            {
                /* Error determining whether an attribute with a given name exists on an object */
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to determine whether an attribute (%s) exists on a dimension scale (%s)",
                               pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
            }

            if (H5Awrite(dimid_att_id, H5T_NATIVE_INT, &i) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to a dimension scale (%s)",
                               pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
            }

            if (H5Sclose(dimid_space_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to release a scalar dataspace",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }

            if (H5Aclose(dimid_att_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to a dimension scale (%s)",
                               pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_dims[i].name);
            }

            if (H5Sclose(space_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to release a simple dataspace",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }

            if (H5Pclose(dcpl_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to close a dataset creation property list",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }
        }
    }

    for (i = 0; i < file->hdf5_num_vars; i++)
    {
        /* Upgrade the dataset of a coordinate variable to a dimension scale */
        if (file->hdf5_vars[i].is_coord_var)
        {
            if (H5DSset_scale(file->hdf5_vars[i].hdf5_dataset_id, file->hdf5_vars[i].name) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to convert a dataset (for coordinate variable %s) to a dimension scale",
                               pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_vars[i].name);
            }

            assert(file->hdf5_vars[i].ndims > 0);
            int dimid = file->hdf5_vars[i].hdf5_dimids[0];
            file->hdf5_dims[dimid].hdf5_dataset_id = file->hdf5_vars[i].hdf5_dataset_id;

            /* Write a special attribute (_Netcdf4Dimid) for the netCDF-4 dimension ID. */
            hid_t dimscale_id = file->hdf5_vars[i].hdf5_dataset_id;
            hid_t dimid_att_id;
            htri_t attr_exists;

            hid_t dimid_space_id = H5Screate(H5S_SCALAR);
            if (dimid_space_id == H5I_INVALID_HID)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }

            /* Writing _Netcdf4Dimid attribute */
            const char* attr_name = "_Netcdf4Dimid";

            attr_exists = H5Aexists(dimscale_id, attr_name);
            if (attr_exists > 0)
            {
                assert(0);
            }
            else if (attr_exists == 0)
            {
                dimid_att_id = H5Acreate2(dimscale_id, attr_name,
                                          H5T_NATIVE_INT, dimid_space_id, H5P_DEFAULT, H5P_DEFAULT);
                if (dimid_att_id == H5I_INVALID_HID)
                {
                    return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                                   "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                                   "The low level (HDF5) I/O library call failed to create a new attribute (%s) attached to a dimension scale (%s)",
                                   pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
                }
            }
            else
            {
                /* Error determining whether an attribute with a given name exists on an object */
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to determine whether an attribute (%s) exists on a dimension scale (%s)",
                               pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
            }

            if (H5Awrite(dimid_att_id, H5T_NATIVE_INT, &dimid) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to write an attribute (%s) attached to a dimension scale (%s)",
                               pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
            }

            if (H5Sclose(dimid_space_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to release a scalar dataspace",
                               pio_get_fname_from_file(file), file->pio_ncid);
            }

            if (H5Aclose(dimid_att_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to close an attribute (%s) attached to a dimension scale (%s)",
                               pio_get_fname_from_file(file), file->pio_ncid, attr_name, file->hdf5_vars[i].name);
            }
        }
    }

    for (i = 0; i < file->hdf5_num_vars; i++)
    {
        if (!file->hdf5_vars[i].is_coord_var)
        {
            int ndims = file->hdf5_vars[i].ndims;
            if (ndims > 0)
            {
                int* dimids = file->hdf5_vars[i].hdf5_dimids;
                for (int j = 0; j < ndims; j++)
                {
                    if (H5DSattach_scale(file->hdf5_vars[i].hdf5_dataset_id, file->hdf5_dims[dimids[j]].hdf5_dataset_id, j) < 0)
                    {
                        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                                       "Ending the define mode for file (%s, ncid=%d) using HDF5 iotype failed. "
                                       "The low level (HDF5) I/O library call failed to attach a dimension scale (for dimension %s) to %dth dimension of a dataset (for variable %s)",
                                       pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[dimids[j]].name, j, file->hdf5_vars[i].name);
                    }

                    /* According to HDF5 developers, the H5DS routines are not parallel, so all the ranks are going to be
                     * doing the same operations. At some point, with enough iterations of the loop, HDF5 might get out of
                     * step between the ranks.
                     * Workaround: place a barrier to sync H5DSattach_scale calls.
                     */
                    MPI_Barrier(ios->io_comm);
                }
            }
        }
    }

    return PIO_NOERR;
}

int spio_hdf5_put_att(iosystem_desc_t *ios, file_desc_t *file, int varid, const char *name,
                      nc_type atttype, PIO_Offset len, const void *op)
{
    hid_t attr_id;
    hid_t space_id;
    hsize_t asize = len;
    htri_t att_exists;
    hid_t loc_id;
    hid_t h5_xtype;

    assert(ios && file && name && op);
    assert(file->iotype == PIO_IOTYPE_HDF5);
    assert(ios->ioproc);

    if (varid == PIO_GLOBAL)
        loc_id = file->hdf5_file_id;
    else
        loc_id = file->hdf5_vars[varid].hdf5_dataset_id;

    if (atttype == NC_CHAR)
    {
        /* String type */
        space_id = H5Screate(H5S_SCALAR);
        if (space_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to create a new scalar dataspace",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        h5_xtype = H5Tcopy(H5T_C_S1);
        if (h5_xtype == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to make a copy of the predefined string datatype in C",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        /* For empty strings, asize is 0, while H5Tset_size() requires that size must be positive */
        if (H5Tset_size(h5_xtype, asize + 1) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to set the total size (%ld bytes) for a derived C-style string datatype",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid, asize + 1);
        }

        if (H5Tset_strpad(h5_xtype, H5T_STR_NULLTERM) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to define the type of padding (NULL-terminated) used for a derived C-style string datatype",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (H5Tset_cset(h5_xtype, H5T_CSET_ASCII) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to set the character set (US ASCII) to be used in a derived C-style string datatype",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }
    }
    else
    {
        space_id = H5Screate_simple(1, &asize, &asize);
        if (space_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        h5_xtype = spio_nc_type_to_hdf5_type(atttype);
        if (h5_xtype == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "Unsupported variable type (type=%x)",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid, atttype);
        }
    }

    /* H5Aexists() returns zero (false), a positive (true) or a negative (failure) value */
    att_exists = H5Aexists(loc_id, name);
    if (att_exists > 0)
    {
        attr_id = H5Aopen(loc_id, name, H5P_DEFAULT);
        if (attr_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) with HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to open this existing attribute",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }
    }
    else if (att_exists == 0)
    {
        attr_id = H5Acreate2(loc_id, name, h5_xtype, space_id, H5P_DEFAULT, H5P_DEFAULT);
        if (attr_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to create a new attribute",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }
    }
    else
    {
        /* Error determining whether an attribute with a given name exists on an object */
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to determine whether this attribute exists",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if (H5Awrite(attr_id, h5_xtype, op) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to write this attribute",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if (H5Sclose(space_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a simple dataspace",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if (H5Aclose(attr_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close this attribute",
                       name, varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    /* String attribute */
    if (atttype == NC_CHAR)
    {
        if (H5Tclose(h5_xtype) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing attribute (%s) associated with variable (varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to release a derived C-style string datatype used by this attribute",
                           name, varid, pio_get_fname_from_file(file), file->pio_ncid);
        }
    }

    return PIO_NOERR;
}

int spio_hdf5_put_var(iosystem_desc_t *ios, file_desc_t *file, int varid,
                      const PIO_Offset *start, const PIO_Offset *count,
                      const PIO_Offset *stride, nc_type xtype, const void *buf)
{
    hsize_t dims[H5S_MAX_RANK];
    hsize_t mdims[H5S_MAX_RANK];

    assert(ios && file && varid >= 0 && buf);
    assert(file->iotype == PIO_IOTYPE_HDF5);
    assert(ios->ioproc);

    hid_t file_space_id = H5Dget_space(file->hdf5_vars[varid].hdf5_dataset_id);
    if (file_space_id == H5I_INVALID_HID)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to make a copy of the dataspace of the dataset associated with this variable",
                       pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    if (H5Sget_simple_extent_dims(file_space_id, dims, mdims) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to retrieve dimension size and maximum size of a dataspace copied from the dataset associated with this variable",
                       pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    int ndims = file->hdf5_vars[varid].ndims;

    /* Extend record dimension if needed */
    if (ndims > 0 && start != NULL && count != NULL && mdims[0] == H5S_UNLIMITED && dims[0] < (hsize_t)(start[0] + count[0]))
    {
        dims[0] = (hsize_t) (start[0] + count[0]);

        if (H5Sclose(file_space_id) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to release a dataspace copied from the dataset associated with this variable",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (H5Dextend(file->hdf5_vars[varid].hdf5_dataset_id, dims) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to extend the dataset associated with this variable",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        file_space_id = H5Dget_space(file->hdf5_vars[varid].hdf5_dataset_id);
        if (file_space_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to make a copy of the dataspace of the dataset (extended) associated with this variable",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
        }
    }

    /* Only the IO master does the IO */
    if (ios->iomaster == MPI_ROOT)
    {
        hsize_t hstart[H5S_MAX_RANK];
        hsize_t hcount[H5S_MAX_RANK];
        hsize_t hstride[H5S_MAX_RANK];

        if (start)
        {
            for (int i = 0; i < ndims; i++)
                hstart[i] = (hsize_t)start[i];
        }
        else
        {
            for (int i = 0; i < ndims; i++)
                hstart[i] = 0;
        }

        if (count)
        {
            for (int i = 0; i < ndims; i++)
                hcount[i] = (hsize_t)count[i];
        }
        else
        {
            for (int i = 0; i < ndims; i++)
                hcount[i] = dims[i];
        }

        if (stride)
        {
            for (int i = 0; i < ndims; i++)
                hstride[i] = (hsize_t)stride[i];
        }
        else
        {
            for (int i = 0; i < ndims; i++)
                hstride[i] = 1;
        }

        hid_t mem_space_id = H5Screate_simple(ndims, hcount, hcount);
        if (mem_space_id == H5I_INVALID_HID)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to create a new simple dataspace",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (ndims > 0)
        {
            if (H5Sselect_hyperslab(file_space_id, H5S_SELECT_SET, hstart, hstride, hcount, NULL) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to select a hyperslab region for a dataspace copied from the dataset associated with this variable",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
            }
        }

        hid_t mem_type_id;
        if (xtype == NC_CHAR)
        {
            /* String type */
            mem_type_id = H5Tcopy(H5T_C_S1);
            if (mem_type_id == H5I_INVALID_HID)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to make a copy of the predefined string datatype in C",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
            }

            if (H5Tset_strpad(mem_type_id, H5T_STR_NULLTERM) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to define the type of padding (NULL-terminated) used for a derived C-style string datatype",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
            }

            if (H5Tset_cset(mem_type_id, H5T_CSET_ASCII) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to set the character set (US ASCII) to be used in a derived C-style string datatype",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
            }
        }
        else
        {
            mem_type_id = spio_nc_type_to_hdf5_type(xtype);
            if (mem_type_id == H5I_INVALID_HID)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                               "Unsupported memory type (type=%x)",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid, xtype);
            }
        }

        /* Independent write */
        if (H5Dwrite(file->hdf5_vars[varid].hdf5_dataset_id, mem_type_id, mem_space_id,
                     file_space_id, file->dxplid_indep, buf) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to write the dataset associated with this variable",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (H5Sclose(mem_space_id) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to release a simple dataspace",
                           pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
        }

        if (xtype == NC_CHAR)
        {
            if (H5Tclose(mem_type_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to release a derived C-style string datatype used by this variable",
                               pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
            }
        }
    }

    if (H5Sclose(file_space_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Writing variable (%s, varid=%d) to file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to release a dataspace copied from the dataset associated with this variable",
                       pio_get_vname_from_file(file, varid), varid, pio_get_fname_from_file(file), file->pio_ncid);
    }

    return PIO_NOERR;
}

int spio_hdf5_close(iosystem_desc_t *ios, file_desc_t *file)
{
    int i;

    assert(ios && file);
    assert(file->iotype == PIO_IOTYPE_HDF5);
    assert(ios->ioproc);

    if (H5Pclose(file->dxplid_coll) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close a dataset transfer property list (collective data transfer)",
                       pio_get_fname_from_file(file), file->pio_ncid);
    }

    if (H5Pclose(file->dxplid_indep) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to close a dataset transfer property list (independent data transfer)",
                       pio_get_fname_from_file(file), file->pio_ncid);
    }

    for (i = 0; i < file->hdf5_num_dims; i++)
    {
        if (!file->hdf5_dims[i].has_coord_var)
        {
            if (H5Dclose(file->hdf5_dims[i].hdf5_dataset_id) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to close a dataset created for a dimension (%s, dimid = %d)",
                               pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name, i);
            }
        }
    }

    for (i = 0; i < file->hdf5_num_vars; i++)
    {
        if (H5Dclose(file->hdf5_vars[i].hdf5_dataset_id) < 0)
        {
            return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                           "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                           "The low level (HDF5) I/O library call failed to close a dataset created for a variable (%s, varid = %d)",
                           pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_vars[i].name, i);
        }

        if (file->hdf5_vars[i].nc_type == NC_CHAR)
        {
            if (H5Tclose(file->hdf5_vars[i].hdf5_type) < 0)
            {
                return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                               "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                               "The low level (HDF5) I/O library call failed to release a derived C-style string datatype created for a variable (%s, varid = %d)",
                               pio_get_fname_from_file(file), file->pio_ncid, file->hdf5_dims[i].name, i);
            }
        }
    }

    if (H5Fclose(file->hdf5_file_id) < 0)
    {
        return pio_err(ios, file, PIO_EHDF5ERR, __FILE__, __LINE__,
                       "Closing file (%s, ncid=%d) using HDF5 iotype failed. "
                       "The low level (HDF5) I/O library call failed to terminate access to an HDF5 file",
                       pio_get_fname_from_file(file), file->pio_ncid);
    }

    return PIO_NOERR;
}
#endif
