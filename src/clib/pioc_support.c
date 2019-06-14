/** @file
 * Support functions for the PIO library.
 */
#include <config.h>
#include <stdio.h>
#if PIO_ENABLE_LOGGING
#include <stdarg.h>
#include <unistd.h>
#endif /* PIO_ENABLE_LOGGING */
#include <pio.h>
#include <pio_internal.h>

#include <execinfo.h>

#ifdef _ADIOS
#include <dirent.h>
#endif

#define VERSNO 2001

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

/**
 * Return a string description of an error code. If zero is passed,
 * the errmsg will be "No error".
 *
 * @param pioerr the error code returned by a PIO function call.
 * @param errmsg Pointer that will get the error message. The message
 * will be PIO_MAX_NAME chars or less.
 * @return 0 on success.
 */
int PIOc_strerror(int pioerr, char *errmsg)
{
    LOG((1, "PIOc_strerror pioerr = %d", pioerr));

    /* Caller must provide this. */
    pioassert(errmsg, "pointer to errmsg string must be provided", __FILE__, __LINE__);

    /* System error? NetCDF and pNetCDF errors are always negative. */
    if (pioerr > 0)
    {
        const char *cp = (const char *)strerror(pioerr);
        if (cp)
            strncpy(errmsg, cp, PIO_MAX_NAME);
        else
            strcpy(errmsg, "Unknown Error");
    }
    else if (pioerr == PIO_NOERR)
        strcpy(errmsg, "No error");
#if defined(_NETCDF)
    else if (pioerr <= NC2_ERR && pioerr >= NC4_LAST_ERROR)     /* NetCDF error? */
        strncpy(errmsg, nc_strerror(pioerr), PIO_MAX_NAME);
#endif /* endif defined(_NETCDF) */
#if defined(_PNETCDF)
    else if (pioerr > PIO_FIRST_ERROR_CODE)     /* pNetCDF error? */
        strncpy(errmsg, ncmpi_strerror(pioerr), PIO_MAX_NAME);
#endif /* defined( _PNETCDF) */
    else
        /* Handle PIO errors. */
        switch(pioerr)
        {
        case PIO_EBADIOTYPE:
            strcpy(errmsg, "Bad IO type");
            break;
#ifdef _ADIOS
        case PIO_EADIOSREAD:
            strcpy(errmsg, "ADIOS IO type does not support read operations");
            break;
#endif
        default:
            strcpy(errmsg, "Unknown Error: Unrecognized error code");
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
int PIOc_set_log_level(int level)
{

#if PIO_ENABLE_LOGGING
    /* Set the log level. */
    pio_log_level = level;

#if NETCDF_C_LOGGING_ENABLED
    int ret;
    
    /* If netcdf logging is available turn it on starting at level = 4. */
    if (level > NC_LEVEL_DIFF)
        if ((ret = nc_set_log_level(level - NC_LEVEL_DIFF)))
            return pio_err(NULL, NULL, ret, __FILE__, __LINE__);
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
void pio_init_gptl(void )
{
#ifdef TIMING_INTERNAL
    pio_timer_ref_cnt += 1;
    if(pio_timer_ref_cnt == 1)
    {
        GPTLinitialize();
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
    if(pio_timer_ref_cnt == 0)
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
void piodie(const char *msg, const char *fname, int line)
{
    fprintf(stderr,"Abort with message %s in file %s at line %d\n",
            msg ? msg : "_", fname ? fname : "_", line);

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
void pioassert(_Bool expression, const char *msg, const char *fname, int line)
{
#ifndef NDEBUG
    if (!expression)
        piodie(msg, fname, line);
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
            fprintf(stderr, "MPI ERROR: %s in file %s at line %d\n",
                    errstring, filename ? filename : "_", line);

        /* Handle all MPI errors as PIO_EIO. */
        return pio_err(ios, file, PIO_EIO, filename, line);
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
            int ret = PIOc_strerror(status, errmsg);
            assert(ret == PIO_NOERR);
            fprintf(stderr, "%s\n", errmsg);
            LOG((1, "check_netcdf errmsg = %s", errmsg));
            piodie(errmsg, fname, line);
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
                    return pio_err(ios, file, PIO_ENOMEM, __FILE__, __LINE__);  
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
                        int ret = PIOc_strerror(prev_err, errmsg);
                        assert(ret == PIO_NOERR);
                        LOG((1, "Error: ranks[%d-%d] = %d (%s)",
                              start_rank, end_rank, prev_err, errmsg));
                        prev_err = err_info[i];
                        start_rank = i;
                    }
                    end_rank++;
                }
                /* The last range */
                if(err_info[end_rank] == prev_err){
                    int ret = PIOc_strerror(prev_err, errmsg);
                    assert(ret == PIO_NOERR);
                    LOG((1, "Error: ranks[%d-%d] = %d (%s)",
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
 * @returns err_num if abort is not called.
 */
int pio_err(iosystem_desc_t *ios, file_desc_t *file, int err_num, const char *fname,
            int line)
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
    if ((ret = PIOc_strerror(err_num, err_msg)))
        return ret;

    /* If logging is in use, log an error message. */
    LOG((0, "%s err_num = %d fname = %s line = %d", err_msg, err_num, fname ? fname : '\0', line));

    /* What error handler should we use? */
    if (file)
        err_handler = file->iosystem->error_handler;
    else if (ios)
        err_handler = ios->error_handler;

    LOG((2, "pio_err chose error handler = %d", err_handler));

    /* Should we abort? */
    if (err_handler == PIO_INTERNAL_ERROR)
    {
        /* For debugging only, this will print a traceback of the call tree.  */
        print_trace(stderr);
        MPI_Abort(MPI_COMM_WORLD, -1);
    }

    /* For PIO_BCAST_ERROR and PIO_RETURN_ERROR error handlers
     * just return the error code back to the caller
     */
    return err_num;
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
    if (!(region = calloc(1, sizeof(io_region))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);

    /* Allocate memory for the array of start indicies. */
    if (!(region->start = calloc(ndims, sizeof(PIO_Offset))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);

    /* Allocate memory for the array of counts. */
    if (!(region->count = calloc(ndims, sizeof(PIO_Offset))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);

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
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* What is the size of the pio type? */
    if ((ret = pioc_pnetcdf_inq_type(0, piotype, NULL, &type_size)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Allocate space for the io_desc_t struct. */
    if (!(*iodesc = calloc(1, sizeof(io_desc_t))))
        return pio_err(ios, NULL, PIO_ENOMEM, __FILE__, __LINE__);

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
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);        

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
int PIOc_freedecomp(int iosysid, int ioid)
{
    iosystem_desc_t *ios;
    io_desc_t *iodesc;
    int mpierr = MPI_SUCCESS, mpierr2;  /* Return code from MPI function calls. */
    int ret = 0;

#ifdef TIMING
    GPTLstart("PIO:PIOc_freedecomp");
#endif
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
        return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* If async is in use, and this is not an IO task, bcast the parameters. */
    if (ios->async)
    {
        int msg = PIO_MSG_FREEDECOMP; /* Message for async notification. */

        PIO_SEND_ASYNC_MSG(ios, msg, &ret, iosysid, ioid);
        if(ret != PIO_NOERR)
        {
            LOG((1, "Error sending async msg for PIO_MSG_FREEDECOMP"));
            return pio_err(ios, NULL, ret, __FILE__, __LINE__);
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
            if (iodesc->rtype[i] != PIO_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(&iodesc->rtype[i])))
                    return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);

        free(iodesc->rtype);
    }

    if (iodesc->stype)
    {
        for (int i = 0; i < iodesc->num_stypes; i++)
            if (iodesc->stype[i] != PIO_DATATYPE_NULL)
                if ((mpierr = MPI_Type_free(iodesc->stype + i)))
                    return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);

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
            return check_mpi(ios, NULL, mpierr, __FILE__, __LINE__);

    ret = pio_delete_iodesc_from_list(ioid);
#ifdef TIMING
    GPTLstop("PIO:PIOc_freedecomp");
#endif

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
int PIOc_readmap(const char *file, int *ndims, int **gdims, PIO_Offset *fmaplen,
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
        return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__);

    if ((mpierr = MPI_Comm_size(comm, &npes)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    if ((mpierr = MPI_Comm_rank(comm, &myrank)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

    if (myrank == 0)
    {
        FILE *fp = fopen(file, "r");
        if (!fp)
            pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__);

        fscanf(fp,"%s%d%s%d%s%d\n",rversstr, &rversno, rnpesstr, &rnpes, rndimsstr, ndims);

        if (rversno != VERSNO)
            return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__);

        if (rnpes < 1 || rnpes > npes)
            return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__);

        if ((mpierr = MPI_Bcast(&rnpes, 1, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(ndims, 1, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if (!(tdims = calloc(*ndims, sizeof(int))))
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        for (int i = 0; i < *ndims; i++)
            fscanf(fp,"%d ", tdims + i);

        if ((mpierr = MPI_Bcast(tdims, *ndims, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

        for (int i = 0; i < rnpes; i++)
        {
            fscanf(fp, "%d %lld", &j, &maplen);
            if (j != i)  // Not sure how this could be possible
                return pio_err(NULL, NULL, PIO_EINVAL, __FILE__, __LINE__);
            if (!(tmap = malloc(maplen * sizeof(PIO_Offset))))
                return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);
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
        if (!(tdims = calloc(*ndims, sizeof(int))))
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);
        if ((mpierr = MPI_Bcast(tdims, *ndims, MPI_INT, 0, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

        if (myrank < rnpes)
        {
            if ((mpierr = MPI_Recv(&maplen, 1, PIO_OFFSET, 0, myrank + npes, comm, &status)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            if (!(tmap = malloc(maplen * sizeof(PIO_Offset))))
                return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__);
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
int PIOc_readmap_from_f90(const char *file, int *ndims, int **gdims, PIO_Offset *maplen,
                          PIO_Offset **map, int f90_comm)
{
    return PIOc_readmap(file, ndims, gdims, maplen, map, MPI_Comm_f2c(f90_comm));
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
int PIOc_write_nc_decomp(int iosysid, const char *filename, int cmode, int ioid,
                         char *title, char *history, int fortran_order)
{
    iosystem_desc_t *ios; /* IO system info. */
    io_desc_t *iodesc;    /* Decomposition info. */
    int max_maplen;       /* The maximum maplen used for any task. */
    int mpierr = MPI_SUCCESS;
    int ret;

    /* Get the IO system info. */
    if (!(ios = pio_get_iosystem_from_id(iosysid)))
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* Check inputs. */
    if (!filename)
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);
    if (title)
        if (strlen(title) > PIO_MAX_NAME)
            return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);
    if (history)
        if (strlen(history) > PIO_MAX_NAME)
            return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);

    LOG((1, "PIOc_write_nc_decomp filename = %s iosysid = %d ioid = %d "
         "ios->num_comptasks = %d", filename, iosysid, ioid, ios->num_comptasks));

    /* Get the IO desc, which describes the decomposition. */
    if (!(iodesc = pio_get_iodesc_from_id(ioid)))
        return pio_err(ios, NULL, PIO_EBADID, __FILE__, __LINE__);

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
    if ((ret = pioc_write_nc_decomp_int(ios, filename, cmode, iodesc->ndims, iodesc->dimlen,
                                        ios->num_comptasks, task_maplen, (int *)full_map, title,
                                        history, fortran_order)))
        return ret;

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
int PIOc_read_nc_decomp(int iosysid, const char *filename, int *ioidp, MPI_Comm comm,
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
        return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__);

    /* Check inputs. */
    if (!filename || !ioidp)
        return pio_err(ios, NULL, PIO_EINVAL, __FILE__, __LINE__);

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
    if ((ret = pioc_read_nc_decomp_int(iosysid, filename, &ndims, &global_dimlen, &num_tasks_decomp,
                                       &task_maplen, &max_maplen, &full_map, title, history,
                                       source_in, version_in, fortran_order)))
        return ret;
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
        ret = PIOc_InitDecomp(iosysid, pio_type, ndims, global_dimlen, task_maplen[my_rank],
                              compmap, ioidp, NULL, NULL, NULL);
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
int pioc_write_nc_decomp_int(iosystem_desc_t *ios, const char *filename, int cmode, int ndims,
                             int *global_dimlen, int num_tasks, int *task_maplen, int *map,
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

    LOG((2, "pioc_write_nc_decomp_int filename = %s ndims = %d num_tasks = %d", filename,
         ndims, num_tasks));

    /* Find the maximum maplen. */
    for (int t = 0; t < num_tasks; t++)
        if (task_maplen[t] > max_maplen)
            max_maplen = task_maplen[t];
    LOG((3, "max_maplen = %d", max_maplen));

    /* Create the netCDF decomp file. */
    if ((ret = PIOc_create(ios->iosysid, filename, cmode | NC_WRITE, &ncid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Write an attribute with the version of this file. */
    char version[PIO_MAX_NAME + 1];
    sprintf(version, "%d.%d.%d", PIO_VERSION_MAJOR, PIO_VERSION_MINOR, PIO_VERSION_PATCH);
    if ((ret = PIOc_put_att_text(ncid, NC_GLOBAL, DECOMP_VERSION_ATT_NAME,
                                 strlen(version) + 1, version)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Write an attribute with the max map len. */
    if ((ret = PIOc_put_att_int(ncid, NC_GLOBAL, DECOMP_MAX_MAPLEN_ATT_NAME,
                                PIO_INT, 1, &max_maplen)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Write title attribute, if the user provided one. */
    if (title)
        if ((ret = PIOc_put_att_text(ncid, NC_GLOBAL, DECOMP_TITLE_ATT_NAME,
                                     strlen(title) + 1, title)))
            return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Write history attribute, if the user provided one. */
    if (history)
        if ((ret = PIOc_put_att_text(ncid, NC_GLOBAL, DECOMP_HISTORY_ATT_NAME,
                                     strlen(history) + 1, history)))
            return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Write a source attribute. */
    char source[] = "Decomposition file produced by PIO library.";
    if ((ret = PIOc_put_att_text(ncid, NC_GLOBAL, DECOMP_SOURCE_ATT_NAME,
                                 strlen(source) + 1, source)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* Write an attribute with array ordering (C or Fortran). */
    char c_order_str[] = DECOMP_C_ORDER_STR;
    char fortran_order_str[] = DECOMP_FORTRAN_ORDER_STR;
    char *my_order_str = fortran_order ? fortran_order_str : c_order_str;
    if ((ret = PIOc_put_att_text(ncid, NC_GLOBAL, DECOMP_ORDER_ATT_NAME,
                                 strlen(my_order_str) + 1, my_order_str)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

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
    if ((ret = PIOc_put_att_text(ncid, NC_GLOBAL, DECOMP_BACKTRACE_ATT_NAME,
                                 strlen(full_bt) + 1, full_bt)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    /* We need a dimension for the dimensions in the data. (Example:
     * for 4D data we will need to store 4 dimension IDs.) */
    int dim_dimid;
    if ((ret = PIOc_def_dim(ncid, DECOMP_DIM_DIM, ndims, &dim_dimid)))
        return ret;

    /* We need a dimension for tasks. If we have 4 tasks, we need to
     * store an array of length 4 with the size of the local array on
     * each task. */
    int task_dimid;
    if ((ret = PIOc_def_dim(ncid, DECOMP_TASK_DIM_NAME, num_tasks, &task_dimid)))
        return ret;

    /* We need a dimension for the map. It's length may vary, we will
     * use the max_maplen for the dimension size. */
    int mapelem_dimid;
    if ((ret = PIOc_def_dim(ncid, DECOMP_MAPELEM_DIM_NAME, max_maplen,
                            &mapelem_dimid)))
        return ret;

    /* Define a var to hold the global size of the array for each
     * dimension. */
    int gsize_varid;
    if ((ret = PIOc_def_var(ncid, DECOMP_GLOBAL_SIZE_VAR_NAME, NC_INT, 1,
                            &dim_dimid, &gsize_varid)))
        return ret;

    /* Define a var to hold the length of the local array on each task. */
    int maplen_varid;
    if ((ret = PIOc_def_var(ncid, DECOMP_MAPLEN_VAR_NAME, NC_INT, 1, &task_dimid,
                            &maplen_varid)))
        return ret;

    /* Define a 2D var to hold the length of the local array on each task. */
    int map_varid;
    int map_dimids[2] = {task_dimid, mapelem_dimid};
    if ((ret = PIOc_def_var(ncid, DECOMP_MAP_VAR_NAME, NC_INT, 2, map_dimids,
                            &map_varid)))
        return ret;

    /* End define mode, to write data. */
    if ((ret = PIOc_enddef(ncid)))
        return ret;

    /* Write the global dimension sizes. */
    if ((PIOc_put_var_int(ncid, gsize_varid, global_dimlen)))
        return ret;

    /* Write the size of the local array on each task. */
    if ((PIOc_put_var_int(ncid, maplen_varid, task_maplen)))
        return ret;

    /* Write the map. */
    if ((PIOc_put_var_int(ncid, map_varid, map)))
        return ret;

    /* Close the netCDF decomp file. */
    if ((ret = PIOc_closefile(ncid)))
        return pio_err(ios, NULL, ret, __FILE__, __LINE__);

    return PIO_NOERR;
}

