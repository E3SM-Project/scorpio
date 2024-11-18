/**
 * @file
 * Public headers for the PIO C interface.
 * @author Jim Edwards
 * @date  2014
 *
 * @see http://code.google.com/p/parallelio/
 */

#ifndef _PIO_H_
#define _PIO_H_
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h> /* memcpy */

#ifdef MPI_SERIAL
#if defined(__cplusplus)
extern "C" {
#endif
#endif

#include <mpi.h>

#ifdef MPI_SERIAL
#if defined(__cplusplus)
}
#endif
#endif

#include "pio_config.h"
#if PIO_USE_PNETCDF
  #define _PNETCDF 1
#endif
#if PIO_USE_NETCDF
  #define _NETCDF 1
  #if PIO_USE_NETCDF4
    #define _NETCDF4 1
  #endif
#endif
#if PIO_USE_ADIOS
  #define _ADIOS2 1
#endif
#if PIO_USE_HDF5
  #define _HDF5 1
#endif
#if PIO_USE_MICRO_TIMING
  #define PIO_MICRO_TIMING 1
#endif
#if PIO_ENABLE_IO_STATS
  #define SPIO_IO_STATS 1
#endif

#ifdef _NETCDF
#include <netcdf.h>
#ifdef _NETCDF4
#include <netcdf_par.h>
#endif
#endif
#ifdef _PNETCDF
#include <pnetcdf.h>
#endif

#ifdef _ADIOS2
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <limits.h>
#include <adios2_c.h>

#define MAX_ADIOS_BUFFER_COUNT (PIO_MAX_CACHED_STEPS_FOR_ADIOS + 16) /* Maximum buffer size for aggregating decomp_id, frame_id, and fillval_id values */
#define BLOCK_MAX_BUFFER ((unsigned long)INT_MAX) /* 2GB limit of MPI_Gatherv */
/* adios end step is called if the number of blocks written out exceeds BLOCK_COUNT_THRESHOLD */
#define BLOCK_COUNT_THRESHOLD ((unsigned long)(1024 * 1024 * 1024 * 1.9))
#define BLOCK_METADATA_SIZE 70 /* Size of adios block metadata */

#endif
#ifdef _HDF5
#include <hdf5.h>
#include <hdf5_hl.h>
#include <unistd.h>
#endif

/* PIO_OFFSET_C_TYPENAME is defined in pio_config.h */
typedef PIO_OFFSET_C_TYPENAME PIO_Offset;

/* The MPI typename corresponding to PIO_Offset, PIO_OFFSET, is defined in pio_config.h */

/** The start ID and maximum number of IDs for IO decompositions. */
#define PIO_IODESC_START_ID 512
#define PIO_IODESC_MAX_IDS 65536

/** The maximum number of variables allowed in a netCDF file. */
#define PIO_MAX_VARS_UB 8192
#if NC_MAX_VARS > PIO_MAX_VARS_UB
#define PIO_MAX_VARS PIO_MAX_VARS_UB
#else
#define PIO_MAX_VARS NC_MAX_VARS
#endif

/** The maximum number of dimensions allowed in a netCDF file. */
#define PIO_MAX_DIMS_UB 1024
#if NC_MAX_DIMS > PIO_MAX_DIMS_UB
#define PIO_MAX_DIMS PIO_MAX_DIMS_UB
#else
#define PIO_MAX_DIMS NC_MAX_DIMS
#endif

/** The maximum number of attributes allowed in a netCDF file. */
#define PIO_MAX_ATTRS_UB 8192
#if NC_MAX_ATTRS > PIO_MAX_ATTRS_UB
#define PIO_MAX_ATTRS PIO_MAX_ATTRS_UB
#else
#define PIO_MAX_ATTRS NC_MAX_ATTRS
#endif

/** Pass this to PIOc_set_iosystem_error_handling() as the iosysid in
 * order to set default error handling. */
#define PIO_DEFAULT (-1)

/** Used in the decomposition netCDF file. */

/* Holds the version of the decomposition file. */
#define DECOMP_VERSION_ATT_NAME "version"

/* Holds the maximum length of any task map. */
#define DECOMP_MAX_MAPLEN_ATT_NAME "max_maplen"

/* Name of title attribute. */
#define DECOMP_TITLE_ATT_NAME "title"

/* Name of history attribute. */
#define DECOMP_HISTORY_ATT_NAME "history"

/* Name of source attribute. */
#define DECOMP_SOURCE_ATT_NAME "source"

/* Name of array order (C or Fortran) attribute. */
#define DECOMP_ORDER_ATT_NAME "array_order"

/* Name of backtrace attribute. */
#define DECOMP_BACKTRACE_ATT_NAME "backtrace"

/* Name for the dim dim in decomp file. */
#define DECOMP_DIM_DIM "dims"

/* Name for the npes dim in decomp file. */
#define DECOMP_TASK_DIM_NAME "task"

/* Name for the npes dim in decomp file. */
#define DECOMP_MAPELEM_DIM_NAME "map_element"

#define DECOMP_NDIMS "ndims"

/* Name of var in decomp file that holds global array sizes. */
#define DECOMP_GLOBAL_SIZE_VAR_NAME "global_size"

/* Name of var in decomp file that holds the length of the map for
 * each task. */
#define DECOMP_MAPLEN_VAR_NAME "maplen"

/* Name of var in decomp file that holds map. */
#define DECOMP_MAP_VAR_NAME "map"

/* String used to indicate a decomposition file is in C
 * array-order. */
#define DECOMP_C_ORDER_STR "C"

/* String used to indicate a decomposition file is in Fortran
 * array-order. */
#define DECOMP_FORTRAN_ORDER_STR "Fortran"

#if defined( _PNETCDF) || defined(_NETCDF)

#define PIO_GLOBAL NC_GLOBAL
#define PIO_UNLIMITED NC_UNLIMITED

/* NetCDF types. */
#define PIO_NAT    NC_NAT
#define PIO_BYTE   NC_BYTE
#define PIO_CHAR   NC_CHAR
#define PIO_SHORT  NC_SHORT
#define PIO_INT    NC_INT
#define PIO_FLOAT  NC_FLOAT
#define PIO_REAL   NC_FLOAT
#define PIO_DOUBLE NC_DOUBLE
#define PIO_UBYTE  NC_UBYTE
#define PIO_USHORT NC_USHORT
#define PIO_UINT   NC_UINT
#define PIO_INT64  NC_INT64
#define PIO_UINT64 NC_UINT64
#define PIO_STRING NC_STRING

/* NetCDF flags. */
#define PIO_WRITE  NC_WRITE
#define PIO_NOWRITE  NC_NOWRITE
#define PIO_CLOBBER NC_CLOBBER
#define PIO_NOCLOBBER NC_NOCLOBBER
#define PIO_FILL NC_FILL
#define PIO_NOFILL NC_NOFILL
#define PIO_MAX_NAME_UB 1024
#if NC_MAX_NAME > PIO_MAX_NAME_UB
#define PIO_MAX_NAME PIO_MAX_NAME_UB
#else
#define PIO_MAX_NAME NC_MAX_NAME
#endif
#define PIO_MAX_VAR_DIMS_UB 1024
#if NC_MAX_VAR_DIMS > PIO_MAX_VAR_DIMS_UB
#define PIO_MAX_VAR_DIMS PIO_MAX_VAR_DIMS_UB
#else
#define PIO_MAX_VAR_DIMS NC_MAX_VAR_DIMS
#endif
#define PIO_64BIT_OFFSET NC_64BIT_OFFSET
#define PIO_64BIT_DATA NC_64BIT_DATA

/** Define the netCDF-based error codes. */
#define PIO_NOERR  NC_NOERR
#define PIO_EBADID NC_EBADID
#define PIO_ENFILE NC_ENFILE
#define PIO_EEXIST NC_EEXIST
#define PIO_EINVAL NC_EINVAL
#define PIO_EPERM NC_EPERM
#define PIO_ENOTINDEFINE NC_ENOTINDEFINE
#define PIO_EINDEFINE NC_EINDEFINE
#define PIO_EINVALCOORDS NC_EINVALCOORDS
#define PIO_EMAXDIMS NC_EMAXDIMS
#define PIO_ENAMEINUSE NC_ENAMEINUSE
#define PIO_ENOTATT NC_ENOTATT
#define PIO_EMAXATTS NC_EMAXATTS
#define PIO_EBADTYPE NC_EBADTYPE
#define PIO_EBADDIM NC_EBADDIM
#define PIO_EUNLIMPOS NC_EUNLIMPOS
#define PIO_EMAXVARS NC_EMAXVARS
#define PIO_ENOTVAR NC_ENOTVAR
#define PIO_EGLOBAL NC_EGLOBAL
#define PIO_ENOTNC NC_ENOTNC
#define PIO_ESTS NC_ESTS
#define PIO_EMAXNAME NC_EMAXNAME
#define PIO_EUNLIMIT NC_EUNLIMIT
#define PIO_ENORECVARS NC_ENORECVARS
#define PIO_ECHAR NC_ECHAR
#define PIO_EEDGE NC_EEDGE
#define PIO_ESTRIDE NC_ESTRIDE
#define PIO_EBADNAME NC_EBADNAME
#define PIO_ERANGE NC_ERANGE
#define PIO_ENOMEM NC_ENOMEM
#define PIO_EVARSIZE NC_EVARSIZE
#define PIO_EDIMSIZE NC_EDIMSIZE
#define PIO_ETRUNC NC_ETRUNC
#define PIO_EAXISTYPE NC_EAXISTYPE
#define PIO_EDAP NC_EDAP
#define PIO_ECURL NC_ECURL
#define PIO_EIO NC_EIO
#define PIO_ENODATA NC_ENODATA
#define PIO_EDAPSVC NC_EDAPSVC
#define PIO_EDAS NC_EDAS
#define PIO_EDDS NC_EDDS
#define PIO_EDATADDS NC_EDATADDS
#define PIO_EDAPURL NC_EDAPURL
#define PIO_EDAPCONSTRAINT NC_EDAPCONSTRAINT
#define PIO_ETRANSLATION NC_ETRANSLATION
#define PIO_EHDFERR NC_EHDFERR
#define PIO_ECANTREAD NC_ECANTREAD
#define PIO_ECANTWRITE NC_ECANTWRITE
#define PIO_ECANTCREATE NC_ECANTCREATE
#define PIO_EFILEMETA NC_EFILEMETA
#define PIO_EDIMMETA NC_EDIMMETA
#define PIO_EATTMETA NC_EATTMETA
#define PIO_EVARMETA NC_EVARMETA
#define PIO_ENOCOMPOUND NC_ENOCOMPOUND
#define PIO_EATTEXISTS NC_EATTEXISTS
#define PIO_ENOTNC4 NC_ENOTNC4
#define PIO_ESTRICTNC3 NC_ESTRICTNC3
#define PIO_ENOTNC3 NC_ENOTNC3
#define PIO_ENOPAR NC_ENOPAR
#define PIO_EPARINIT NC_EPARINIT
#define PIO_EBADGRPID NC_EBADGRPID
#define PIO_EBADTYPID NC_EBADTYPID
#define PIO_ETYPDEFINED NC_ETYPDEFINED
#define PIO_EBADFIELD NC_EBADFIELD
#define PIO_EBADCLASS NC_EBADCLASS
#define PIO_EMAPTYPE NC_EMAPTYPE
#define PIO_ELATEFILL NC_ELATEFILL
#define PIO_ELATEDEF NC_ELATEDEF
#define PIO_EDIMSCALE NC_EDIMSCALE
#define PIO_ENOGRP NC_ENOGRP
#define PIO_ESTORAGE NC_ESTORAGE
#define PIO_EBADCHUNK NC_EBADCHUNK
#define PIO_ENOTBUILT NC_ENOTBUILT
#define PIO_EDISKLESS NC_EDISKLESS

/* These are the netCDF default fill values. */
#define PIO_FILL_BYTE NC_FILL_BYTE
#define PIO_FILL_CHAR NC_FILL_CHAR
#define PIO_FILL_SHORT NC_FILL_SHORT
#define PIO_FILL_INT NC_FILL_INT
#define PIO_FILL_FLOAT NC_FILL_FLOAT
#define PIO_FILL_DOUBLE NC_FILL_DOUBLE
#define PIO_FILL_UBYTE NC_FILL_UBYTE
#define PIO_FILL_USHORT NC_FILL_USHORT
#define PIO_FILL_UINT NC_FILL_UINT
#define PIO_FILL_INT64 NC_FILL_INT64
#define PIO_FILL_UINT64 NC_FILL_UINT64
#endif /*  defined( _PNETCDF) || defined(_NETCDF) */

/** Define the extra error codes for the parallel-netcdf library. */
#ifdef _PNETCDF
#define PIO_EINDEP  NC_EINDEP
#define PIO_REQ_NULL NC_REQ_NULL
#else  /* _PNETCDF */
#define PIO_EINDEP  (-203)
#define PIO_REQ_NULL (-1)
#endif /* _PNETCDF */

/** Define error codes for PIO. */
#define PIO_FIRST_ERROR_CODE (-500)
#define PIO_EBADIOTYPE  (-500)
#define PIO_EINTERNAL  (-501)

#ifdef _ADIOS2
/** Define error codes for ADIOS. */
#define PIO_EADIOSREAD (-300)
#define PIO_EADIOS2ERR (-301)
#endif

#ifdef _HDF5
/** Define error codes for HDF5. */
#define PIO_EHDF5ERR (-700)
#endif

/**
 * Rearranger comm type. The rearranger option values must match the
 * definitions in the fortran interface.
 */
enum PIO_REARR_COMM_TYPE
{
    /** Point to point */
    PIO_REARR_COMM_P2P = (0),

    /** Collective */
    PIO_REARR_COMM_COLL
};

/**
 * Rearranger comm flow control direction. The rearranger option
 * values must match the definitions in the fortran interface.
 */
enum PIO_REARR_COMM_FC_DIR
{
    /** Comp procs to io procs and vice versa */
    PIO_REARR_COMM_FC_2D_ENABLE = (0),

    /** Comp procs to io procs only */
    PIO_REARR_COMM_FC_1D_COMP2IO,

    /** IO procs to comp procs only */
    PIO_REARR_COMM_FC_1D_IO2COMP,

    /** Disable flow control */
    PIO_REARR_COMM_FC_2D_DISABLE
};

/* Constant to indicate unlimited requests. */
#define PIO_REARR_COMM_UNLIMITED_PEND_REQ -1

/**
 * Rearranger comm flow control options.
 */
typedef struct rearr_comm_fc_opt
{
    /** Enable handshake */
    bool hs;

    /** Enable isends - if false use blocking sends */
    bool isend;

    /** Max pending requests
     * (PIO_REARR_COMM_UNLIMITED_PEND_REQ => unlimited pend req).
     * This is the number of messages allowed to be in flight at one
     * time. On some systems posting all messages at once creates a
     * significant bottleneck in communications and throttling in this
     * manner improves overall performance. */
    int max_pend_req;
} rearr_comm_fc_opt_t;

/**
 * Rearranger options
 */
typedef struct rearr_opt
{
    /** Comm type - see PIO_REARR_COMM_TYPE */
    int comm_type;

    /** Comm flow control dir - see PIO_REARR_COMM_FC_DIR */
    int fcd;

    /** flow control opts, comp to io procs */
    rearr_comm_fc_opt_t comp2io;

    /** flow control opts, io to comp procs */
    rearr_comm_fc_opt_t io2comp;
} rearr_opt_t;

/**
 * These are the supported methods of reading/writing netCDF
 * files. (Not all methods can be used with all netCDF files.)
 */
enum PIO_IOTYPE
{
    /** Parallel Netcdf  (parallel) */
    PIO_IOTYPE_PNETCDF = 1,

    /** Netcdf3 Classic format (serial) */
    PIO_IOTYPE_NETCDF = 2,

    /**  NetCDF4 (HDF5) compressed format (serial) */
    PIO_IOTYPE_NETCDF4C = 3,

    /** NetCDF4 (HDF5) parallel */
    PIO_IOTYPE_NETCDF4P = 4,

    /** NetCDF4 (HDF5) parallel NCZarr */
    PIO_IOTYPE_NETCDF4P_NCZARR = 5,

    /** ADIOS parallel */
    PIO_IOTYPE_ADIOS = 6,

    /** ADIOS parallel with compression */
    PIO_IOTYPE_ADIOSC = 7,

    /** HDF5 parallel */
    PIO_IOTYPE_HDF5 = 8,

    /** HDF5 parallel with compression */
    PIO_IOTYPE_HDF5C = 9
};

/**
 * These are the supported output data rearrangement methods.
 */
enum PIO_REARRANGERS
{
    /** Box rearranger. */
    PIO_REARR_BOX = 1,

    /** Subset rearranger. */
    PIO_REARR_SUBSET = 2,

    /** Let the library choose the rearranger. */
    PIO_REARR_ANY = 3,

    /** Contig rearranger (data aggregation + contiguous data) */
    PIO_REARR_CONTIG = 4,
};

/**
 * These are the supported error handlers.
 */
enum PIO_ERROR_HANDLERS
{
    /** Errors cause abort. */
    PIO_INTERNAL_ERROR = (-51),

    /** Error codes from io process with rank 0
      * is broadcasted to all processes. */
    PIO_BCAST_ERROR = (-52),

    /** Error codes are reduced across all processes. */
    PIO_REDUCE_ERROR = (-53),

    /** Errors are returned to caller with no internal action. */
    PIO_RETURN_ERROR = (-54)
};

#include "pio_api.h"

#endif  // _PIO_H_
