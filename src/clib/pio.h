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

#ifdef PIO_MICRO_TIMING
/** Some fwd declarations to avoid including internal headers */
typedef struct mtimer_info *mtimer_t;
#endif

/* Forward decl of hash map used for ADIOS reads */
struct spio_hmap;

/**
 * Variable description structure.
 */
typedef struct var_desc_t
{
    /* Variable ID. */
    int varid;

    /* Variable name - cached */
    char vname[PIO_MAX_NAME + 1];

    /* Variable description */
    char vdesc[PIO_MAX_NAME + 1];
    
    /* Non-zero if this is a record var (i.e. uses unlimited
     * dimension). */
    int rec_var;

    /* Number of dimensions for this variable - cached data */
    int ndims;
    
    /** The record number to be written. Ignored if there is no
     * unlimited dimension. */
    int record;

    /** FIXME: Combine request related members to a separate struct */
    /** ID of each outstanding pnetcdf request for this variable. */
    int *request;

    /** Size of each outstanding pnetcdf request for this variable */
    PIO_Offset *request_sz;

    /** Number of requests bending with pnetcdf. */
    int nreqs;

    /* Holds the fill value of this var. */
    void *fillvalue;

    /* The PIO data type (PIO_INT, PIO_FLOAT, etc.) */
    int pio_type;

    /* The size of the data type (2 for PIO_SHORT, 4 for PIO_INT, etc.) */
    PIO_Offset type_size;

    /* Size of one record of the variable (number of bytes)*/
    PIO_Offset vrsize;

    /* Bytes pending to be read */
    PIO_Offset rb_pend;

    /* Bytes pending to be written out */
    PIO_Offset wb_pend;

#ifdef PIO_MICRO_TIMING
    mtimer_t rd_mtimer;
    mtimer_t rd_rearr_mtimer;

    mtimer_t wr_mtimer;
    mtimer_t wr_rearr_mtimer;
#endif

    /** Non-zero if fill mode is turned on for this var. */
    int use_fill;

    /** Buffer that contains the holegrid fill values used to fill in
     * missing sections of data when using the subset rearranger. */
    void *fillbuf;
} var_desc_t;

/**
 * IO region structure.
 *
 * Each IO region is a unit of data which can be described using start
 * and count arrays. Each IO task may in general have multiple io
 * regions per variable.  The box rearranger will have at most one io
 * region per variable.
 *
 * The write from a particular IO task is divided into 1 or more
 * regions each of which can be described using start and count. The
 * io_region typedef is a linked list of those regions.
 */
typedef struct io_region
{
    /** The offset from the beginning of the data buffer to the
     * beginning of this region.  */
    int loffset;

    /** Start array for this region. */
    PIO_Offset *start;

    /** Count array for this region. */
    PIO_Offset *count;

    /** Pointer to the next io_region in the list. */
    struct io_region *next;
} io_region;

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
 * IO descriptor structure.
 *
 * This structure defines the mapping for a given variable between
 * compute and IO decomposition.
 */
typedef struct io_desc_t
{
    /** The ID of this io_desc_t. */
    int ioid;

    /** The length of the decomposition map. */
    int maplen;

    /** A 1-D array with iodesc->maplen elements, which are the
     * 1-based mappings to the global array for that task. */
    PIO_Offset *map;

    /** Number of tasks involved in the communication between comp and
     * io tasks. */
    int nrecvs;

    /** Local size of the decomposition array on the compute node. */
    int ndof;

    /** All vars included in this io_desc_t have the same number of
     * dimensions. */
    int ndims;

    /** An array of size ndims with the length of each dimension. */
    int *dimlen;

    /** The actual number of IO tasks participating. */
    int num_aiotasks;

    /** The rearranger in use for this variable. */
    int rearranger;

    /** Maximum number of regions in the decomposition. */
    int maxregions;

    /** Does this decomp leave holes in the field (true) or write
     * everywhere (false) */
    bool needsfill;

    /** The maximum number of bytes of this iodesc before flushing. */
    int maxbytes;

    /** The PIO type of the data. */
    int piotype;

    /** The size of one element of the piotype. */
    int piotype_size;

    /** The MPI type of the data. */
    MPI_Datatype mpitype;

    /** The size in bytes of a datum of MPI type mpitype. */
    int mpitype_size;

    /** Length of the iobuffer on this task for a single field on the
     * IO node. The arrays from compute nodes gathered and rearranged
     * to the io-nodes (which are sometimes collocated with compute
     * nodes), each io task contains data from the compmap of one or
     * more compute tasks in the iomap array. */
    PIO_Offset llen;

    /** Maximum llen participating. */
    PIO_Offset maxiobuflen;

    /** Array (length nrecvs) of computation tasks received from. */
    int *rfrom;

    /** Array (length nrecvs) of counts of data to be received from
     * each computation task by the IO tasks. */
    int *rcount;

    /** Array (length numiotasks) of data counts to send to each task
     * in the communication in pio_swapm(). */
    int *scount;

    /** Array (length ndof) for the BOX rearranger with the index
     * for computation taks (send side during writes). */
    PIO_Offset *sindex;

    /** Index for the IO tasks (receive side during writes). */
    PIO_Offset *rindex;

    /** Array (of length nrecvs) of receive MPI types in pio_swapm() call. */
    MPI_Datatype *rtype;

    /** Array of send MPI types in pio_swapm() call. */
    MPI_Datatype *stype;

    /** Number of send MPI types in pio_swapm() call. */
    int num_stypes;

    /** Used when writing fill data. */
    int holegridsize;

    /** max holegridsize across all io tasks, needed for netcdf and netcdf4c serial */
    int maxholegridsize;

    /** Used when writing fill data. */
    int maxfillregions;

    /** Linked list of regions. */
    io_region *firstregion;

    /** Used when writing fill data. */
    io_region *fillregion;

    /** Rearranger flow control options
     *  (handshake, non-blocking sends, pending requests)
     */
    rearr_opt_t rearr_opts;

    /** In the subset communicator each io task is associated with a
     * unique group of comp tasks this is the communicator for that
     * group. */
    MPI_Comm subset_comm;

#if PIO_SAVE_DECOMPS
    /* Indicates whether this iodesc has been saved to disk (the
     * decomposition is dumped to disk)
     */
    bool is_saved;
#endif

    /** Pointer to the next io_desc_t in the list. */
    struct io_desc_t *next;
} io_desc_t;

/* Forward decl for I/O file summary stats info */
struct spio_io_fstats_summary;

/**
 * IO system descriptor structure.
 *
 * This structure contains the general IO subsystem data and MPI
 * structure
 */
typedef struct iosystem_desc_t
{
    /** The ID of this iosystem_desc_t. This will be obtained by
     * calling PIOc_Init_Intercomm() or PIOc_Init_Intracomm(). */
    int iosysid;

    /* I/O System name */
    char sname[PIO_MAX_NAME + 1];

    /** This is an MPI intra communicator that includes all the tasks in
     * both the IO and the computation communicators. */
    MPI_Comm union_comm;

    /** This is an MPI intra communicator that includes all the tasks
     * involved in IO. */
    MPI_Comm io_comm;

    /** This is an MPI intra communicator that includes all the tasks
     * involved in computation. */
    MPI_Comm comp_comm;

    /** This is an MPI inter communicator between IO communicator and
     * computation communicator. */
    MPI_Comm intercomm;

    /** This is a copy (but not an MPI copy) of either the comp (for
     * non-async) or the union (for async) communicator. */
    MPI_Comm my_comm;

    /** This MPI group contains the processors involved in
     * computation. */
    MPI_Group compgroup;

    /** This MPI group contains the processors involved in I/O. */
    MPI_Group iogroup;

    /** The number of tasks in the IO communicator. */
    int num_iotasks;

    /** The number of tasks in the computation communicator. */
    int num_comptasks;

    /** The number of tasks in the union communicator (will be
     * num_comptasks for non-async, num_comptasks + num_iotasks for
     * async). */
    int num_uniontasks;

    /** Rank of this task in the union communicator. */
    int union_rank;

    /** The rank of this process in the computation communicator, or -1
     * if this process is not part of the computation communicator. */
    int comp_rank;

    /** The rank of this process in the IO communicator, or -1 if this
     * process is not part of the IO communicator. */
    int io_rank;

    /** Set to MPI_ROOT if this task is the master of IO communicator, 0
     * otherwise. */
    int iomaster;

    /** Set to MPI_ROOT if this task is the master of comp communicator, 0
     * otherwise. */
    int compmaster;

    /** Rank of IO root task (which is rank 0 in io_comm) in the union
     * communicator. Will always be 0 for async situations. */
    int ioroot;

    /** Rank of computation root task (which is rank 0 in
     * comm_comms[cmp]) in the union communicator. Will always = number
     * of IO tasks in async situations. */
    int comproot;

    /** An array of the ranks of all IO tasks within the union
     * communicator. */
    int *ioranks;

    /** An array of the ranks of all computation tasks within the
     * union communicator. */
    int *compranks;

    /** Controls handling errors. */
    int error_handler;

    /** The rearranger decides which parts of a distributed array are
     * handled by which IO tasks. */
    int default_rearranger;

    /** True if asynchronous interface is in use. */
    bool async;

    /** True if this task is a member of the IO communicator. */
    bool ioproc;

    /** True if this task is a member of a computation
     * communicator. */
    bool compproc;

    /** MPI Info object. */
    MPI_Info info;

    /** Async I/O service message info */
    struct async_ios_msg_info_{
      int seq_num;
      int prev_msg;
    } async_ios_msg_info;

    /** Index of this component in the list of components. */
    int comp_idx;

    /** Rearranger options. */
    rearr_opt_t rearr_opts;

#ifdef _ADIOS2
    /* ADIOS handles */
    adios2_adios *adiosH; /* Handle for ADIOS write */
    adios2_adios *adios_readerH; /* Handle for ADIOS read */
    int adios_io_process;
    MPI_Comm adios_comm;
    int adios_rank, num_adiostasks;
    /* Block merging setup */
    MPI_Comm block_comm;
    int block_myrank, block_nprocs;
#endif

    /** I/O statistics associated with this I/O system */
    struct spio_io_fstats_summary *io_fstats;

    /** Pointer to the next iosystem_desc_t in the list. */
    struct iosystem_desc_t *next;
} iosystem_desc_t;

/**
 * The multi buffer holds data from one or more variables. Data are
 * accumulated in the multi-buffer.
 */
typedef struct wmulti_buffer
{
    /** The ID that describes the decomposition, as returned from
     * PIOc_Init_Decomp().  */
    int ioid;

    /** Non-zero if this is a buffer for a record var. */
    int recordvar;

    /** Number of arrays of data in the multibuffer. Each array had
     * data for one var or record. When multibuffer is flushed, all
     * arrays are written and num_arrays returns to zero. */
    int num_arrays;

    /** Size of this variables data on local task. All vars in the
     * multi-buffer have the same size. */
    int arraylen;

    /** Array of varids. */
    int *vid;

    /** An array of current record numbers, for record vars. One
     * element per variable. */
    int *frame;

    /** Array of fill values used for each var. */
    void *fillvalue;

    /** Pointer to the data. */
    void *data;

    /** Pointer to the next multi-buffer in the list. */
    struct wmulti_buffer *next;
} wmulti_buffer;

#ifdef _ADIOS2
/* Interval map for ADIOS read */
typedef struct adios_interval_map_t
{
    int n_adios_steps;
    short **map;
} adios_interval_map_t;

/* Maximum char array length to store "darray" or "put_var", for ADIOS read */
#define NC_OP_TAG_MAX_LEN 8

/** Variable definition information saved at pioc_def_var,
 * so that ADIOS can define the variable at write time when
 * local dimensions and offsets are known.
 */
typedef struct adios_var_desc_t
{
    /** Variable name */
    char * name;

    /** NC type give at def_var time */
    int nc_type;

    /** Type converted from NC type to adios type */
    adios2_type adios_type;
    size_t adios_type_size;

    /** Number of dimensions */
    int ndims;

    /** Global dims (dim var ids) */
    int * gdimids;

    /** Number of attributes defined for this variable */
    int nattrs;

    /** ADIOS varID, if it has already been defined.
     * We avoid defining again when writing multiple records over time
     */
    adios2_variable* adios_varid; // 0: undefined yet

    /* to handle PIOc_setframe with different decompositions */
    adios2_variable* decomp_varid;
    adios2_variable* frame_varid;
    adios2_variable* fillval_varid;
    adios2_variable* num_block_writers_varid;

    /* to handle multi-dimensional temporal variables */
    adios2_variable* start_varid;
    adios2_variable* count_varid;

    /* to buffer decomp id, frame id, fill value, and writer blocks */
    int32_t *decomp_buffer;
    int32_t *frame_buffer;
    char *fillval_buffer;
    int32_t fillval_size;
    int32_t *num_wb_buffer;
    int32_t decomp_cnt, frame_cnt, fillval_cnt, num_wb_cnt;
    int32_t max_buffer_cnt;

    /* Simplified version of an interval map implementation
     * index is a frame_id and the value is the adios_step */
    adios_interval_map_t* interval_map; /* For ADIOS read */

    /* Is this variable written with put_var or darray? */
    char nc_op_tag[NC_OP_TAG_MAX_LEN]; /* For ADIOS read */
} adios_var_desc_t;

/* Track attributes */
typedef struct adios_att_desc_t
{
    /** Attribute name */
    char *att_name;

    /** NC type give at def_att time */
    nc_type att_type;

    /** length of attribute value */
    PIO_Offset att_len;

    /** ncid of the attribute */
    int att_ncid;

    /** attribute varid */
    int att_varid;

    /** Type converted from NC type to adios type */
    adios2_type adios_type;
} adios_att_desc_t;
#endif /* _ADIOS2 */

#ifdef _HDF5
typedef struct hdf5_dim_desc_t
{
    /** Dimension name */
    char* name;

    /** Dimension length */
    PIO_Offset len;

    /** True if the dimension has a coordinate variable */
    bool has_coord_var;

    hid_t hdf5_dataset_id;
} hdf5_dim_desc_t;

typedef struct hdf5_var_desc_t
{
    /** Variable name */
    char* name;

    /* Alternative name for a non-coordinate variable with the same name as a dimension */
    char* alt_name;

    /** NC type give at def_var time */
    int nc_type;

    /** Type converted from NC type to HDF5 type */
    hid_t hdf5_type;

    /** Number of dimensions */
    int ndims;

    /** Dimension IDs of the variable */
    int* hdf5_dimids;

    /** True if the variable is a coordinate variable */
    bool is_coord_var;

    hid_t hdf5_dataset_id;
} hdf5_var_desc_t;

typedef struct hdf5_att_desc_t
{
    /** Attribute name */
    char *att_name;

    /** NC type give at def_att time */
    nc_type att_type;

    /** length of attribute value */
    PIO_Offset att_len;

    /** ncid of the attribute */
    int att_ncid;

    /** attribute varid */
    int att_varid;
} hdf5_att_desc_t;
#endif

/**
 * File descriptor structure.
 *
 * This structure holds information associated with each open file
 */
typedef struct file_desc_t
{
    /** The IO system ID used to open this file. */
    iosystem_desc_t *iosystem;

    /** The ncid returned for this file by the underlying library
     * (netcdf or pnetcdf). */
    int fh;

#ifdef _ADIOS2
    /** Save the filename, now just for printing it at close */
    char *filename;

    /** ADIOS file handler is 64bit integer */
    adios2_engine *engineH;

    /** Check if begin_step has been invoked. It is used to invoke end_step **/
    int begin_step_called;
    int num_step_calls;;
    int max_step_calls;

    /*
     * Used to call adios2_end_step to avoid buffer overflow in MPI_Gatherv
     * during ADIOS metadata write operation.
     *
     * if num_written_blocks * BLOCK_METADATA_SIZE >= BLOCK_COUNT_THRESHOLD, call adios2_end_step
     * (Not implemented in this version. adios2_end_step is called if num_step_calls >= max_step_calls (= PIO_MAX_CACHED_STEPS_FOR_ADIOS))
     */
    unsigned int num_written_blocks;

    int write_decomp_id;
    int write_frame_id;
    int write_fillval_id;

    /** Handler for ADIOS group (of variables) */
    adios2_io *ioH;

    /** ADIOS output transport method name, POSIX or MPI_AGGREGATE */
    char transport[PIO_MAX_NAME];

    /** Parameters for the transport method, required for MPI_AGGREGATE.
     * Created automatically from the application setup */
    char params[PIO_MAX_NAME];

    /** Need to store the dim names for finding them and using them when defining variables */
    char *dim_names[PIO_MAX_DIMS];
    PIO_Offset dim_values[PIO_MAX_DIMS];

    /** Number of dim vars defined */
    int num_dim_vars;

    /** Variable information, max PIO_MAX_VARS variables allowed */
    struct adios_var_desc_t adios_vars[PIO_MAX_VARS];

    /** Number of vars defined */
    int num_vars;

    /** Number of global attributes defined. Needed to support PIOc_inq_nattrs() */
    int num_gattrs;

    /* all procs rank, etc */
    MPI_Comm all_comm;
    int all_rank, num_alltasks;

    /* ADIOS rank, etc */
    int adios_io_process;
    MPI_Comm adios_comm;
    int adios_rank, num_adiostasks;

    /* Merging distributed array blocks to reduce I/O overhead */
    /* Grouping of processes for block merging */
    MPI_Comm block_comm;
    int block_myrank, block_nprocs;
    int *block_list;

    /* Buffers for merging distributed array blocks */
    unsigned int *array_counts;
    unsigned int array_counts_size;
    unsigned int *array_disp;
    unsigned int array_disp_size;
    char *block_array;
    size_t block_array_size;

    /* Track attributes */
    /** attribute information. Allow PIO_MAX_VARS for now. */
    struct adios_att_desc_t adios_attrs[PIO_MAX_ATTRS];
    int num_attrs;

    int fillmode;

    /* Handle PIO_Offset */
    int pio_offset_size;
    adios2_type pio_offset_type;

    /** Array for decompositions that has been written already (must write only once) */
    int n_written_ioids;
    int written_ioids[PIO_MAX_ADIOS_DECOMPS]; /* written_ioids[N] = ioid if that decomp has been already written, */

    /** Store current frameid for end_step in PIO_setframe */
    int current_frame;

    /* Some caches (hash tables) for ADIOS read */
    struct spio_hmap *cache_data_blocks;
    struct spio_hmap *cache_block_sizes;
    struct spio_hmap *cache_darray_info;

    char io_name_reader[PIO_MAX_NAME + 1]; /* Name of io object, for ADIOS read */
    size_t adios_reader_num_decomp_blocks; /* Number of decomposition blocks, for ADIOS read */

    /* Indicates whether the decomposition maps (for ADIOS write) need to be stored in BP files. Default is true. */
    bool store_adios_decomp;
#endif /* _ADIOS2 */

#ifdef _HDF5
    hid_t hdf5_file_id;

    /* Collective dataset transfer property list, used by H5Dwrite */
    hid_t dxplid_coll;

    /* Independent dataset transfer property list, used by H5Dwrite */
    hid_t dxplid_indep;

    struct hdf5_dim_desc_t hdf5_dims[PIO_MAX_DIMS];

    /** Number of dims defined */
    int hdf5_num_dims;

    struct hdf5_var_desc_t hdf5_vars[PIO_MAX_VARS];

    /** Number of vars defined */
    int hdf5_num_vars;

    struct hdf5_att_desc_t hdf5_attrs[PIO_MAX_ATTRS];

    /** Number of attrs defined */
    int hdf5_num_attrs;

    /** Number of global attrs defined */
    int hdf5_num_gattrs;
#endif /* _HDF5 */

    /* File name - cached */
    char fname[PIO_MAX_NAME + 1];

    /** The ncid that will be returned to the user. */
    int pio_ncid;

    /** The PIO_TYPE value that was used to open this file. */
    int iotype;

    /** List of variables in this file (deprecated). */
    struct var_desc_t varlist[PIO_MAX_VARS];

    /* Number of unlimited dim ids, if no unlimited id present = 0 */
    int num_unlim_dimids;

    /* Unlimited dim ids, if no unlimited id present = NULL */
    int *unlim_dimids;

    /** Mode used when file was opened. */
    int mode;

    /** The wmulti_buffer is used to aggregate multiple variables with
     * the same communication pattern prior to a write. */
    struct wmulti_buffer buffer;

    /* Bytes pending to be read on this file*/
    PIO_Offset rb_pend;

    /* Bytes pending to be written out for this file */
    PIO_Offset wb_pend;

    /** Data buffer per IO decomposition for this file. */
    /** Cache for storing data corresponding to multiple
     * variables binned on the I/O decomposition used by
     * the variable */
    void *mvcache;

    /** I/O statistics associated with this file */
    struct spio_io_fstats_summary *io_fstats;

    /** Pointer to the next file_desc_t in the list of open files. */
    struct file_desc_t *next;

    /** True if this task should participate in IO (only true for one
     * task with netcdf serial files. */
    int do_io;

    /** True if we need reserve some extra space in the header when
     * creating NetCDF files to accommodate anticipated changes. */
    bool reserve_extra_header_space;
} file_desc_t;

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

    /** ADIOS parallel */
    PIO_IOTYPE_ADIOS = 5,

    /** HDF5 parallel */
    PIO_IOTYPE_HDF5 = 6
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
    PIO_REARR_ANY = 3
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
