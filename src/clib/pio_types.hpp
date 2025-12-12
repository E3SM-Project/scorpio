#ifndef __PIO_TYPES_HPP__
#define __PIO_TYPES_HPP__

#include <cstdio>
#include <cstdlib>
#include <cstdbool>
#include <cstring>

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
#endif

#ifdef _HDF5
#include <hdf5.h>
#include <hdf5_hl.h>
#include <unistd.h>
#endif

#include <atomic>
#include <mutex>

#ifdef PIO_MICRO_TIMING
/** Some fwd declarations to avoid including internal headers */
typedef struct mtimer_info *mtimer_t;
#endif

/* Forward decl of hash map used for ADIOS reads */
struct spio_hmap;

/* Fwd declaration to use back pointer to var_desc_t in viobuf_cache */
struct var_desc_t;

/** The viobuf_cache is used to cache the rearranged data for the
 * variable. The iobuf inside the cache is freed when the data
 * is written out.
 */
typedef struct viobuf_cache
{
    /* Pointer to user buffer, ubuf points to internal PIO
     * buffer that caches the user data
     */
    void *ubuf;
    size_t ubuf_sz;
    /* Pointer to buffer containing the rearranged data */
    void *iobuf;
    size_t iobuf_sz;
    /* Pointer to fillvalue for this iobuf */
    void *fillvalue;
    size_t fillvalue_sz;
    /* Variable record/frame number corresponding to this
     * buffer.
     * Note: Multiple frames for a variable can be cached
     * at a time in a list of viobuf_cache s
     */
    int rec_num;
    /* Any outstanding request associated with this cache */
    /* Used to keep track of a write request for the iobuf */
    int req;
    /* Back pointer to the var_desc that contains this cache */
    struct var_desc_t *vdesc;
    /* The next iobuf cache for this variable, each iobuf
     * cache corresponds to a record/frame of the variable
     */
    struct viobuf_cache *next;
} viobuf_cache_t;

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

    /** Data buffers (to store rearranged data) for this var */
    /** Data buffers are stored in a singly linked list */
    viobuf_cache_t *viobuf_lhead;
    viobuf_cache_t *viobuf_ltail;

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


namespace SPIO{
  namespace DataRearr{
    class Contig_rearr;
  }
}

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

    /* FIXME: Once we have classes for subset/box this ptr should be to a base rearr class */
    SPIO::DataRearr::Contig_rearr *rearr;

    /* Number of pending async ops using this I/O desc */
    std::atomic_int nasync_pend_ops;

    /** Pointer to the next io_desc_t in the list. */
    struct io_desc_t *next;
} io_desc_t;

/**
 * PIO asynchronous operation types
 */
typedef enum pio_async_op_type
{
    PIO_ASYNC_INVALID_OP = 0,
    PIO_ASYNC_REARR_OP,
    PIO_ASYNC_PNETCDF_WRITE_OP,
    PIO_ASYNC_HDF5_WRITE_OP,
    PIO_ASYNC_FILE_WRITE_OPS,
    PIO_ASYNC_FILE_CLOSE_OP,
    PIO_ASYNC_NUM_OP_TYPES
} pio_async_op_type_t;

/**
 * PIO asynchronous op
 */
typedef struct pio_async_op
{
    pio_async_op_type_t op_type;
    void *pdata;
    /* Blocking wait function for this async op
     * param 1 : A user defined data pointer
     * return : PIO_NOERR on success, pio error code on failure
     */
    int (*wait)(void *);
    /* Non-blocking function for making progress on this async op
     * param 1 : A user defined data pointer
     * param 2 : Pointer to a flag that is set to true if async op
     * is complete, false otherwise
     * return : PIO_NOERR on success, pio error code on failure
     */
    int (*poke)(void *, int *);
    /* Free function for user defined pdata */
    void (*free)(void *);
    struct pio_async_op *next;
} pio_async_op_t;

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

    /* Comm that includes procs from comp_comm that are local to this
     * compute node (share the same memory) */
    MPI_Comm node_comm;

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
    #ifdef _SPIO_ADIOS_USE_COMPRESSION
    /* ADIOS operator for applying a specific lossless compression method (e.g., Blosc2, BZip2) */
    adios2_operator* lossless_compression_operator;
    int adios_lossless_compression_method;
    #ifdef _SPIO_ADIOS_USE_LOSSY_COMPRESSION
    /* ADIOS operator for applying a specific lossy compression method (e.g., SZ, MGARD, ZFP) */
    adios2_operator* lossy_compression_operator;
    int adios_lossy_compression_method;
    #endif
    #endif
#endif

    /** I/O statistics associated with this I/O system */
    struct spio_io_fstats_summary *io_fstats;

    /* Number of pending async operations on this iosystem */
    int nasync_pend_ops;

    /* List of pending async operations on this iosystem */
    pio_async_op_t *async_pend_ops;

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

#ifdef _SPIO_ADIOS_USE_COMPRESSION
enum ADIOS_COMPRESSION_METHOD
{
    ADIOS_COMPRESSION_METHOD_BLOSC2 = 1,

    ADIOS_COMPRESSION_METHOD_BZIP2 = 2,

    ADIOS_COMPRESSION_METHOD_MGARD = 3,

    ADIOS_COMPRESSION_METHOD_SZ = 4,

    ADIOS_COMPRESSION_METHOD_ZFP = 5
};
#endif
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
    /** A datatype converter for user buffers */
    void *dt_converter;

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

    /* Number of pending async operations on this file */
    int nasync_pend_ops;

    /* List of pending async operations on this file */
    pio_async_op_t *async_pend_ops;

    /** Total number of pending ops on this file, including
      * nasync_pend_ops. In the case where nasync_pend_ops == 0
      * npend_ops shows any other pending ops (e.g. non-blocking
      * write done of rearranged data, still need to wait 
      * on it) */
    std::atomic<int> npend_ops;
    std::atomic<bool> is_hard_closed;
    std::mutex *pmtx;

    /** Pointer to the next file_desc_t in the list of open files. */
    struct file_desc_t *next;

    /** True if this task should participate in IO (only true for one
     * task with netcdf serial files. */
    int do_io;

    /** True if we need reserve some extra space in the header when
     * creating NetCDF files to accommodate anticipated changes. */
    bool reserve_extra_header_space;

    /** True if this is an existing file reopened */
    bool is_reopened;
} file_desc_t;

#endif // __PIO_TYPES_HPP__
