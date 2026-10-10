#include <vector>
#include <cassert>
#include <cstdlib>

#include "pio_config.h"
#include "pio.h"
#include "pio_tests.h"
#include "pio_internal.h"

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
#include "gptl.h"
#endif
#endif

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

static const int FAIL = -1;

/* Create (Alloc + Init) an I/O system and return it */
iosystem_desc_t *get_iosystem(MPI_Comm comm, int wrank, int wsz)
{
  static int iosysid = 1;
  iosystem_desc_t *ios = (iosystem_desc_t *) calloc(1, sizeof(iosystem_desc_t));
  if(!ios){
    LOG_RANK0(wrank, "Unable to allocate memory for I/O system\n");
    return NULL;
  }

  ios->iosysid = iosysid++;
  ios->union_comm = comm;
  ios->num_uniontasks = wsz;
  ios->union_rank = wrank;
  /* Every proc is a compute proc */
  ios->comp_comm = comm;
  ios->num_comptasks = wsz;
  ios->comp_rank = wrank;
  ios->compproc = true;

  return ios;
}

void free_iosystem(iosystem_desc_t *ios)
{
  if(ios) { free(ios); }
}

io_desc_t *get_iodesc(iosystem_desc_t *ios, int rearranger,
                      const std::vector<PIO_Offset> &compmap, const std::vector<int> &gdimlen)
{
  /* map_zero_based = true.
   * Note: compmap below is given as a 0-based map (SUBSET internally converts it to 1-based)
   */
  return new io_desc_t(ios, PIO_DOUBLE, static_cast<int>(gdimlen.size()), gdimlen.data(),
                        static_cast<int>(compmap.size()), compmap.data(), rearranger, true);
}

void free_iodesc(io_desc_t *iodesc)
{
  if(iodesc) { delete iodesc; }
}

/* For PIO_REARR_SUBSET decomps with async_io_desc_info still NULL (the
 * default for every freshly constructed io_desc_t) select_io_desc_info()
 * must fall back to sync_io_desc_info
 * (Regardless of whether async writes are enabled)
 */
int test_select_subset_no_async(int wrank, iosystem_desc_t *ios)
{
  std::vector<int> gdimlen = {4};
  std::vector<PIO_Offset> compmap = {0, 1};

  io_desc_t *iodesc = get_iodesc(ios, PIO_REARR_SUBSET, compmap, gdimlen);
  if(!iodesc){
    LOG_RANK0(wrank, "Unable to create I/O desc for PIO_REARR_SUBSET\n");
    return PIO_EINTERNAL;
  }

  int ret = PIO_NOERR;
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5)) != &(iodesc->sync_io_desc_info)){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5) for a SUBSET decomp did not return sync_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_NETCDF4P)) != &(iodesc->sync_io_desc_info)){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_NETCDF4P) for a SUBSET decomp did not return sync_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }

  free_iodesc(iodesc);
  return ret;
}

/* For PIO_REARR_CONTIG decomps with async_io_desc_info still NULL (the
 * default for every freshly constructed io_desc_t) select_io_desc_info()
 * must fall back to sync_io_desc_info
 * (Regardless of whether async writes are enabled)
 */
int test_select_contig_no_async(int wrank, iosystem_desc_t *ios)
{
  std::vector<int> gdimlen = {4};
  std::vector<PIO_Offset> compmap = {0, 1};

  io_desc_t *iodesc = get_iodesc(ios, PIO_REARR_CONTIG, compmap, gdimlen);
  if(!iodesc){
    LOG_RANK0(wrank, "Unable to create I/O desc for PIO_REARR_CONTIG\n");
    return PIO_EINTERNAL;
  }

  int ret = PIO_NOERR;
  if(iodesc->async_io_desc_info != NULL){
    LOG_RANK0(wrank, "Newly constructed CONTIG io_desc_t unexpectedly has a non-NULL async_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }

  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5)) != &(iodesc->sync_io_desc_info)){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5) for a CONTIG decomp with NULL async_io_desc_info did not return sync_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }

  free_iodesc(iodesc);
  return ret;
}

#if PIO_USE_ASYNC_WR_THREAD
/* For PIO_REARR_CONTIG decomps, once async_io_desc_info has been allocated
 * select_io_desc_info() must return it for HDF5/HDF5C writes and must still
 * return sync_io_desc_info for any other iotype.
 */
int test_select_contig_with_async(int wrank, iosystem_desc_t *ios)
{
  std::vector<int> gdimlen = {4};
  std::vector<PIO_Offset> compmap = {0, 1};

  io_desc_t *iodesc = get_iodesc(ios, PIO_REARR_CONTIG, compmap, gdimlen);
  if(!iodesc){
    LOG_RANK0(wrank, "Unable to create I/O desc for PIO_REARR_CONTIG\n");
    return PIO_EINTERNAL;
  }

  int ret = PIO_NOERR;
  try{
    /* Manually allocate the stub async io_desc_info_t - io_desc_t's
     * destructor deletes it
     */
    iodesc->async_io_desc_info = new io_desc_info_t(ios, PIO_DOUBLE,
        static_cast<int>(gdimlen.size()), gdimlen.data(),
        static_cast<int>(compmap.size()), compmap.data(), PIO_REARR_CONTIG, true);
  }
  catch(...){
    LOG_RANK0(wrank, "Unable to create stub async io_desc_info_t\n");
    free_iodesc(iodesc);
    return PIO_EINTERNAL;
  }

  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5)) != iodesc->async_io_desc_info){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5) for a CONTIG decomp with non-NULL async_io_desc_info did not return async_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5C)) != iodesc->async_io_desc_info){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5C) for a CONTIG decomp with non-NULL async_io_desc_info did not return async_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_NETCDF4P)) != &(iodesc->sync_io_desc_info)){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_NETCDF4P) for a CONTIG decomp with non-NULL async_io_desc_info unexpectedly bypassed sync_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }

  free_iodesc(iodesc);
  return ret;
}

/* For PIO_REARR_SUBSET decomps, once async_io_desc_info has been allocated
 * (mimicking what Task 6 does in subset_rearranger_init()), select_io_desc_info()
 * must return it for HDF5/HDF5C writes - this is the behavior Task 7 added;
 * it did not exist before this plan (see the old, now-corrected comment on
 * test_select_subset_no_async() above). */
int test_select_subset_with_async(int wrank, iosystem_desc_t *ios)
{
  std::vector<int> gdimlen = {4};
  std::vector<PIO_Offset> compmap = {0, 1};

  io_desc_t *iodesc = get_iodesc(ios, PIO_REARR_SUBSET, compmap, gdimlen);
  if(!iodesc){
    LOG_RANK0(wrank, "Unable to create I/O desc for PIO_REARR_SUBSET\n");
    return PIO_EINTERNAL;
  }

  int ret = PIO_NOERR;
  try{
    iodesc->async_io_desc_info = new io_desc_info_t(ios, PIO_DOUBLE,
        static_cast<int>(gdimlen.size()), gdimlen.data(),
        static_cast<int>(compmap.size()), compmap.data(), PIO_REARR_SUBSET,
        /*map_zero_based=*/true);
  }
  catch(...){
    LOG_RANK0(wrank, "Unable to create stub async io_desc_info_t for PIO_REARR_SUBSET\n");
    free_iodesc(iodesc);
    return PIO_EINTERNAL;
  }

  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5)) != iodesc->async_io_desc_info){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5) for a SUBSET decomp with non-NULL async_io_desc_info did not return async_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }
  /* Review Focus: two variables sharing the same (iodesc, iotype, ios)
   * triple - e.g. two PIO_IOTYPE_HDF5 variables on the same ioid - must be
   * routed identically, so rearranged data and the later write dispatch
   * never disagree about which rank set holds it. select_io_desc_info()
   * takes no write-specific input, so a second call with the same iotype
   * must return the exact same reference as the first. */
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5)) != iodesc->async_io_desc_info){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5) for a SUBSET decomp is not deterministic across repeated calls\n");
    ret = PIO_EINTERNAL;
  }
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_NETCDF4P)) != &(iodesc->sync_io_desc_info)){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_NETCDF4P) for a SUBSET decomp with non-NULL async_io_desc_info unexpectedly bypassed sync_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }

  free_iodesc(iodesc);
  return ret;
}

/* Same as test_select_subset_with_async() above, for PIO_REARR_BOX. */
int test_select_box_with_async(int wrank, iosystem_desc_t *ios)
{
  std::vector<int> gdimlen = {4};
  std::vector<PIO_Offset> compmap = {0, 1};

  io_desc_t *iodesc = get_iodesc(ios, PIO_REARR_BOX, compmap, gdimlen);
  if(!iodesc){
    LOG_RANK0(wrank, "Unable to create I/O desc for PIO_REARR_BOX\n");
    return PIO_EINTERNAL;
  }

  int ret = PIO_NOERR;
  try{
    iodesc->async_io_desc_info = new io_desc_info_t(ios, PIO_DOUBLE,
        static_cast<int>(gdimlen.size()), gdimlen.data(),
        static_cast<int>(compmap.size()), compmap.data(), PIO_REARR_BOX,
        /*map_zero_based=*/true);
  }
  catch(...){
    LOG_RANK0(wrank, "Unable to create stub async io_desc_info_t for PIO_REARR_BOX\n");
    free_iodesc(iodesc);
    return PIO_EINTERNAL;
  }

  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5)) != iodesc->async_io_desc_info){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5) for a BOX decomp with non-NULL async_io_desc_info did not return async_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }
  /* Review Focus: two variables sharing the same (iodesc, iotype, ios)
   * triple must be routed identically - see the equivalent check in
   * test_select_subset_with_async() above for the full rationale. */
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_HDF5)) != iodesc->async_io_desc_info){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_HDF5) for a BOX decomp is not deterministic across repeated calls\n");
    ret = PIO_EINTERNAL;
  }
  if(&(iodesc->select_io_desc_info(PIO_IOTYPE_NETCDF4P)) != &(iodesc->sync_io_desc_info)){
    LOG_RANK0(wrank, "select_io_desc_info(PIO_IOTYPE_NETCDF4P) for a BOX decomp with non-NULL async_io_desc_info unexpectedly bypassed sync_io_desc_info\n");
    ret = PIO_EINTERNAL;
  }

  free_iodesc(iodesc);
  return ret;
}
#endif /* PIO_USE_ASYNC_WR_THREAD */

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
  int nerrs = 0, ret = PIO_NOERR;
  assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);

  iosystem_desc_t *ios = get_iosystem(comm, wrank, wsz);
  if(!ios){
    LOG_RANK0(wrank, "Unable to create mock I/O system\n");
    *num_errors += 1;
    return 1;
  }

  try{
    ret = test_select_subset_no_async(wrank, ios);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_select_subset_no_async() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_select_subset_no_async() PASSED\n");
  }

  try{
    ret = test_select_contig_no_async(wrank, ios);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_select_contig_no_async() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_select_contig_no_async() PASSED\n");
  }

#if PIO_USE_ASYNC_WR_THREAD
  try{
    ret = test_select_contig_with_async(wrank, ios);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_select_contig_with_async() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_select_contig_with_async() PASSED\n");
  }

  try{
    ret = test_select_subset_with_async(wrank, ios);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_select_subset_with_async() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_select_subset_with_async() PASSED\n");
  }

  try{
    ret = test_select_box_with_async(wrank, ios);
  }
  catch(...){
    ret = PIO_EINTERNAL;
  }
  if(ret != PIO_NOERR){
    LOG_RANK0(wrank, "test_select_box_with_async() FAILED, ret = %d\n", ret);
    nerrs++;
  }
  else{
    LOG_RANK0(wrank, "test_select_box_with_async() PASSED\n");
  }
#endif

  free_iosystem(ios);

  *num_errors += nerrs;
  return nerrs;
}

int main(int argc, char *argv[])
{
  int ret;
  int wrank, wsz;
  int num_errors;
#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLinitialize();
  if(ret != 0){
    LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  ret = pio_test_mpi_init(&argc, &argv);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Init() FAILED, ret = %d\n", ret);
    return ret;
  }

  ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
    return ret;
  }
  ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
  if(ret != MPI_SUCCESS){
    LOG_RANK0(wrank, "MPI_Comm_size() FAILED, ret = %d\n", ret);
    return ret;
  }

  num_errors = 0;
  ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
  if(ret != 0){
    LOG_RANK0(wrank, "Test driver FAILED\n");
    return FAIL;
  }
  else{
    LOG_RANK0(wrank, "All tests PASSED\n");
  }

  MPI_Finalize();

#ifdef SPIO_ENABLE_GPTL_TIMING
#ifndef SPIO_ENABLE_GPTL_TIMING_INTERNAL
  ret = GPTLfinalize();
  if(ret != 0){
    LOG_RANK0(wrank, "GPTLfinalize() FAILED, ret = %d\n", ret);
    return ret;
  }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

  if(num_errors != 0){
    LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
    return FAIL;
  }
  return 0;
}
