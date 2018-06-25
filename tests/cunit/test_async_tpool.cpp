#include <vector>
#include <thread>
#include <chrono>
#include <algorithm>
#include <cassert>
#include "pio_async_mtq.hpp"
#include "pio_async_tpool.hpp"
extern "C"{
#include <pio.h>
#include <pio_internal.h>
#include <pio_tests.h>
}

#define LOG_RANK0(rank, ...)                     \
            do{                                   \
                if(rank == 0)                     \
                {                                 \
                    fprintf(stderr, __VA_ARGS__); \
                }                                 \
            }while(0);

static const int FAIL = -1;

class UType{
  public:
    int nwaits;
};

std::ostream &operator<<(std::ostream &ostr, const UType &utype)
{
  ostr << "(" << utype.nwaits << "), ";
  return ostr;
}

std::ostream &operator<<(std::ostream &ostr, const UType *putype)
{
  ostr << "(" << putype->nwaits << "), ";
  return ostr;
}

int pio_utype_wait(void *pdata)
{
  UType *ut = static_cast<UType *>(pdata);
  assert(ut);

  ut->nwaits++;
  return PIO_NOERR;
}

int pio_poke_func_unavail(void *pdata, int *)
{
  assert(0);
  return PIO_NOERR;
}

void pio_noop_free(void *pdata)
{
}

/* Test enqueue and dequeue operations in a multi-threaded queue containing
 * pointers to user defined types */
int test_tpool_putype(int wrank, const int max_elems_in_q)
{
    assert((wrank >= 0) && (max_elems_in_q > 0));

    std::vector<UType> udata;
    PIO_Util::PIO_async_tpool_manager tpool_mgr;
    PIO_Util::PIO_async_tpool *tpool = tpool_mgr.get_tpool_instance();

    for(int i=0; i<max_elems_in_q; i++){
      UType tmp_data;
      tmp_data.nwaits = 0;
      udata.push_back(tmp_data);  
    }

    for(int i=0; i<max_elems_in_q; i++){
      pio_async_op_t *op = (pio_async_op_t *)calloc(1, sizeof(pio_async_op_t));
      /* Only file write ops are supported */
      op->op_type = PIO_ASYNC_FILE_WRITE_OPS;
      op->pdata = &(udata[i]);
      op->wait = pio_utype_wait;
      op->poke = pio_poke_func_unavail;
      op->free = pio_noop_free;
      tpool->enqueue(op);
    }

    /* tpool is managed by the tpool manager - no delete/free reqd */

    return PIO_NOERR;
}

int test_driver(MPI_Comm comm, int wrank, int wsz, int *num_errors)
{
    int nerrs = 0, ret = PIO_NOERR;
    assert((comm != MPI_COMM_NULL) && (wrank >= 0) && (wsz > 0) && num_errors);
    
    /* Test enqueueing 10 ptr to user defined elems into the thread pool,
     */
    try{
      ret = test_tpool_putype(wrank, 10);
    }
    catch(...){
      ret = PIO_EINTERNAL;
    }
    if(ret != PIO_NOERR)
    {
        LOG_RANK0(wrank, "test_tpool_putype(10 elems) FAILED, ret = %d\n", ret);
        nerrs++;
    }
    else{
        LOG_RANK0(wrank, "test_tpool_putype(10 elems) PASSED\n");
    }


    *num_errors += nerrs;
    return nerrs;
}

int main(int argc, char *argv[])
{
    int ret;
    int wrank, wsz;
    int num_errors;
#ifdef TIMING
#ifndef TIMING_INTERNAL
    ret = GPTLinitialize();
    if(ret != 0)
    {
        LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
        return ret;
    }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

    ret = MPI_Init(&argc, &argv);
    if(ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Init() FAILED, ret = %d\n", ret);
        return ret;
    }

    ret = MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
    if(ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
        return ret;
    }
    ret = MPI_Comm_size(MPI_COMM_WORLD, &wsz);
    if(ret != MPI_SUCCESS)
    {
        LOG_RANK0(wrank, "MPI_Comm_rank() FAILED, ret = %d\n", ret);
        return ret;
    }

    num_errors = 0;
    ret = test_driver(MPI_COMM_WORLD, wrank, wsz, &num_errors);
    if(ret != 0)
    {
        LOG_RANK0(wrank, "Test driver FAILED\n");
        return FAIL;
    }
    else{
        LOG_RANK0(wrank, "All tests PASSED\n");
    }

    MPI_Finalize();

#ifdef TIMING
#ifndef TIMING_INTERNAL
    ret = GPTLfinalize();
    if(ret != 0)
    {
        LOG_RANK0(wrank, "GPTLinitialize() FAILED, ret = %d\n", ret);
        return ret;
    }
#endif /* TIMING_INTERNAL */
#endif /* TIMING */

    if(num_errors != 0)
    {
        LOG_RANK0(wrank, "Total errors = %d\n", num_errors);
        return FAIL;
    }
    return 0;
}
