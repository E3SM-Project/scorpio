#include "pio_bget_utils.hpp"

namespace PIO_Util{
namespace BGet_Util{

void *Bget_mpool::mblock_alloc(std::size_t sz)
{
  void *p = malloc(sz);
  if(p != NULL){
    mblocks_.push_back(p);
  }
  return p;
}

void Bget_mpool::mblock_free(void *p)
{
  /* We do the simplest thing here, we ignore the frees
   * Ideally, we could move the freed block into a free block list
   * that can be added back to the memory pool
   * All the allocated blocks are freed in the destructor
   */ 
}

Bget_mpool::~Bget_mpool()
{
  for(std::list<void *>::iterator iter = mblocks_.begin();
      iter != mblocks_.end(); ++iter){
    assert(*iter);
    free(*iter);
  }
  mblocks_.clear();
}

Bget_mpool *Bget_mpool_manager::mpool_ = NULL;

void *Bget_mpool_manager::mpool_init(std::size_t sz)
{
  void *pblock = NULL;
  /* Ignore multiple initializations */
  if(mpool_ == NULL){
    mpool_ = new Bget_mpool();
    assert(mpool_);
    pblock = mpool_->mblock_alloc(sz);
  }
  return pblock;
}

Bget_mpool *Bget_mpool_manager::get_mpool_instance(void )
{
  if(mpool_ == NULL){
    mpool_ = new Bget_mpool();
  }
  return mpool_;
}

void Bget_mpool_manager::mpool_finalize(void )
{
  if(mpool_){
    delete mpool_;
  }
  mpool_ = NULL;
}

} // namespace PIO_Util
} //namespace BGet_Util

/* The C interfaces */
void *pio_bget_mblock_alloc(bufsize sz)
{
  return PIO_Util::BGet_Util::Bget_mpool_manager::get_mpool_instance()->mblock_alloc(static_cast<std::size_t>(sz));
}

void pio_bget_mblock_free(void *p)
{
  PIO_Util::BGet_Util::Bget_mpool_manager::get_mpool_instance()->mblock_free(p);
}

void *pio_bget_mpool_init(bufsize sz)
{
  return PIO_Util::BGet_Util::Bget_mpool_manager::mpool_init(static_cast<std::size_t>(sz));
}

void pio_bget_mpool_finalize(void )
{
  PIO_Util::BGet_Util::Bget_mpool_manager::mpool_finalize();
}
