#include "spio_dt_converter.hpp"

void *SPIO_Util::File_Util::DTConverter::convert(int ncid, void *buf, std::size_t sz, nc_type from_pio_type, nc_type to_pio_type)
{
  if(from_pio_type == to_pio_type){
    /* No conversion required, return the buffer as is */
    return buf;
  }

  std::size_t nelems = sz / size_of(from_pio_type);
  
  Cachebuf cbuf = { bget(nelems * size_of(to_pio_type)), to_pio_type };

  copy_to(buf, from_pio_type, cbuf.buf, to_pio_type, nelems);

  /* Only the cbufs_ map access needs to be serialized with the other threads
   * (the calling thread and the async write thread pool worker can both reach
   * this function for the same file). The conversion above writes into a
   * freshly allocated buffer that is not reachable from cbufs_ yet, so it is
   * deliberately left outside the critical section - holding the lock across
   * the (potentially large) conversion loop would serialize the async
   * worker's conversions against the calling thread's for no benefit. */
  {
    std::lock_guard<std::mutex> lg(cbufs_mtx_);

    std::map<int, std::vector<Cachebuf> >::iterator cbufs_iter = cbufs_.find(ncid);
    if(cbufs_iter != cbufs_.end()){
      cbufs_iter->second.push_back(cbuf);
    }
    else{
      cbufs_.insert(std::make_pair(ncid, std::vector<Cachebuf>({cbuf})));
    }
  }

  return cbuf.buf;
}

void *SPIO_Util::File_Util::DTConverter::convert(const void *buf, std::size_t sz, nc_type from_pio_type, nc_type to_pio_type)
{
  std::size_t nelems = sz / size_of(from_pio_type);

  void *rbuf = bget(nelems * size_of(to_pio_type));

  copy_to(buf, from_pio_type, rbuf, to_pio_type, nelems);

  return rbuf;
}

void SPIO_Util::File_Util::DTConverter::free(int ncid)
{
  std::lock_guard<std::mutex> lg(cbufs_mtx_);

  std::map<int, std::vector<Cachebuf> >::iterator cbufs_iter = cbufs_.find(ncid);
  if(cbufs_iter != cbufs_.end()){
    for(std::vector<Cachebuf>::iterator iter = cbufs_iter->second.begin();
          iter != cbufs_iter->second.end(); ++iter){
      brel(iter->buf);
    }
    cbufs_.erase(cbufs_iter);
  }
}

void SPIO_Util::File_Util::DTConverter::clear(void)
{
  std::lock_guard<std::mutex> lg(cbufs_mtx_);

  for(std::map<int, std::vector<Cachebuf> >::iterator cbufs_iter = cbufs_.begin();
        cbufs_iter != cbufs_.end(); ++cbufs_iter){
    for(std::vector<Cachebuf>::iterator iter = cbufs_iter->second.begin();
          iter != cbufs_iter->second.end(); ++iter){
      brel(iter->buf);
    }
  }
  cbufs_.clear();
}
