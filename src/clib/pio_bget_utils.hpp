#ifndef __PIO_BGET_UTILS_HPP__
#define __PIO_BGET_UTILS_HPP__

#include <list>

extern "C"{

#include "config.h"
#include "pio_internal.h"
#include "bget.h"
#include "pio_bget_utils.h"

} // extern "C"

namespace PIO_Util{
  namespace BGet_Util{

    /* Memory pool to be used with BGET
     * - The block allocations are made using malloc
     */
    class Bget_mpool{
      public:
        void *mblock_alloc(std::size_t sz);
        void mblock_free(void *p);
      private:
        friend class Bget_mpool_manager;
        Bget_mpool() = default;
        ~Bget_mpool();
        std::list<void *> mblocks_;
    }; // class BGet_MPool

    /* Memory pool manager
     * Ensures that there is only one instance of a memory pool at a time
     */
    class Bget_mpool_manager{
      public:
        Bget_mpool_manager() = default;
        ~Bget_mpool_manager() = default;
        static void *mpool_init(std::size_t sz);
        static Bget_mpool *get_mpool_instance(void );
        static void mpool_finalize(void );
      private:
        static Bget_mpool *mpool_;
    };

  } // namespace BGet_Util
} // namespace PIO_Util

#endif // __PIO_BGET_UTILS_HPP__
