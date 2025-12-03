#ifndef __SPIO_DT_CONVERTER_HPP__
#define __SPIO_DT_CONVERTER_HPP__

#include <map>
#include <vector>
#include <algorithm>

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_file_mvcache.h"

namespace SPIO_Util{
  namespace File_Util{
    /* Datatype converter class.
     * Caches temp buffers (as a result of conversion) and these
     * buffers can be freed using free()
     */
    class DTConverter{
      public:
        /* Convert buffer to requested type, buffer size, sz, is in bytes */
        void *convert(int ncid, void *buf, std::size_t sz, int from_pio_type, int to_pio_type);
        /* Free the scratch/temp buffers associated with ncid/file */
        void free(int ncid);
        /* Clear all scratch/temp buffers */
        void clear(void );

        static inline std::size_t size_of(int pio_type){
          switch(pio_type){
            case PIO_DOUBLE : return sizeof(double);
            case PIO_FLOAT  : return sizeof(float);
            case PIO_INT    : return sizeof(int);
            case PIO_UINT   : return sizeof(unsigned int);
            case PIO_SHORT  : return sizeof(short int);
            case PIO_USHORT : return sizeof(unsigned short int);
            case PIO_INT64  : return sizeof(int64_t);
            case PIO_UINT64 : return sizeof(uint64_t);
            case PIO_CHAR   : return sizeof(char);
            case PIO_BYTE   : return sizeof(char);
            case PIO_UBYTE  : return sizeof(unsigned char);
            default         : assert(0);
          }
        }

      private:
        struct Cachebuf{
          void *buf;
          int piotype;
        };
        /* Internal map to store scrach/temp bufs for each file/ncid */
        std::map<int, std::vector<Cachebuf> > cbufs_;

        template<typename F, typename T>
        static inline void copy_to(F *from_buf, T *to_buf, std::size_t nelems){
          std::transform(from_buf, from_buf + nelems, to_buf,
            [](F val){ return static_cast<T>(val); });
        }

        template<typename F>
        static inline void copy_to(F *from_buf, void *to_buf, int to_pio_type, std::size_t nelems){
          switch(to_pio_type){
            case PIO_DOUBLE : copy_to(from_buf, static_cast<double *>(to_buf), nelems); break;
            case PIO_FLOAT  : copy_to(from_buf, static_cast<float *>(to_buf), nelems); break;
            case PIO_INT    : copy_to(from_buf, static_cast<int *>(to_buf), nelems); break;
            case PIO_UINT   : copy_to(from_buf, static_cast<unsigned int *>(to_buf), nelems); break;
            case PIO_SHORT  : copy_to(from_buf, static_cast<short int *>(to_buf), nelems); break;
            case PIO_USHORT : copy_to(from_buf, static_cast<unsigned short int *>(to_buf), nelems); break;
            case PIO_INT64  : copy_to(from_buf, static_cast<int64_t *>(to_buf), nelems); break;
            case PIO_UINT64 : copy_to(from_buf, static_cast<uint64_t *>(to_buf), nelems); break;
            case PIO_CHAR   : copy_to(from_buf, static_cast<char *>(to_buf), nelems); break;
            case PIO_BYTE   : copy_to(from_buf, static_cast<char *>(to_buf), nelems); break;
            case PIO_UBYTE  : copy_to(from_buf, static_cast<unsigned char *>(to_buf), nelems); break;
            default         : assert(0);
          }
        }

        static inline void copy_to(void *from_buf, int from_pio_type, void *to_buf, int to_pio_type, std::size_t nelems){
          switch(from_pio_type){
            case PIO_DOUBLE : copy_to(static_cast<double *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_FLOAT  : copy_to(static_cast<float *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_INT    : copy_to(static_cast<int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_UINT   : copy_to(static_cast<unsigned int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_SHORT  : copy_to(static_cast<short int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_USHORT : copy_to(static_cast<unsigned short int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_INT64  : copy_to(static_cast<int64_t *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_UINT64 : copy_to(static_cast<uint64_t *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_CHAR   : copy_to(static_cast<char *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_BYTE   : copy_to(static_cast<char *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_UBYTE  : copy_to(static_cast<unsigned char *>(from_buf), to_buf, to_pio_type, nelems); break;
            default         : assert(0);
          }
        }
    };
  } // namespace File_Util
} // namespace SPIO_Util

#endif // __SPIO_DT_CONVERTER_HPP__
