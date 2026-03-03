#ifndef __SPIO_DT_CONVERTER_HPP__
#define __SPIO_DT_CONVERTER_HPP__

#include <map>
#include <vector>
#include <algorithm>
#include <cstdint>

#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"

namespace SPIO_Util{
  namespace File_Util{
    /* Datatype converter class.
     * Caches temp buffers (as a result of conversion) and these
     * buffers can be freed using free()
     */
    class DTConverter{
      public:
        /* Convert buffer to requested type, buffer size, sz, is in bytes
         * - the returned buffer (converted buffer) is owned by the datatype converter &
         * can be freed via the free(ncid) member function
         */
        void *convert(int ncid, void *buf, std::size_t sz, nc_type from_pio_type, nc_type to_pio_type);
        /* Convert buffer to requested type, buffer size, sz, is in bytes
         * - the returned buffer (converted buffer) is owned by the caller &
         * needs to be freed using the brel(PTR_RETURNED_BY_FUNCTION) function
         * by the caller
         */
        void *convert(const void *buf, std::size_t sz, nc_type from_pio_type, nc_type to_pio_type);
        /* Check if the converter has any cached buffers - useful for debugging */
        bool empty(void ) const { return cbufs_.empty(); }
        /* Free the scratch/temp buffers associated with ncid/file */
        void free(int ncid);
        /* Clear all scratch/temp buffers */
        void clear(void );

        static inline std::size_t size_of(nc_type pio_type){
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
        static inline void copy_to(F *from_buf, void *to_buf, nc_type to_pio_type, std::size_t nelems){
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

        static inline void copy_to(const void *from_buf, nc_type from_pio_type, void *to_buf, nc_type to_pio_type, std::size_t nelems){
          switch(from_pio_type){
            case PIO_DOUBLE : copy_to(static_cast<const double *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_FLOAT  : copy_to(static_cast<const float *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_INT    : copy_to(static_cast<const int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_UINT   : copy_to(static_cast<const unsigned int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_SHORT  : copy_to(static_cast<const short int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_USHORT : copy_to(static_cast<const unsigned short int *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_INT64  : copy_to(static_cast<const int64_t *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_UINT64 : copy_to(static_cast<const uint64_t *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_CHAR   : copy_to(static_cast<const char *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_BYTE   : copy_to(static_cast<const char *>(from_buf), to_buf, to_pio_type, nelems); break;
            case PIO_UBYTE  : copy_to(static_cast<const unsigned char *>(from_buf), to_buf, to_pio_type, nelems); break;
            default         : assert(0);
          }
        }
    };
  } // namespace File_Util
} // namespace SPIO_Util

#endif // __SPIO_DT_CONVERTER_HPP__
