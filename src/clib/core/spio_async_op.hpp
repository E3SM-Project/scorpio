#ifndef __SPIO_ASYNC_OP_HPP__
#define __SPIO_ASYNC_OP_HPP__

#include <functional>
#include <string>

namespace SPIO_Util{

/**
 * PIO asynchronous op
 */
class Async_op{
  public:
    /**
     * PIO asynchronous operation types
     */
    enum class Op_type{
      SPIO_ASYNC_INVALID_OP,
      SPIO_ASYNC_REARR_OP,
      SPIO_ASYNC_PNETCDF_WRITE_OP,
      SPIO_ASYNC_HDF5_CREATE_OP,
      SPIO_ASYNC_HDF5_DEF_VAR_OP,
      SPIO_ASYNC_HDF5_PUT_ATT_OP,
      SPIO_ASYNC_HDF5_ENDDEF_OP,
      SPIO_ASYNC_HDF5_SET_FRAME_OP,
      SPIO_ASYNC_HDF5_PUT_VAR_OP,
      SPIO_ASYNC_HDF5_WRITE_OP,
      SPIO_ASYNC_FILE_WRITE_OPS,
      SPIO_ASYNC_FILE_CLOSE_OP
    };

    static std::string op_type_to_string(Op_type type){
      const std::string UNKNOWN_OP("SPIO_ASYNC_INVALID_OP");
      switch(type){
        case Op_type::SPIO_ASYNC_INVALID_OP : return "SPIO_ASYNC_INVALID_OP";
        case Op_type::SPIO_ASYNC_REARR_OP : return "SPIO_ASYNC_REARR_OP";
        case Op_type::SPIO_ASYNC_PNETCDF_WRITE_OP : return "SPIO_ASYNC_PNETCDF_WRITE_OP";
        case Op_type::SPIO_ASYNC_HDF5_CREATE_OP : return "SPIO_ASYNC_HDF5_CREATE_OP";
        case Op_type::SPIO_ASYNC_HDF5_DEF_VAR_OP : return "SPIO_ASYNC_HDF5_DEF_VAR_OP";
        case Op_type::SPIO_ASYNC_HDF5_PUT_ATT_OP : return "SPIO_ASYNC_HDF5_PUT_ATT_OP";
        case Op_type::SPIO_ASYNC_HDF5_ENDDEF_OP : return "SPIO_ASYNC_HDF5_ENDDEF_OP";
        case Op_type::SPIO_ASYNC_HDF5_SET_FRAME_OP : return "SPIO_ASYNC_HDF5_SET_FRAME_OP";
        case Op_type::SPIO_ASYNC_HDF5_PUT_VAR_OP : return "SPIO_ASYNC_HDF5_PUT_VAR_OP";
        case Op_type::SPIO_ASYNC_HDF5_WRITE_OP : return "SPIO_ASYNC_HDF5_WRITE_OP";
        case Op_type::SPIO_ASYNC_FILE_WRITE_OPS : return "SPIO_ASYNC_FILE_WRITE_OPS";
        case Op_type::SPIO_ASYNC_FILE_CLOSE_OP : return "SPIO_ASYNC_FILE_CLOSE_OP";
        default : return UNKNOWN_OP;
      }
    }

    Async_op(Op_type type, void *pdata,
              std::function<int (void *)> wait_fn,
              std::function<int (void *, bool &)> poke_fn,
              std::function<void (void *)> free_fn):
              type_(type), pdata_(pdata),
              wait_fn_(wait_fn), poke_fn_(poke_fn), free_fn_(free_fn){}
  
    Op_type type(void ) const { return type_; }
    void *data(void ) const { return pdata_; }
    int wait(void ) { return wait_fn_(pdata_); }
    int poke(bool &is_complete) { return poke_fn_(pdata_, is_complete); }
    int poke(void ) { bool is_complete = false; return poke_fn_(pdata_, is_complete); }
    void free(void ) { return free_fn_(pdata_); }

  private:
    Op_type type_;
    void *pdata_;
    /* Blocking wait function for this async op
     * param 1 : A user defined data pointer
     * return : PIO_NOERR on success, pio error code on failure
     */
    std::function<int (void *)> wait_fn_;
    /* Non-blocking function for making progress on this async op
     * param 1 : A user defined data pointer
     * param 2 : Pointer to a flag that is set to true if async op
     * is complete, false otherwise
     * return : PIO_NOERR on success, pio error code on failure
     */
    std::function<int (void *, bool &)> poke_fn_;
    /* Free function for user defined pdata */
    std::function<void (void *)> free_fn_;
};

} // namespace SPIO_Util

#endif // __SPIO_ASYNC_OP_HPP__
