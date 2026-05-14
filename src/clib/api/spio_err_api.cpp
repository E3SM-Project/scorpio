#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
//#include "spio_gptl_utils.hpp"

/* ========== Error handling APIs =========== */
int PIOc_strerror(int pioerr, char *errmsg, size_t errmsg_sz)
{
#if SPIO_ENABLE_API_TIMING
  //SPIO_Util::GPTL_Util::GPTL_timer func_timer("SPIO:PIOc_strerror");
#endif
  /* FIXME: Handle non I/O system specific calls */
  return PIOc_strerror_impl(pioerr, errmsg, errmsg_sz);
}

int PIOc_set_log_level(int level)
{
#if SPIO_ENABLE_API_TIMING
  //SPIO_Util::GPTL_Util::GPTL_timer func_timer("SPIO:PIOc_set_log_level");
#endif
  /* FIXME: Handle non I/O system specific calls */
  return PIOc_set_log_level_impl(level);
}


