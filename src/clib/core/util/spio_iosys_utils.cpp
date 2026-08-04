/** @file
 * Utils related to I/O System
 */
#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "spio_iosys_utils.hpp"

bool SPIO_Util::Iosys_Util::iosys_uses_async_io_service(iosystem_desc_t *iosys)
{
  assert(iosys);
  return (iosys->intercomm != MPI_COMM_NULL);
}
