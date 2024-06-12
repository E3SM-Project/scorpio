#ifndef __SPIO_REARRANGE_ANY_H__
#define __SPIO_REARRANGE_ANY_H__

#include "pio_config.h"
#include "pio.h"
//#include "pio_internal.h"

#if defined(__cplusplus)
extern "C" {
#endif

/* Initialize the PIO_REARR_ANY rearranger global info */
void spio_init_pio_rearr_any(void );
/* Get the optimal PIO rearranger for an I/O decomposition */
int spio_get_opt_pio_rearr(iosystem_desc_t *iosys, int local_decomp_map_len);

#if defined(__cplusplus)
}
#endif

#endif // __SPIO_REARRANGE_ANY_H__
