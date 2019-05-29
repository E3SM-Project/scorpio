#ifndef __PIO_BGET_UTILS_H__
#define __PIO_BGET_UTILS_H__

#include "config.h"
#include "pio_internal.h"
#include "bget.h"

/* Allocate a memory block to use with BGET */
void *pio_bget_mblock_alloc(bufsize sz);
/* Free a memory block no longer used by BGET */
void pio_bget_mblock_free(void *p);

/* Initialize the memory pool */
void *pio_bget_mpool_init(bufsize sz);

/* Finalize the memory pool (consisting of multiple memory blocks) 
 * used (via pio_bget_mblock_alloc/free) by BGET
 */
void pio_bget_mpool_finalize(void );

#endif /* __PIO_BGET_UTILS_H__ */
