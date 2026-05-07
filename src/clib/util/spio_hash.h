#ifndef __SPIO_HASH_H__
#define __SPIO_HASH_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>

/* FIXME: This is a simple C hashmap wrapper to replace a C hashtable implementation
 * added for ADIOS read support. This replacement is intended as a temporary solution,
 * and eventually needs to be completely removed & replaced by a C++ unordered map
 */
typedef struct spio_hmap spio_hash_t;
/* Hashmap class */
struct spio_hmap {
  /* Put data associated with a key to the hash map using a pointer to the data
   * The map now owns the data */
  bool  (*put)    (spio_hash_t *tbl, const char *key, void *data);
  /* Get the data, pointer to the data, associated with the key */
  void *(*get)    (spio_hash_t *tbl, const char *key);
  /* Remove/free the data associated with the key */
  bool  (*remove) (spio_hash_t *tbl, const char *key);
  /* Get the size of the hash table, i.e., the number of elements in it */
  int   (*size)   (spio_hash_t *tbl);
  /* Clear the hash table - delete/remove all elements in it */
  void  (*clear)  (spio_hash_t *tbl);
  /* Free the hash table. Also deletes/removes all elements in it */
  void  (*free)   (spio_hash_t *tbl);

  /* Points to a C++ unordered hash map to do the real work */
  void *umap_;
};

/* Create a hash map */
spio_hash_t* spio_hash(int range);

#ifdef __cplusplus
}
#endif

#endif /* __SPIO_HASH_H__ */
