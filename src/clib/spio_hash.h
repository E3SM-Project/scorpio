#ifndef __SPIO_HASH_H__
#define __SPIO_HASH_H__

#ifdef SPIO_USE_QLIBC_HASH
/******************************************************************************
 * qLibc - http://www.qdecoder.org
 *
 * Copyright (c) 2010-2012 Seungyoung Kim.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/
/*
  Modified/simplified by ADIOS team for the purpose of variable/attribute store
*/
#ifndef __HASHTBL_H_
#define __HASHTBL_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>


typedef struct qhnobj_s qhnobj_t;  
typedef struct qhslot_s qhslot_t;  
typedef struct qhashtbl_s spio_hash_t;

struct qhnobj_s {
    uint32_t hash;     /*!< 32bit-hash value of object name */
    char *key;         /*!< object key */
    void *value;       /*!< object value */
    qhnobj_t *next;    /*!< for chaining next collision object */
};

// Head node in hash table 
struct qhslot_s {
    qhnobj_t *head;    /*!< The first collision object for gets */
    qhnobj_t *tail;    /*!< The last collision object for puts */
};

struct qhashtbl_s {
    /* capsulated member functions */
    bool  (*put)    (spio_hash_t *tbl, const char *fullpath, const void *data);
    bool  (*put2)   (spio_hash_t *tbl, const char *path,  const char *name, const void *data);
    void *(*get)    (spio_hash_t *tbl, const char *fullpath);
    void *(*get2)   (spio_hash_t *tbl, const char *path,  const char *name);
    bool  (*remove) (spio_hash_t *tbl, const char *fullpath);

    int   (*size)   (spio_hash_t *tbl);
    void  (*clear)  (spio_hash_t *tbl);
    void  (*debug)  (spio_hash_t *tbl, FILE *out, bool detailed);

    void  (*free)   (spio_hash_t *tbl);

    /* private variables - do not access directly */
    int num;         /*!< number of objects in this table */
    int range;       /*!< hash range, vertical number of slots */
    qhslot_t *slots; /*!< slot head node */

    /* private debug variables */
    int ncalls_get; // number of calls to get()
    int nwalks_get; // number of walking steps in hash list in get()
    int ncalls_put; // number of calls to put()
    int nwalks_put; // number of walking steps in hash list in put()
};

spio_hash_t* spio_hash(int range);

#ifdef __cplusplus
}
#endif

#endif

#else /* SPIO_USE_QLIBC_HASH */

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

#endif /* SPIO_USE_QLIBC_HASH */

#endif /* __SPIO_HASH_H__ */
