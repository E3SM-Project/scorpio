#include <iostream>
#include <stdbool.h>
#include "spio_hash.h"
#include <new>
#include <string>
#include <cassert>
#include <unordered_map>


static bool spio_hmap_put(spio_hash_t *tbl, const char *key, void *data);
static void *spio_hmap_get(spio_hash_t *tbl, const char *key);
static bool spio_hmap_remove(spio_hash_t *tbl, const char *key);
static int spio_hmap_size(spio_hash_t *tbl);
static void spio_hmap_clear(spio_hash_t *tbl);
static void spio_hmap_free(spio_hash_t *tbl);

/**
 * Initialize the hash map struct
 * Ignore range provided by the user
 */
spio_hash_t *spio_hash(int range)
{
  spio_hash_t *tbl = (spio_hash_t *)calloc(1, sizeof(spio_hash_t));
  if(tbl == NULL){
    throw std::bad_alloc();
  }

  /* Create C++ unordered hash map */
  tbl->umap_ = static_cast<void *>(new std::unordered_map<std::string, void *>());

  /* Member methods to access the hash map */
  tbl->put        = spio_hmap_put;
  tbl->get        = spio_hmap_get;
  tbl->remove     = spio_hmap_remove;
  tbl->size       = spio_hmap_size;
  tbl->clear      = spio_hmap_clear;
  tbl->free       = spio_hmap_free;

  return tbl;
}

static bool spio_hmap_put(spio_hash_t *tbl, const char *key, void *data)
{
  assert(tbl && tbl->umap_);

  if(!key){
    return false;
  }

  std::unordered_map<std::string, void *> *umap =
    static_cast<std::unordered_map<std::string, void *> *>(tbl->umap_);
  std::pair<std::unordered_map<std::string, void *>::iterator, bool>
    res = umap->insert(std::make_pair(std::string(key), data));

    return res.second;
}

static void *spio_hmap_get(spio_hash_t *tbl, const char *key)
{
  assert(tbl && tbl->umap_ && key);
  std::unordered_map<std::string, void *> *umap =
    static_cast<std::unordered_map<std::string, void *> *>(tbl->umap_);

  std::unordered_map<std::string, void *>::iterator iter =
    umap->find(std::string(key));

  if(iter != umap->end()){
    return iter->second;
  }
  else{
    return NULL;
  }
}

static bool spio_hmap_remove(spio_hash_t *tbl, const char *key)
{
  assert(tbl && tbl->umap_ && key);
  std::unordered_map<std::string, void *> *umap =
    static_cast<std::unordered_map<std::string, void *> *>(tbl->umap_);

  std::unordered_map<std::string, void *>::iterator iter =
    umap->find(std::string(key));

  if(iter == umap->end()){
    return false;
  }

  /* Note: Since the data is owned by the hash table, its freed */
  free(iter->second);
  umap->erase(iter);

  return true;
}

int spio_hmap_size(spio_hash_t *tbl)
{
  assert(tbl && tbl->umap_);
  std::unordered_map<std::string, void *> *umap =
    static_cast<std::unordered_map<std::string, void *> *>(tbl->umap_);

  return static_cast<int>(umap->size());
}

void spio_hmap_clear(spio_hash_t *tbl)
{
  assert(tbl && tbl->umap_);
  std::unordered_map<std::string, void *> *umap =
    static_cast<std::unordered_map<std::string, void *> *>(tbl->umap_);

  /* Iterate over all elements and ensure that we free the data associated
   * with the keys
   */
  std::unordered_map<std::string, void *>::iterator iter = umap->begin();
  while(iter != umap->end()){
    free(iter->second);
    iter = umap->erase(iter);
  }
}

void spio_hmap_free(spio_hash_t *tbl)
{
  if(tbl == NULL){
    return;
  }

  /* Clear the hash table */
  spio_hmap_clear(tbl);

  /* Free the C++ unordered hash map */
  std::unordered_map<std::string, void *> *umap =
    static_cast<std::unordered_map<std::string, void *> *>(tbl->umap_);
  delete(umap);

  /* Free the hash table itself */
  free(tbl);
}
