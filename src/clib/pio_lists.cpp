/**
 * @file
 * PIO list functions.
 */
#include <map>
#include <stdexcept>
#include <cstring>
#include <cstdio>
#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>
#ifdef PIO_MICRO_TIMING
#include "pio_timer.h"
#endif
#include "spio_file_mvcache.h"
#include "spio_io_summary.h"
#include "spio_hash.h"
#include "spio_dt_converter.hpp"
#include <mutex>

namespace SPIO_Util{
  namespace SPIO_Lists{
    namespace GVars{
      std::map<int, io_desc_t *> pio_iodesc_list;
      std::map<int, iosystem_desc_t *> pio_iosystem_list;
      /* This list is independent of the I/O system because users only provide the
       * file id during a call - hence instead of deducing the I/O system that the file
       * belongs to, the file list is separate from the I/O system (list)
       */
      std::map<int, file_desc_t *> pio_file_list;
    }
  }
}

/* Arbitrary start ids for structures */
const int PIO_FILE_START_ID = 16;
const int PIO_IOSYSTEM_START_ID = 2048;

/** 
 * Add a new entry to the global list of open files.
 *
 * This function guarantees that files (id of the
 * files) are unique across the comm provided
 *
 * @param file Pointer to the file_desc_t struct for the file.
 * @param comm MPI Communicator across which the files
 * need to be unique
 * @returns The id for the file added to the list
 */
int pio_add_to_file_list(file_desc_t *file, MPI_Comm comm)
{
  static int pio_file_next_id = PIO_FILE_START_ID;

  assert(file);
  LOG((2, "pio_add_to_file_list file = %p", file));

  if(comm != MPI_COMM_NULL){
    int tmp_id = pio_file_next_id;
    int mpierr = MPI_Allreduce(&tmp_id, &pio_file_next_id, 1, MPI_INT, MPI_MAX, comm);
    assert(mpierr == MPI_SUCCESS);
  }
  file->pio_ncid = pio_file_next_id;
  pio_file_next_id++;

  SPIO_Util::SPIO_Lists::GVars::pio_file_list.insert({file->pio_ncid, file});
  LOG((2, "file %p (%s, pio_ncid=%d) added to list",
      file, pio_get_fname_from_file(file), file->pio_ncid));

  return file->pio_ncid;
}

/** 
 * Get the file structure (file_desc_t) associated with the file id
 *
 * @param ncid The file id associated with the file being looked up
 * @param pfile Pointer to the file structure pointer to contain the
 * returned file structure pointer
 *
 * @returns 0 for success, error code otherwise.
 */
int pio_get_file(int ncid, file_desc_t **pfile)
{
  LOG((2, "pio_get_file ncid = %d", ncid));

  file_desc_t *file = NULL;

  if(ncid == PIO_GLOBAL){
    /* Don't handle error, return a pio_err(), here since this function is called for
     * PIO_GLOBAL and in that case is handled by the caller */
    return PIO_EBADID;
  }

  try{
    file = SPIO_Util::SPIO_Lists::GVars::pio_file_list.at(ncid);  
  } catch(const std::out_of_range &e){
    return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                    "Searching for file (ncid=%d) in internal file list failed. Invalid file id provided", ncid);
  }

  if(pfile){
    assert(file && file->iosystem && iotype_is_valid(file->iotype));
    *pfile = file;
  }

  return PIO_NOERR;
}

/** 
 * Free a file structure (file_desc_t)
 *
 * @param file Pointer to the file structure (file_desc_t)
 * associated with the file
 * @returns 0 for success, error code otherwise
 */
int pio_free_file(file_desc_t *file)
{
  assert(file);

  LOG((2, "pio_free_file(%s, ncid=%d)", pio_get_fname_from_file(file), file->pio_ncid));
  /* Free any fill values that were allocated. */
  for (int v = 0; v < PIO_MAX_VARS; v++){
    if (file->varlist[v].fillvalue){
      free(file->varlist[v].fillvalue);
    }
#ifdef PIO_MICRO_TIMING
    mtimer_destroy(&(file->varlist[v].rd_mtimer));
    mtimer_destroy(&(file->varlist[v].rd_rearr_mtimer));
    mtimer_destroy(&(file->varlist[v].wr_mtimer));
    mtimer_destroy(&(file->varlist[v].wr_rearr_mtimer));
#endif
  }

  delete(file->pmtx);

  free(file->unlim_dimids);
  free(file->io_fstats);
  spio_file_mvcache_finalize(file);

#ifdef _HDF5
  if(file->dt_converter != NULL){
    delete(static_cast<SPIO_Util::File_Util::DTConverter *>(file->dt_converter));
  }
#endif

#ifdef _ADIOS2
  if (file->cache_data_blocks != NULL){
    /* This call also frees file->cache_data_blocks */
    file->cache_data_blocks->free(file->cache_data_blocks);
  }

  if (file->cache_block_sizes != NULL){
    /* This call also frees file->cache_block_sizes */
    file->cache_block_sizes->free(file->cache_block_sizes);
  }

  if (file->cache_darray_info != NULL){
    /* This call also frees file->cache_darray_info */
    file->cache_darray_info->free(file->cache_darray_info);
  }
#endif

  /* Free the memory used for this file. */
  free(file);
  
  return PIO_NOERR;
}

/** 
 * Delete a file from the list of open files.
 * Note: The file is deleted from the list & freed
 *
 * @param ncid The file id associated with the file to be deleted
 * @returns 0 for success, error code otherwise
 */
int pio_delete_file_from_list(int ncid)
{
  LOG((2, "pio_delete_file_from_list(ncid=%d)", ncid));
  std::map<int, file_desc_t *>::iterator iter = SPIO_Util::SPIO_Lists::GVars::pio_file_list.find(ncid);
  if(iter == SPIO_Util::SPIO_Lists::GVars::pio_file_list.end()){
    return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                    "Deleting file (ncid=%d) from internal list failed. Invalid file id provided", ncid);
  }

  file_desc_t *file = (*iter).second;
  SPIO_Util::SPIO_Lists::GVars::pio_file_list.erase(iter);

  return pio_free_file(file);
}

int spio_close_all_files_and_delete_from_list(int iosysid)
{
  int ret = PIO_NOERR;
  std::vector<int> ncids_to_del_from_list;
  for(std::map<int, file_desc_t *>::iterator iter = SPIO_Util::SPIO_Lists::GVars::pio_file_list.begin();
      iter != SPIO_Util::SPIO_Lists::GVars::pio_file_list.end(); ++iter){
    file_desc_t *file = iter->second;
    assert(file);
    if(file->iosystem->iosysid == iosysid){
      //ret = spio_hard_closefile(file->iosystem, file, true);
      ret = spio_wait_on_hard_close(file->iosystem, file);
      if(ret != PIO_NOERR){
        return pio_err(file->iosystem, file, PIO_EINTERNAL, __FILE__, __LINE__,
                        "Error closing file (hard close failed)");
      }
      ncids_to_del_from_list.push_back(file->pio_ncid);
    }
  }
  for(std::vector<int>::iterator iter = ncids_to_del_from_list.begin();
        iter != ncids_to_del_from_list.end(); ++iter){
    ret = pio_delete_file_from_list(*iter);
    if(ret != PIO_NOERR){
      return pio_err(NULL, NULL, PIO_EINTERNAL, __FILE__, __LINE__,
                      "Error deleting file from global list");
    }
  }

  return PIO_NOERR;
}

/* Check if the file is still open - due to async ops */
int spio_close_soft_closed_file(const char *filename)
{
  int ret = PIO_NOERR;
  std::vector<int> ncids_to_del_from_list;

  assert(filename);
  for(std::map<int, file_desc_t *>::iterator iter = SPIO_Util::SPIO_Lists::GVars::pio_file_list.begin();
      iter != SPIO_Util::SPIO_Lists::GVars::pio_file_list.end(); ++iter){
    file_desc_t *file = iter->second;
    if(std::string(file->fname) == std::string(filename)){
      //ret = spio_hard_closefile(file->iosystem, file, true);
      ret = spio_wait_on_hard_close(file->iosystem, file);
      if(ret != PIO_NOERR){
        return pio_err(file->iosystem, file, PIO_EINTERNAL, __FILE__, __LINE__,
                        "Error closing file (hard close failed)");
      }
      ncids_to_del_from_list.push_back(file->pio_ncid);
    }
  }

  for(std::vector<int>::iterator iter = ncids_to_del_from_list.begin();
        iter != ncids_to_del_from_list.end(); ++iter){
    ret = pio_delete_file_from_list(*iter);
    if(ret != PIO_NOERR){
      return pio_err(NULL, NULL, PIO_EINTERNAL, __FILE__, __LINE__,
                      "Error deleting file from global list");
    }
  }

  return PIO_NOERR;
}

/** Print I/O stats for all files in the iosystem
 *
 * This call can be expensive when a lot of files are open
 * (and not expensive when called during I/O system finalize,
 * when all the I/O systems finalize at the same time and the
 * user is careful about closing all open files before the
 * I/O systems are finalized)
 * This call can be used to force writing out file I/O stats
 * for all the files in an I/O system (e.g. not all files
 * are closed and we need the file I/O stats of these files)
 * @param iosysp Pointer to the I/O system, iosystem_desc_t
 * @returns PIO_NOERR on success, error code otherwise
 */
int spio_write_all_file_iostats(iosystem_desc_t *iosysp)
{
  for(std::map<int, file_desc_t *>::iterator iter =
        SPIO_Util::SPIO_Lists::GVars::pio_file_list.begin();
      iter != SPIO_Util::SPIO_Lists::GVars::pio_file_list.end(); ++iter){
    if(iter->second->iosystem == iosysp){
      assert(iter->second->iosystem->iosysid == iosysp->iosysid);
      return spio_write_file_io_summary(iter->second);
    }
  }
  return PIO_NOERR;
}

static int finalize_file_list(void )
{
  /* No valid I/O systems, clear out the file list */
  for(std::map<int, file_desc_t *>::iterator fiter =
          SPIO_Util::SPIO_Lists::GVars::pio_file_list.begin();
        fiter != SPIO_Util::SPIO_Lists::GVars::pio_file_list.end(); ++fiter){
    file_desc_t *file = (*fiter).second;
    pio_free_file(file);
  }

  SPIO_Util::SPIO_Lists::GVars::pio_file_list.clear();

  return PIO_NOERR;
}

/** 
 * Delete iosystem (iosytem_desc_t) from the global list of
 * available iosystems
 *
 * @param iosysid The id of the iosystem to delete
 * @returns 0 on success, error code otherwise
 */
int pio_delete_iosystem_from_list(int iosysid)
{
  LOG((2, "pio_delete_iosystem_from_list(iosysid=%d)", iosysid));
  std::map<int, iosystem_desc_t *>::iterator iter =
                                SPIO_Util::SPIO_Lists::GVars::pio_iosystem_list.find(iosysid);
  if(iter != SPIO_Util::SPIO_Lists::GVars::pio_iosystem_list.end()){
    iosystem_desc_t *iosys = (*iter).second;
    free(iosys);
    SPIO_Util::SPIO_Lists::GVars::pio_iosystem_list.erase(iter);
    if(SPIO_Util::SPIO_Lists::GVars::pio_iosystem_list.size() == 0){
      /* Unfortunately in some cases not all procs cleanup the files as needed
       * e.g. PIO_RETURN_ERROR + non-I/O procs on a create file call
       */
      return finalize_file_list();
    }
  }
  else{
    return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                    "Deleting iosystem (iosysid=%d) from the internal global list failed. Invalid iosystem id provided", iosysid);
  }
  return PIO_NOERR;
}

/**
 * Add iosystem (iosystem_desc_t) to a global list.
 * This function guarantees that iosystems (ioid of the
 * iosystems) are unique across the comm provided
 *
 * @param ios Pointer to the iosystem info (iosystem_desc_t)
 * to add to the list.
 * @param comm MPI Communicator across which the iosystems
 * need to be unique
 * @returns The id of the newly added iosystem.
 */
int pio_add_to_iosystem_list(iosystem_desc_t *ios, MPI_Comm comm)
{
  static int pio_iosystem_next_ioid = PIO_IOSYSTEM_START_ID;

  assert(ios);
  LOG((2, "pio_add_to_iosystem_list(ios=%p)", ios));

  if(comm != MPI_COMM_NULL){
    int tmp_id = pio_iosystem_next_ioid;
    int mpierr = MPI_Allreduce(&tmp_id, &pio_iosystem_next_ioid, 1,
                                MPI_INT, MPI_MAX, comm);
    assert(mpierr == MPI_SUCCESS);
  }
  ios->iosysid = pio_iosystem_next_ioid;
  pio_iosystem_next_ioid++;

  SPIO_Util::SPIO_Lists::GVars::pio_iosystem_list.insert({ios->iosysid, ios});
  LOG((2, "iosystem %p (iosysid=%d) added to the global list", ios, ios->iosysid));

  return ios->iosysid;
}

/** 
 * Get iosystem info (iosystem_desc_t) from the global list of available
 * iosystems.
 *
 * @param iosysid The id of the iosystem to lookup
 * @returns pointer to iosystem_desc_t, or NULL if not found.
 */
iosystem_desc_t *pio_get_iosystem_from_id(int iosysid)
{
  LOG((2, "pio_get_iosystem_from_id(iosysid=%d)", iosysid));

  iosystem_desc_t *ios = NULL;
  try{
    ios = SPIO_Util::SPIO_Lists::GVars::pio_iosystem_list.at(iosysid);  
  } catch(const std::out_of_range &e){
    LOG((1, "Finding iosytem corresponding to iosysid = %d failed. Invalid iosystem id provided", iosysid));
  }

  return ios;
}

/** 
 * Get the number of open/available/valid iosystems.
 *
 * @param niosysid Pointer to integer that will receive the number of
 * valid iosystems
 * @returns 0 for success.
 */
int pio_num_iosystem(int *niosys)
{
  assert(niosys);

  *niosys = static_cast<int>(SPIO_Util::SPIO_Lists::GVars::pio_iosystem_list.size());

  return PIO_NOERR;
}

/** 
 * Add an I/O descriptor (io_desc_t) to the global
 * list of valid iodescs.
 * This function guarantees that iodescs (id of the
 * iodescs) are unique across the comm provided
 *
 * @param io_desc_t Pointer to the I/O descriptor (io_desc_t)
 * to add to the list
 * @param comm MPI Communicator across which the iosystems
 * need to be unique
 * @returns the ioid of the newly added iodesc.
 */
int pio_add_to_iodesc_list(io_desc_t *iodesc, MPI_Comm comm)
{
  static int pio_iodesc_next_id = PIO_IODESC_START_ID;

  assert(iodesc);
  LOG((2, "pio_add_to_iodesc_list(iodesc=%p)", iodesc));

  if(comm != MPI_COMM_NULL){
    int tmp_id = pio_iodesc_next_id;
    int mpierr = MPI_Allreduce(&tmp_id, &pio_iodesc_next_id, 1, MPI_INT, MPI_MAX, comm);
    assert(mpierr == MPI_SUCCESS);
  }
  iodesc->ioid = pio_iodesc_next_id;
  pio_iodesc_next_id++;

  SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.insert({iodesc->ioid, iodesc});
  LOG((2, "Added iodesc %p (ioid=%d) to the global list", iodesc, iodesc->ioid));
  return iodesc->ioid;
}

/** 
 * Get the I/O descriptor (io_desc_t) associated with a I/O descriptor id.
 *
 * @param ioid The id of the I/O descriptor (io_desc_t) to lookup
 * @returns Pointer to the I/O descriptor (io_desc_t) associated with the id
 */
io_desc_t *pio_get_iodesc_from_id(int ioid)
{
  LOG((2, "pio_get_iodesc_from_id(ioid=%d)", ioid));

  io_desc_t *iodesc = NULL;
  try{
    iodesc = SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.at(ioid);  
  } catch(const std::out_of_range &e){
    LOG((1, "Finding I/O descriptor corresponding to ioid = %d failed. Invalid I/O descriptor id provided", ioid));
  }

  return iodesc;
}

/** 
 * Delete a I/O descriptor from the global list of valid I/O descriptors
 *
 * @param ioid The id of the I/O descriptor (io_desc_t) to delete
 * @returns 0 on success, error code otherwise.
 * @author Jayesh Krishna
 */
int pio_delete_iodesc_from_list(int ioid)
{
  LOG((2, "pio_delete_iodesc_from_list(ioid=%d)", ioid));
  std::map<int, io_desc_t *>::iterator iter = SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.find(ioid);
  if(iter != SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.end()){
    io_desc_t *iodesc = (*iter).second;
    assert(iodesc);
    if(iodesc->nasync_pend_ops == 0){
      free(iodesc);
      SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.erase(iter);
    }
  }
  else{
    return pio_err(NULL, NULL, PIO_EBADID, __FILE__, __LINE__,
                  "Deleting I/O descriptor info (ioid=%d) from internal global list failed. Invalid I/O descriptor id provided", ioid);
  }
  return PIO_NOERR;
}

int pio_delete_all_iodescs(void )
{
  for(std::map<int, io_desc_t *>::iterator iter = SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.begin();
        iter != SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.end(); ++iter){
    io_desc_t *iodesc = (*iter).second;
    assert(iodesc && (iodesc->nasync_pend_ops == 0));
    free(iodesc);
  }

  SPIO_Util::SPIO_Lists::GVars::pio_iodesc_list.clear();
  return PIO_NOERR;
}
