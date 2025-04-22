/*********************************************************************
 *
 *  Copyright (C) 2013, Northwestern University and Argonne National Laboratory
 *  See COPYRIGHT notice in top-level directory.
 *
 *********************************************************************/
/* $Id$ */

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * This example shows how to use ncmpi_put_vara_int_all() to write a 2D 4-byte
 * integer array in parallel. It first defines a netCDF variable of size
 * global_ny * global_nx where
 *    global_ny == NY and
 *    global_nx == (NX * number of MPI processes).
 * The data partitioning pattern is a column-wise partitioning across all
 * processes. Each process writes a subarray of size ny * nx.
 *
 *    To compile:
 *        mpicc -O2 put_vara.c -o put_vara -lpnetcdf
 *
 * Example commands for MPI run and outputs from running ncmpidump on the
 * output netCDF file produced by this example program:
 *
 *    % mpiexec -n 4 ./put_vara /pvfs2/wkliao/testfile.nc
 *
 *    % ncmpidump /pvfs2/wkliao/testfile.nc
 *    netcdf testfile {
 *    // file format: CDF-5 (big variables)
 *    dimensions:
 *            y = 10 ;
 *            x = 16 ;
 *    variables:
 *            int var(y, x) ;
 *                var:str_att_name = "example attribute of type text." ;
 *                var:float_att_name = 0.f, 1.f, 2.f, 3.f, 4.f, 5.f, 6.f, 7.f ;
 *                var:short_att_name = 1000s ;
 *    // global attributes:
 *                :history = "Mon Aug 13 21:27:48 2018" ;
 *       "" ;
 *    data:
 *
 *     var =
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3,
 *         0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3 ;
 *    }
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#include <vector>
#include <iostream>
#include <sstream>
#include <algorithm>
#include <iterator>
#include <initializer_list>
#include <cstdio>
#include <cstdlib>
#include <cstring> /* strcpy(), strncpy() */
#include <unistd.h> /* getopt() */
#include <ctime>   /* time() localtime(), asctime() */
#include <cmath>
#include <cassert>
#include <mpi.h>
#include <pnetcdf.h>

#define NZ 2
#define NY 7
#define NX 2

static int verbose;

#define ERR {if(err!=NC_NOERR){printf("Error at %s:%d : %s\n", __FILE__,__LINE__, ncmpi_strerror(err));nerrs++;}}

static void
usage(char *argv0)
{
    const char *help =
    "Usage: %s [-h] | [-q] [-k format] [file_name]\n"
    "       [-h] Print help\n"
    "       [-q] Quiet mode (reports when fail)\n"
    "       [-k format] file format: 1 for CDF-1, 2 for CDF-2, 3 for NetCDF4,\n"
    "                                4 for NetCDF4 classic model, 5 for CDF-5\n"
    "       [filename] output netCDF file name\n";
    fprintf(stderr, help, argv0);
}

/*----< pnetcdf_check_mem_usage() >------------------------------------------*/
/* check PnetCDF library internal memory usage */
static int
pnetcdf_check_mem_usage(MPI_Comm comm)
{
    int err, nerrs=0, rank;
    MPI_Offset malloc_size, sum_size;

    MPI_Comm_rank(comm, &rank);

    /* print info about PnetCDF internal malloc usage */
    err = ncmpi_inq_malloc_max_size(&malloc_size);
    if (err == NC_NOERR) {
        MPI_Reduce(&malloc_size, &sum_size, 1, MPI_OFFSET, MPI_SUM, 0, MPI_COMM_WORLD);
        if (rank == 0 && verbose)
            printf("maximum heap memory allocated by PnetCDF internally is %lld bytes\n",
                   sum_size);

        /* check if there is any PnetCDF internal malloc residue */
        err = ncmpi_inq_malloc_size(&malloc_size);
        MPI_Reduce(&malloc_size, &sum_size, 1, MPI_OFFSET, MPI_SUM, 0, MPI_COMM_WORLD);
        if (rank == 0 && sum_size > 0)
            printf("heap memory allocated by PnetCDF internally has %lld bytes yet to be freed\n",
                   sum_size);
    }
    else if (err != NC_ENOTENABLED) {
        printf("Error at %s:%d: %s\n", __FILE__,__LINE__,ncmpi_strerror(err));
        nerrs++;
    }
    return nerrs;
}

void convert_gidx_to_dim_idx(int gidx, int ndims, const MPI_Offset *gdimlen, std::vector<MPI_Offset> &dim_idx)
{
  int dim_chunk_sz[ndims];

  assert(ndims > 0);
  assert(gdimlen && (static_cast<int>(dim_idx.size()) == ndims));

  dim_chunk_sz[ndims - 1] = 1;
  for(int i = ndims - 2; i >= 0; i--){
    dim_chunk_sz[i] = dim_chunk_sz[i + 1] * gdimlen[i + 1];
  }

  for(int i = 0; i < ndims; i++){
    dim_idx[i] = gidx / dim_chunk_sz[i];

    gidx -= dim_idx[i] * dim_chunk_sz[i];
  }

  assert(gidx == 0);
}

/* Debug print a vector */
template<typename T>
static void print_1dvec(const std::vector<T> &v)
{
  std::ostringstream ostr;
  ostr << "[";
  std::copy(v.cbegin(), v.cend(), std::ostream_iterator<T>(ostr, ","));
  ostr << "]";
  std::cout << ostr.str().c_str() << "\n" << std::flush;
}


void partition_range_to_contig_dim_ranges(int ndims, const MPI_Offset *gdimlen,
                      MPI_Offset start_off, MPI_Offset end_off,
                      std::vector<MPI_Offset> &start, std::vector<MPI_Offset> &count)
{
  assert(ndims > 0);
  assert(gdimlen && (start_off >= 0) && (end_off >= start_off));
  
  start.clear(); count.clear();

  MPI_Offset cur_tot_count = end_off - start_off;

  if(cur_tot_count == 0) return;

  // First get start_off to a multiple of last dim len
  // e.g. v[z][y][x], get range to get to multiple of x
  MPI_Offset cur_range_start = start_off;
  MPI_Offset cur_range_count = 0;
  MPI_Offset off_from_last_dim_mult = cur_range_start % gdimlen[ndims - 1];
  if(off_from_last_dim_mult != 0){
    cur_range_count = gdimlen[ndims - 1] - off_from_last_dim_mult;
    if(start_off + cur_range_count > end_off){
      cur_range_count = end_off - start_off;
    }
    start.push_back(cur_range_start);
    count.push_back(cur_range_count);

    cur_range_start += cur_range_count;
    cur_tot_count -= cur_range_count;
  }

  if(cur_tot_count == 0) return;

  // Break/partition the cur_range_start to end_off to contiguous dim chunks
  // e.g. v[z][y][x], break up remaining range to ranges of sizes y*x, x, 1
  std::vector<MPI_Offset> dim_chunk_sz(ndims, 1);
  for(int i = ndims - 2; i >= 0; i--){
    dim_chunk_sz[i] = dim_chunk_sz[i + 1] * gdimlen[i + 1];
  }

  MPI_Offset cur_range_end = cur_range_start;
  for(int i = 0; i < ndims; i++){
    cur_range_end = dim_chunk_sz[i] * (end_off / dim_chunk_sz[i]);
    if(cur_range_end != 0){
      cur_range_count = cur_range_end - cur_range_start;
      start.push_back(cur_range_start);
      count.push_back(cur_range_count);

      cur_range_start += cur_range_count;
      cur_tot_count -= cur_range_count;
    }
    if(cur_tot_count == 0) break;
  }

  assert(cur_tot_count == 0);
}

int validate_starts_counts(MPI_Offset start_off, MPI_Offset end_off,
                            const std::vector<std::vector<MPI_Offset> > &starts,
                            const std::vector<std::vector<MPI_Offset> > &counts)
{
  const int FAIL = 1, SUCCESS = 0;

  if(start_off > end_off){
    std::cerr << "ERROR: Start offset provided needs to be <= end offset\n";
    return FAIL;
  }

  if(starts.size() != counts.size()){
    std::cerr << "ERROR: Mismatched starts/counts array sizes\n";
    return FAIL;
  }

  if(starts.size() == 0) return SUCCESS;

  if(starts[0].size() == 0){
    std::cerr << "ERROR: No start arrays in the starts vector\n";
    return FAIL;
  }

  if(starts[0][0] != start_off){
    std::cerr << "ERROR: Invalid start offset, starts[0][0] = " << starts[0][0] << ", expected = " << start_off << "\n" << std::flush;
    return FAIL;
  }

  MPI_Offset next_exp_off = start_off;
  for(std::size_t i = 0; i < starts.size(); i++){
    if(starts[i].size() != counts[i].size()){
      std::cerr << "ERROR: Mismatch in start/count array sizes: starts[" << i << "].size() = " << starts[i].size() << ", counts[" << i << "].size() = " << counts[i].size() << "\n" << std::flush;
      return FAIL;
    }
    for(std::size_t j = 0; j < starts[i].size(); j++){
      if(starts[i][j] != next_exp_off){
        std::cout << "ERROR : Mismatch in start offset, starts[" << i << "][" << j << "] = " << starts[i][j] << ", expected = " << next_exp_off << "\n" << std::flush;
        return FAIL;
      }
      next_exp_off += counts[i][j];
    }
  }

  return SUCCESS;
}

void test_partition_ranges(void )
{
  const MPI_Offset ZMAX = 10;
  const MPI_Offset YMAX = 10;
  const MPI_Offset XMAX = 10;
  const int MAX_NPROCS = 6;

  for(MPI_Offset z = 1; z < ZMAX; z++){
    for(MPI_Offset y = 1; y < YMAX; y++){
      for(MPI_Offset x = 1; x < XMAX; x++){
        std::vector<MPI_Offset> gdimlen = {z, y, x};
        MPI_Offset tot_gdimlen = 1;
        std::vector<MPI_Offset> start, count;
        std::vector<std::vector<MPI_Offset> > starts, counts;

        for(std::size_t d = 0; d < gdimlen.size(); d++){
          tot_gdimlen *= gdimlen[d];
        }
        std::cout << "Testing gdimlen[] : "; print_1dvec(gdimlen);
        for(int nprocs = 1; nprocs < std::min(MAX_NPROCS, static_cast<int>(tot_gdimlen)); nprocs++){
          
          MPI_Offset proc_chunk_sz = tot_gdimlen / nprocs;
          for(int rank = 0; rank < nprocs; rank++){
            MPI_Offset start_off = rank * proc_chunk_sz;
            if(rank == nprocs - 1) proc_chunk_sz = tot_gdimlen - (nprocs - 1) * proc_chunk_sz;
            MPI_Offset end_off = start_off + proc_chunk_sz;

            partition_range_to_contig_dim_ranges(gdimlen.size(), gdimlen.data(), start_off, end_off, start, count);
            std::cout << "[" << rank << "/" << nprocs << "]: partition_range start/count [" << start_off << "," << end_off <<") : "; print_1dvec(start); print_1dvec(count);
            starts.push_back(start); counts.push_back(count);
          }
          validate_starts_counts(0, tot_gdimlen, starts, counts);
          starts.clear(); counts.clear();
        }
      }
    }
  }
}

static inline void convert_off_to_start_dim_idx(const std::vector<MPI_Offset> &dim_chunk_sz, int start_off, std::vector<MPI_Offset> &start_dim_idx)
{
  assert(dim_chunk_sz.size() == start_dim_idx.size());
  for(std::size_t i = 0; i < start_dim_idx.size(); i++){
    start_dim_idx[i] = 0;
  }
  for(std::size_t i = 0; i < dim_chunk_sz.size(); i++){
    start_dim_idx[i] = start_off / dim_chunk_sz[i];

    start_off -= start_dim_idx[i] * dim_chunk_sz[i];
    if(start_off == 0) break;
  }
  assert(start_off == 0);
}

static inline void convert_off_to_count_dim_idx(int ndims, const MPI_Offset *gdimlen, const std::vector<MPI_Offset> &dim_chunk_sz, int count_off, std::vector<MPI_Offset> &count_dim_idx)
{
  assert(dim_chunk_sz.size() == count_dim_idx.size());
  assert(static_cast<int>(count_dim_idx.size()) == ndims);

  for(int i = 0; i < ndims; i++){
    count_dim_idx[i] = gdimlen[i];
  }
  for(std::size_t i = 0; i < dim_chunk_sz.size(); i++){
    count_dim_idx[i] = count_off / dim_chunk_sz[i];
    
    count_off -= count_dim_idx[i] * dim_chunk_sz[i];
    if(count_off == 0) break;
  }

  assert(count_off == 0);
  for(std::size_t i = 0; i < count_dim_idx.size(); i++){
    if(count_dim_idx[i] != 0){
      break;
    }
    else{
      count_dim_idx[i] = 1;
    }
  }
}

void convert_start_count_off_to_dim_idx(int ndims, const MPI_Offset *gdimlen,
                    std::vector<MPI_Offset> &start_off, std::vector<MPI_Offset> &count_off,
                    std::vector<std::vector<MPI_Offset> > &starts, std::vector<std::vector<MPI_Offset> > &counts)
{
  assert((ndims > 0) && gdimlen);
  assert(start_off.size() == count_off.size());

  std::vector<MPI_Offset> dim_chunk_sz(ndims, 1);
  for(int i = ndims - 2; i >= 0; i--){
    dim_chunk_sz[i] = dim_chunk_sz[i + 1] * gdimlen[i + 1];
  }
  std::vector<MPI_Offset> start(ndims, 0), count(ndims, 1);
  assert(start_off.size() == count_off.size());
  for(std::size_t i = 0; i < start_off.size(); i++){
    convert_off_to_start_dim_idx(dim_chunk_sz, start_off[i], start);
    convert_off_to_count_dim_idx(ndims, gdimlen, dim_chunk_sz, count_off[i], count);
    starts.push_back(start); counts.push_back(count);
  }
}

void partition_buf(int rank, int nprocs, int ndims, const MPI_Offset *gdimlen,
                    std::vector<std::vector<MPI_Offset> > &starts, std::vector<std::vector<MPI_Offset> > &counts)
{
  std::vector<MPI_Offset> start_coff, count_coff;

  MPI_Offset tot_gdimlen = 1;
  for(int i = 0; i < ndims; i++){
    tot_gdimlen *= gdimlen[i];
  }

  MPI_Offset iochunk_sz = tot_gdimlen / nprocs;
  assert(iochunk_sz > 0);

  MPI_Offset start_off = rank * iochunk_sz;
  if(rank == nprocs - 1){
    iochunk_sz = tot_gdimlen - (nprocs - 1) * iochunk_sz;
  }
  MPI_Offset end_off = start_off + iochunk_sz;

  //std::cout << "start/end off : " << start_off << ", " << end_off << "\n" << std::flush;
  partition_range_to_contig_dim_ranges(ndims, gdimlen, start_off, end_off, start_coff, count_coff);
  //usleep(rank * 1000000);
  //std::cout << "start/end contig off : "; print_1dvec(start_coff); print_1dvec(count_coff);
  convert_start_count_off_to_dim_idx(ndims, gdimlen, start_coff, count_coff, starts, counts);
}

/*
// Hard-coded partition for var [2, 7, 2] with 3 procs
void hc_partition_buf(int rank, int nprocs, int ndims, const MPI_Offset *gdimlen,
                    std::vector<std::vector<MPI_Offset> > &starts, std::vector<std::vector<MPI_Offset> > &counts)
{
  std::vector<MPI_Offset> hc_start(ndims, 0), hc_count(ndims, 1);
  if(rank == 0){
    hc_start = {0, 0, 0};
    hc_count = {1, 4, 2};

    starts.push_back(hc_start);
    counts.push_back(hc_count);
  }
  else if(rank == 1){
    hc_start = {0, 4, 0};
    hc_count = {1, 3, 2};

    starts.push_back(hc_start);
    counts.push_back(hc_count);

    hc_start = {1, 0, 0};
    hc_count = {1, 1, 2};

    starts.push_back(hc_start);
    counts.push_back(hc_count);
  }
  else if(rank == 2){
    hc_start = {1, 1, 0};
    hc_count = {1, 6, 2};

    starts.push_back(hc_start);
    counts.push_back(hc_count);
  }
  else{
    assert(0);
  }
}
*/

/*----< pnetcdf_io() >-------------------------------------------------------*/
static int
pnetcdf_io(MPI_Comm comm, char *filename, int cmode)
{
    int i, j, z, rank, nprocs, err, nerrs=0;
    int ncid, dimid[3];
    int dimid_x_1d2d;
    int var1did, buf1d[NX];
    int var2did, buf2d[NY][NX];
    int var3did, buf3d[NZ][NY][NX], var3d_rankid, buf3d_rank[NZ][NY][NX];
    char str_att[128];
    float float_att[100];
    MPI_Offset  global_nz, global_ny, global_nx, gdimlen[3];
    //MPI_Offset  global_tot_sz;
    MPI_Offset  global_nx_1d2d;
    MPI_Offset start[3], count[3];
    std::vector<std::vector<MPI_Offset> > starts, counts;

    MPI_Comm_rank(comm, &rank);
    MPI_Comm_size(comm, &nprocs);

    /* create a new file for writing ----------------------------------------*/
    cmode |= NC_CLOBBER;
    err = ncmpi_create(comm, filename, cmode, MPI_INFO_NULL, &ncid); ERR

    for (i=0; i<NX; i++)
         buf1d[i] = rank;

    /* the global array is NY * (NX * nprocs) */
    global_nz = NZ;
    global_ny = NY;
    global_nx = NX;
    global_nx_1d2d = NX * nprocs;

    //global_tot_sz = global_nz * global_ny * global_nx;
    gdimlen[0] = global_nz;
    gdimlen[1] = global_ny;
    gdimlen[2] = global_nx;

    for (i=0; i<NY; i++)
        for (j=0; j<NX; j++)
             buf2d[i][j] = rank;

    for (z=0; z<NZ; z++)
        for (i=0; i<NY; i++)
            for (j=0; j<NX; j++){
                   buf3d[z][i][j] = z * (NY * NX) + i * NX + j + rank * (1 * NY * NX);
                   buf3d_rank[z][i][j] = rank;
            }

    /* add a global attribute: a time stamp at rank 0 */
    time_t ltime = time(NULL); /* get the current calendar time */
    asctime_r(localtime(&ltime), str_att);
    sprintf(str_att, "Mon Aug 13 21:27:48 2018");

    /* make sure the time string are consistent among all processes */
    MPI_Bcast(str_att, sizeof(str_att), MPI_CHAR, 0, comm);

    err = ncmpi_put_att_text(ncid, NC_GLOBAL, "history", strlen(str_att),
                             &str_att[0]); ERR

    /* define dimensions x and y */
    err = ncmpi_def_dim(ncid, "Z", global_nz, &dimid[0]); ERR
    err = ncmpi_def_dim(ncid, "Y", global_ny, &dimid[1]); ERR
    err = ncmpi_def_dim(ncid, "X", global_nx, &dimid[2]); ERR
    err = ncmpi_def_dim(ncid, "X_1d2d", global_nx_1d2d, &dimid_x_1d2d); ERR

    /* define a 2D variable of integer type */
    err = ncmpi_def_var(ncid, "var3d", NC_INT, 3, &dimid[0], &var3did); ERR
    err = ncmpi_def_var(ncid, "var3d_rank", NC_INT, 3, &dimid[0], &var3d_rankid); ERR

    dimid[2] = dimid_x_1d2d;
    /* define a 1D variable of integer type */
    err = ncmpi_def_var(ncid, "var1d", NC_INT, 1, &dimid[2], &var1did); ERR

    /* define a 2D variable of integer type */
    err = ncmpi_def_var(ncid, "var2d", NC_INT, 2, &dimid[1], &var2did); ERR

    /* add attributes to the variable */
    strcpy(str_att, "example attribute of type text.");
    err = ncmpi_put_att_text(ncid, var2did, "str_att_name", strlen(str_att),
                             &str_att[0]); ERR

    for (i=0; i<8; i++) float_att[i] = i;
    err = ncmpi_put_att_float(ncid, var2did, "float_att_name", NC_FLOAT, 8,
                              &float_att[0]); ERR
    short short_att=1000;
    err = ncmpi_put_att_short(ncid, var2did, "short_att_name", NC_SHORT, 1,
                              &short_att); ERR

    /* do not forget to exit define mode */
    err = ncmpi_enddef(ncid); ERR

    /* now we are in data mode */
    start[0] = NX * rank;
    count[0] = NX;

    err = ncmpi_put_vara_int_all(ncid, var1did, start, count, &buf1d[0]); ERR

    start[0] = 0;
    start[1] = NX * rank;
    count[0] = NY;
    count[1] = NX;

    err = ncmpi_put_vara_int_all(ncid, var2did, start, count, &buf2d[0][0]); ERR

    partition_buf(rank, nprocs, 3, gdimlen, starts, counts);

    assert(starts.size() == counts.size());
    MPI_Offset **cstarts = (MPI_Offset **) calloc(starts.size(), sizeof(MPI_Offset));
    MPI_Offset **ccounts = (MPI_Offset **) calloc(counts.size(), sizeof(MPI_Offset));

    for(std::size_t i = 0; i < starts.size(); i++){
      //printf("start[:] = {%ld, %ld, %ld}\n", starts[i][0], starts[i][1], starts[i][2]); fflush(stdout);
      //printf("count[:] = {%ld, %ld, %ld}\n", counts[i][0], counts[i][1], counts[i][2]); fflush(stdout);

      cstarts[i] = starts[i].data();
      ccounts[i] = counts[i].data();
    }

    err = ncmpi_iput_varn_int(ncid, var3did, starts.size(), cstarts, ccounts, &buf3d[0][0][0], NULL); ERR
    err = ncmpi_iput_varn_int(ncid, var3d_rankid, starts.size(), cstarts, ccounts, &buf3d_rank[0][0][0], NULL); ERR

    err = ncmpi_wait_all(ncid, NC_REQ_ALL, NULL, NULL); ERR

    free(cstarts); free(ccounts);

    err = ncmpi_close(ncid); ERR

    return nerrs;
}

int main(int argc, char** argv)
{
    extern int optind;
    extern char *optarg;
    char filename[256];
    int i, rank, kind=0, cmode=0, nerrs=0;

    MPI_Init(&argc, &argv);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    verbose = 1;

    /* get command-line arguments */
    while ((i = getopt(argc, argv, "hqk:")) != EOF)
        switch(i) {
            case 'q': verbose = 0;
                      break;
            case 'k': kind = atoi(optarg);
                      break;
            case 'h':
            default:  if (rank==0) usage(argv[0]);
                      MPI_Finalize();
                      return 1;
        }
    if (argv[optind] == NULL) strcpy(filename, "testfile.nc");
    else                      snprintf(filename, 256, "%s", argv[optind]);

    MPI_Bcast(filename, 256, MPI_CHAR, 0, MPI_COMM_WORLD);

    if (verbose && rank == 0) printf("%s: example of using put_vara APIs\n",__FILE__);

    switch (kind) {
        case(2): cmode = NC_64BIT_OFFSET;             break;
        case(3): cmode = NC_NETCDF4;                  break;
        case(4): cmode = NC_NETCDF4|NC_CLASSIC_MODEL; break;
        case(5): cmode = NC_64BIT_DATA;               break;
        default: cmode = 0;
    }

#ifndef PNETCDF_DRIVER_NETCDF4
    /* netcdf4 driver is not enabled, skip */
    if (kind == 3 || kind == 4) {
        MPI_Finalize();
        return 0;
    }
#endif
    nerrs += pnetcdf_io(MPI_COMM_WORLD, filename, cmode);
//    test_partition_ranges();

    nerrs += pnetcdf_check_mem_usage(MPI_COMM_WORLD);

    MPI_Finalize();
    return (nerrs > 0);
}

