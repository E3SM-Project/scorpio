#include "pio_rearr_utils.hpp"
#include "spio_gptl_utils.hpp"
#include <iostream>
#include <vector>

namespace SPIO_Util{
  namespace Rearr_Util{
    char gsend_hs = 'h';
    int isend_hs(int to_proc, MPI_Comm comm, int comm_rank, int comm_sz, MPI_Request &req)
    {
      /* Tag identifies who the data is from */
      int tag = comm_sz + comm_rank;

      return MPI_Isend(&gsend_hs, 1, MPI_CHAR, to_proc, tag, comm, &req);
    }

    char grecv_hs;
    int irecv_hs(int from_proc, MPI_Comm comm, int comm_rank, int comm_sz, MPI_Request &req)
    {
      /* Tag identifies who the data is from */
      int tag = comm_sz + from_proc;

      return MPI_Irecv(&grecv_hs, 1, MPI_CHAR, from_proc, tag, comm, &req);
    }

  }// namespace Rearr_Util
}// namespace SPIO_Util

int SPIO_Util::Rearr_Util::gatherw(const void *sendbuf, int sendcount,
                                    MPI_Datatype sendtype,
                                    void *recvbuf, const std::vector<int> &recvcounts,
                                    const std::vector<int> &rdispls,
                                    const std::vector<MPI_Datatype> &recvtypes,
                                    int root, MPI_Comm comm, const rearr_comm_fc_opt_t *fc)
{
  int ret = PIO_NOERR, comm_rank = -1, comm_sz = 0;

  SPIO_Util::GPTL_Util::GPTL_wrapper("PIO:SPIO_Util::Rearr_Util::gatherw");

  //assert((sendcount == 0) || sendbuf);

  ret = MPI_Comm_rank(comm, &comm_rank);
  if(ret != MPI_SUCCESS){
    return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
  }

  ret = MPI_Comm_size(comm, &comm_sz);
  if(ret != MPI_SUCCESS){
    return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
  }

  if(comm_rank == root){
    /* No data to gather, return */
    /* FIXME: Handle erroneous calls with senders trying to send data with invalid/null recvbuf */
    if(!recvbuf){
      return PIO_NOERR;
    }
  }
  else{
    if(!sendbuf || (sendcount == 0)){
      return PIO_NOERR;
    }
  }

  /* Post the receives on root, send a handshake to indicate that receives are posted and
   * send the data to root.
   */

  /* Post recv for handshake - to indicate gather buffer is ready - on all non-root procs */
  if(comm_rank != root){
    MPI_Request hs_req, send_req;
    MPI_Status hs_status, send_status;

    /* Tag identifies the sender */
    int tag = comm_sz + comm_rank;

    ret = irecv_hs(root, comm, comm_rank, comm_sz, hs_req);
    if(ret != PIO_NOERR){
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Posting handshake for gather failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }

    ret = MPI_Wait(&hs_req, &hs_status);
    if(ret != MPI_SUCCESS){
      /* FIXME: Take a look at the status too */
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Waiting on handshake for gather failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }

    /* The non-blocking nature of the send can be used in future to avoid blocking on gather */
    /* Send the data to root */
    ret = MPI_Isend(sendbuf, sendcount, sendtype, root, tag, comm, &send_req);
    if(ret != PIO_NOERR){
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Posting send for data to gather failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }

    /* We are blocking on the gather now, but this could change in future */
    ret = MPI_Wait(&send_req, &send_status);
    if(ret != MPI_SUCCESS){
      /* FIXME: Take a look at the status too */
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Waiting on sending data for gather failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }
  }
  else{
    assert((recvcounts.size() == rdispls.size()) && (rdispls.size() == recvtypes.size()));
    assert(recvcounts.size() == static_cast<size_t>(comm_sz));

    /* Post receives for the data to gather */
    std::vector<MPI_Request> recv_reqs(comm_sz, MPI_REQUEST_NULL);
    for(int i=0; i < comm_sz; i++){
      if(recvcounts[i] > 0){
        /* Tag identifies who the data is from - the sender */
        int tag = comm_sz + i;
        ret = MPI_Irecv(static_cast<char *>(recvbuf) + rdispls[i], recvcounts[i], recvtypes[i],
                        i, tag, comm, &recv_reqs[i]);
        if(ret != PIO_NOERR){
          return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                          "Posting recv for data from proc %d to gather failed on proc (%d), comm = (id=%x, sz=%d)",
                          i, comm_rank, comm, comm_sz);
        }
      }
    }

    /* Send data to self */
    MPI_Request send_req = MPI_REQUEST_NULL;
    if(sendcount > 0){
      /* Tag identifies who the data is from - the sender */
      int send_tag = comm_sz + comm_rank;
      ret = MPI_Isend(sendbuf, sendcount, sendtype, root, send_tag, comm, &send_req);
      if(ret != MPI_SUCCESS){
        return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                        "Posting send for data to gather failed on proc (%d), comm = (id=%x, sz=%d)",
                        comm_rank, comm, comm_sz);
      }
    }

    /* Send handshakes */
    std::vector<MPI_Request> hs_reqs(comm_sz, MPI_REQUEST_NULL);
    for(int i=0; i < comm_sz; i++){
      if(i == root){
        /* We don't need handshake for root process - since recvs are already posted before sends
         * on the root
         */
        continue;
      }
      if(recvcounts[i] > 0){
        ret = isend_hs(i, comm, comm_rank, comm_sz, hs_reqs[i]);
        if(ret != MPI_SUCCESS){
          return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                          "Posting send for handshake from root (%d) to proc (%d), comm = (id=%x, sz=%d)",
                          comm_rank, i, comm, comm_sz);
        }
      }
    }

    /* Wait on sent data, handshakes & then receives - gather the data */
    ret = MPI_Wait(&send_req, MPI_STATUS_IGNORE);
    if(ret == MPI_SUCCESS){
      ret = MPI_Waitall(static_cast<int>(hs_reqs.size()), hs_reqs.data(), MPI_STATUSES_IGNORE);
      if(ret == MPI_SUCCESS){
        ret = MPI_Waitall(static_cast<int>(recv_reqs.size()), recv_reqs.data(), MPI_STATUSES_IGNORE);
      }
    }

    if(ret != MPI_SUCCESS){
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Waiting on data/handshakes/recvs failed on root process(%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }
  }

  return ret;
}

int SPIO_Util::Rearr_Util::scatterw(const void *sendbuf, const std::vector<int> &sendcounts,
                                    const std::vector<int> &sdispls,
                                    const std::vector<MPI_Datatype> &sendtypes,
                                    void *recvbuf, int recvcount,
                                    MPI_Datatype recvtype,
                                    int root, MPI_Comm comm, const rearr_comm_fc_opt_t *fc)
{
  int ret = PIO_NOERR, comm_rank = -1, comm_sz = 0;

  SPIO_Util::GPTL_Util::GPTL_wrapper("PIO:SPIO_Util::Rearr_Util::scatterw");

  ret = MPI_Comm_rank(comm, &comm_rank);
  if(ret != MPI_SUCCESS){
    return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
  }

  ret = MPI_Comm_size(comm, &comm_sz);
  if(ret != MPI_SUCCESS){
    return check_mpi(NULL, NULL, ret, __FILE__, __LINE__);
  }

  if(comm_rank == root){
    /* No data to scatter, return */
    /* FIXME: Handle erroneous calls with receivers waiting for data etc */
    if(!sendbuf){
      return PIO_NOERR;
    }
  }
  else{
    if(!recvbuf || (recvcount == 0)){
      return PIO_NOERR;
    }
  }

  /* Post the receives on compute procs, send a handshake to indicate that receives are posted and
   * send the data from root.
   */

  /* Send handshake - to indicate scatter recv buffer is ready - on all non-root procs */
  if(comm_rank != root){
    MPI_Request hs_req, recv_req;
    MPI_Status hs_status, recv_status;

    /* Tag identifies the sender */
    int tag = comm_sz + root;

    /* Post recv for data from root */
    ret = MPI_Irecv(recvbuf, recvcount, recvtype, root, tag, comm, &recv_req);
    if(ret != PIO_NOERR){
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Posting recv for data for scatter failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }

    /* Send handshake to root to indicate that recv has been posted */
    ret = isend_hs(root, comm, comm_rank, comm_sz, hs_req);
    if(ret != PIO_NOERR){
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Posting handshake for scatter failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }

    ret = MPI_Wait(&hs_req, &hs_status);
    if(ret != MPI_SUCCESS){
      /* FIXME: Take a look at the status too */
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Waiting on handshake, sent to root, for scatter failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }

    /* We are blocking on scatter data from root now, but this could change in future */
    ret = MPI_Wait(&recv_req, &recv_status);
    if(ret != MPI_SUCCESS){
      /* FIXME: Take a look at the status too */
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Waiting on receiving data for scatter failed on proc (%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }
  }
  else{
    assert((sendcounts.size() == sdispls.size()) && (sdispls.size() == sendtypes.size()));
    assert(sendcounts.size() == static_cast<size_t>(comm_sz));

    /* Wait for handshakes from processes - to indicate that recvs are posted for the scatter data */
    /* Post receives for handshakes */
    std::vector<MPI_Request> hs_reqs(comm_sz, MPI_REQUEST_NULL);
    for(int i=0; i < comm_sz; i++){
      if(i == root){
        /* We don't need handshake for root process - since recvs are posted before sends
         * on the root
         */
        continue;
      }
      if(sendcounts[i] > 0){
        ret = irecv_hs(i, comm, comm_rank, comm_sz, hs_reqs[i]);
        if(ret != MPI_SUCCESS){
          return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                          "Posting recv for handshake to root (%d) from proc (%d), comm = (id=%x, sz=%d)",
                          comm_rank, i, comm, comm_sz);
        }
      }
    }

    /* Wait on handshakes from procs */
    ret = MPI_Waitall(static_cast<int>(hs_reqs.size()), hs_reqs.data(), MPI_STATUSES_IGNORE);
    if(ret != MPI_SUCCESS){
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Receiving handshakes on root (%d) failed, comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }

    /* Post recv for the data from scatter */
    /* Tag identifies the sender */
    int tag = comm_sz + root;
    MPI_Request recv_req = MPI_REQUEST_NULL;

    /* Post recv for data from self/root */
    if(recvcount > 0){
      ret = MPI_Irecv(recvbuf, recvcount, recvtype, root, tag, comm, &recv_req);
      if(ret != PIO_NOERR){
        return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                        "Posting recv for data from self for scatter failed on root proc (%d), comm = (id=%x, sz=%d)",
                        comm_rank, comm, comm_sz);
      }
    }

    /* Send the data - scatter data to procs */
    std::vector<MPI_Request> send_reqs(comm_sz, MPI_REQUEST_NULL);
    for(int i=0; i < comm_sz; i++){
      if(sendcounts[i] > 0){
        /* Tag identifies who the data is from - the sender */
        int tag = comm_sz + root;
        ret = MPI_Isend(static_cast<const char *>(sendbuf) + sdispls[i], sendcounts[i], sendtypes[i],
                        i, tag, comm, &send_reqs[i]);
        if(ret != PIO_NOERR){
          return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                          "Posting send for data to proc %d for scatter failed on proc (%d), comm = (id=%x, sz=%d)",
                          i, comm_rank, comm, comm_sz);
        }
      }
    }

    /* Wait on sends to other processes and then wait on data received from self */
    ret = MPI_Waitall(static_cast<int>(send_reqs.size()), send_reqs.data(), MPI_STATUSES_IGNORE);
    if(ret == MPI_SUCCESS){
      ret = MPI_Wait(&recv_req, MPI_STATUS_IGNORE);
    }
    if(ret != MPI_SUCCESS){
      return pio_err(NULL, NULL, ret, __FILE__, __LINE__,
                      "Waiting on data/recvs failed on root process(%d), comm = (id=%x, sz=%d)",
                      comm_rank, comm, comm_sz);
    }
  }

  return ret;
}

/* FIXME: Refactor all the functions below */
namespace SPIO_Util{
  namespace Rearr_Util{
    static inline int ceil2(int i)
    {
      int p = 1;
      while(p < i){
        p *= 2;
      }

      return(p);
    }

    static inline int pair(int np, int p, int k)
    {
      int q = (p + 1) ^ k ;
      int pair = (q > np - 1) ? -1 : q;
      return pair;
    }
  }
}

int SPIO_Util::Rearr_Util::alltoallw(const void *sendbuf, const int *sendcounts,
                                      const int *sdispls, const MPI_Datatype *sendtypes,
                                      void *recvbuf, const int *recvcounts,
                                      const int *rdispls, const MPI_Datatype *recvtypes,
                                      MPI_Comm comm, const rearr_comm_fc_opt_t *fc)
{
  int ntasks;  /* Number of tasks in communicator comm. */
  int my_rank; /* Rank of this task in comm. */
  int tag;
  int offset_t;
  int steps;
  int istep;
  int rstep;
  int p;
  int maxreq;
  int maxreqh;
  int hs = 1; /* Used for handshaking. */
  void *ptr;
  MPI_Status status; /* Not actually used - replace with MPI_STATUSES_IGNORE. */
  int mpierr;  /* Return code from MPI functions. */

  SPIO_Util::GPTL_Util::GPTL_wrapper("PIO:SPIO_Util::Rearr_Util::alltoallw");
  LOG((2, "pio_swapm fc->hs = %d fc->isend = %d fc->max_pend_req = %d", fc->hs,
       fc->isend, fc->max_pend_req));

  /* Get my rank and size of communicator. */
  if((mpierr = MPI_Comm_size(comm, &ntasks))){
    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
  }
  if((mpierr = MPI_Comm_rank(comm, &my_rank))){
    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
  }

  LOG((2, "ntasks = %d my_rank = %d", ntasks, my_rank));

  /* Now we know the size of these arrays. */
  int swapids[ntasks];
  MPI_Request rcvids[ntasks];
  MPI_Request sndids[ntasks];
  MPI_Request hs_rcvids[ntasks];

  /* If fc->max_pend_req == 0 no throttling is requested and the default
   * mpi_alltoallw function is used. */
  if(fc->max_pend_req == 0){
    /* Some MPI implementations (some vers of OpenMPI, MPICH 4.0 etc) do not
     * allow passing MPI_DATATYPE_NULL to comm functions (MPI_Alltoallw) even
     * though the send or recv length is 0, so using a dummy MPI type instead
     * of MPI_DATATYPE_NULL
     */
    MPI_Datatype dummy_dt = MPI_CHAR;
    MPI_Datatype sndtypes[ntasks], rcvtypes[ntasks];
    for(int i = 0; i < ntasks; i++){
      sndtypes[i] = sendtypes[i];
      if(sndtypes[i] == MPI_DATATYPE_NULL){
        sndtypes[i] = dummy_dt;
      }
      rcvtypes[i] = recvtypes[i];
      if(rcvtypes[i] == MPI_DATATYPE_NULL){
        rcvtypes[i] = dummy_dt;
      }
    }

    /* Call the MPI alltoall without flow control. */
    LOG((3, "Calling MPI_Alltoallw without flow control."));
    if((mpierr = MPI_Alltoallw(sendbuf, sendcounts, sdispls, sndtypes, recvbuf,
                                recvcounts, rdispls, rcvtypes, comm))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
    return PIO_NOERR;
  }

  /* an index for communications tags */
  offset_t = ntasks;

  /* Send to self. */
  if(sendcounts[my_rank] > 0){
    void *sptr, *rptr;
    tag = my_rank + offset_t;
    sptr = (char *)sendbuf + sdispls[my_rank];
    rptr = (char *)recvbuf + rdispls[my_rank];

#ifdef ONEWAY
    /* If ONEWAY is true we will post mpi_sendrecv comms instead
     * of irecv/send. */
    if((mpierr = MPI_Sendrecv(sptr, sendcounts[my_rank],sendtypes[my_rank],
                               my_rank, tag, rptr, recvcounts[my_rank], recvtypes[my_rank],
                               my_rank, tag, comm, &status))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
#else
    if((mpierr = MPI_Irecv(rptr, recvcounts[my_rank], recvtypes[my_rank],
                            my_rank, tag, comm, rcvids))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
    if((mpierr = MPI_Send(sptr, sendcounts[my_rank], sendtypes[my_rank],
                           my_rank, tag, comm))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }

    if((mpierr = MPI_Wait(rcvids, &status))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
#endif
  }

  LOG((2, "Done sending to self... sending to other procs"));

  /* When send to self is complete there is nothing left to do if
   * ntasks==1. */
  if(ntasks == 1){
    return PIO_NOERR;
  }

  for(int i = 0; i < ntasks; i++){
    rcvids[i] = MPI_REQUEST_NULL;
    swapids[i] = 0;
  }

  if(fc->isend){
    for (int i = 0; i < ntasks; i++){
      sndids[i] = MPI_REQUEST_NULL;
    }
  }

  if(fc->hs){
    for(int i = 0; i < ntasks; i++){
      hs_rcvids[i] = MPI_REQUEST_NULL;
    }
  }

  steps = 0;
  for(istep = 0; istep < SPIO_Util::Rearr_Util::ceil2(ntasks) - 1; istep++){
    p = SPIO_Util::Rearr_Util::pair(ntasks, istep, my_rank);
    if(p >= 0 && (sendcounts[p] > 0 || recvcounts[p] > 0)){
      swapids[steps++] = p;
    }
  }

  LOG((3, "steps=%d", steps));

  if(steps == 0){
    return PIO_NOERR;
  }

  if(steps == 1){
    maxreq = 1;
    maxreqh = 1;
  }
  else{
    if(fc->max_pend_req == PIO_REARR_COMM_UNLIMITED_PEND_REQ){
      maxreq = steps;
      maxreqh = steps;
    }
    else if(fc->max_pend_req > 1 && fc->max_pend_req < steps){
      maxreq = fc->max_pend_req;
      maxreqh = maxreq / 2;
    }
    else if(fc->max_pend_req == 1){
      /* Note that steps >= 2 here */
      maxreq = 2;
      maxreqh = 1;
    }
    else{
      maxreq = steps;
      maxreqh = steps;
    }
  }

  LOG((2, "fc->max_pend_req=%d, maxreq=%d, maxreqh=%d", fc->max_pend_req, maxreq, maxreqh));

  /* If handshaking is in use, do a nonblocking recieve to listen
   * for it. */
  if(fc->hs){
    for(istep = 0; istep < maxreq; istep++){
      p = swapids[istep];
      if(sendcounts[p] > 0){
        tag = my_rank + offset_t;
        if((mpierr = MPI_Irecv(&hs, 1, MPI_INT, p, tag, comm, hs_rcvids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
    }
  }

  /* Post up to maxreq irecv's. */
  for(istep = 0; istep < maxreq; istep++){
    p = swapids[istep];
    if(recvcounts[p] > 0){
      tag = p + offset_t;
      ptr = (char *)recvbuf + rdispls[p];

      if((mpierr = MPI_Irecv(ptr, recvcounts[p], recvtypes[p], p, tag, comm,
                              rcvids + istep))){
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
      }

      if(fc->hs){
        if((mpierr = MPI_Send(&hs, 1, MPI_INT, p, tag, comm))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
    }
  }

  /* Tell the paired task that this tasks' has posted it's irecvs'. */
  rstep = maxreq;
  for(istep = 0; istep < steps; istep++){
    p = swapids[istep];
    if(sendcounts[p] > 0){
      tag = my_rank + offset_t;
      /* If handshake is enabled don't post sends until the
       * receiving task has posted recvs. */
      if(fc->hs){
        if((mpierr = MPI_Wait(hs_rcvids + istep, &status))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
        hs_rcvids[istep] = MPI_REQUEST_NULL;
      }
      ptr = (char *)sendbuf + sdispls[p];

      /* On some software stacks MPI_Irsend() is either not available, not
       * a major issue anymore, or is buggy. With PIO1 we have found that
       * although the code correctly posts receives before the irsends,
       * on some systems (software stacks) the code hangs. However the
       * code works fine with isends. The _USE_MPI_RSEND macro should be
       * used to use mpi_irsends, the default is mpi_isend
       */
      if(fc->hs && fc->isend){
#ifndef _USE_MPI_RSEND
        if((mpierr = MPI_Isend(ptr, sendcounts[p], sendtypes[p], p, tag, comm,
                                sndids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
#else
        if((mpierr = MPI_Irsend(ptr, sendcounts[p], sendtypes[p], p, tag, comm,
                                 sndids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
#endif
      }
      else if(fc->isend){
        if((mpierr = MPI_Isend(ptr, sendcounts[p], sendtypes[p], p, tag, comm,
                                 sndids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
      else{
        if((mpierr = MPI_Send(ptr, sendcounts[p], sendtypes[p], p, tag, comm))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
    }

    /* We did comms in sets of size max_reqs, if istep > maxreqh-1
     * then there is a remainder that must be handled. */
    if(istep > maxreqh - 1){
      p = istep - maxreqh;
      if(rcvids[p] != MPI_REQUEST_NULL){
        if((mpierr = MPI_Wait(rcvids + p, &status))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
        rcvids[p] = MPI_REQUEST_NULL;
      }
      if(rstep < steps){
        p = swapids[rstep];
        if(fc->hs && sendcounts[p] > 0){
          tag = my_rank + offset_t;
          if((mpierr = MPI_Irecv(&hs, 1, MPI_INT, p, tag, comm, hs_rcvids+rstep))){
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
          }
        }
        if(recvcounts[p] > 0){
          tag = p + offset_t;

          ptr = (char *)recvbuf + rdispls[p];
          if((mpierr = MPI_Irecv(ptr, recvcounts[p], recvtypes[p], p, tag, comm, rcvids + rstep))){
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
          }
          if(fc->hs){
            if((mpierr = MPI_Send(&hs, 1, MPI_INT, p, tag, comm))){
              return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            }
          }
        }
        rstep++;
      }
    }
  }

  /* If steps > 0 there could still be outstanding messages, wait for
   * them here. */
  if(steps > 0){
    LOG((2, "Waiting for outstanding msgs"));
    if((mpierr = MPI_Waitall(steps, rcvids, MPI_STATUSES_IGNORE))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
    if(fc->isend){
      if((mpierr = MPI_Waitall(steps, sndids, MPI_STATUSES_IGNORE))){
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
      }
    }
  }

  return PIO_NOERR;
}

/* FIXME: Refactor alltoall and alltoallw to combine common code */
int SPIO_Util::Rearr_Util::alltoall(const void *sendbuf, int sendcount,
                                      MPI_Datatype sendtype,
                                      void *recvbuf, int recvcount,
                                      MPI_Datatype recvtype,
                                      MPI_Comm comm, const rearr_comm_fc_opt_t *fc)
{
  int ntasks;  /* Number of tasks in communicator comm. */
  int my_rank; /* Rank of this task in comm. */
  int tag;
  int offset_t;
  int steps;
  int istep;
  int rstep;
  int p;
  int maxreq;
  int maxreqh;
  int hs = 1; /* Used for handshaking. */
  void *ptr;
  MPI_Status status; /* Not actually used - replace with MPI_STATUSES_IGNORE. */
  int mpierr;  /* Return code from MPI functions. */

  int sendtype_sz = 0, recvtype_sz = 0;

  if(sendtype != MPI_DATATYPE_NULL){
    if((mpierr = MPI_Type_size(sendtype, &sendtype_sz))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
  }

  if(recvtype != MPI_DATATYPE_NULL){
    if((mpierr = MPI_Type_size(recvtype, &recvtype_sz))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
  }

  SPIO_Util::GPTL_Util::GPTL_wrapper("PIO:SPIO_Util::Rearr_Util::alltoallw");
  LOG((2, "pio_swapm fc->hs = %d fc->isend = %d fc->max_pend_req = %d", fc->hs,
       fc->isend, fc->max_pend_req));

  /* Get my rank and size of communicator. */
  if((mpierr = MPI_Comm_size(comm, &ntasks))){
    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
  }
  if((mpierr = MPI_Comm_rank(comm, &my_rank))){
    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
  }

  LOG((2, "ntasks = %d my_rank = %d", ntasks, my_rank));

  /* Now we know the size of these arrays. */
  int swapids[ntasks];
  MPI_Request rcvids[ntasks];
  MPI_Request sndids[ntasks];
  MPI_Request hs_rcvids[ntasks];

  /* If fc->max_pend_req == 0 no throttling is requested and the default
   * mpi_alltoallw function is used. */
  if(fc->max_pend_req == 0){
    /* Some MPI implementations (some vers of OpenMPI, MPICH 4.0 etc) do not
     * allow passing MPI_DATATYPE_NULL to comm functions (MPI_Alltoall) even
     * though the send or recv length is 0, so using a dummy MPI type instead
     * of MPI_DATATYPE_NULL
     */
    MPI_Datatype dummy_dt = MPI_CHAR;
    MPI_Datatype tmp_sendtype = sendtype, tmp_recvtype = recvtype;
    if(tmp_sendtype == MPI_DATATYPE_NULL){
      tmp_sendtype = dummy_dt;
    }
    if(tmp_recvtype == MPI_DATATYPE_NULL){
      tmp_recvtype = dummy_dt;
    }

    /* Call the MPI alltoall without flow control. */
    LOG((3, "Calling MPI_Alltoall without flow control."));
    if((mpierr = MPI_Alltoall(sendbuf, sendcount, tmp_sendtype,
                              recvbuf, recvcount, tmp_recvtype, comm))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
    return PIO_NOERR;
  }

  /* an index for communications tags */
  offset_t = ntasks;

  /* Send to self. */
  if(sendcount > 0){
    void *sptr, *rptr;
    tag = my_rank + offset_t;
    sptr = (char *)sendbuf + my_rank * sendcount * sendtype_sz;
    rptr = (char *)recvbuf + my_rank * recvcount * recvtype_sz;

#ifdef ONEWAY
    /* If ONEWAY is true we will post mpi_sendrecv comms instead
     * of irecv/send. */
    if((mpierr = MPI_Sendrecv(sptr, sendcount,sendtype,
                               my_rank, tag, rptr, recvcount, recvtype,
                               my_rank, tag, comm, &status))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
#else
    if((mpierr = MPI_Irecv(rptr, recvcount, recvtype,
                            my_rank, tag, comm, rcvids))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
    if((mpierr = MPI_Send(sptr, sendcount, sendtype,
                           my_rank, tag, comm))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }

    if((mpierr = MPI_Wait(rcvids, &status))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
#endif
  }

  LOG((2, "Done sending to self... sending to other procs"));

  /* When send to self is complete there is nothing left to do if
   * ntasks==1. */
  if(ntasks == 1){
    return PIO_NOERR;
  }

  for(int i = 0; i < ntasks; i++){
    rcvids[i] = MPI_REQUEST_NULL;
    swapids[i] = 0;
  }

  if(fc->isend){
    for (int i = 0; i < ntasks; i++){
      sndids[i] = MPI_REQUEST_NULL;
    }
  }

  if(fc->hs){
    for(int i = 0; i < ntasks; i++){
      hs_rcvids[i] = MPI_REQUEST_NULL;
    }
  }

  steps = 0;
  for(istep = 0; istep < SPIO_Util::Rearr_Util::ceil2(ntasks) - 1; istep++){
    p = SPIO_Util::Rearr_Util::pair(ntasks, istep, my_rank);
    if(p >= 0 && (sendcount > 0 || recvcount > 0)){
      swapids[steps++] = p;
    }
  }

  LOG((3, "steps=%d", steps));

  if(steps == 0){
    return PIO_NOERR;
  }

  if(steps == 1){
    maxreq = 1;
    maxreqh = 1;
  }
  else{
    if(fc->max_pend_req == PIO_REARR_COMM_UNLIMITED_PEND_REQ){
      maxreq = steps;
      maxreqh = steps;
    }
    else if(fc->max_pend_req > 1 && fc->max_pend_req < steps){
      maxreq = fc->max_pend_req;
      maxreqh = maxreq / 2;
    }
    else if(fc->max_pend_req == 1){
      /* Note that steps >= 2 here */
      maxreq = 2;
      maxreqh = 1;
    }
    else{
      maxreq = steps;
      maxreqh = steps;
    }
  }

  LOG((2, "fc->max_pend_req=%d, maxreq=%d, maxreqh=%d", fc->max_pend_req, maxreq, maxreqh));

  /* If handshaking is in use, do a nonblocking recieve to listen
   * for it. */
  if(fc->hs){
    for(istep = 0; istep < maxreq; istep++){
      p = swapids[istep];
      if(sendcount > 0){
        tag = my_rank + offset_t;
        if((mpierr = MPI_Irecv(&hs, 1, MPI_INT, p, tag, comm, hs_rcvids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
    }
  }

  /* Post up to maxreq irecv's. */
  for(istep = 0; istep < maxreq; istep++){
    p = swapids[istep];
    if(recvcount > 0){
      tag = p + offset_t;
      ptr = (char *)recvbuf + p * recvcount * recvtype_sz;

      if((mpierr = MPI_Irecv(ptr, recvcount, recvtype, p, tag, comm,
                              rcvids + istep))){
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
      }

      if(fc->hs){
        if((mpierr = MPI_Send(&hs, 1, MPI_INT, p, tag, comm))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
    }
  }

  /* Tell the paired task that this tasks' has posted it's irecvs'. */
  rstep = maxreq;
  for(istep = 0; istep < steps; istep++){
    p = swapids[istep];
    if(sendcount > 0){
      tag = my_rank + offset_t;
      /* If handshake is enabled don't post sends until the
       * receiving task has posted recvs. */
      if(fc->hs){
        if((mpierr = MPI_Wait(hs_rcvids + istep, &status))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
        hs_rcvids[istep] = MPI_REQUEST_NULL;
      }
      ptr = (char *)sendbuf + p * sendcount * sendtype_sz;

      /* On some software stacks MPI_Irsend() is either not available, not
       * a major issue anymore, or is buggy. With PIO1 we have found that
       * although the code correctly posts receives before the irsends,
       * on some systems (software stacks) the code hangs. However the
       * code works fine with isends. The _USE_MPI_RSEND macro should be
       * used to use mpi_irsends, the default is mpi_isend
       */
      if(fc->hs && fc->isend){
#ifndef _USE_MPI_RSEND
        if((mpierr = MPI_Isend(ptr, sendcount, sendtype, p, tag, comm,
                                sndids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
#else
        if((mpierr = MPI_Irsend(ptr, sendcount, sendtype, p, tag, comm,
                                 sndids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
#endif
      }
      else if(fc->isend){
        if((mpierr = MPI_Isend(ptr, sendcount, sendtype, p, tag, comm,
                                 sndids + istep))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
      else{
        if((mpierr = MPI_Send(ptr, sendcount, sendtype, p, tag, comm))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
      }
    }

    /* We did comms in sets of size max_reqs, if istep > maxreqh-1
     * then there is a remainder that must be handled. */
    if(istep > maxreqh - 1){
      p = istep - maxreqh;
      if(rcvids[p] != MPI_REQUEST_NULL){
        if((mpierr = MPI_Wait(rcvids + p, &status))){
          return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
        rcvids[p] = MPI_REQUEST_NULL;
      }
      if(rstep < steps){
        p = swapids[rstep];
        if(fc->hs && sendcount > 0){
          tag = my_rank + offset_t;
          if((mpierr = MPI_Irecv(&hs, 1, MPI_INT, p, tag, comm, hs_rcvids+rstep))){
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
          }
        }
        if(recvcount > 0){
          tag = p + offset_t;

          ptr = (char *)recvbuf + p * recvcount * recvtype_sz;
          if((mpierr = MPI_Irecv(ptr, recvcount, recvtype, p, tag, comm, rcvids + rstep))){
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
          }
          if(fc->hs){
            if((mpierr = MPI_Send(&hs, 1, MPI_INT, p, tag, comm))){
              return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            }
          }
        }
        rstep++;
      }
    }
  }

  /* If steps > 0 there could still be outstanding messages, wait for
   * them here. */
  if(steps > 0){
    LOG((2, "Waiting for outstanding msgs"));
    if((mpierr = MPI_Waitall(steps, rcvids, MPI_STATUSES_IGNORE))){
      return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }
    if(fc->isend){
      if((mpierr = MPI_Waitall(steps, sndids, MPI_STATUSES_IGNORE))){
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
      }
    }
  }

  return PIO_NOERR;
}
