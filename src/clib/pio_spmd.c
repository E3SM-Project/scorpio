/**
 * @file
 * Algorithms modeled after spmd_utils in the Community
 * Atmosphere Model; C translation. This includes MPI_Gather,
 * MPI_Gatherv, and MPI_Alltoallw with flow control options.
 *
 * @author Jim Edwards
 * @date 2014
 */
#include <pio_config.h>
#include <pio.h>
#include <pio_internal.h>

/**
 * Returns the smallest power of 2 greater than
 * or equal to i.
 *
 * @param i input number
 * @returns the smallest power of 2 greater than
 * or equal to i.
 * @author Jim Edwards
 */
int ceil2(int i)
{
    int p = 1;
    while (p < i)
        p *= 2;

    return(p);
}

/**
 * Given integers p and k between 0 and np-1,
 * if (p+1)^k <= np-1 then return (p+1)^k else -1.
 *
 * @param np
 * @param p integer between 0 and np - 1.
 * @param k integer between 0 and np - 1.
 * @returns (p + 1) ^ k else -1.
 * @author Jim Edwards
 */
int pair(int np, int p, int k)
{
    int q = (p + 1) ^ k ;
    int pair = (q > np - 1) ? -1 : q;
    return pair;
}

/**
 * Provides the functionality of MPI_Alltoallw with flow control
 * options. Generalized all-to-all communication allowing different
 * datatypes, counts, and displacements for each partner
 *
 * @param sendbuf starting address of send buffer
 * @param sendcounts integer array equal to the number of tasks in
 * communicator comm (ntasks). It specifies the number of elements to
 * send to each processor
 * @param sdispls integer array (of length ntasks). Entry j
 * specifies the displacement in bytes (relative to sendbuf) from
 * which to take the outgoing data destined for process j.
 * @param sendtypes array of datatypes (of length ntasks). Entry j
 * specifies the type of data to send to process j.
 * @param recvbuf address of receive buffer.
 * @param recvcounts integer array (of length ntasks) specifying the
 * number of elements that can be received from each processor.
 * @param rdispls integer array (of length ntasks). Entry i
 * specifies the displacement in bytes (relative to recvbuf) at which
 * to place the incoming data from process i.
 * @param recvtypes array of datatypes (of length ntasks). Entry i
 * specifies the type of data received from process i.
 * @param rcvids array of requests corresponding to data recieved from
 * each rank. If NULL, the function internally allocates the array
 * @param nrcvids Size of the rcvids array
 * @param sndids array of requests corresponding to data sent to
 * each rank. If NULL, the function internally allocates the array
 * @param nsndids Size of the sndids array
 * @param comm MPI communicator for the MPI_Alltoallw call.
 * @param fc pointer to the struct that provided flow control options.
 * @returns 0 for success, error code otherwise.
 * @author Jim Edwards
 */
int pio_swapm(void *sendbuf, int *sendcounts, int *sdispls, MPI_Datatype *sendtypes,
              void *recvbuf, int *recvcounts, int *rdispls, MPI_Datatype *recvtypes,
              pio_swapm_req *ureq,
              MPI_Comm comm, rearr_comm_fc_opt_t *fc)
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
    bool ureq_from_usr = true;
    int mpierr;  /* Return code from MPI functions. */

#ifdef TIMING
    GPTLstart("PIO:pio_swapm");
#endif
    LOG((2, "pio_swapm fc->hs = %d fc->isend = %d fc->max_pend_req = %d", fc->hs,
         fc->isend, fc->max_pend_req));

    /* Get my rank and size of communicator. */
    if ((mpierr = MPI_Comm_size(comm, &ntasks)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    if ((mpierr = MPI_Comm_rank(comm, &my_rank)))
        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

    LOG((2, "ntasks = %d my_rank = %d", ntasks, my_rank));

    /* Now we know the size of these arrays. */
    int swapids[ntasks];
    MPI_Request hs_rcvids[ntasks];


    /* Print some debugging info, if logging is enabled. */
#if PIO_ENABLE_LOGGING
    {
        for (int p = 0; p < ntasks; p++)
            LOG((3, "sendcounts[%d] = %d sdispls[%d] = %d sendtypes[%d] = %d recvcounts[%d] = %d "
                 "rdispls[%d] = %d recvtypes[%d] = %d", p, sendcounts[p], p, sdispls[p], p,
                 sendtypes[p], p, recvcounts[p], p, rdispls[p], p, recvtypes[p]));
    }
#endif /* PIO_ENABLE_LOGGING */

    /* If fc->max_pend_req == 0 no throttling is requested and the default
     * mpi_alltoallw function is used. */
    if (fc->max_pend_req == 0)
    {
        /* Call the MPI alltoall without flow control. */
        LOG((3, "Calling MPI_Alltoallw without flow control."));
        if ((mpierr = MPI_Alltoallw(sendbuf, sendcounts, sdispls, sendtypes, recvbuf,
                                    recvcounts, rdispls, recvtypes, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
#ifdef TIMING
        GPTLstop("PIO:pio_swapm");
#endif
        return PIO_NOERR;
    }

    /* an index for communications tags */
    offset_t = ntasks;

    /* Send to self. */
    if (sendcounts[my_rank] > 0)
    {
        void *sptr, *rptr;
        MPI_Request rcvid;
        tag = my_rank + offset_t;
        sptr = (char *)sendbuf + sdispls[my_rank];
        rptr = (char *)recvbuf + rdispls[my_rank];

        /*
          MPI_Type_get_extent(sendtypes[my_rank], &lb, &extent);
          printf("%s %d %d %d\n",__FILE__,__LINE__,extent, lb);
          MPI_Type_get_extent(recvtypes[my_rank], &lb, &extent);
          printf("%s %d %d %d\n",__FILE__,__LINE__,extent, lb);
        */

#ifdef ONEWAY
        /* If ONEWAY is true we will post mpi_sendrecv comms instead
         * of irecv/send. */
        if ((mpierr = MPI_Sendrecv(sptr, sendcounts[my_rank],sendtypes[my_rank],
                                   my_rank, tag, rptr, recvcounts[my_rank], recvtypes[my_rank],
                                   my_rank, tag, comm, &status)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
#else
        if ((mpierr = MPI_Irecv(rptr, recvcounts[my_rank], recvtypes[my_rank],
                                my_rank, tag, comm, &rcvid)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Send(sptr, sendcounts[my_rank], sendtypes[my_rank],
                               my_rank, tag, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

        if ((mpierr = MPI_Wait(&rcvid, &status)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
#endif
    }

    LOG((2, "Done sending to self... sending to other procs"));

    /* When send to self is complete there is nothing left to do if
     * ntasks==1. */
    if (ntasks == 1)
    {
#ifdef TIMING
        GPTLstop("PIO:pio_swapm");
#endif
        return PIO_NOERR;
    }

    for (int i = 0; i < ntasks; i++)
    {
        swapids[i] = 0;
    }

    if (fc->hs)
        for (int i = 0; i < ntasks; i++)
            hs_rcvids[i] = MPI_REQUEST_NULL;

    steps = 0;
    for (istep = 0; istep < ceil2(ntasks) - 1; istep++)
    {
        p = pair(ntasks, istep, my_rank);
        if (p >= 0 && (sendcounts[p] > 0 || recvcounts[p] > 0))
            swapids[steps++] = p;
    }

    LOG((3, "steps=%d", steps));

    if (steps == 0)
    {
#ifdef TIMING
        GPTLstop("PIO:pio_swapm");
#endif
        return PIO_NOERR;
    }

    if (steps == 1)
    {
        maxreq = 1;
        maxreqh = 1;
    }
    else
    {
        if (fc->max_pend_req == PIO_REARR_COMM_UNLIMITED_PEND_REQ)
        {
            maxreq = steps;
            maxreqh = steps;
        }
        else if (fc->max_pend_req > 1 && fc->max_pend_req < steps)
        {
            maxreq = fc->max_pend_req;
            maxreqh = maxreq / 2;
        }
        else if (fc->max_pend_req == 1)
        {
            /* Note that steps >= 2 here */
            maxreq = 2;
            maxreqh = 1;
        }
        else
        {
            maxreq = steps;
            maxreqh = steps;
        }
    }

    LOG((2, "fc->max_pend_req=%d, maxreq=%d, maxreqh=%d", fc->max_pend_req, maxreq, maxreqh));

    if(ureq == NULL)
    {
        ureq_from_usr = false;
        ureq = (pio_swapm_req *)calloc(1, sizeof(pio_swapm_req));
        if(ureq == NULL)
        {
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Error exchanging data among processes. Unable to allocate memory (%lld bytes) for caching user requests", (unsigned long long) (sizeof(pio_swapm_req)));
        }
    }

    ureq->rcvids = (MPI_Request *)malloc(steps * sizeof(MPI_Request));
    if(ureq->rcvids == NULL)
    {
        return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                        "Error exchanging data among processes. Unable to allocate memory (%lld bytes) for caching MPI requests for recvs", (unsigned long long) (steps * sizeof(MPI_Request)));
    }
    for(int i=0; i<steps; i++)
    {
        ureq->rcvids[i] = MPI_REQUEST_NULL;
    }
    ureq->nrcvids = steps;
    if (fc->isend)
    {
        ureq->sndids = (MPI_Request *)malloc(steps * sizeof(MPI_Request));
        if(ureq->sndids == NULL)
        {
            return pio_err(NULL, NULL, PIO_ENOMEM, __FILE__, __LINE__,
                            "Error exchanging data among processes. Unable to allocate memory (%lld bytes) for caching MPI requests for sends", (unsigned long long) (steps * sizeof(MPI_Request)));
        }
        for (int i = 0; i < steps; i++)
        {
            ureq->sndids[i] = MPI_REQUEST_NULL;
        }
        ureq->nsndids = steps;
    }

    /* If handshaking is in use, do a nonblocking recieve to listen
     * for it. */
    if (fc->hs)
    {
        for (istep = 0; istep < maxreq; istep++)
        {
            p = swapids[istep];
            if (sendcounts[p] > 0)
            {
                tag = my_rank + offset_t;
                if ((mpierr = MPI_Irecv(&hs, 1, MPI_INT, p, tag, comm, hs_rcvids + istep)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            }
        }
    }

    /* Post up to maxreq irecv's. */
    for (istep = 0; istep < maxreq; istep++)
    {
        p = swapids[istep];
        if (recvcounts[p] > 0)
        {
            tag = p + offset_t;
            ptr = (char *)recvbuf + rdispls[p];

            if ((mpierr = MPI_Irecv(ptr, recvcounts[p], recvtypes[p], p, tag, comm,
                                    ureq->rcvids + istep)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

            if (fc->hs)
                if ((mpierr = MPI_Send(&hs, 1, MPI_INT, p, tag, comm)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
    }

    /* Tell the paired task that this tasks' has posted it's irecvs'. */
    rstep = maxreq;
    for (istep = 0; istep < steps; istep++)
    {
        p = swapids[istep];
        if (sendcounts[p] > 0)
        {
            tag = my_rank + offset_t;
            /* If handshake is enabled don't post sends until the
             * receiving task has posted recvs. */
            if (fc->hs)
            {
                if ((mpierr = MPI_Wait(hs_rcvids + istep, &status)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
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
            if (fc->hs && fc->isend)
            {
#ifndef _USE_MPI_RSEND
                if ((mpierr = MPI_Isend(ptr, sendcounts[p], sendtypes[p], p, tag, comm,
                                        ureq->sndids + istep)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
#else
                if ((mpierr = MPI_Irsend(ptr, sendcounts[p], sendtypes[p], p, tag, comm,
                                         ureq->sndids + istep)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
#endif
            }
            else if (fc->isend)
            {
                if ((mpierr = MPI_Isend(ptr, sendcounts[p], sendtypes[p], p, tag, comm,
                                         ureq->sndids + istep)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            }
            else
            {
                if ((mpierr = MPI_Send(ptr, sendcounts[p], sendtypes[p], p, tag, comm)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            }
        }

        /* We did comms in sets of size max_reqs, if istep > maxreqh-1
         * then there is a remainder that must be handled. */
        if (istep > maxreqh - 1)
        {
            p = istep - maxreqh;
            if (ureq->rcvids[p] != MPI_REQUEST_NULL)
            {
                if ((mpierr = MPI_Wait(ureq->rcvids + p, &status)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                ureq->rcvids[p] = MPI_REQUEST_NULL;
            }
            if (rstep < steps)
            {
                p = swapids[rstep];
                if (fc->hs && sendcounts[p] > 0)
                {
                    tag = my_rank + offset_t;
                    if ((mpierr = MPI_Irecv(&hs, 1, MPI_INT, p, tag, comm, hs_rcvids+rstep)))
                        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                }
                if (recvcounts[p] > 0)
                {
                    tag = p + offset_t;

                    ptr = (char *)recvbuf + rdispls[p];
                    if ((mpierr = MPI_Irecv(ptr, recvcounts[p], recvtypes[p], p, tag, comm, ureq->rcvids + rstep)))
                        return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                    if (fc->hs)
                        if ((mpierr = MPI_Send(&hs, 1, MPI_INT, p, tag, comm)))
                            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                }
                rstep++;
            }
        }
    }

    /* There could still be outstanding messages, wait for
     * them here. */
    /* But if user provided the request don't wait */
    if(!ureq_from_usr)
    {
        LOG((2, "Waiting for outstanding msgs"));
        if ((mpierr = MPI_Waitall(ureq->nrcvids, ureq->rcvids, MPI_STATUSES_IGNORE)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if (fc->isend)
            if ((mpierr = MPI_Waitall(ureq->nsndids, ureq->sndids, MPI_STATUSES_IGNORE)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        pio_swapm_req_free((void *) ureq);
    }

#ifdef TIMING
    GPTLstop("PIO:pio_swapm");
#endif
    return PIO_NOERR;
}

/**
 * Non-blocking wait on swapm request
 * @param p User swapm request (the request passed to pio_swapm() call)
 * @param flag Pointer to a flag that holds the status of the user request.
 * The flag is 0 if the request is pending and 1 if the request is completed
 * @returns PIO_NOERR on success, a pio error code otherwise
 * - Similar to MPI_Testall(), makes progress on all requests
 */
int pio_swapm_iwait(void *p, int *flag)
{
    pio_swapm_req *ureq = (pio_swapm_req *)p;
    int mpierr;

    assert((ureq != NULL) && (flag != NULL));
    *flag = 1;
    if(ureq->nrcvids != 0)
    {
        mpierr = MPI_Testall(ureq->nrcvids, ureq->rcvids, flag, MPI_STATUSES_IGNORE);
        if(mpierr != MPI_SUCCESS)
        {
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }

        if(*flag == 1)
        {
            free(ureq->rcvids);
            ureq->nrcvids = 0;
        }
    }
    if(ureq->nsndids != 0)
    {
        mpierr = MPI_Testall(ureq->nsndids, ureq->sndids, flag, MPI_STATUSES_IGNORE);
        if(mpierr != MPI_SUCCESS)
        {
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }

        if(*flag == 1)
        {
            free(ureq->sndids);
            ureq->nsndids = 0;
        }
    }
    return PIO_NOERR;
}

/**
 * Blocking wait on user swapm request
 * @param p User swapm request (the request passed to pio_swapm() call)
 * @returns PIO_NOERR on success, a pio error code otherwise
 * - Similar to MPI_Waitall(), waits for the request to complete
 */
int pio_swapm_wait(void *p)
{
    pio_swapm_req *ureq = (pio_swapm_req *)p;
    int mpierr;

    assert(ureq != NULL);
    if(ureq->nrcvids != 0)
    {
        mpierr = MPI_Waitall(ureq->nrcvids, ureq->rcvids, MPI_STATUSES_IGNORE);
        if(mpierr != MPI_SUCCESS)
        {
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }

        ureq->nrcvids = 0;
    }
    if(ureq->nsndids != 0)
    {
        mpierr = MPI_Waitall(ureq->nsndids, ureq->sndids, MPI_STATUSES_IGNORE);
        if(mpierr != MPI_SUCCESS)
        {
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }

        ureq->nsndids = 0;
    }
    return PIO_NOERR;
}

/**
 * Provides the functionality of MPI_Gatherv with flow control
 * options. This function is not currently used, but we hope it will
 * be useful in future optimizations.
 *
 * @param sendbuf starting address of send buffer.
 * @param sendcnt number of elements in send buffer.
 * @param sendtype data type of send buffer elements.
 * @param recvbuf address of receive buffer.
 * @param recvcnts integer array (of length group size) containing the
 * number of elements that are received from each process (significant
 * only at root).
 * @param displs integer array (of length group size). Entry i
 * specifies the displacement relative to recvbuf at which to place
 * the incoming data from process i (significant only at root).
 * @param recvtype data type of recv buffer elements (significant only
 * at root).
 * @param root rank of receiving process.
 * @param comm communicator.
 * @param flow_cntl if non-zero, flow control will be used.
 * @returns 0 for success, error code otherwise.
 * @author Jim Edwards
 */
int pio_fc_gatherv(const void *sendbuf, int sendcnt, MPI_Datatype sendtype,
                   void *recvbuf, const int *recvcnts, const int *displs,
                   MPI_Datatype recvtype, int root, MPI_Comm comm, int flow_cntl)
{
    bool fc_gather;
    int gather_block_size;
    int mytask, nprocs;
    int mtag;
    MPI_Status status;
    int hs;
    int dsize;
    int mpierr;  /* Return code from MPI functions. */

    if (flow_cntl > 0)
    {
        fc_gather = true;
        gather_block_size = min(flow_cntl, MAX_GATHER_BLOCK_SIZE);
    }
    else
    {
        fc_gather = false;
    }

    if (fc_gather)
    {
        if ((mpierr = MPI_Comm_rank(comm, &mytask)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        if ((mpierr = MPI_Comm_size(comm, &nprocs)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

        mtag = 2 * nprocs;
        hs = 1;

        if (mytask == root)
        {
            int preposts = min(nprocs-1, gather_block_size);
            int head = 0;
            int count = 0;
            int tail = 0;
            MPI_Request rcvid[gather_block_size];

            if ((mpierr = MPI_Type_size(recvtype, &dsize)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

            for (int p = 0; p < nprocs; p++)
            {
                if (p != root)
                {
                    if (recvcnts[p] > 0)
                    {
                        count++;
                        if (count > preposts)
                        {
                            if ((mpierr = MPI_Wait(rcvid + tail, &status)))
                                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                            tail = (tail + 1) % preposts;
                        }

                        void *ptr = (void *)((char *)recvbuf + dsize * displs[p]);

                        if ((mpierr = MPI_Irecv(ptr, recvcnts[p], recvtype, p, mtag, comm, rcvid + head)))
                            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                        head = (head + 1) % preposts;
                        if ((mpierr = MPI_Send(&hs, 1, MPI_INT, p, mtag, comm)))
                            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                    }
                }
            }

            /* copy local data */
            if ((mpierr = MPI_Type_size(sendtype, &dsize)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            if ((mpierr = MPI_Sendrecv(sendbuf, sendcnt, sendtype, mytask, 102, recvbuf, recvcnts[mytask],
                                       recvtype, mytask, 102, comm, &status)))
                return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);

            count = min(count, preposts);
            if (count > 0)
                if ((mpierr = MPI_Waitall(count, rcvid, MPI_STATUSES_IGNORE)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
        }
        else
        {
            if (sendcnt > 0)
            {
                if ((mpierr = MPI_Recv(&hs, 1, MPI_INT, root, mtag, comm, &status)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
                if ((mpierr = MPI_Send(sendbuf, sendcnt, sendtype, root, mtag, comm)))
                    return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
            }
        }
    }
    else
    {
        if ((mpierr = MPI_Gatherv(sendbuf, sendcnt, sendtype, recvbuf, recvcnts,
                                  displs, recvtype, root, comm)))
            return check_mpi(NULL, NULL, mpierr, __FILE__, __LINE__);
    }

    return PIO_NOERR;
}
