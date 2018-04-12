Introduction
-------------
In this document we discuss the design of the asynchronous write enhancement in PIO. Before discussing the asynchronous design a brief discussion on how PIO write works currently is included for comparison.

PIO write API
---------------
PIO has two variants of the write APIs,

1) PIO_put_var : This API is typically used by applications to write a scalar or 1D array (dimensions like lat, lon) to a NetCDF file. The data is assumed to be the same across all procs and PIO2 only writes the data out from the root I/O process. There are two forms of the API typically used in applications,

ret = PIO_put_var(pio_file, pio_var, pval);

and

ret = PIO_put_var(pio_file, pio_var, start, count, pval)

2) PIO_write_darray : This API is typically used by applications to write multidimensionalarrays distributed across multiple processes. The data decomposition (distribution) is specified by creating an io decomposition and passing it to the PIO_write_darray API.

call PIO_write_darray(pio_file, pio_var, iodesc, buf, ierr)

Existing support for asynchronous I/O (Asynchronous I/O service)
---------------------------------------
In the previous (before the asynchronous framework described below was introduced) version of PIO users could access asynchronous I/O as a service. This feature is still available in the current version of PIO. In asynchronous I/O as a service the total number of processes are split by the user into two disjoint set of processes, I/O processes and compute processes. The I/O processes wait in a loop for asynchronous I/O service messages from the compute processes and execute them. The compute processes create and send messages to the I/O processes when a PIO API is called, including all information pertaining to the function call (args etc).

Asynchronous I/O as a service is still available (not deprecated or removed) in the current version of PIO. However the asynchronous I/O as a service makes minimal use of the generic asynchronous framework described below (e.g. We currently do not use the generic asynchronous framework to send the messages corresponding to these asynchronous I/O as a service calls).

PIO write flow without asynchronous data rearrangement
-------------------------------------------------------
The section below briefly documents the PIO write flow without (before) the asynchronous data rearrangement enhancement.

PIO_put_var() : 
 For iotypes other than PnetCDF PIO uses a blocking write of data.
 For PnetCDF iotype PIO uses a buffered write, where the data written out is cached in PnetCDF, for multidimensional arrays (usually coordinate values like ncol, lat, lon). The write is considered complete when the request associated with the buffered write is complete. However, for scalar values a blocking write is used instead.

PIO_write_darray() :
 All data written out using PIO_write_darray is cached by PIO. The cached data is written out when either 
 the PIO internal buffer exceeds a pre-defined limit 
 or
 the user calls PIO_syncfile()/PIO_closefile() . 
 All variables/data (and multiple frames/records of these variables) written out using the same iodesc are cached in a list of "write multi buffers". When PIO runs out of cache space or when the user calls PIO_syncfile()/PIO_closefile() all pending variables/data to be written out for a single iodesc are rearranged to a buffer associated with the file. The buffer is then written out using a blocking write call for iotypes other than PnetCDF. For PnetCDF these buffers are written out using non-blocking write calls and the write is considered complete when the requests associated with the buffered write is complete.
  For the SUBSET rearranger the data to be written out has multiple contiguous regions (For the SUBSET rearranger data rearrangement is done between a continuous SUBSET of processes. So we end up with multiple contiguous regions of data to be written out. On the contrary, the BOX rearranger performs data rearrangement *potentially* between all processes, so that data in an io process is always contiguous, a single region). For NetCDF4P, data for multiple variables is written out one region at a time (The first region is written out for all variables in multiple NetCDF calls then the second region and so on). For PnetCDF, data for multiple regions is written out one variable at a time (All regions for a variable are written out in a single non-blocking pnetcdf call).
 We need to note that for all iotypes the data rearrangement of the data occurs when the data is ready to be written out.

Asynchronous I/O - general design
-----------------------------------
 There are several asynchronous operations in PIO. Currently these operations are blocking or have specific (custom) structures to keep track of the progress/completion. This enhancement introduces a generic framework for asynchronous operations in PIO.
 We have observed that data rearrangement in PIO is the significant cost when writing out model data using PnetCDF (and other iotypes). In this enhancement we asynchronously perform the data rearrangment while the data is buffered in PIO.

Asynchronous work items and work queues
-----------------------------------------
 Asynchronous work items represent asynchronous operations that complete sometime in future. The "pio_async_op" structure is used to represent a generic asynchronous work item. The structure allows users to store user-specific (specific to the asynchronous operation) data and wait on it. It also includes poke (check the progress of the asynchronous operation) and free (freeing the user data) functions that are customized by the user. This generic structure can be used to represent any asynchronous operation in PIO.
 An asynchronous work (e.g. non-blocking write of data) consists of multiple work items. The "pio_async_op" can be used to create queues that represent these different work items. The "file_desc_t" structure that keeps track of a file contains the work queue, "async_pend_ops", for all asynchronous operations associated with the file.

Asynchronous wait functions
-----------------------------
 The generic "pio_async_op" contains a wait function for that specific asynchronous work item. The framework introduces a PIO file-specific function, pio_file_async_pend_ops_wait(), that waits on all asynchronous operations queued in a work queue associated with the file.
 Sometimes it is convenient or beneficial for performance to combine multiple work items of a specific kind/type. The framework introduces a generic wait function, pio_file_async_pend_ops_kwait(), to wait on multiple work items of a specific kind in a work queue.

PIO write flow with asynchronous data rearrangement
----------------------------------------------------
 The data written out using PIO_write_darray() is buffered as before but data rearrangement is started asynchronously. When the data is ready to be written, rearrangments are ensured to be complete by waiting on pending asynchronous rearrangement ops and written out using non-blocking (PnetCDF) or blocking (non-PnetCDF) low level I/O library APIs. 
  For PnetCDF an asynchronous operation is now added for each non-blocking write (For non PnetCDF iotypes writes are blocking and hence don't need an associated asynchronous op). To ensure that the write is complete, the new code includes waits on these pending asynchronous operations.
  pioc_support.c currently contains all the util functions to queue, dequeue/remove and wait on asynchronous work items associated with a file. 
  To add an asynchronous item corresponding to data rearrangment for example,

* An asynchronous work item of type pio_async_op_t is created. This is a generic asynchronous work item.
  - The user data and the wait function is set by PIO.
  - The user data in this asynchronous work item is a structure containing MPI requests
    corresponding to pending MPI isends and irecvs corresponding to data rearrangment.
  - The wait function for this work item is passed the user data and waits on the
    individual MPI requests in the user data. 

  Note that the asynchronous work item of type, pio_async_op_t, can be used to represent another (other than data rearrangment for example) asynchronous work item by setting the user_data and wait/poke functions accordingly
