#ifndef _ADIOS2PIO_NM_LIB_C_H_
#define _ADIOS2PIO_NM_LIB_C_H_

#ifdef MPI_SERIAL
#if defined(__cplusplus)
extern "C" {
#endif
#endif

#include <mpi.h>

#ifdef MPI_SERIAL
#if defined(__cplusplus)
}
#endif
#endif

#ifdef __cplusplus
extern "C" {
#endif

extern int C_API_ConvertBPToNC(const char *infilepath, const char *outfilename, const char *piotype, int rearr_type, MPI_Comm comm_in);

#ifdef __cplusplus
}
#endif

#endif /* #ifndef _ADIOS2PIO_NM_LIB_C_H_ */
