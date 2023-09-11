      PROGRAM check_nc4_chunk_consts
        IMPLICIT NONE
        include 'netcdf.inc'
        INTEGER, PARAMETER :: PIO_CONTIGUOUS = NF_CONTIGUOUS
        INTEGER, PARAMETER :: PIO_CHUNKED = NF_CHUNKED
        INTEGER, PARAMETER :: PIO_COMPACT = NF_COMPACT
      END PROGRAM check_nc4_chunk_consts
