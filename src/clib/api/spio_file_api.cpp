#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"

/* ================= File APIs ================= */
int PIOc_deletefile(int iosysid, const char *filename)
{
  return PIOc_deletefile_impl(iosysid, filename);
}

int PIOc_createfile(int iosysid, int *ncidp, const int *iotype, const char *fname, int mode)
{
  return PIOc_createfile_impl(iosysid, ncidp, iotype, fname, mode);
}

int PIOc_create(int iosysid, const char *path, int cmode, int *ncidp)
{
  return PIOc_create_impl(iosysid, path, cmode, ncidp);
}

int PIOc_openfile(int iosysid, int *ncidp, int *iotype, const char *fname, int mode)
{
  return PIOc_openfile_impl(iosysid, ncidp, iotype, fname, mode);
}

int PIOc_openfile2(int iosysid, int *ncidp, int *iotype, const char *fname, int mode)
{
  return PIOc_openfile2_impl(iosysid, ncidp, iotype, fname, mode);
}

int PIOc_openfile_retry(int iosysid, int *ncidp, int *iotype,
                        const char *filename, int mode, int retry)
{
  return PIOc_openfile_retry_impl(iosysid, ncidp, iotype, filename, mode, retry);
}

int PIOc_open(int iosysid, const char *path, int mode, int *ncidp)
{
  return PIOc_open_impl(iosysid, path, mode, ncidp);
}

int PIOc_closefile(int ncid)
{
  return PIOc_closefile_impl(ncid);
}

int PIOc_File_is_Open(int ncid)
{
  return PIOc_File_is_Open_impl(ncid);
}

/* Set the error hanlding for a file. */
int PIOc_Set_File_Error_Handling(int ncid, int method)
{
  return PIOc_Set_File_Error_Handling_impl(ncid, method);
}

int PIOc_sync(int ncid)
{
  return PIOc_sync_impl(ncid);
}
