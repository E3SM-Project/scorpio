#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include "spio_tracer.hpp"

/* ================= File APIs ================= */
int PIOc_deletefile(int iosysid, const char *filename)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_deletefile");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*filename", filename).flush();
#endif
  return PIOc_deletefile_impl(iosysid, filename);
}

int PIOc_createfile(int iosysid, int *ncidp, const int *iotype, const char *fname, int mode)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_createfile");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*ncidp", ncidp).add_arg("*iotype", iotype).
    add_arg("*fname", fname).add_arg("mode", mode).flush();
#endif
  ret = PIOc_createfile_impl(iosysid, ncidp, iotype, fname, mode);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ncidp", ncidp);
#endif
  return ret;
}

int PIOc_create(int iosysid, const char *path, int cmode, int *ncidp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_create");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*path", path).add_arg("cmode", cmode).
    add_arg("*ncidp", ncidp).flush();
#endif
  ret = PIOc_create_impl(iosysid, path, cmode, ncidp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ncidp", ncidp);
#endif
  return ret;
}

int PIOc_openfile(int iosysid, int *ncidp, int *iotype, const char *fname, int mode)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_openfile");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*ncidp", ncidp).add_arg("*iotype", iotype).
    add_arg("*fname", fname).add_arg("mode", mode).flush();
#endif
  ret = PIOc_openfile_impl(iosysid, ncidp, iotype, fname, mode);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ncidp", ncidp).add_rval("*iotype", iotype);
#endif
  return ret;
}

int PIOc_openfile2(int iosysid, int *ncidp, int *iotype, const char *fname, int mode)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_openfile2");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*ncidp", ncidp).add_arg("*iotype", iotype).
    add_arg("*fname", fname).add_arg("mode", mode).flush();
#endif
  ret = PIOc_openfile2_impl(iosysid, ncidp, iotype, fname, mode);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ncidp", ncidp).add_rval("*iotype", iotype);
#endif
  return ret;
}

int PIOc_openfile_retry(int iosysid, int *ncidp, int *iotype,
                        const char *filename, int mode, int retry)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_openfile_retry");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*ncidp", ncidp).add_arg("*iotype", iotype).
    add_arg("*filename", filename).add_arg("mode", mode).add_arg("retry", retry).flush();
#endif
  ret = PIOc_openfile_retry_impl(iosysid, ncidp, iotype, filename, mode, retry);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ncidp", ncidp).add_rval("*iotype", iotype);
#endif
  return ret;
}

int PIOc_open(int iosysid, const char *path, int mode, int *ncidp)
{
  int ret = PIO_NOERR;
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_open");
  tr.set_iosys_id(iosysid).add_arg("iosysid", iosysid).
    add_arg("*path", path).add_arg("mode", mode).
    add_arg("*ncidp", ncidp).flush();
#endif
  ret = PIOc_open_impl(iosysid, path, mode, ncidp);

#if SPIO_ENABLE_API_TRACING
  tr.add_rval("*ncidp", ncidp);
#endif
  return ret;
}

int PIOc_closefile(int ncid)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_closefile");
  tr.set_file_id(ncid).add_arg("ncid", ncid).flush();
#endif
  return PIOc_closefile_impl(ncid);
}

int PIOc_File_is_Open(int ncid)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_File_is_Open");
  tr.set_file_id(ncid).add_arg("ncid", ncid).flush();
#endif
  return PIOc_File_is_Open_impl(ncid);
}

/* Set the error hanlding for a file. */
int PIOc_Set_File_Error_Handling(int ncid, int method)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_Set_File_Error_Handling");
  tr.set_file_id(ncid).add_arg("ncid", ncid).add_arg("method", method).flush();
#endif
  return PIOc_Set_File_Error_Handling_impl(ncid, method);
}

int PIOc_sync(int ncid)
{
#if SPIO_ENABLE_API_TRACING
  SPIO_Util::Tracer::Timed_func_call_tracer tr("PIOc_sync");
  tr.set_file_id(ncid).add_arg("ncid", ncid).flush();
#endif
  return PIOc_sync_impl(ncid);
}
