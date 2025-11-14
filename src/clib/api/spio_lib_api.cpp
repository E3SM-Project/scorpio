#include "pio_config.h"
#include "pio.h"
#include "pio_internal.h"
#include "pio_api_impl.h"
#include <string>

/* ========== APIs to get information about the library =========== */
int PIOc_get_version_major(void )
{
  return PIO_VERSION_MAJOR;
}

int PIOc_get_version_minor(void )
{
  return PIO_VERSION_MINOR;
}

int PIOc_get_version_patch(void )
{
  return PIO_VERSION_PATCH;
}

int PIOc_get_version_string(char *vstr, int len)
{
  if(vstr && (len > 0)){
    const std::string SEP = ".";
    std::string v = std::to_string(PIO_VERSION_MAJOR) + SEP +
                    std::to_string(PIO_VERSION_MINOR) + SEP +
                    std::to_string(PIO_VERSION_PATCH);
    strncpy(vstr, v.c_str(), len);
  }

  return PIO_NOERR;
}

int PIOc_get_version_hash(char *vstr, int len)
{
  if(vstr && (len > 0)){
    strncpy(vstr, PIO_VERSION_HASH, len);
  }

  return PIO_NOERR;
}

int PIOc_get_version(int *major, int *minor, int *patch, char *vstr, int vstr_len)
{
  if(major){
    *major = PIO_VERSION_MAJOR;
  }
  if(minor){
    *minor = PIO_VERSION_MINOR;
  }
  if(patch){
    *patch = PIO_VERSION_PATCH;
  }

  return PIOc_get_version_string(vstr, vstr_len);
}

