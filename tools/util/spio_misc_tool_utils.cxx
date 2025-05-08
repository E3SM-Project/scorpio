#include "spio_misc_tool_utils.h"
#include <stdexcept>
#include <stack>
#include <iostream>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <dirent.h>

namespace spio_tool_utils{

  std::string iotype_to_string(PIO_IOTYPE iotype)
  {
    switch(iotype){
      case PIO_IOTYPE_PNETCDF:
            return "PIO_IOTYPE_PNETCDF";
      case PIO_IOTYPE_NETCDF:
            return "PIO_IOTYPE_NETCDF";
      case PIO_IOTYPE_NETCDF4C:
            return "PIO_IOTYPE_NETCDF4C";
      case PIO_IOTYPE_NETCDF4P:
            return "PIO_IOTYPE_NETCDF4P";
      case PIO_IOTYPE_ADIOS:
            return "PIO_IOTYPE_ADIOS";
      default:
            return "UNKNOWN";
    }
  }

  bool gsuccess(MPI_Comm comm, int lspio_err)
  {
    bool lsucc = (lspio_err == PIO_NOERR) ? true : false;
    bool gsucc = lsucc;
    int mpierr = MPI_SUCCESS;

    mpierr = MPI_Allreduce(&lsucc, &gsucc, 1, MPI_C_BOOL, MPI_LAND, comm);
    if(mpierr != MPI_SUCCESS){
      throw std::runtime_error("MPI_Allreduce failed while trying to determine when a function was successful or not");
    }

    return gsucc;
  }

  void rmdir_f(const std::string &dname)
  {
    struct Dir_info{
      std::string dname;
      DIR *dstream;
    };
    std::stack<Dir_info> dinfos;

    /* Add directory stream of root dir to the stack */
    DIR *ds = opendir(dname.c_str());
    if(ds){
      dinfos.push({dname, ds});
    }

    /* Recursively process directory streams from the stack */
    while(!dinfos.empty()){
      Dir_info dinfo = dinfos.top();
      dinfos.pop();

      struct dirent *dir_entry;
      do{
        dir_entry = readdir(dinfo.dstream);
        if(dir_entry){
          std::string dname = std::string(dir_entry->d_name);
          std::string fdname = dinfo.dname + "/" + std::string(dir_entry->d_name);

          /* Ignore processing "." & ".." directories */
          if((dname == ".") || (dname == "..")) continue;

          struct stat st;
          if(!lstat(fdname.c_str(), &st)){
            if(S_ISLNK(st.st_mode)){
              /* Delete the symbolic link - don't follow symbolic links */
              unlink(fdname.c_str());
            }
            else if(S_ISDIR(st.st_mode)){
              /* Push details of the parent directory in stack and process this directory */
              dinfos.push(dinfo);

              dinfo.dname = fdname;
              dinfo.dstream = opendir(fdname.c_str());
              /* FIXME: Do we need to signal this error back to the caller */
              if(!dinfo.dstream) break;

              continue;
            }
            else{
              /* Delete the file */
              unlink(fdname.c_str());
            }
          }
        }
      }while(dir_entry != NULL);

      closedir(dinfo.dstream);
      rmdir(dinfo.dname.c_str());
    }
  }

} // namespace spio_tool_utils
