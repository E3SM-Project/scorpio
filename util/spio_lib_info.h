#ifndef __SPIO_LIB_INFO_H__
#define __SPIO_LIB_INFO_H__

#include <string>
#include <vector>

#include "pio_config.h"
#include "pio.h"

namespace spio_tool_utils{
  namespace spio_lib_info{

    /* Get the library version */
    std::string get_lib_version(void );

    /* A summary of the Library (version, supported io types etc) */
    std::string get_lib_summary(void );

    /* Get the supported SCORPIO I/O types */
    void get_supported_iotypes(std::vector<PIO_IOTYPE> &supported_iotypes);

  } // namespace spio_lib_info
} // namespace spio_tool_utils

#endif // __SPIO_LIB_INFO_H__
