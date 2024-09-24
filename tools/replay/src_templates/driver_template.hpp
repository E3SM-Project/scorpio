#ifndef __SPIO_REPLAY_DRIVER_HPP_INLCUDED__
#define __SPIO_REPLAY_DRIVER_HPP_INLCUDED__

#include <functional>

__IOSYS_HEADER_INCLUDES__

namespace spio_replay_driver{

class iosys{
public:
  std::function<int(void)> init;
  std::function<int(void)> finalize;
  std::function<void(void)> run;
  int phase;
  std::string info;
};

} // namespace spio_replay_driver

#endif //__SPIO_REPLAY_DRIVER_HPP_INLCUDED__
