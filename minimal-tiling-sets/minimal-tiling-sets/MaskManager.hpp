#ifndef MaskManager_hpp
#define MaskManager_hpp

#include <chrono>
#include <iostream>
#include <string>

#include "Dropbox.hpp"

namespace TilingSystem {
class MaskManager {
 public:
  struct LoggingParameters {
    std::chrono::duration<float> resultsSavingPeriod = std::chrono::seconds(308);
  };

  MaskManager(Dropbox& dropbox, const LoggingParameters& parameters);
  void run(int threadCount);
  void requestTermination();
  bool canBeSafelyTerminated();

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}  // namespace TilingSystem

#endif /* MaskManager_hpp */
