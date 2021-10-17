#ifndef Mask_hpp
#define Mask_hpp

#include <chrono>
#include <iostream>
#include <memory>
#include <nlohmann/json.hpp>
#include <utility>
#include <vector>

#include "Dropbox.hpp"

namespace TilingSystem {
class Mask {
 public:
  struct LoggingParameters {
    std::function<void(const nlohmann::json&)> updateStatus = {};
    std::string filename;
    std::string statusFilename;
    std::chrono::duration<float> resultsSavingPeriod = std::chrono::seconds(308);
  };

  Mask(const std::pair<int, int>& size, int id, Dropbox& dropbox, const LoggingParameters& parameters);
  void findMinimalSets();
  void requestTermination();
  bool canBeSafelyTerminated();
  bool isDone();

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}  // namespace TilingSystem

#endif /* Mask_hpp */
