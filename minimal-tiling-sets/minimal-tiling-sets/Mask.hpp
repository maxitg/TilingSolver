#ifndef Mask_hpp
#define Mask_hpp

#include <chrono>
#include <iostream>
#include <memory>
#include <utility>
#include <vector>

namespace TilingSystem {
class Mask {
 public:
  struct LoggingParameters {
    std::ostream* progressStream = &std::cout;
    std::chrono::duration<float> progressLoggingPeriod = std::chrono::seconds(0);
    std::string filename;
    std::chrono::duration<float> resultsSavingPeriod = std::chrono::minutes(1);
    LoggingParameters(const std::string& filename) : filename(filename) {}
  };

  Mask(const std::pair<int, int>& size, int id, const std::string& filename);
  Mask(const std::pair<int, int>& size, int id, const LoggingParameters& parameters);
  void findMinimalSets(int maxGridSize);

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}  // namespace TilingSystem

#endif /* Mask_hpp */
