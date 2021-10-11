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
    std::string dropboxAppKey;
    std::string dropboxFilename;
    std::string dropboxRefreshToken;
    std::chrono::duration<float> resultsSavingPeriod = std::chrono::minutes(5);
  };

  Mask(const std::pair<int, int>& size, int id, const LoggingParameters& parameters);
  void findMinimalSets();

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}  // namespace TilingSystem

#endif /* Mask_hpp */
