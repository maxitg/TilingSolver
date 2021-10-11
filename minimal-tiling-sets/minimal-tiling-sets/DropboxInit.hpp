#ifndef DropboxInit_hpp
#define DropboxInit_hpp

#include <memory>
#include <string>

namespace TilingSystem {
class DropboxInit {
 public:
  enum class Error { OK, NoDataDirectory, RandomGenerationFailed, FailedToGetAccessToken };

  DropboxInit(const std::string& appKey, const std::string& configFilename);
  const std::string& refreshToken();
  const std::string& dataDirectory();

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}  // namespace TilingSystem

#endif /* DropboxInit_hpp */
