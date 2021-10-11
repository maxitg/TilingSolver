#ifndef DropboxInit_hpp
#define DropboxInit_hpp

#include <memory>
#include <string>

namespace TilingSystem {
class DropboxInit {
 public:
  DropboxInit(const std::string& appKey, const std::string& configFilename);
  const std::string& codeVerifier();
  const std::string& authorizationCode();
  const std::string& dataDirectory();

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}  // namespace TilingSystem

#endif /* DropboxInit_hpp */
