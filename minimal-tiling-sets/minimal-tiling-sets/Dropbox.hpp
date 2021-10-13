#ifndef DropboxInit_hpp
#define DropboxInit_hpp

#include <memory>
#include <nlohmann/json.hpp>
#include <optional>
#include <string>

namespace TilingSystem {
class Dropbox {
 public:
  enum class Error { OK, NoDataDirectory, RandomGenerationFailed, FailedToGetAccessToken, FailedToWriteConfig };

  Dropbox(const std::string& appKey, const std::string& configFilename);
  bool lockFile(const std::string& filename, const std::function<void(const nlohmann::json&)>& logError);
  bool unlockFile(const std::string& filename, const std::function<void(const nlohmann::json&)>& logError);
  std::optional<nlohmann::json> downloadJSON(const std::string& filename,
                                             const nlohmann::json& defaultContents,
                                             const std::function<void(const nlohmann::json&)>& logError);
  bool uploadJSON(const std::string& filename,
                  const nlohmann::json& json,
                  const std::function<void(const nlohmann::json&)>& logError);

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};
}  // namespace TilingSystem

#endif /* DropboxInit_hpp */
