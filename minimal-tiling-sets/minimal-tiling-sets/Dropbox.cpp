#include "Dropbox.hpp"

#include <openssl/err.h>
#include <openssl/rand.h>
#include <openssl/sha.h>

#include <boost/algorithm/string.hpp>
#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/binary_from_base64.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <fstream>
#include <mutex>
#include <nlohmann/json.hpp>

#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "httplib.h"

// TODO: do rate limiting on Dropbox

namespace TilingSystem {
class Dropbox::Implementation {
 private:
  const std::string dataDirectoryKey_ = "DropboxDataDirectory";
  const std::string refreshTokenKey_ = "DropboxRefreshToken";

  const std::string appKey_;
  const std::string configFilename_;

  std::string codeVerifier_;
  std::string authorizationCode_;
  std::string refreshToken_;
  std::string dataDirectory_;

  std::mutex accessTokenMutex_;
  std::string accessToken_;
  std::chrono::time_point<std::chrono::steady_clock> accessTokenExpiration_ =
      std::chrono::time_point<std::chrono::steady_clock>::min();

 public:
  Implementation(const std::string& appKey, const std::string& configFilename)
      : appKey_(appKey), configFilename_(configFilename) {
    readConfig();
    if (refreshToken_.empty()) {
      generateCodeVerifier();
      requestAuthorizationCode();
      requestRefreshToken();
      writeConfig();
    }
  }

  bool lockFile(const std::string& filename, const std::function<void(const std::string&)>& logError) {
    auto accessToken = getAccessToken(logError);
    if (!accessToken) return false;
    auto result = httplib::Client("https://content.dropboxapi.com")
                      .Post("/2/files/upload",
                            {{"Authorization", "Bearer " + accessToken.value()},
                             {"Dropbox-API-Arg",
                              nlohmann::json({{"path", dataDirectory_ + "/" + filename + ".lock"},
                                              {"mode", "add"},
                                              {"autorename", false},
                                              {"mute", true},
                                              {"strict_conflict", true}})
                                  .dump()}},
                            "",
                            "text/plain; charset=dropbox-cors-hack");
    if (result->status != 200) {
      logError("Failed to lock the data file.");
      logError(result->body);
      return false;
    } else {
      return true;
    }
  }

  bool unlockFile(const std::string& filename, const std::function<void(const std::string&)>& logError) {
    auto accessToken = getAccessToken(logError);
    if (!accessToken) return false;
    auto result = httplib::Client("https://api.dropboxapi.com")
                      .Post("/2/files/delete_v2",
                            {{"Authorization", "Bearer " + accessToken.value()}},
                            nlohmann::json({{"path", dataDirectory_ + "/" + filename + ".lock"}}).dump(),
                            "application/json");
    if (result->status != 200) {
      logError("Failed to unlock the data file.");
      logError(result->body);
      return false;
    } else {
      return true;
    }
  }

  std::optional<nlohmann::json> downloadJSON(const std::string& filename,
                                             const nlohmann::json& defaultContents,
                                             const std::function<void(const std::string&)>& logError) {
    auto accessToken = getAccessToken(logError);
    if (!accessToken) return false;
    auto result = httplib::Client("https://content.dropboxapi.com")
                      .Post("/2/files/download",
                            {{"Authorization", "Bearer " + accessToken.value()},
                             {"Dropbox-API-Arg", nlohmann::json({{"path", dataDirectory_ + "/" + filename}}).dump()}},
                            "",
                            "text/plain");
    if (result->status != 200) {
      auto json = nlohmann::json::parse(result->body);
      if (json.is_object() && json.count("error") && json["error"].count("path") &&
          json["error"]["path"].count(".tag") && json["error"]["path"][".tag"].is_string() &&
          json["error"]["path"][".tag"] == "not_found") {
        return defaultContents;
      } else {
        logError("Failed to download existing data from Dropbox.");
        logError(result->body);
        return std::nullopt;
      }
    } else {
      return nlohmann::json::parse(result->body);
    }
  }

  bool uploadJSON(const std::string& filename,
                  const nlohmann::json& json,
                  const std::function<void(const std::string&)>& logError) {
    auto accessToken = getAccessToken(logError);
    if (!accessToken) return false;
    auto result =
        httplib::Client("https://content.dropboxapi.com")
            .Post("/2/files/upload",
                  {{"Authorization", "Bearer " + accessToken.value()},
                   {"Dropbox-API-Arg",
                    nlohmann::json(
                        {{"path", dataDirectory_ + "/" + filename}, {"mode", "overwrite"}, {"autorename", false}})
                        .dump()}},
                  json.dump(2),
                  "text/plain; charset=dropbox-cors-hack");
    if (result->status != 200) {
      logError("Failed to upload data to Dropbox.");
      logError(result->body);
      return false;
    } else {
      return true;
    }
    return false;
  }

 private:
  void readConfig() {
    std::ifstream file(configFilename_);
    nlohmann::json configData;
    if (file.is_open()) file >> configData;
    if (!file.is_open() || !configData.count(dataDirectoryKey_)) {
      std::cerr << "Config does not specify data directory. Create " + configFilename_ +
                       " with the following contents: "
                << std::endl;
      std::cerr << "{" << std::endl;
      std::cerr << "  \"" + dataDirectoryKey_ + "\":[___DirName___]," << std::endl;
      std::cerr << "}" << std::endl;
      throw Error::NoDataDirectory;
    } else {
      dataDirectory_ = configData[dataDirectoryKey_];
    }
    if (configData.count(refreshTokenKey_)) refreshToken_ = configData[refreshTokenKey_];
    file.close();
  }

  void writeConfig() {
    std::ofstream file(configFilename_);
    if (file.is_open()) {
      file << nlohmann::json({{dataDirectoryKey_, dataDirectory_}, {refreshTokenKey_, refreshToken_}}).dump(2);
    } else {
      std::cerr << "Could not write config to " + configFilename_ + ": " + std::string(std::strerror(errno));
      throw Error::FailedToWriteConfig;
    }
    return file.close();
  }

  void generateCodeVerifier() {
    const std::string possible = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-._~";
    while (codeVerifier_.length() < 128) {
      unsigned char randomChar;
      int rc = RAND_bytes(&randomChar, 1);
      if (rc != 1) {
        unsigned long error = ERR_get_error();
        char errorDescription[256];
        ERR_error_string(error, errorDescription);
        std::cerr << "Could not generate random bytes with OpenSLL: " + std::string(errorDescription) << std::endl;
        throw Error::RandomGenerationFailed;
      } else if (randomChar < 66) {
        codeVerifier_.push_back(possible[randomChar]);
      }
    }
  }

  void requestAuthorizationCode() {
    const std::string dropboxURL = "https://www.dropbox.com";
    httplib::Client dropboxHTTPClient = httplib::Client(dropboxURL);
    std::cout << "Go to: " << dropboxURL << "/oauth2/authorize?client_id=" << appKey_
              << "&response_type=code&code_challenge=" << codeChallenge()
              << "&code_challenge_method=S256&token_access_type=offline" << std::endl;
    std::cout << "Access code: ";
    std::cin >> authorizationCode_;
  }

  std::string codeChallenge() {
    unsigned char codeVerifierSHA[SHA256_DIGEST_LENGTH];
    std::vector<unsigned char> codeVerifierUnsignedChars(codeVerifier_.begin(), codeVerifier_.end());
    SHA256(codeVerifierUnsignedChars.data(), codeVerifierUnsignedChars.size(), codeVerifierSHA);
    return base64Encode(std::string(codeVerifierSHA, codeVerifierSHA + SHA256_DIGEST_LENGTH));
  }

  std::string base64Encode(const std::string& raw) {
    using Base64Iterator = boost::archive::iterators::base64_from_binary<
        boost::archive::iterators::transform_width<std::string::const_iterator, 6, 8>>;
    auto encoded = std::string(Base64Iterator(std::begin(raw)), Base64Iterator(std::end(raw)));
    for (auto& encodedChar : encoded) {
      if (encodedChar == '+') {
        encodedChar = '-';
      } else if (encodedChar == '/') {
        encodedChar = '_';
      }
    }
    return encoded;
  }

  void requestRefreshToken() {
    auto result = httplib::Client("https://api.dropboxapi.com")
                      .Post("/oauth2/token",
                            httplib::MultipartFormDataItems{{"code", authorizationCode_},
                                                            {"grant_type", "authorization_code"},
                                                            {"code_verifier", codeVerifier_},
                                                            {"client_id", appKey_}});
    if (result->status != 200) {
      std::cerr << "Failed to get a refresh token from Dropbox.";
      throw Error::FailedToGetAccessToken;
    } else {
      nlohmann::json resultJSON = nlohmann::json::parse(result->body);
      refreshToken_ = resultJSON["refresh_token"];
      accessToken_ = resultJSON["access_token"];
      accessTokenExpiration_ = std::chrono::steady_clock::now() + std::chrono::seconds(resultJSON["expires_in"]);
    }
  }

  std::optional<std::string> getAccessToken(const std::function<void(const std::string&)>& logError) {
    std::lock_guard<std::mutex> accessTokenLock(accessTokenMutex_);
    if (std::chrono::steady_clock::now() < accessTokenExpiration_) return accessToken_;
    auto result =
        httplib::Client("https://api.dropboxapi.com")
            .Post("/oauth2/token",
                  httplib::MultipartFormDataItems{
                      {"grant_type", "refresh_token"}, {"refresh_token", refreshToken_}, {"client_id", appKey_}});
    if (result->status != 200) {
      logError("Failed to get an access token from Dropbox.");
      logError(result->body);
      return std::nullopt;
    } else {
      nlohmann::json resultJSON = nlohmann::json::parse(result->body);
      accessToken_ = resultJSON["access_token"];
      accessTokenExpiration_ = std::chrono::steady_clock::now() + std::chrono::seconds(resultJSON["expires_in"]);
      return accessToken_;
    }
  }
};

Dropbox::Dropbox(const std::string& appKey, const std::string& configFilename)
    : implementation_(std::make_shared<Implementation>(appKey, configFilename)) {}

bool Dropbox::lockFile(const std::string& filename, const std::function<void(const std::string&)>& logError) {
  return implementation_->lockFile(filename, logError);
}

bool Dropbox::unlockFile(const std::string& filename, const std::function<void(const std::string&)>& logError) {
  return implementation_->unlockFile(filename, logError);
}

std::optional<nlohmann::json> Dropbox::downloadJSON(const std::string& filename,
                                                    const nlohmann::json& defaultContents,
                                                    const std::function<void(const std::string&)>& logError) {
  return implementation_->downloadJSON(filename, defaultContents, logError);
}

bool Dropbox::uploadJSON(const std::string& filename,
                         const nlohmann::json& json,
                         const std::function<void(const std::string&)>& logError) {
  return implementation_->uploadJSON(filename, json, logError);
}
}  // namespace TilingSystem
