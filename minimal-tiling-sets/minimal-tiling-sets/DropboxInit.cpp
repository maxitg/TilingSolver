#include "DropboxInit.hpp"

#include <openssl/err.h>
#include <openssl/rand.h>
#include <openssl/sha.h>

#include <boost/algorithm/string.hpp>
#include <boost/archive/iterators/base64_from_binary.hpp>
#include <boost/archive/iterators/binary_from_base64.hpp>
#include <boost/archive/iterators/transform_width.hpp>
#include <fstream>
#include <nlohmann/json.hpp>

#define CPPHTTPLIB_OPENSSL_SUPPORT
#include "httplib.h"

namespace TilingSystem {
class DropboxInit::Implementation {
 private:
  const std::string dataDirectoryKey_ = "DropboxDataDirectory";
  const std::string refreshTokenKey_ = "DropboxRefreshToken";

  const std::string appKey_;
  const std::string configFilename_;

  std::string codeVerifier_;
  std::string authorizationCode_;
  std::string refreshToken_;
  std::string dataDirectory_;

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

  const std::string& refreshToken() { return refreshToken_; }
  const std::string& dataDirectory() { return dataDirectory_; }

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
      std::cerr << "Failed to get an access token from Dropbox.";
      throw Error::FailedToGetAccessToken;
    } else {
      nlohmann::json resultJSON = nlohmann::json::parse(result->body);
      refreshToken_ = resultJSON["refresh_token"];
    }
  }
};

DropboxInit::DropboxInit(const std::string& appKey, const std::string& configFilename)
    : implementation_(std::make_shared<Implementation>(appKey, configFilename)) {}

const std::string& DropboxInit::refreshToken() { return implementation_->refreshToken(); }
const std::string& DropboxInit::dataDirectory() { return implementation_->dataDirectory(); }
}  // namespace TilingSystem
