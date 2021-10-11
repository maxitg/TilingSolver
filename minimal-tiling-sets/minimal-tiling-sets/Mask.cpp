#include "Mask.hpp"

#define CPPHTTPLIB_OPENSSL_SUPPORT
#include <cryptominisat5/cryptominisat.h>

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <nlohmann/json.hpp>
#include <optional>
#include <queue>
#include <sstream>
#include <thread>
#include <unordered_set>
#include <vector>

#include "httplib.h"

namespace TilingSystem {
using PatternSet = std::vector<std::vector<std::vector<int>>>;
void hashCombine(size_t* seed, int value) {
  (*seed) ^= std::hash<int>()(value) + 0x9e3779b9 + ((*seed) << 6) + ((*seed) >> 2);
}

struct PatternSetHash {
  size_t operator()(const PatternSet& patternSet) const {
    size_t seed = 0;
    hashCombine(&seed, static_cast<int>(patternSet.size()));
    for (const auto& pattern : patternSet) {
      hashCombine(&seed, static_cast<int>(pattern.size()));
      for (const auto& row : pattern) {
        hashCombine(&seed, static_cast<int>(row.size()));
        for (const auto cell : row) {
          hashCombine(&seed, cell);
        }
      }
    }
    return seed;
  }
};

class Mask::Implementation {
 private:
  std::pair<int, int> maskSize_;
  int maskID_;
  LoggingParameters loggingParameters_;

  std::chrono::time_point<std::chrono::steady_clock> lastProgressLogTime_ =
      std::chrono::time_point<std::chrono::steady_clock>::min();
  std::chrono::time_point<std::chrono::steady_clock> lastResultsSavingTime_ =
      std::chrono::time_point<std::chrono::steady_clock>::min();

  int patternCount_;
  CMSat::SATSolver solver_;
  int maxSetSize_ = 0;
  int maxPeriod_ = 0;
  std::vector<int> patternVariables_;
  std::vector<std::vector<int>> cellVariables_;
  std::vector<std::vector<int>> symmetries_;

  bool isDone_ = false;
  std::map<std::vector<bool>, int> periods_;
  std::map<std::vector<bool>, int> periodLowerBounds_;
  int minimalGridSize_ = 2;
  std::map<std::vector<bool>, int> maximalGridSizes_;
  std::string dropboxDataDirectory_;

  const int expectedDataVersion = 3;
  const std::chrono::seconds sleepBetweenLockTries_ = std::chrono::seconds(1);
  const std::chrono::seconds sleepBetweenFileDownloads_ = std::chrono::seconds(1);
  const std::chrono::seconds sleepBetweenMergeConflicts_ = std::chrono::minutes(1);
  const std::chrono::seconds sleepBetweenFileUploads_ = std::chrono::seconds(1);
  const std::chrono::seconds sleepBetweenUnlockTries_ = std::chrono::seconds(1);
  std::string accessToken_;
  std::chrono::time_point<std::chrono::steady_clock> accessTokenExpiration_ =
      std::chrono::time_point<std::chrono::steady_clock>::min();

 public:
  Implementation(const std::pair<int, int>& size, int id, const LoggingParameters& loggingParameters)
      : maskSize_(size), maskID_(id), loggingParameters_(loggingParameters), patternCount_(1 << bitCount(maskID_)) {
    solver_.set_num_threads(1);
    syncWithDropbox(SaveResultsPriority::Force);
    patternVariables_ = initPatternVariables(&solver_);
    cellVariables_ = initSpatialVariables(&solver_, minimalGridSize_, GridBoundary::Finite);
    initSpatialClauses(&solver_, std::nullopt, patternVariables_, cellVariables_, GridBoundary::Finite);
    for (const auto& [minimalSet, period] : periods_) {
      int size = setSize(minimalSet);
      maxSetSize_ = std::max(maxSetSize_, size);
      maxPeriod_ = std::max(maxPeriod_, static_cast<int>(period));
      forbidMinimalSet(minimalSet);
    }
    if (!periodLowerBounds_.empty()) {
      printWithTimeAndMask(std::to_string(periodLowerBounds_.size()) + " sets have no periods.");
    }
    initSymmetries();
  }

  void findMinimalSets() { solve(); }

 private:
  enum class SaveResultsPriority { Normal, Force };

  void syncWithDropbox(const SaveResultsPriority& priority) {
    if (priority != SaveResultsPriority::Force &&
        std::chrono::steady_clock::now() < lastResultsSavingTime_ + loggingParameters_.resultsSavingPeriod) {
      return;
    }
    printWithTimeAndMask("Syncing with Dropbox...");

    bool isMergeSuccessful = false;
    // All operations must succeed. Otherwise, data may be lost.
    while (!lockDropboxFile()) {
      std::this_thread::sleep_for(sleepBetweenLockTries_);
      printWithTimeAndMask("Retrying lock...");
    }
    std::optional<nlohmann::json> json;
    while (!isMergeSuccessful) {
      while (!(json = jsonFromDropbox())) {
        std::this_thread::sleep_for(sleepBetweenFileDownloads_);
        printWithTimeAndMask("Retrying download...");
      }
      if (!mergeData(&json.value())) {
        printWithTimeAndMask(
            "Waiting " +
            std::to_string(std::chrono::duration_cast<std::chrono::seconds>(sleepBetweenMergeConflicts_).count()) +
            " seconds for the user to fix the conflict.");
        std::this_thread::sleep_for(sleepBetweenMergeConflicts_);
      } else {
        isMergeSuccessful = true;
      }
    }
    while (!writeToDropbox(json.value())) {
      std::this_thread::sleep_for(sleepBetweenFileUploads_);
      printWithTimeAndMask("Retrying upload...");
    }
    while (!unlockDropboxFile()) {
      std::this_thread::sleep_for(sleepBetweenUnlockTries_);
      printWithTimeAndMask("Retrying unlock...");
    }

    lastResultsSavingTime_ = std::chrono::steady_clock::now();
    printWithTimeAndMask("Done.");
  }

  bool lockDropboxFile() {
    std::string accessToken = getAccessToken();
    if (accessToken.empty()) return false;
    auto result = httplib::Client("https://content.dropboxapi.com")
                      .Post("/2/files/upload",
                            {{"Authorization", "Bearer " + accessToken},
                             {"Dropbox-API-Arg",
                              nlohmann::json({{"path", loggingParameters_.dropboxFilename + ".lock"},
                                              {"mode", "add"},
                                              {"autorename", false},
                                              {"mute", true},
                                              {"strict_conflict", true}})
                                  .dump()}},
                            "",
                            "text/plain; charset=dropbox-cors-hack");
    if (result->status != 200) {
      printWithTimeAndMask("Failed to lock the data file.");
      printWithTimeAndMask(result->body);
      return false;
    } else {
      return true;
    }
  }

  bool unlockDropboxFile() {
    std::string accessToken = getAccessToken();
    if (accessToken.empty()) return false;
    auto result = httplib::Client("https://api.dropboxapi.com")
                      .Post("/2/files/delete_v2",
                            {{"Authorization", "Bearer " + accessToken}},
                            nlohmann::json({{"path", loggingParameters_.dropboxFilename + ".lock"}}).dump(),
                            "application/json");
    if (result->status != 200) {
      printWithTimeAndMask("Failed to unlock the data file.");
      printWithTimeAndMask(result->body);
      return false;
    } else {
      return true;
    }
  }

  std::optional<nlohmann::json> jsonFromDropbox() {
    std::string accessToken = getAccessToken();
    if (accessToken.empty()) return false;
    auto result =
        httplib::Client("https://content.dropboxapi.com")
            .Post("/2/files/download",
                  {{"Authorization", "Bearer " + accessToken},
                   {"Dropbox-API-Arg", nlohmann::json({{"path", loggingParameters_.dropboxFilename}}).dump()}},
                  "",
                  "text/plain");
    if (result->status != 200) {
      auto json = nlohmann::json::parse(result->body);
      if (json.is_object() && json.count("error") && json["error"].count("path") &&
          json["error"]["path"].count(".tag") && json["error"]["path"][".tag"].is_string() &&
          json["error"]["path"][".tag"] == "not_found") {
        printWithTimeAndMask("Creating a new data file on Dropbox.");
        return nlohmann::json({{"Version", expectedDataVersion}});
      } else {
        printWithTimeAndMask("Failed to download existing data from Dropbox.");
        printWithTimeAndMask(result->body);
        return std::nullopt;
      }
    } else {
      return nlohmann::json::parse(result->body);
    }
  }

  bool writeToDropbox(const nlohmann::json& json) {
    std::string accessToken = getAccessToken();
    if (accessToken.empty()) return false;
    auto result =
        httplib::Client("https://content.dropboxapi.com")
            .Post("/2/files/upload",
                  {{"Authorization", "Bearer " + accessToken},
                   {"Dropbox-API-Arg",
                    nlohmann::json(
                        {{"path", loggingParameters_.dropboxFilename}, {"mode", "overwrite"}, {"autorename", false}})
                        .dump()}},
                  json.dump(2),
                  "text/plain; charset=dropbox-cors-hack");
    if (result->status != 200) {
      printWithTimeAndMask("Failed to upload data to Dropbox.");
      printWithTimeAndMask(result->body);
      return false;
    } else {
      return true;
    }
  }

  std::string getAccessToken() {
    if (std::chrono::steady_clock::now() < accessTokenExpiration_) return accessToken_;
    auto result = httplib::Client("https://api.dropboxapi.com")
                      .Post("/oauth2/token",
                            httplib::MultipartFormDataItems{{"grant_type", "refresh_token"},
                                                            {"refresh_token", loggingParameters_.dropboxRefreshToken},
                                                            {"client_id", loggingParameters_.dropboxAppKey}});
    if (result->status != 200) {
      printWithTimeAndMask("Failed to get an access token from Dropbox. Skipping synchronization: " + result->body);
      return "";
    } else {
      nlohmann::json resultJSON = nlohmann::json::parse(result->body);
      accessToken_ = resultJSON["access_token"];
      accessTokenExpiration_ = std::chrono::steady_clock::now() + std::chrono::seconds(resultJSON["expires_in"]);
      return accessToken_;
    }
  }

  bool mergeData(nlohmann::json* data) {
    const std::string versionKey = "Version";
    const std::string isDoneKey = "IsDone";
    const std::string periodsKey = "Periods";
    // These are tileable but not periodic up to specified sizes (this will usually be a single currently-processed set)
    const std::string periodLowerBoundsKey = "PeriodLowerBounds";
    const std::string minimalGridSizeKey = "MinimalGridSize";
    // Maximal grid size a particular set can tile. This will only be recorded when the grid needs to be enlarged.
    const std::string maximalGridSizesKey = "MaximalGridSizes";

    if (!data->count(versionKey) || (*data)[versionKey] != expectedDataVersion) {
      printSynchronizationError("Dropbox data is not of the expected version " + std::to_string(expectedDataVersion) +
                                ".");
      return false;
    }

    if (data->count(isDoneKey) && (*data)[isDoneKey].is_boolean()) {
      isDone_ = isDone_ || (*data)[isDoneKey];
    } else if (data->count(isDoneKey)) {
      printSynchronizationError(isDoneKey + " key should be a boolean in");
      return false;
    }
    (*data)[isDoneKey] = isDone_;

    if (!syncSetMap(
            *data,
            periodsKey,
            &periods_,
            "Period",
            [](const std::vector<bool>& set) { return true; },
            [this](const std::string& setDescription, int jsonValue, int localValue, int* result) {
              printSynchronizationError("Local period for " + setDescription + " is different from the one in");
              return false;
            })) {
      return false;
    }
    (*data)[periodsKey] = jsonFromMap(periods_);

    if (!syncSetMap(
            *data,
            periodLowerBoundsKey,
            &periodLowerBounds_,
            "Period lower bound",
            [this](const std::vector<bool>& set) { return !periods_.count(set); },
            [this](const std::string& setDescription, int jsonValue, int localValue, int* result) {
              *result = std::max(jsonValue, localValue);
              return true;
            })) {
      return false;
    }
    (*data)[periodLowerBoundsKey] = jsonFromMap(periodLowerBounds_);

    if (data->count(minimalGridSizeKey) && (*data)[minimalGridSizeKey].is_number_integer() &&
        (*data)[minimalGridSizeKey] > 0) {
      minimalGridSize_ = std::max(minimalGridSize_, static_cast<int>((*data)[minimalGridSizeKey]));
    } else if (data->count(minimalGridSizeKey)) {
      printSynchronizationError(minimalGridSizeKey + " key should be a positive integer in");
      return false;
    }
    (*data)[minimalGridSizeKey] = minimalGridSize_;

    if (!syncSetMap(
            *data,
            maximalGridSizesKey,
            &maximalGridSizes_,
            "Maximal grid size",
            [](const std::vector<bool>& set) { return true; },
            [this](const std::string& setDescription, int jsonValue, int localValue, int* result) {
              printSynchronizationError("Maximal grid size for " + setDescription + " is different from the one in");
              return false;
            })) {
      return false;
    }
    (*data)[maximalGridSizesKey] = jsonFromMap(maximalGridSizes_);

    return true;
  }

  nlohmann::json jsonFromMap(const std::map<std::vector<bool>, int>& map) {
    nlohmann::json result = nlohmann::json::object();
    for (const auto& [set, value] : map) {
      result[setDescription(set)] = value;
    }
    return result;
  }

  bool syncSetMap(
      const nlohmann::json& json,
      const std::string& jsonKey,
      std::map<std::vector<bool>, int>* localData,
      const std::string& dataName,
      const std::function<bool(const std::vector<bool>& set)>& shouldAddNewSet,
      const std::function<bool(const std::string& setDescription, int jsonValue, int localValue, int* result)>&
          resolveConflict) {
    if (json.count(jsonKey) && json[jsonKey].is_object()) {
      for (auto it = json[jsonKey].begin(); it != json[jsonKey].end(); ++it) {
        std::vector<bool> set;
        try {
          set = fromSetDescription(it.key());
        } catch (...) {
          return false;
        }
        if (!it.value().is_number_integer() || it.value() < 0) {
          printSynchronizationError(dataName + " of " + it.key() + " is not a non-negative integer in");
          return false;
        }

        if (localData->count(set) && localData->at(set) != it.value()) {
          int result;
          if (!resolveConflict(it.key(), it.value(), localData->at(set), &result)) {
            return false;
          }
          (*localData)[set] = result;
        }
        if (!localData->count(set) && shouldAddNewSet(set)) {
          (*localData)[set] = it.value();
        }
      }
    } else if (json.count(jsonKey)) {
      printSynchronizationError("Key " + jsonKey + " should be an object in");
      return false;
    }
    return true;
  }

  void printSynchronizationError(const std::string& message) const {
    printWithTimeAndMask("WARNING: " + message + " " + loggingParameters_.dropboxFilename + ".");
  }

  std::string setDescription(const std::vector<bool>& set) {
    std::ostringstream str;
    int digitIdx = 0;
    int currentDigit = 0;
    for (bool var : set) {
      if (digitIdx == 0) currentDigit = 0;
      currentDigit = currentDigit * 2 + var;
      if (digitIdx == 3) str << std::hex << currentDigit;
      digitIdx = (digitIdx + 1) % 4;
    }
    if (digitIdx != 0) str << std::hex << currentDigit;
    return str.str();
  }

  std::vector<bool> fromSetDescription(const std::string& description) const {
    std::vector<bool> result;
    int hexDigit = 0;
    int digitStartIndex = 0;
    if (description.length() != (patternCount_ + 3) / 4) {
      printSynchronizationError("Invalid length of set description " + description + " in");
      throw false;
    }
    for (int i = 0; i < patternCount_; ++i) {
      if (i % 4 == 0) {
        std::reverse(result.begin() + digitStartIndex, result.end());
        digitStartIndex = static_cast<int>(result.size());
        char digit = description.at(i / 4);
        if (!((digit >= '0' && digit <= '9') || (digit >= 'a' && digit <= 'f'))) {
          printSynchronizationError("Invalid digit in set description " + description + " in");
          throw false;
        }
        hexDigit = std::stoi(std::string({digit}), nullptr, 16);
      }
      result.push_back(hexDigit & 1);
      hexDigit >>= 1;
    }
    std::reverse(result.begin() + digitStartIndex, result.end());
    return result;
  }

  void initSymmetries() {
    PatternSet allPatterns;
    for (int patternIndex = 0; patternIndex < patternCount_; ++patternIndex) {
      allPatterns.push_back({});
      int maskDigit = 1;
      int patternDigit = 1;
      for (int y = 0; y < maskSize_.first; ++y) {
        allPatterns.back().push_back({});
        for (int x = 0; x < maskSize_.second; ++x) {
          if (maskID_ & maskDigit) {
            allPatterns.back().back().push_back(patternIndex & patternDigit ? 1 : 0);
            patternDigit <<= 1;
          } else {
            allPatterns.back().back().push_back(-1);
          }
          maskDigit <<= 1;
        }
      }
    }

    std::unordered_set<PatternSet, PatternSetHash> encounteredSets = {allPatterns};
    std::queue<PatternSet> newSets;
    newSets.push(allPatterns);

    while (!newSets.empty()) {
      const auto currentSet = newSets.front();
      newSets.pop();
      if (isMaskConsistent(currentSet)) addSymmetry(currentSet);
      const auto transformedSets = {
          flipZeroOne(currentSet), reverse(currentSet), transpose(currentSet), shiftRows(currentSet)};
      for (const auto& transformedSet : transformedSets) {
        if (transformedSet.has_value()) {
          const auto insertionResult = encounteredSets.insert(transformedSet.value());
          if (insertionResult.second) newSets.push(transformedSet.value());
        }
      }
    }
    printWithTimeAndMask("Found " + std::to_string(symmetries_.size()) + " symmetries.");
  }

  static std::optional<PatternSet> flipZeroOne(const PatternSet& input) {
    PatternSet output = input;
    for (int i = 0; i < output.size(); ++i) {
      for (int j = 0; j < output[i].size(); ++j) {
        for (int k = 0; k < output[i][j].size(); ++k) {
          if (output[i][j][k] == 0) output[i][j][k] = 1;
          else if (output[i][j][k] == 1)
            output[i][j][k] = 0;
        }
      }
    }
    return output;
  }

  static std::optional<PatternSet> reverse(const PatternSet& input) {
    PatternSet result = input;
    for (auto& pattern : result) {
      std::reverse(pattern.begin(), pattern.end());
    }
    return result;
  }

  static std::optional<PatternSet> transpose(const PatternSet& input) {
    PatternSet result;
    result.resize(input.size());
    for (int i = 0; i < result.size(); ++i) {
      result[i].resize(input[i][0].size());
      for (int j = 0; j < result[i].size(); ++j) {
        result[i][j].resize(input[i].size());
        for (int k = 0; k < result[i][j].size(); ++k) {
          result[i][j][k] = input[i][k][j];
        }
      }
    }
    return result;
  }

  static std::optional<PatternSet> shiftRows(const PatternSet& input) {
    PatternSet result;
    for (int i = 0; i < input.size(); ++i) {
      result.push_back({});
      for (int j = 0; j < input[i].size(); ++j) {
        result.back().push_back({});
        for (int k = 0; k < j; ++k) result.back().back().push_back(-1);
        for (int k = 0; k < input[i][j].size(); ++k) result.back().back().push_back(input[i][j][k]);
        for (int k = j + 1; k < input[i].size(); ++k) result.back().back().push_back(-1);
      }

      deleteColumns(&result.back(), maskedColumnsCount(result.back(), ColumnsToDelete::Front), ColumnsToDelete::Front);
      deleteColumns(&result.back(), maskedColumnsCount(result.back(), ColumnsToDelete::Back), ColumnsToDelete::Back);
      if (result.back().front().size() != input.back().front().size()) return std::nullopt;
    }
    return result;
  }

  enum class ColumnsToDelete { Front, Back };

  static int maskedColumnsCount(const std::vector<std::vector<int>>& pattern, ColumnsToDelete frontOrBack) {
    int maxCount = std::numeric_limits<int>::max();
    for (const auto& row : pattern) {
      int currentCount = 0;
      if (frontOrBack == ColumnsToDelete::Front) {
        for (int i = 0; i < row.size() && row[i] == -1; ++i) ++currentCount;
      } else {
        for (int i = static_cast<int>(row.size()) - 1; i >= 0 && row[i] == -1; --i) ++currentCount;
      }
      maxCount = std::min(maxCount, currentCount);
    }
    return maxCount;
  }

  static void deleteColumns(std::vector<std::vector<int>>* pattern, int count, ColumnsToDelete frontOrBack) {
    for (auto& row : *pattern) {
      if (frontOrBack == ColumnsToDelete::Front) std::reverse(row.begin(), row.end());
      for (int i = 0; i < count; ++i) row.pop_back();
      if (frontOrBack == ColumnsToDelete::Front) std::reverse(row.begin(), row.end());
    }
  }

  bool isMaskConsistent(const PatternSet& transformedSet) {
    for (const auto& pattern : transformedSet) {
      if (maskSize_.first != pattern.size()) return false;
      int maskDigit = 1;
      for (const auto& row : pattern) {
        if (maskSize_.second != row.size()) return false;
        for (const auto value : row) {
          if (!(maskID_ & maskDigit) && value != -1) return false;
          maskDigit <<= 1;
        }
      }
    }
    return true;
  }

  void addSymmetry(const PatternSet& transformedSet) {
    symmetries_.push_back({});
    for (const auto& pattern : transformedSet) {
      int index = 0;
      for (auto rowIt = pattern.rbegin(); rowIt != pattern.rend(); ++rowIt) {
        for (auto valueIt = rowIt->rbegin(); valueIt != rowIt->rend(); ++valueIt) {
          if (*valueIt == 0) index <<= 1;
          else if (*valueIt == 1)
            index = (index << 1) | 1;
        }
      }
      symmetries_.back().push_back(index);
    }
  }

  std::vector<int> initPatternVariables(CMSat::SATSolver* solver) {
    std::vector<int> patternVariables;
    patternVariables.reserve(patternCount_);
    for (int i = 0; i < patternCount_; ++i) {
      solver->new_var();
      patternVariables.push_back(solver->nVars() - 1);
    }
    return patternVariables;
  }

  enum class GridBoundary { Finite, Periodic };

  std::vector<std::vector<int>> initSpatialVariables(CMSat::SATSolver* solver,
                                                     int gridSize,
                                                     GridBoundary boundary) const {
    std::vector<std::vector<int>> cellVariables;
    int ySize, xSize;
    if (boundary == GridBoundary::Finite) {
      ySize = gridSize + maskSize_.first - 1;
      xSize = gridSize + maskSize_.second - 1;
    } else {
      ySize = gridSize;
      xSize = gridSize;
    }
    cellVariables.resize(ySize);
    for (int i = 0; i < cellVariables.size(); ++i) {
      cellVariables[i].resize(xSize);
      for (int j = 0; j < cellVariables[i].size(); ++j) {
        solver->new_var();
        cellVariables[i][j] = solver->nVars() - 1;
      }
    }
    return cellVariables;
  }

  // pattern variables are only used if set is a nullopt. Otherwise, unset tiles are skipped.
  void initSpatialClauses(CMSat::SATSolver* solver,
                          const std::optional<std::vector<bool>>& set,
                          const std::optional<std::vector<int>>& patternVariables,
                          const std::vector<std::vector<int>>& cellVariables,
                          GridBoundary boundary) const {
    int yEnd, xEnd;
    if (boundary == GridBoundary::Finite) {
      yEnd = static_cast<int>(cellVariables.size()) - (maskSize_.first - 1);
      xEnd = cellVariables.empty() ? 0 : static_cast<int>(cellVariables[0].size()) - (maskSize_.second - 1);
    } else {
      yEnd = static_cast<int>(cellVariables.size());
      xEnd = cellVariables.empty() ? 0 : static_cast<int>(cellVariables[0].size());
    }
    for (int y = 0; y < yEnd; ++y) {
      for (int x = 0; x < xEnd; ++x) {
        initSpatialClausesAt(solver, set, patternVariables, cellVariables, y, x, boundary);
      }
    }
  }

  void initSpatialClausesAt(CMSat::SATSolver* solver,
                            const std::optional<std::vector<bool>>& set,
                            const std::optional<std::vector<int>>& patternVariables,
                            const std::vector<std::vector<int>>& cellVariables,
                            int y,
                            int x,
                            GridBoundary boundary) const {
    std::vector<int> patternClauseVariables;
    for (int patternIndex = 0; patternIndex < patternCount_; ++patternIndex) {
      if (set && !set.value().at(patternIndex)) continue;
      solver->new_var();
      int clauseVariable = solver->nVars() - 1;
      patternClauseVariables.push_back(clauseVariable);
      std::vector<CMSat::Lit> longClause;
      const auto clauseLit = CMSat::Lit(clauseVariable, false);
      longClause.push_back(clauseLit);
      if (patternVariables) {
        const auto patternLit = CMSat::Lit(patternVariables.value()[patternIndex], false);
        longClause.push_back(~patternLit);
        solver->add_clause({~clauseLit, patternLit});
      }
      int patternDigit = 1;
      for (int maskY = 0; maskY < maskSize_.first; ++maskY) {
        for (int maskX = 0; maskX < maskSize_.second; ++maskX) {
          if ((maskID_ >> (maskY * maskSize_.second + maskX)) & 1) {
            int cellY, cellX;
            if (boundary == GridBoundary::Periodic) {
              cellY = (y + maskY) % cellVariables.size();
              cellX = (x + maskX) % cellVariables[y].size();
            } else {
              cellY = y + maskY;
              cellX = x + maskX;
            }
            CMSat::Lit cellLit = CMSat::Lit(cellVariables[cellY][cellX], !(patternIndex & patternDigit));
            longClause.push_back(~cellLit);
            solver->add_clause({~clauseLit, cellLit});
            patternDigit <<= 1;
          }
        }
      }
      solver->add_clause(longClause);
    }
    std::vector<CMSat::Lit> entireTileClause;
    for (const auto clauseVariable : patternClauseVariables) {
      entireTileClause.push_back(CMSat::Lit(clauseVariable, false));
    }
    solver->add_clause(entireTileClause);
  }

  int bitCount(int number) {
    int result = 0;
    while (number) {
      result += (number & 1);
      number >>= 1;
    }
    return result;
  }

  void solve() {
    if (isDone_) {
      printWithTimeAndMask("Already done.");
    } else {
      printWithTimeAndMask("Searching for minimal sets...");
    }

    while (!isDone_) {
      logProgress();
      syncWithDropbox(SaveResultsPriority::Normal);
      if (!periodLowerBounds_.empty()) {
        const auto& [set, lowerPeriodBound] = *periodLowerBounds_.begin();
        const auto periodToTry = lowerPeriodBound + 1;
        if (isTileable(set, periodToTry, GridBoundary::Finite)) {
          if (isTileable(set, periodToTry, GridBoundary::Periodic)) {
            addAndForbidSymmetricSets(set, periodToTry);
            maxPeriod_ = std::max(periodToTry, maxPeriod_);
            periodLowerBounds_.erase(set);
          } else {
            periodLowerBounds_[set] = periodToTry;
          }
        } else {
          maximalGridSizes_[set] = periodToTry - 1;
          increaseGridSize(periodToTry);
          periodLowerBounds_.erase(set);
        }
      } else if (const auto possibleMinimalSet = findSet()) {
        auto minimalSet = possibleMinimalSet.value();
        minimizeSet(&minimalSet);
        periodLowerBounds_[minimalSet] = 0;
      } else {
        isDone_ = true;
        syncWithDropbox(SaveResultsPriority::Force);
        printWithTimeAndMask("Done.");
      }
    }

    printResults();
  }

  void printResults() const {
    printWithTimeAndMask("There are a total of " + std::to_string(periods_.size()) + " minimal sets.");
    printWithTimeAndMask("Maximal period is " + std::to_string(maxPeriod_));
    printWithTimeAndMask("Largest finite grid is " + std::to_string(minimalGridSize_));
  }

  void logProgress() {
    if (std::chrono::steady_clock::now() < lastProgressLogTime_ + loggingParameters_.progressLoggingPeriod) return;
    std::string periodCount = "✓ " + std::to_string(periods_.size());
    std::string unknownString = "? " + std::to_string(periodLowerBounds_.size());
    if (!periodLowerBounds_.empty()) {
      const auto& [set, periodLowerBound] = *periodLowerBounds_.begin();
      unknownString += " (" + setDescription(set) + ": " + std::to_string(periodLowerBound) + ")";
    }
    std::string maxPeriod = "<> " + std::to_string(maxPeriod_);
    std::string minGridSize = "# " + std::to_string(minimalGridSize_);
    std::string maxSetSize = "|| " + std::to_string(maxSetSize_);

    printWithTimeAndMask(periodCount + " " + unknownString + " " + maxPeriod + " " + minGridSize + " " + maxSetSize);
    lastProgressLogTime_ = std::chrono::steady_clock::now();
  }

  void printWithTimeAndMask(const std::string& msg) const {
    auto t = std::time(nullptr);
    auto tm = *std::localtime(&t);
    *loggingParameters_.progressStream << std::put_time(&tm, "%d-%m-%Y %H-%M-%S") << " [" << maskSize_.first << "-"
                                       << maskSize_.second << "-" << maskID_ << "]: " << msg << std::endl;
  }

  size_t addAndForbidSymmetricSets(const std::vector<bool>& set, int period) {
    std::unordered_set<std::vector<bool>> transformedSets;
    auto size = setSize(set);
    for (const auto& symmetry : symmetries_) {
      std::vector<bool> transformedSet;
      for (int i = 0; i < set.size(); ++i) {
        transformedSet.push_back(set[symmetry[i]]);
      }
      transformedSets.insert(transformedSet);
    }
    for (const auto& transformedSet : transformedSets) {
      periods_[transformedSet] = period;
      maxSetSize_ = std::max(maxSetSize_, size);
      forbidMinimalSet(transformedSet);
    }
    return transformedSets.size();
  }

  int setSize(const std::vector<bool>& set) {
    int result = 0;
    for (const auto value : set) {
      if (value) ++result;
    }
    return result;
  }

  std::optional<std::vector<bool>> findSet() {
    if (solver_.solve() == CMSat::l_True) {
      std::vector<bool> result;
      for (auto patternVariable : patternVariables_) {
        result.push_back(solver_.get_model()[patternVariable] == CMSat::l_True ? true : false);
      }
      return result;
    } else {
      return std::nullopt;
    }
  }

  void forbidMinimalSet(const std::vector<bool>& set) {
    std::vector<CMSat::Lit> clause;
    for (int i = 0; i < set.size(); ++i) {
      if (set[i]) {
        clause.push_back(CMSat::Lit(patternVariables_[i], true));
      }
    }
    solver_.add_clause(clause);
  }

  bool isTileable(const std::vector<bool>& set, int size, GridBoundary boundary) const {
    CMSat::SATSolver solver;
    solver.set_num_threads(1);
    initSpatialClauses(&solver, set, std::nullopt, initSpatialVariables(&solver, size, boundary), boundary);
    return solver.solve() == CMSat::l_True;
  }

  void increaseGridSize(int newSize) {
    while (minimalGridSize_ < newSize) incrementGridSize();
  }

  void incrementGridSize() {
    ++minimalGridSize_;

    for (int y = 0; y < minimalGridSize_ + maskSize_.first - 2; ++y) {
      solver_.new_var();
      cellVariables_[y].push_back(solver_.nVars() - 1);
    }
    cellVariables_.push_back({});
    for (int x = 0; x < minimalGridSize_ + maskSize_.second - 1; ++x) {
      solver_.new_var();
      cellVariables_.back().push_back(solver_.nVars() - 1);
    }

    for (int i = 0; i < minimalGridSize_ - 1; ++i) {
      initSpatialClausesAt(
          &solver_, std::nullopt, patternVariables_, cellVariables_, i, minimalGridSize_ - 1, GridBoundary::Finite);
      initSpatialClausesAt(
          &solver_, std::nullopt, patternVariables_, cellVariables_, minimalGridSize_ - 1, i, GridBoundary::Finite);
    }
    initSpatialClausesAt(&solver_,
                         std::nullopt,
                         patternVariables_,
                         cellVariables_,
                         minimalGridSize_ - 1,
                         minimalGridSize_ - 1,
                         GridBoundary::Finite);
  }

  void minimizeSet(std::vector<bool>* set) {
    for (int i = 0; i < set->size(); ++i) {
      if (set->at(i)) {
        (*set)[i] = false;
        if (!isTileable(*set, minimalGridSize_, GridBoundary::Finite)) (*set)[i] = true;
      }
    }
  }
};

Mask::Mask(const std::pair<int, int>& size, int id, const LoggingParameters& loggingParameters)
    : implementation_(std::make_shared<Implementation>(size, id, loggingParameters)) {}
void Mask::findMinimalSets() { implementation_->findMinimalSets(); }
}  // namespace TilingSystem
