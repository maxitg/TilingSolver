#include "Mask.hpp"

#include <cryptominisat5/cryptominisat.h>

#include <algorithm>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <nlohmann/json.hpp>
#include <optional>
#include <queue>
#include <sstream>
#include <thread>
#include <unordered_set>
#include <vector>

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
  int maxSetSize_;
  int maxPeriod_;
  std::vector<int> patternVariables_;
  std::vector<std::vector<int>> cellVariables_;
  std::vector<std::vector<int>> symmetries_;

  const std::string isDoneKey = "IsDone";
  const std::string periodsKey = "Periods";
  // These are tileable but not periodic up to specified sizes (this will usually be a single currently-processed set)
  const std::string periodLowerBoundsKey = "PeriodLowerBounds";
  const std::string minimalGridSizeKey = "MinimalGridSize";
  // Maximal grid size a particular set can tile. This will only be recorded when the grid needs to be enlarged.
  const std::string maximalGridSizesKey = "MaximalGridSizes";
  nlohmann::json minimalSetsJSON_;

 public:
  Implementation(const std::pair<int, int>& size, int id, const LoggingParameters& loggingParameters)
      : maskSize_(size), maskID_(id), loggingParameters_(loggingParameters) {
    init();
  }

  void findMinimalSets() { solve(); }

 private:
  void init() {
    patternCount_ = 1 << bitCount(maskID_);
    std::ifstream file(loggingParameters_.filename);
    if (file.is_open()) {
      file >> minimalSetsJSON_;
      if (minimalSetsJSON_[isDoneKey].is_boolean() && minimalSetsJSON_[isDoneKey]) {
        printWithTimeAndMask("Already solved.");
      }
    }
    if (!minimalSetsJSON_.count(isDoneKey)) minimalSetsJSON_[isDoneKey] = false;
    if (!minimalSetsJSON_.count(periodsKey)) minimalSetsJSON_[periodsKey] = std::unordered_map<std::string, int>();
    if (!minimalSetsJSON_.count(periodLowerBoundsKey))
      minimalSetsJSON_[periodLowerBoundsKey] = std::unordered_map<std::string, int>();
    if (!minimalSetsJSON_.count(minimalGridSizeKey)) minimalSetsJSON_[minimalGridSizeKey] = 2;
    if (!minimalSetsJSON_.count(maximalGridSizesKey))
      minimalSetsJSON_[maximalGridSizesKey] = std::unordered_map<std::string, int>();

    solver_.set_num_threads(1);
    maxSetSize_ = 0;
    maxPeriod_ = 0;
    patternVariables_ = initPatternVariables(&solver_);
    cellVariables_ = initSpatialVariables(&solver_, minimalSetsJSON_[minimalGridSizeKey], GridBoundary::Finite);
    initSpatialClauses(&solver_, std::nullopt, patternVariables_, cellVariables_, GridBoundary::Finite);
    for (auto setAndPeriod = minimalSetsJSON_[periodsKey].begin(); setAndPeriod != minimalSetsJSON_[periodsKey].end();
         ++setAndPeriod) {
      const auto minimalSet = fromSetDescription(setAndPeriod.key());
      int size = setSize(minimalSet);
      maxSetSize_ = std::max(maxSetSize_, size);
      maxPeriod_ = std::max(maxPeriod_, static_cast<int>(setAndPeriod.value()));
      forbidMinimalSet(minimalSet);
    }
    if (!minimalSetsJSON_[periodLowerBoundsKey].empty()) {
      printWithTimeAndMask(std::to_string(minimalSetsJSON_[periodLowerBoundsKey].size()) + " sets have no periods.");
    }
    initSymmetries();
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
    printWithTimeAndMask("Searching for minimal sets...");

    while (!minimalSetsJSON_[isDoneKey]) {
      logProgress();
      saveResults();
      if (!minimalSetsJSON_[periodLowerBoundsKey].empty()) {
        nlohmann::json::iterator setStringAndLowerPeriodBound = minimalSetsJSON_[periodLowerBoundsKey].begin();
        const auto setString = setStringAndLowerPeriodBound.key();
        const auto set = fromSetDescription(setString);
        const auto periodToTry = static_cast<int>(setStringAndLowerPeriodBound.value()) + 1;
        if (isTileable(set, periodToTry, GridBoundary::Finite)) {
          if (isTileable(set, periodToTry, GridBoundary::Periodic)) {
            addAndForbidSymmetricSets(set, periodToTry);
            maxPeriod_ = std::max(periodToTry, maxPeriod_);
            minimalSetsJSON_[periodLowerBoundsKey].erase(setString);
          } else {
            minimalSetsJSON_[periodLowerBoundsKey][setString] = periodToTry;
          }
        } else {
          minimalSetsJSON_[maximalGridSizesKey][setString] = periodToTry - 1;
          increaseGridSize(periodToTry);
          minimalSetsJSON_[periodLowerBoundsKey].erase(setString);
        }
      } else if (const auto possibleMinimalSet = findSet()) {
        auto minimalSet = possibleMinimalSet.value();
        minimizeSet(&minimalSet);
        minimalSetsJSON_[periodLowerBoundsKey][setDescription(minimalSet)] = 0;
      } else {
        minimalSetsJSON_[isDoneKey] = true;
        saveResults(SaveResultsPriority::Force);
        printWithTimeAndMask("Done.");
      }
    }

    printResults();
  }

  void printResults() const {
    printWithTimeAndMask("There are a total of " + std::to_string(minimalSetsJSON_[periodsKey].size()) +
                         " minimal sets.");
    printWithTimeAndMask("Maximal period is " + std::to_string(maxPeriod_));
    printWithTimeAndMask("Largest finite grid is " +
                         std::to_string(static_cast<int>(minimalSetsJSON_[minimalGridSizeKey])));
  }

  void logProgress() {
    if (std::chrono::steady_clock::now() < lastProgressLogTime_ + loggingParameters_.progressLoggingPeriod) return;
    std::string periodCount = "✓ " + std::to_string(minimalSetsJSON_[periodsKey].size());
    std::string unknownString = "? " + std::to_string(minimalSetsJSON_[periodLowerBoundsKey].size());
    if (!minimalSetsJSON_[periodLowerBoundsKey].empty()) {
      nlohmann::json::iterator firstUnknown = minimalSetsJSON_[periodLowerBoundsKey].begin();
      unknownString += " (" + firstUnknown.key() + ": " + std::to_string(static_cast<int>(firstUnknown.value())) + ")";
    }
    std::string maxPeriod = "<> " + std::to_string(maxPeriod_);
    std::string minGridSize = "# " + std::to_string(static_cast<int>(minimalSetsJSON_[minimalGridSizeKey]));
    std::string maxSetSize = "|| " + std::to_string(maxSetSize_);

    printWithTimeAndMask(periodCount + " " + unknownString + " " + maxPeriod + " " + minGridSize + " " + maxSetSize);
    lastProgressLogTime_ = std::chrono::steady_clock::now();
  }

  enum class SaveResultsPriority { Normal, Force };

  void saveResults(const SaveResultsPriority& priority = SaveResultsPriority::Normal) {
    if (priority != SaveResultsPriority::Force &&
        std::chrono::steady_clock::now() < lastResultsSavingTime_ + loggingParameters_.resultsSavingPeriod) {
      return;
    }
    std::ofstream file(loggingParameters_.filename);
    if (file.is_open()) {
      file << minimalSetsJSON_.dump(2);
    } else {
      printWithTimeAndMask("WARNING! Could not save results to a file: " + std::string(std::strerror(errno)));
    }
    lastResultsSavingTime_ = std::chrono::steady_clock::now();
  }

  void printWithTimeAndMask(const std::string& msg) const {
    auto t = std::time(nullptr);
    auto tm = *std::localtime(&t);
    std::cout << std::put_time(&tm, "%d-%m-%Y %H-%M-%S") << " [" << maskSize_.first << "-" << maskSize_.second << "-"
              << maskID_ << "]: " << msg << std::endl;
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
      minimalSetsJSON_[periodsKey][setDescription(transformedSet)] = period;
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

  std::vector<bool> fromSetDescription(const std::string& description) {
    std::vector<bool> result;
    int hexDigit = 0;
    int digitStartIndex = 0;
    for (int i = 0; i < patternCount_; ++i) {
      if (i % 4 == 0) {
        std::reverse(result.begin() + digitStartIndex, result.end());
        digitStartIndex = static_cast<int>(result.size());
        hexDigit = std::stoi(std::string({description.at(i / 4)}), nullptr, 16);
      }
      result.push_back(hexDigit & 1);
      hexDigit >>= 1;
    }
    std::reverse(result.begin() + digitStartIndex, result.end());
    return result;
  }

  bool isTileable(const std::vector<bool>& set, int size, GridBoundary boundary) const {
    CMSat::SATSolver solver;
    solver.set_num_threads(1);
    initSpatialClauses(&solver, set, std::nullopt, initSpatialVariables(&solver, size, boundary), boundary);
    return solver.solve() == CMSat::l_True;
  }

  void increaseGridSize(int newSize) {
    while (minimalSetsJSON_[minimalGridSizeKey] < newSize) incrementGridSize();
  }

  void incrementGridSize() {
    minimalSetsJSON_[minimalGridSizeKey] = static_cast<int>(minimalSetsJSON_[minimalGridSizeKey]) + 1;
    int minimalGridSize = static_cast<int>(minimalSetsJSON_[minimalGridSizeKey]);

    for (int y = 0; y < minimalGridSize + maskSize_.first - 2; ++y) {
      solver_.new_var();
      cellVariables_[y].push_back(solver_.nVars() - 1);
    }
    cellVariables_.push_back({});
    for (int x = 0; x < minimalGridSize + maskSize_.second - 1; ++x) {
      solver_.new_var();
      cellVariables_.back().push_back(solver_.nVars() - 1);
    }

    for (int i = 0; i < minimalGridSize - 1; ++i) {
      initSpatialClausesAt(
          &solver_, std::nullopt, patternVariables_, cellVariables_, i, minimalGridSize - 1, GridBoundary::Finite);
      initSpatialClausesAt(
          &solver_, std::nullopt, patternVariables_, cellVariables_, minimalGridSize - 1, i, GridBoundary::Finite);
    }
    initSpatialClausesAt(&solver_,
                         std::nullopt,
                         patternVariables_,
                         cellVariables_,
                         minimalGridSize - 1,
                         minimalGridSize - 1,
                         GridBoundary::Finite);
  }

  void minimizeSet(std::vector<bool>* set) {
    for (int i = 0; i < set->size(); ++i) {
      if (set->at(i)) {
        (*set)[i] = false;
        if (!isTileable(*set, minimalSetsJSON_[minimalGridSizeKey], GridBoundary::Finite)) (*set)[i] = true;
      }
    }
  }
};

Mask::Mask(const std::pair<int, int>& size, int id, const std::string& filename)
    : Mask(size, id, {LoggingParameters(filename)}) {}
Mask::Mask(const std::pair<int, int>& size, int id, const LoggingParameters& loggingParameters)
    : implementation_(std::make_shared<Implementation>(size, id, loggingParameters)) {}
void Mask::findMinimalSets() { implementation_->findMinimalSets(); }
}  // namespace TilingSystem
