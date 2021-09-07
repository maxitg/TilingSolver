#include "Mask.hpp"

#include <cryptominisat5/cryptominisat.h>

#include <iostream>
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

  int patternCount_;
  int currentGridSize_;
  CMSat::SATSolver solver_;
  std::vector<std::vector<bool>> minimalSets_;
  bool isSolved_ = false;
  int maxGridSize_ = -1;
  std::vector<int> patternVariables_;
  std::vector<std::vector<int>> cellVariables_;
  std::vector<std::vector<int>> symmetries_;

 public:
  Implementation(const std::pair<int, int>& size, int id) : maskSize_(size), maskID_(id) {}

  const std::vector<std::vector<bool>>& minimalSets(int maxGridSize) {
    if (maxGridSize_ != maxGridSize) {
      maxGridSize_ = maxGridSize;
      init();
    }
    if (!isSolved_) solve();
    return minimalSets_;
  }

 private:
  void init() {
    patternCount_ = 1 << bitCount(maskID_);
    currentGridSize_ = 1;
    solver_.set_num_threads(std::thread::hardware_concurrency());
    minimalSets_ = {};
    isSolved_ = false;
    patternVariables_ = initPatternVariables(&solver_);
    cellVariables_ = initSpatialVariables(&solver_, currentGridSize_);
    initSpatialClauses(&solver_, std::nullopt, patternVariables_, cellVariables_);
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
    std::cout << "Found " << symmetries_.size() << " symmetries." << std::endl;
  }

  static std::optional<PatternSet> flipZeroOne(const PatternSet& input) {
    PatternSet output = input;
    for (int i = 0; i < output.size(); ++i) {
      for (int j = 0; j < output[i].size(); ++j) {
        for (int k = 0; k < output[i][j].size(); ++k) {
          if (output[i][j][k] == 0) output[i][j][k] = 1;
          else if (output[i][j][k] == 1) output[i][j][k] = 0;
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
      int maskDigit = 1;
      for (const auto& row : pattern) {
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
      for (const auto& row : pattern) {
        for (const auto value : row) {
          if (value == 0) index <<= 1;
          else if (value == 1) index = (index << 1) | 1;
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

  std::vector<std::vector<int>> initSpatialVariables(CMSat::SATSolver* solver, int gridSize) {
    std::vector<std::vector<int>> cellVariables;
    cellVariables.resize(gridSize + maskSize_.first - 1);
    for (int i = 0; i < cellVariables.size(); ++i) {
      cellVariables[i].resize(gridSize + maskSize_.second - 1);
      for (int j = 0; j < cellVariables[i].size(); ++j) {
        solver->new_var();
        cellVariables[i][j] = solver->nVars() - 1;
      }
    }
    return cellVariables;
  }

  static void printClause(const std::vector<CMSat::Lit>& clause) {
    for (const auto lit : clause) {
      if (!lit.sign()) std::cout << "!";
      std::cout << lit.var() << " ";
    }
    std::cout << std::endl;
  }

  // pattern variables are only used if set is a nullopt. Otherwise, unset tiles are skipped.
  void initSpatialClauses(CMSat::SATSolver* solver,
                          const std::optional<std::vector<bool>>& set,
                          const std::optional<std::vector<int>>& patternVariables,
                          const std::vector<std::vector<int>>& cellVariables) const {
    for (int y = 0; y < cellVariables.size() - (maskSize_.first - 1); ++y) {
      for (int x = 0; x < cellVariables[y].size() - (maskSize_.second - 1); ++x) {
        initSpatialClausesAt(solver, set, patternVariables, cellVariables, y, x);
      }
    }
  }

  void initSpatialClausesAt(CMSat::SATSolver* solver,
                            const std::optional<std::vector<bool>>& set,
                            const std::optional<std::vector<int>>& patternVariables,
                            const std::vector<std::vector<int>>& cellVariables,
                            int y,
                            int x) const {
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
            auto cellLit = CMSat::Lit(cellVariables[y + maskY][x + maskX], !(patternIndex & patternDigit));
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
    if (isSolved_) return;
    std::cout << "Searching:";
    while (const auto possibleMinimalSet = findSet()) {
      if (isTileableToMaxSize(possibleMinimalSet.value())) {
        auto minimalSet = possibleMinimalSet.value();
        minimizeSet(&minimalSet);
        addAndForbidSymmetricSets(minimalSet);
        std::cout << " " << minimalSets_.size();
        std::cout.flush();
      } else {
        incrementGridSize();
      }
    }
    std::cout << std::endl;
  }

  void addAndForbidSymmetricSets(const std::vector<bool>& set) {
    std::unordered_set<std::vector<bool>> transformedSets;
    for (const auto& symmetry : symmetries_) {
      std::vector<bool> transformedSet;
      for (int i = 0; i < set.size(); ++i) {
        transformedSet.push_back(set[symmetry[i]]);
      }
      transformedSets.insert(transformedSet);
    }
    for (const auto& transformedSet : transformedSets) {
      minimalSets_.push_back(transformedSet);
      forbidMinimalSet(transformedSet);
    }
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
    str << maskSize_.first << '-' << maskSize_.second << '-' << maskID_ << ':';
    for (bool var : set) {
      str << (var ? '1' : '0');
    }
    return str.str();
  }

  bool isTileableToMaxSize(const std::vector<bool>& set) {
    CMSat::SATSolver solver;
    solver.set_num_threads(std::thread::hardware_concurrency());
    initSpatialClauses(&solver, set, std::nullopt, initSpatialVariables(&solver, maxGridSize_));
    return solver.solve() == CMSat::l_True;
  }

  void incrementGridSize() {
    ++currentGridSize_;
    std::cout << " #" << currentGridSize_;
    std::cout.flush();

    for (int y = 0; y < currentGridSize_ + maskSize_.first - 2; ++y) {
      solver_.new_var();
      cellVariables_[y].push_back(solver_.nVars() - 1);
    }
    cellVariables_.push_back({});
    for (int x = 0; x < currentGridSize_ + maskSize_.second - 1; ++x) {
      solver_.new_var();
      cellVariables_.back().push_back(solver_.nVars() - 1);
    }

    for (int i = 0; i < currentGridSize_ - 1; ++i) {
      initSpatialClausesAt(&solver_, std::nullopt, patternVariables_, cellVariables_, i, currentGridSize_ - 1);
      initSpatialClausesAt(&solver_, std::nullopt, patternVariables_, cellVariables_, currentGridSize_ - 1, i);
    }
    initSpatialClausesAt(
        &solver_, std::nullopt, patternVariables_, cellVariables_, currentGridSize_ - 1, currentGridSize_ - 1);
  }

  void minimizeSet(std::vector<bool>* set) {
    for (int i = 0; i < set->size(); ++i) {
      if (set->at(i)) {
        (*set)[i] = false;
        if (!isTileableToMaxSize(*set)) (*set)[i] = true;
      }
    }
  }
};

Mask::Mask(const std::pair<int, int>& size, int id) : implementation_(std::make_shared<Implementation>(size, id)) {}
const std::vector<std::vector<bool>>& Mask::minimalSets(int maxGridSize) {
  return implementation_->minimalSets(maxGridSize);
}
}  // namespace TilingSystem
