#include "Mask.hpp"

#include <iostream>
#include <optional>
#include <vector>
#include <thread>
#include <sstream>

#include <cryptominisat5/cryptominisat.h>

namespace TilingSystem {
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
  int lastVariable_;
  std::vector<int> patternVariables_;
  std::vector<std::vector<int>> cellVariables_;

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
    currentGridSize_ = 2;
    solver_.set_num_threads(std::thread::hardware_concurrency());
    minimalSets_ = {};
    isSolved_ = false;
    lastVariable_ = -1;
    initPatternVariables();
    initSpatialVariables();
    initSpatialClauses();
  }

  int makeVariable() {
    solver_.new_var();
    return ++lastVariable_;
  }

  void initPatternVariables() {
    patternVariables_.resize(patternCount_);
    for (int i = 0; i < patternVariables_.size(); ++i) {
      patternVariables_[i] = makeVariable();
    }
  }

  void initSpatialVariables() {
    cellVariables_.resize(currentGridSize_ + maskSize_.first - 1);
    for (int i = 0; i < cellVariables_.size(); ++i) {
      cellVariables_[i].resize(currentGridSize_ + maskSize_.second - 1);
      for (int j = 0; j < cellVariables_[i].size(); ++j) {
        cellVariables_[i][j] = makeVariable();
      }
    }
  }

  static void printClause(const std::vector<CMSat::Lit>& clause) {
    for (const auto lit : clause) {
      if (!lit.sign()) std::cout << "!";
      std::cout << lit.var() << " ";
    }
    std::cout << std::endl;
  }

  void initSpatialClauses() {
    for (int y = 0; y < currentGridSize_; ++y) {
      for (int x = 0; x < currentGridSize_; ++x) {
        std::vector<int> patternClauseVariables;
        for (int patternIndex = 0; patternIndex < patternCount_; ++patternIndex) {
          int clauseVariable = makeVariable();
          patternClauseVariables.push_back(clauseVariable);
          std::vector<CMSat::Lit> longClause;
          const auto clauseLit = CMSat::Lit(clauseVariable, false);
          const auto patternLit = CMSat::Lit(patternVariables_[patternIndex], false);
          longClause.push_back(clauseLit);
          longClause.push_back(~patternLit);
          solver_.add_clause({~clauseLit, patternLit});
          int patternDigit = 1;
          for (int maskY = 0; maskY < maskSize_.first; ++maskY) {
            for (int maskX = 0; maskX < maskSize_.second; ++maskX) {
              if ((maskID_ >> (maskY * maskSize_.second + maskX)) & 1) {
                auto cellLit = CMSat::Lit(cellVariables_[y + maskY][x + maskX], !(patternIndex & patternDigit));
                longClause.push_back(~cellLit);
                solver_.add_clause({~clauseLit, cellLit});
                patternDigit <<= 1;
              }
            }
          }
          solver_.add_clause(longClause);
        }
        std::vector<CMSat::Lit> entireTileClause;
        for (const auto clauseVariable : patternClauseVariables) {
          entireTileClause.push_back(CMSat::Lit(clauseVariable, false));
        }
        solver_.add_clause(entireTileClause);
      }
    }
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
        minimalSets_.push_back(minimalSet);
        std::cout << std::endl << setDescription(minimalSet);
        forbidMinimalSet(minimalSet);
      } else {
        incrementGridSize();
      }
    }
    std::cout << std::endl;
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

  bool isTileableToMaxSize(const std::vector<bool>& set) { return true; }
  void minimizeSet(std::vector<bool>* set) {}
  void incrementGridSize() {}
};

Mask::Mask(const std::pair<int, int>& size, int id) : implementation_(std::make_shared<Implementation>(size, id)) {}
const std::vector<std::vector<bool>>& Mask::minimalSets(int maxGridSize) {
  return implementation_->minimalSets(maxGridSize);
}
}
