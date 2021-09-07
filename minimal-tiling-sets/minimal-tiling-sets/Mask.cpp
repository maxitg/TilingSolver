#include "Mask.hpp"

#include <cryptominisat5/cryptominisat.h>

#include <iostream>
#include <optional>
#include <sstream>
#include <thread>
#include <vector>

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
    currentGridSize_ = 1;
    solver_.set_num_threads(std::thread::hardware_concurrency());
    minimalSets_ = {};
    isSolved_ = false;
    patternVariables_ = initPatternVariables(&solver_);
    cellVariables_ = initSpatialVariables(&solver_, currentGridSize_);
    initSpatialClauses(&solver_, std::nullopt, patternVariables_, cellVariables_);
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

  bool isTileableToMaxSize(const std::vector<bool>& set) {
    CMSat::SATSolver solver;
    initSpatialClauses(&solver, set, std::nullopt, initSpatialVariables(&solver, maxGridSize_));
    return solver.solve() == CMSat::l_True;
  }

  void incrementGridSize() {
    ++currentGridSize_;
    std::cout << " #" << currentGridSize_;

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
