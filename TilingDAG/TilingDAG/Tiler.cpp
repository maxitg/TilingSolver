#include "Tiler.hpp"

#include <iostream>
#include <unordered_map>

struct Coordinates {
  int y;
  int x;
  bool operator==(const Coordinates& other) const { return x == other.x && y == other.y; }
  Coordinates operator+(const Coordinates& other) const { return {y + other.y, x + other.x}; }
  Coordinates operator+=(const Coordinates& other) {
    x += other.x;
    y += other.y;
    return *this;
  }
};

template <class T>
inline static void hash_combine(std::size_t* seed, const T& v) {
  std::hash<T> hasher;
  *seed ^= hasher(v) + 0x9e3779b9 + ((*seed) << 6) + ((*seed) >> 2);
}

namespace std {
template <>
struct hash<Coordinates> {
  inline size_t operator()(const Coordinates& displacement) const {
    size_t seed = 0;
    hash_combine<int>(&seed, displacement.x);
    hash_combine<int>(&seed, displacement.y);
    return seed;
  }
};
}  // namespace std

class Tiler::Implementation {
 private:
  int maskBitCount_;
  std::vector<std::pair<Coordinates, std::vector<PatternSet>>> patternNeighborRules_;
  std::vector<PatternSet> minimalSets_;
  int sizeDone_ = 0;
  int gridSize_;
  std::vector<Coordinates> tilingOrder_;

 public:
  Implementation(const Mask& mask, int gridSize, const std::vector<PatternSet>& minimalSets)
      : gridSize_(gridSize), minimalSets_(minimalSets), tilingOrder_(getSpiral()) {
    initPatternNeighborRules(mask);
  }

  void tileToSize(int maxSize) {
    while (sizeDone_ < maxSize) tileNextSize();
  }

  const std::vector<PatternSet>& minimalSets() { return minimalSets_; }

 private:
  void initPatternNeighborRules(const Mask& mask) {
    std::vector<std::pair<int, int>> bitPositions;
    for (int i = 0; i < mask.size(); ++i) {
      for (int j = 0; j < mask[i].size(); ++j) {
        if (mask[i][j]) bitPositions.push_back(std::make_pair(i, j));
      }
    }
    maskBitCount_ = static_cast<int>(bitPositions.size());

    std::unordered_map<Coordinates, std::vector<std::pair<int, int>>> groupedIndexDisplacements;

    for (int i = 0; i < bitPositions.size(); ++i) {
      for (int j = 0; j < bitPositions.size(); ++j) {
        if (i == j) continue;
        groupedIndexDisplacements[{bitPositions[j].first - bitPositions[i].first,
                                   bitPositions[j].second - bitPositions[i].second}]
            .push_back(std::make_pair(i, j));
      }
    }

    for (const auto& displacementAndIndices : groupedIndexDisplacements) {
      patternNeighborRules_.push_back(std::make_pair(
          displacementAndIndices.first, bitMapToPatternMap(bitPositions.size(), displacementAndIndices.second)));
    }
  }

  std::vector<PatternSet> bitMapToPatternMap(size_t maskBitCount, const std::vector<std::pair<int, int>>& bitMap) {
    std::vector<PatternSet> result;
    for (int neighborPattern = 0; neighborPattern < (1 << maskBitCount); ++neighborPattern) {
      result.push_back(bitMapToPatternRule(maskBitCount, neighborPattern, bitMap));
    }
    return result;
  }

  PatternSet bitMapToPatternRule(size_t maskBitCount,
                                 int neighborPattern,
                                 const std::vector<std::pair<int, int>>& bitMap) {
    // neighborPattern: {01010}
    // bitMap: {0 -> 1, 2 -> 3}
    // patternPattern: {_0_0_} -> {-1, 0, -1, 0, -1}
    // then we find all pattern sets that match it
    PatternSet result = 0;
    std::vector<int> patternPattern(maskBitCount, -1);
    for (const auto& bitMapping : bitMap) {
      patternPattern[bitMapping.second] = neighborPattern & (1 << bitMapping.first) ? 1 : 0;
    }
    for (int i = 0; i < (1 << maskBitCount); ++i) {
      bool ok = true;
      for (int digit = 0; digit < maskBitCount; ++digit) {
        if (patternPattern[digit] != -1 && patternPattern[digit] != ((i & (1 << digit)) ? 1 : 0)) ok = false;
      }
      if (ok) result |= (1ULL << i);
    }
    return result;
  }

  std::vector<Coordinates> getSpiral() {
    std::vector<Coordinates> result;
    Coordinates currentCell = {gridSize_ / 2, gridSize_ / 2};  // bottom-right off center if the size is even
    Coordinates currentVector = {-1, 0};
    for (int stride = 1; stride <= gridSize_; ++stride) {
      for (int pass = 0; pass < 2; ++pass) {
        for (int k = 0; k < stride; ++k) {
          if (stride == gridSize_ && pass == 1) break;
          result.push_back(currentCell);
          currentCell += currentVector;
        }
        nextVector(&currentVector);
      }
    }

    return result;
  }

  void nextVector(Coordinates* vector) {
    std::swap(vector->y, vector->x);
    if (vector->y != 0) vector->y = -vector->y;
  }

  using PatternGrid = std::vector<std::vector<int>>;

  void tileNextSize() {
    std::cout << "Tiling size " << sizeDone_ + 1 << std::endl;
    const auto& minimalSetsBefore = minimalSets_.size();
    PatternGrid grid(gridSize_, std::vector<int>(gridSize_, -1));
    attemptTiling(&grid, 0, 0);
    ++sizeDone_;
    std::cout << "Done: " << minimalSets_.size() - minimalSetsBefore << std::endl;
  }

  void attemptTiling(PatternGrid* grid, int currentIndex, PatternSet currentSet) {
    if (currentIndex == tilingOrder_.size()) {
      minimalSets_.push_back(currentSet);
      return;
    }
    PatternSet allowedCurrentSets = getAllowedCurrentSets(*grid, currentIndex);
    for (int currentPatternIndex = 0; currentPatternIndex < (1 << maskBitCount_); ++currentPatternIndex) {
      PatternSet currentPattern = (1ULL << currentPatternIndex);

      if (!(allowedCurrentSets & currentPattern)) continue;
      if (setSize(currentSet | currentPattern) > sizeDone_ + 1) continue;
      if ((currentSet | currentPattern) != currentSet && isSupersetOfAMinimalSet(currentSet | currentPattern)) continue;

      if (allowedCurrentSets & currentPattern) {
        size_t minimalSetCountBefore = minimalSets_.size();
        (*grid)[tilingOrder_[currentIndex].y][tilingOrder_[currentIndex].x] = currentPatternIndex;
        attemptTiling(grid, currentIndex + 1, currentSet | currentPattern);
        (*grid)[tilingOrder_[currentIndex].y][tilingOrder_[currentIndex].x] = -1;
        if (minimalSets_.size() > minimalSetCountBefore && isSupersetOfAMinimalSet(currentSet)) return;
      }
    }
  }

  bool isSupersetOfAMinimalSet(PatternSet set) {
    for (const auto& minimalSet : minimalSets_) {
      if ((set & minimalSet) == minimalSet) return true;
    }
    return false;
  }

  int setSize(PatternSet set) {
    int result = 0;
    while (set) {
      result += set & 1;
      set >>= 1;
    }
    return result;
  }

  PatternSet getAllowedCurrentSets(const PatternGrid& grid, int currentIndex) {
    const auto& currentPosition = tilingOrder_[currentIndex];
    PatternSet remainingPatterns = (~0ULL) - (~0ULL << (1 << maskBitCount_));
    for (const auto& displacementAndConstraints : patternNeighborRules_) {
      const auto& displacement = displacementAndConstraints.first;
      const auto& constraints = displacementAndConstraints.second;
      Coordinates destination = currentPosition + displacement;
      if (isDestinationInGrid(destination) && grid[destination.y][destination.x] >= 0) {
        remainingPatterns &= constraints[grid[destination.y][destination.x]];
      }
    }
    return remainingPatterns;
  }

  bool isDestinationInGrid(const Coordinates& destination) {
    return destination.y >= 0 && destination.x >= 0 && destination.y < gridSize_ && destination.x < gridSize_;
  }
};

Tiler::Tiler(const Mask& mask, int gridSize, const std::vector<PatternSet>& minimalSets)
    : implementation_(std::make_shared<Implementation>(mask, gridSize, minimalSets)) {}

void Tiler::tileToSize(int maxSize) { implementation_->tileToSize(maxSize); }

const std::vector<Tiler::PatternSet>& Tiler::minimalSets() { return implementation_->minimalSets(); }
