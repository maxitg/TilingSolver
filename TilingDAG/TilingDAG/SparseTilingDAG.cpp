#include "SparseTilingDAG.hpp"

#include <unordered_set>

class SparseTilingDAG::Implementation {
private:
  int bitCount_;
  int currentSize_ = 0;
  std::unordered_set<uint64_t> currentSizeTileable_;

public:
  Implementation(int bitCount) : bitCount_(bitCount) {}
  int bitCount() { return bitCount_; }
  int currentSize() { return currentSize_; }

  std::vector<uint64_t> unknownSubsetsOfCurrentSize() {
    std::vector<uint64_t> result;
    enumerateSubsets(bitCount(), currentSize(), [this, &result](uint64_t subset) {
      if (!currentSizeTileable_.count(subset)) result.push_back(subset);
    });
    return result;
  }

  void setTileable(uint64_t bits) {
    currentSizeTileable_.insert(bits);
  }

  void setRestUntileableAndIncrementSize() {
    std::unordered_set<uint64_t> nextSizeTileable;
    for (const uint64_t currentSizeBits : currentSizeTileable_) {
      uint64_t currentDigit = 1;
      for (int bit = 0; bit < bitCount(); ++bit) {
        if (!(currentSizeBits & currentDigit)) nextSizeTileable.insert(currentSizeBits | currentDigit);
        currentDigit *= 2;
      }
    }
    currentSizeTileable_ = nextSizeTileable;
    ++currentSize_;
  }

private:
  static void enumerateSubsets(int supersetSize, int subsetSize, std::function<void(uint64_t)> process) {
    if (supersetSize > 64) throw std::runtime_error("enumerateSubsets: supersets over size 64 are not supported");
    if (subsetSize > supersetSize) {
      throw std::runtime_error("enumerateSubsets: supersetSize should be larger than subsetSize");
    }

    uint64_t currentSubset = firstSubset(subsetSize);

    while(!isLastSubset(supersetSize, currentSubset)) {
      process(currentSubset);
      currentSubset = nextSubset(currentSubset);
    }
    process(currentSubset);
  }

  static uint64_t firstSubset(int subsetSize) {
    uint64_t result = 0;
    uint64_t currentDigit = 1;
    for (int i = 0; i < subsetSize; ++i) {
      result |= currentDigit;
      currentDigit *= 2;
    }
    return result;
  }

  static bool isLastSubset(int supersetSize, uint64_t subset) {
    uint64_t currentDigit = 1;
    bool foundOne = false;
    for (int i = 0; i < supersetSize; ++i) {
      if (subset & currentDigit) foundOne = true;
      else if (foundOne) return false;
      currentDigit *= 2;
    }
    return true;
  }

  static uint64_t nextSubset(uint64_t currentSubset) {
    return addRightmostOnes(onesCount(currentSubset), currentSubset + rightmostOneDigit(currentSubset));
  }

  static int onesCount(uint64_t subset) {
    uint64_t currentDigit = 1;
    int result = 0;
    while (subset) {
      if (currentDigit & subset) {
        ++result;
        subset -= currentDigit;
      }
      currentDigit *= 2;
    }
    return result;
  }

  static uint64_t rightmostOneDigit(uint64_t subset) {
    uint64_t currentDigit = 1;
    while (!(currentDigit & subset)) currentDigit *= 2;
    return currentDigit;
  }

  static uint64_t addRightmostOnes(int sizeTarget, uint64_t currentSubset) {
    int missingDigitCount = sizeTarget - onesCount(currentSubset);
    uint64_t currentDigit = 1;
    uint64_t result = currentSubset;
    for (int i = 0; i < missingDigitCount; ++i) {
      result += currentDigit;
      currentDigit *= 2;
    }
    return result;
  }
};

SparseTilingDAG::SparseTilingDAG(int bitCount) : implementation_(std::make_shared<Implementation>(bitCount)) {}

int SparseTilingDAG::bitCount() { return implementation_->bitCount(); }

int SparseTilingDAG::currentSize() { return implementation_->currentSize(); }

std::vector<uint64_t> SparseTilingDAG::unknownSubsetsOfCurrentSize() {
  return implementation_->unknownSubsetsOfCurrentSize();
}

void SparseTilingDAG::setTileable(uint64_t bits) { implementation_->setTileable(bits); }

void SparseTilingDAG::setRestUntileableAndIncrementSize() { implementation_->setRestUntileableAndIncrementSize(); }
