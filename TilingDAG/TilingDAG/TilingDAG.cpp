#include "TilingDAG.hpp"

#include <cmath>
#include <functional>
#include <iostream>
#include <vector>

class TilingDAG::Implementation {
 private:
  int bitCount_;
  std::vector<bool> knownTileable_;
  int knownTileableCount_ = 0;
  std::vector<bool> knownUntileable_;
  int knownUntileableCount_ = 0;

 public:
  Implementation(int bitCount)
      : bitCount_(bitCount),
        knownTileable_(std::pow(2, bitCount), false),
        knownUntileable_(std::pow(2, bitCount), false) {}

  int bitCount() { return bitCount_; }

  void setTileable(unsigned patternBits) {
    if (knownTileable_[patternBits]) return;
    knownTileable_[patternBits] = true;
    ++knownTileableCount_;
    for (int bitIndex = 0; bitIndex < bitCount_; ++bitIndex) {
      setTileable(patternBits | (1U << bitIndex));
    }
  }

  void setUntileable(unsigned patternBits) {
    if (knownUntileable_[patternBits]) return;
    knownUntileable_[patternBits] = true;
    ++knownUntileableCount_;
    for (int bitIndex = 0; bitIndex < bitCount_; ++bitIndex) {
      setUntileable(patternBits & (static_cast<unsigned>(knownUntileable_.size()) - 1 - (1U << bitIndex)));
    }
  }

  void setUntileableUpToSize(int size) {
    scanUnknownSubsetsOfSize(size, [this](unsigned subset) {
      knownUntileable_[subset] = true;
      ++knownUntileableCount_;
    });
  }

  Tileability tileability(unsigned patternBits) {
    if (knownTileable_[patternBits]) return Tileability::Tileable;
    else if (knownUntileable_[patternBits])
      return Tileability::Untileable;
    else
      return Tileability::Unknown;
  }

  unsigned tileableCount() { return knownTileableCount_; }

  unsigned untileableCount() { return knownUntileableCount_; }

  unsigned unknownCount() {
    return static_cast<unsigned>(knownTileable_.size()) - knownTileableCount_ - knownUntileableCount_;
  }

  std::vector<unsigned> unknownSubsetsOfSize(int size) {
    std::vector<unsigned> result;
    scanUnknownSubsetsOfSize(size, [&result](unsigned subset) { result.push_back(subset); });
    return result;
  }

 private:
  void scanUnknownSubsetsOfSize(int size, const std::function<void(unsigned)>& func) {
    std::vector<bool> visited(knownTileable_.size(), false);
    scanUnknownSubsetsOfSize(0, size, func, &visited);
  }

  void scanUnknownSubsetsOfSize(unsigned start,
                                int remainingExtensions,
                                const std::function<void(unsigned)>& func,
                                std::vector<bool>* visited) {
    if ((*visited)[start] || knownTileable_[start]) return;
    (*visited)[start] = true;
    if (!remainingExtensions) {
      if (!knownUntileable_[start]) func(start);
      return;
    }
    for (int bitIndex = 0; bitIndex < bitCount_; ++bitIndex) {
      scanUnknownSubsetsOfSize(start | (1U << bitIndex), remainingExtensions - 1, func, visited);
    }
  }
};

TilingDAG::TilingDAG(int bitCount) : implementation_(std::make_shared<Implementation>(bitCount)) {}

int TilingDAG::bitCount() { return implementation_->bitCount(); }

void TilingDAG::setTileable(unsigned patternBits) { implementation_->setTileable(patternBits); }

void TilingDAG::setUntileable(unsigned patternBits) { implementation_->setUntileable(patternBits); }

void TilingDAG::setUntileableUpToSize(int size) { implementation_->setUntileableUpToSize(size); }

TilingDAG::Tileability TilingDAG::tileability(unsigned patternBits) {
  return implementation_->tileability(patternBits);
}

unsigned TilingDAG::tileableCount() { return implementation_->tileableCount(); }

unsigned TilingDAG::untileableCount() { return implementation_->untileableCount(); }

unsigned TilingDAG::unknownCount() { return implementation_->unknownCount(); }

std::vector<unsigned> TilingDAG::unknownSubsetsOfSize(int size) { return implementation_->unknownSubsetsOfSize(size); }
