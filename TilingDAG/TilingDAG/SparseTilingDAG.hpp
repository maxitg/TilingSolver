#ifndef SparseTilingDAG_
#define SparseTilingDAG_

/* The classes below are exported */
#pragma GCC visibility push(default)

#include <memory>
#include <vector>

// Specifically designed for size-by-size evaluation.
class SparseTilingDAG {
 public:
  SparseTilingDAG(int bitCount);
  int bitCount();
  int currentSize();

  std::vector<uint64_t> unknownSubsetsOfCurrentSize();
  // The number of set bits must be currentSize()
  void setTileable(uint64_t bits);
  void setRestUntileableAndIncrementSize();

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};

#pragma GCC visibility pop
#endif
