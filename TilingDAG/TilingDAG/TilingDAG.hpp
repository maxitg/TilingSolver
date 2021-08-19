#ifndef TilingDAG_
#define TilingDAG_

/* The classes below are exported */
#pragma GCC visibility push(default)

#include <memory>
#include <vector>

class TilingDAG {
 public:
  enum class Tileability { Unknown = 0, Tileable = 1, Untileable = 2 };

  TilingDAG(int bitCount = 32);
  int bitCount();
  void setTileable(unsigned patternBits);
  void setUntileable(unsigned patternBits);
  void setUntileableUpToSize(int size);
  Tileability tileability(unsigned patternBits);
  unsigned tileableCount();
  unsigned untileableCount();
  unsigned unknownCount();
  std::vector<unsigned> unknownSubsetsOfSize(int size);

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};

#pragma GCC visibility pop
#endif
