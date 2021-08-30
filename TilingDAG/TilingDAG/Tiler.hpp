#ifndef Tiler_
#define Tiler_

/* The classes below are exported */
#pragma GCC visibility push(default)

#include <memory>
#include <vector>

class Tiler {
 public:
  using Mask = std::vector<std::vector<bool>>;
  using PatternSet = uint64_t;

 public:
  Tiler(const Mask& mask, int gridSize, const std::vector<PatternSet>& minimalSets = {});
  void tileToSize(int maxSize);
  const std::vector<PatternSet>& minimalSets();

 private:
  class Implementation;
  std::shared_ptr<Implementation> implementation_;
};

#pragma GCC visibility pop
#endif
