#include "TilingDAG.hpp"
#include "SparseTilingDAG.hpp"
#include "Tiler.hpp"

#include <iostream>

int main(int argc, const char * argv[]) {
  Tiler tiler({{0, 1, 0}, {1, 1, 1}, {0, 1, 0}}, 9);
  tiler.tileToSize(32);
  const auto& minimalSets = tiler.minimalSets();
  for (auto& set : minimalSets) {
    std::cout << set << " ";
  }
  std::cout << std::endl;

  // SparseTilingDAG dag(16);
  // dag.setRestUntileableAndIncrementSize();
  // std::cout << dag.unknownSubsetsOfCurrentSize().size() << std::endl;
  // dag.setTileable(512);
  // std::cout << dag.unknownSubsetsOfCurrentSize().size();
  // for (int i = 0; i < 15; ++i) {
  //   dag.setRestUntileableAndIncrementSize();
  //   std::cout << ", " << dag.unknownSubsetsOfCurrentSize().size();
  // }
  // std::cout << std::endl;

  // TilingDAG dag(32);
  // dag.setTileable(65535);
  // // dag.setUntileableUpToSize(3);
  // std::cout << dag.untileableCount() << std::endl;
  // std::cout << dag.tileableCount() << std::endl;
  // std::cout << dag.unknownCount() << std::endl;
  return 0;
}
