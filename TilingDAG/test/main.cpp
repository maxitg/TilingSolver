#include "TilingDAG.hpp"

#include <iostream>

int main(int argc, const char * argv[]) {
  TilingDAG dag(32);
  dag.setTileable(65535);
  // dag.setUntileableUpToSize(3);
  std::cout << dag.untileableCount() << std::endl;
  std::cout << dag.tileableCount() << std::endl;
  std::cout << dag.unknownCount() << std::endl;
  return 0;
}
