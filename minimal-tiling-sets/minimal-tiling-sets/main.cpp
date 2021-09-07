#include <iostream>
#include <vector>

#include "Mask.hpp"

int main(int argc, const char * argv[]) {
  if (argc != 5) std::cout << "Usage: minimal-tiling-sets sizeY sizeX maskID maxGridSize" << std::endl;
  std::pair<int, int> size = std::make_pair(std::stoi(std::string(argv[1])), std::stoi(std::string(argv[2])));
  int maskID = std::stoi(std::string(argv[3]));
  int maxGridSize = std::stoi(std::string(argv[4]));
  auto mask = TilingSystem::Mask(size, maskID);
  std::vector<std::vector<bool>> minimalSets = mask.minimalSets(maxGridSize);
  std::cout << "Found " << minimalSets.size() << " minimal sets." << std::endl;
  return 0;
}
