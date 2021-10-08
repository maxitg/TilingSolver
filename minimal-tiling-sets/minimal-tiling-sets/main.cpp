#include <boost/dll/runtime_symbol_info.hpp>
#include <iostream>
#include <vector>

#include "Mask.hpp"

std::string minimalSetsPath(const std::pair<int, int>& maskSize, int maskID) {
  std::string filename =
      std::to_string(maskSize.first) + "-" + std::to_string(maskSize.second) + "-" + std::to_string(maskID) + ".json";
  return (boost::dll::program_location().parent_path().parent_path() / "minimal-sets" / filename).string();
}

int main(int argc, const char* argv[]) {
  if (argc != 4) std::cout << "Usage: minimal-tiling-sets sizeY sizeX maskID" << std::endl;
  std::pair<int, int> size = std::make_pair(std::stoi(std::string(argv[1])), std::stoi(std::string(argv[2])));
  int maskID = std::stoi(std::string(argv[3]));
  const auto filename = minimalSetsPath(size, maskID);
  std::cout << "Writing results to " << filename << std::endl;
  auto mask = TilingSystem::Mask(size, maskID, filename);
  mask.findMinimalSets();
  return 0;
}
