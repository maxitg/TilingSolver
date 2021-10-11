#include <iostream>
#include <vector>

#include "DropboxInit.hpp"
#include "Mask.hpp"

std::string minimalSetsPath(const std::pair<int, int>& maskSize, int maskID, const std::string& dataDirectory) {
  std::string filename =
      std::to_string(maskSize.first) + "-" + std::to_string(maskSize.second) + "-" + std::to_string(maskID) + ".json";
  return dataDirectory + "/" + filename;
}

int main(int argc, const char* argv[]) {
  if (argc != 4) std::cout << "Usage: minimal-tiling-sets sizeY sizeX maskID" << std::endl;
  std::pair<int, int> size = std::make_pair(std::stoi(std::string(argv[1])), std::stoi(std::string(argv[2])));
  int maskID = std::stoi(std::string(argv[3]));

  const std::string dropboxAppKey = "tmlt7oeepzda36p";
  const std::string configFilename = std::string(std::getenv("HOME")) + "/.minimal-tiling-sets";

  TilingSystem::Mask::LoggingParameters parameters;
  try {
    TilingSystem::DropboxInit dropboxInit(dropboxAppKey, configFilename);
    parameters.dropboxAppKey = dropboxAppKey;
    parameters.dropboxFilename = minimalSetsPath(size, maskID, dropboxInit.dataDirectory());
    parameters.dropboxRefreshToken = dropboxInit.refreshToken();
  } catch (TilingSystem::DropboxInit::Error& error) {
    return 1;
  }

  std::cout << "Writing results to " << parameters.dropboxFilename << std::endl;
  auto mask = TilingSystem::Mask(size, maskID, parameters);
  mask.findMinimalSets();
  return 0;
}
