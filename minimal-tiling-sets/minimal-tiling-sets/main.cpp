#include <iostream>
#include <thread>
#include <vector>

#include "Dropbox.hpp"
#include "MaskManager.hpp"

int main(int argc, const char* argv[]) {
  if (argc > 2) {
    std::cerr << "Usage: minimal-tiling-sets threadCount" << std::endl;
    return 2;
  }
  int threadCount;
  if (argc == 1) {
    threadCount = std::thread::hardware_concurrency();
  } else {
    try {
      threadCount = std::stoi(argv[1]);
    } catch (...) {
      std::cerr << "threadCount must be an integer." << std::endl;
      return 2;
    }
    if (threadCount <= 0) {
      std::cerr << "threadCount must be positive." << std::endl;
      return 2;
    }
  }

  const std::string dropboxAppKey = "tmlt7oeepzda36p";
  const std::string configFilename = std::string(std::getenv("HOME")) + "/.minimal-tiling-sets";

  TilingSystem::MaskManager::LoggingParameters parameters;

  // TODO: update status.json on termination

  try {
    TilingSystem::Dropbox dropbox(dropboxAppKey, configFilename);
    auto manager = TilingSystem::MaskManager(dropbox, parameters);
    manager.run(threadCount);
  } catch (TilingSystem::Dropbox::Error& error) {
    return 1;
  }
  return 0;
}
