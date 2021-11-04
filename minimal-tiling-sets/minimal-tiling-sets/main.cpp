#include <signal.h>

#include <iostream>
#include <thread>
#include <vector>

#include "Dropbox.hpp"
#include "MaskManager.hpp"

namespace {
std::function<void(int)> signalHandler;
void signalHandlerFunc(int signal) { signalHandler(signal); }
}  // namespace

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

  const std::string dropboxAppKey = "539qkotir8jem0d";
  const std::string configFilename = std::string(std::getenv("HOME")) + "/.minimal-tiling-sets";

  TilingSystem::MaskManager::LoggingParameters parameters;

  try {
    TilingSystem::Dropbox dropbox(dropboxAppKey, configFilename);
    auto manager = TilingSystem::MaskManager(dropbox, parameters);

    // Intercept signals to allow syncing to finish
    std::shared_ptr<std::thread> terminatorThread;
    const std::chrono::milliseconds sleepBetweenExitTries = std::chrono::milliseconds(75);
    signalHandler = [&manager, &terminatorThread, &sleepBetweenExitTries](int signal) {
      if (signal == SIGINT) std::cout << " <- ";
      std::cout << "Requesting termination. Hold on..." << std::endl;
      manager.requestTermination();
      terminatorThread = std::make_shared<std::thread>([&manager, &sleepBetweenExitTries]() {
        while (true) {
          std::this_thread::sleep_for(sleepBetweenExitTries);
          if (manager.canBeSafelyTerminated()) {
            std::cout << "Safe termination complete." << std::endl;
            exit(3);
          }
        }
      });
    };
    struct sigaction sigHandler;
    sigHandler.sa_handler = signalHandlerFunc;
    sigemptyset(&sigHandler.sa_mask);
    sigaction(SIGINT, &sigHandler, NULL);
    sigaction(SIGTERM, &sigHandler, NULL);
    sigaction(SIGQUIT, &sigHandler, NULL);

    manager.run(threadCount);
  } catch (TilingSystem::Dropbox::Error& error) {
    return 1;
  }
  return 0;
}
