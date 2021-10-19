#include "Utils.hpp"

#include <chrono>
#include <iomanip>
#include <random>
#include <sstream>
#include <string>

std::string currentWallTimeString() {
  std::time_t now_t = std::time(nullptr);
  std::stringstream timeStream;
  timeStream << std::put_time(std::gmtime(&now_t), "%FT%T.");

  const auto now = std::chrono::system_clock::now();
  const auto secondsSinceEpoch = std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch());
  const std::string milliseconds = std::to_string(
      (std::chrono::duration_cast<std::chrono::milliseconds>(now.time_since_epoch() - secondsSinceEpoch)).count());

  return timeStream.str() + std::string(3 - milliseconds.length(), '0') + milliseconds + "Z";
}

std::string randomString() {
  std::random_device rd;
  std::mt19937 generator(rd());
  std::uniform_int_distribution dist(32, 126);
  std::string result;
  for (int i = 0; i < 32; ++i) {
    result.push_back(dist(generator));
  }
  return result;
}
