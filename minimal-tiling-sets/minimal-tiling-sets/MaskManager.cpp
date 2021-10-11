#include "MaskManager.hpp"

#include <mutex>
#include <nlohmann/json.hpp>
#include <queue>
#include <string>
#include <thread>
#include <unordered_set>

#include "Mask.hpp"

namespace TilingSystem {
class MaskManager::Implementation {
 private:
  const int expectedStatusVersion_ = 1;

  const std::chrono::milliseconds sleepBetweenStatusUpdates_ = std::chrono::milliseconds(19237);
  const std::chrono::milliseconds sleepBetweenUnlockTries_ = std::chrono::milliseconds(1202);
  const std::chrono::milliseconds sleepBetweenUploadTries_ = std::chrono::milliseconds(1202);

  const std::string idleCPUsKey = "IdleCPUs";
  const std::string masksDoneKey = "MasksDone";
  const std::string masksInProgressKey = "MasksInProgress";

  const nlohmann::json defaultStatus = {
      {idleCPUsKey, 0}, {masksDoneKey, nlohmann::json::array()}, {masksInProgressKey, nlohmann::json::array()}};

  const std::function<void(const std::string& msg)> cerrPrint = [](const std::string& msg) { std::cerr << msg; };

  Dropbox& dropbox_;
  LoggingParameters loggingParameters_;
  std::chrono::time_point<std::chrono::steady_clock> lastProgressLogTime_ =
      std::chrono::time_point<std::chrono::steady_clock>::min();

  int availableThreads_;
  int lastReportedAvailableThreads_ = 0;

  std::mutex masksDoneMutex_;
  std::queue<std::string> masksDone_;

  std::vector<std::thread> threads_;

 public:
  Implementation(Dropbox& dropbox, const LoggingParameters& parameters)
      : dropbox_(dropbox), loggingParameters_(parameters) {}

  void run(int threadCount) {
    availableThreads_ = threadCount;
    while (true) {
      runOnce();
      std::this_thread::sleep_for(sleepBetweenStatusUpdates_);
    }
  }

 private:
  void runOnce() {
    std::optional<nlohmann::json> tasks;
    if (!(tasks = dropbox_.downloadJSON("tasks.json", nlohmann::json::array(), cerrPrint))) return;

    if (!dropbox_.lockFile("status.json", cerrPrint)) return;

    auto status = dropbox_.downloadJSON("status.json", defaultStatus, cerrPrint);
    if (status) {
      recordDoneTasks(&status.value());
      startJobsIfNeeded(tasks.value(), &status.value());
      updateIdleThreads(&status.value());
      // TODO: sort MasksDone and MasksInProgress.
      // TODO: add version to status.
      // TODO: figure out what to do with progress monitoring.
      // TODO: hide lock files.
      while (!dropbox_.uploadJSON("status.json", status.value(), cerrPrint)) {
        std::this_thread::sleep_for(sleepBetweenUploadTries_);
      };
    }

    while (!dropbox_.unlockFile("status.json", cerrPrint)) {
      std::this_thread::sleep_for(sleepBetweenUnlockTries_);
    }
  }

  void recordDoneTasks(nlohmann::json* status) {
    std::lock_guard<std::mutex> masksDoneLock(masksDoneMutex_);
    std::unordered_set<std::string> newMasksInProgress;
    for (const auto& mask : (*status)[masksInProgressKey]) {
      newMasksInProgress.insert(std::string(mask));
    }
    while (!masksDone_.empty()) {
      (*status)[masksDoneKey].push_back(masksDone_.front());
      newMasksInProgress.erase(masksDone_.front());
      ++availableThreads_;
      masksDone_.pop();
    }
    (*status)[masksInProgressKey] = newMasksInProgress;
  }

  struct MaskSpec {
    std::pair<int, int> size;
    int id;
  };

  void startJobsIfNeeded(const nlohmann::json& tasks, nlohmann::json* status) {
    std::unordered_set<std::string> masksDone;
    for (auto& task : (*status)[masksDoneKey]) {
      masksDone.insert(std::string(task));
    }
    std::unordered_set<std::string> masksInProgress;
    for (auto& task : (*status)[masksInProgressKey]) {
      masksInProgress.insert(std::string(task));
    }
    for (const auto& task : tasks) {
      if (!availableThreads_ || masksDone.count(task) || masksInProgress.count(task))
        continue;
      --availableThreads_;
      (*status)[masksInProgressKey].push_back(task);
      std::optional<MaskSpec> maskSpec;
      if (!(maskSpec = parseMaskDescription(task))) {
        std::cerr << "Invalid mask specification in tasks.json: " << task << std::endl;
        continue;
      }
      threads_.push_back(std::thread([this, task, maskSpec] {
        Mask::LoggingParameters parameters;
        parameters.filename = std::string(task) + ".json";
        parameters.progressLoggingPeriod = loggingParameters_.progressLoggingPeriod;
        parameters.progressStream = loggingParameters_.progressStream;
        parameters.resultsSavingPeriod = loggingParameters_.resultsSavingPeriod;
        Mask mask(maskSpec->size, maskSpec->id, dropbox_, parameters);
        mask.findMinimalSets();
        std::lock_guard<std::mutex> masksDoneLock(masksDoneMutex_);
        masksDone_.push(task);
      }));
    }
  }

  static std::optional<MaskSpec> parseMaskDescription(const std::string& description) {
    std::vector<int> values;

    std::function<bool(const std::string&)> checkAndAddValue = [&values](const std::string& str) {
      int value;
      try {
        value = std::stoi(str);
      } catch (...) {
        return false;
      }
      values.push_back(value);
      return true;
    };

    size_t last = 0;
    size_t next = 0;
    while ((next = description.find("-", last)) != std::string::npos) {
      if (!checkAndAddValue(description.substr(last, next - last))) return std::nullopt;
      last = next + 1;
    }
    if (!checkAndAddValue(description.substr(last))) return std::nullopt;
    if (values.size() != 3) return std::nullopt;
    return MaskSpec{std::make_pair(values[0], values[1]), values[2]};
  }

  void updateIdleThreads(nlohmann::json* status) {
    (*status)[idleCPUsKey] =
        static_cast<int>((*status)[idleCPUsKey]) + (availableThreads_ - lastReportedAvailableThreads_);
    lastReportedAvailableThreads_ = availableThreads_;
  }
};

MaskManager::MaskManager(Dropbox& dropbox, const MaskManager::LoggingParameters& parameters)
    : implementation_(std::make_shared<Implementation>(dropbox, parameters)) {}

void MaskManager::run(int threadCount) { implementation_->run(threadCount); }
}  // namespace TilingSystem
