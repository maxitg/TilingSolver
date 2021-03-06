#include "MaskManager.hpp"

#include <mutex>
#include <nlohmann/json.hpp>
#include <queue>
#include <string>
#include <thread>
#include <unordered_set>

#include "Mask.hpp"
#include "Utils.hpp"

namespace TilingSystem {
class MaskManager::Implementation {
 private:
  const int expectedStatusVersion_ = 1;

  const std::chrono::milliseconds sleepBetweenStatusUpdates_ = std::chrono::milliseconds(19237);
  const std::chrono::milliseconds sleepBetweenLockTries_ = std::chrono::milliseconds(1202);
  const std::chrono::milliseconds sleepBetweenUnlockTries_ = std::chrono::milliseconds(1202);
  const std::chrono::milliseconds sleepBetweenUploadTries_ = std::chrono::milliseconds(1202);

  const std::string versionKey = "Version";
  const std::string idleCPUsKey = "IdleCPUs";
  const std::string masksDoneKey = "MasksDone";
  const std::string masksInProgressKey = "MasksInProgress";

  const nlohmann::json defaultStatus = {{versionKey, expectedStatusVersion_},
                                        {idleCPUsKey, 0},
                                        {masksDoneKey, nlohmann::json::array()},
                                        {masksInProgressKey, nlohmann::json::array()}};

  Dropbox& dropbox_;
  LoggingParameters loggingParameters_;
  std::string lockCode_;
  std::chrono::time_point<std::chrono::steady_clock> lastProgressLogTime_ =
      std::chrono::time_point<std::chrono::steady_clock>::min();

  const std::string statusFilename = ".status.json";

  int availableThreads_;
  int lastReportedAvailableThreads_ = 0;

  std::mutex masksDoneMutex_;
  std::queue<std::string> masksDone_;
  std::mutex ourMasksInProgressMutex_;
  std::unordered_set<std::string> ourMasksInProgress_;

  std::mutex maskStatusMutex_;

  std::vector<std::thread> threads_;

  std::vector<std::weak_ptr<Mask>> maskPtrs_;

  bool terminationStarted_ = false;
  bool terminationRequested_ = false;
  bool syncInProgress_ = false;
  bool terminated_ = false;
  std::mutex terminationSyncMutex_;

  std::mutex printMutex_;

 public:
  Implementation(Dropbox& dropbox, const LoggingParameters& parameters)
      : dropbox_(dropbox), loggingParameters_(parameters), lockCode_(randomString()) {}

  void run(int threadCount) {
    availableThreads_ = threadCount;
    while (!terminationRequested_) {
      runOnce();
      std::this_thread::sleep_for(sleepBetweenStatusUpdates_);
    }
  }

  void requestTermination() {
    if (terminationStarted_) return;
    terminationStarted_ = true;
    for (const auto& maskPtr : maskPtrs_) {
      auto sharedPtr = maskPtr.lock();
      if (sharedPtr) sharedPtr->requestTermination();
    }
    terminationRequested_ = true;
    if (!syncInProgress_) terminationSync();
  }

  bool canBeSafelyTerminated() {
    if (!terminated_) return false;
    for (const auto& maskPtr : maskPtrs_) {
      auto sharedPtr = maskPtr.lock();
      if (sharedPtr && !sharedPtr->canBeSafelyTerminated()) {
        return false;
      }
    }
    return true;
  }

 private:
  std::function<void(const nlohmann::json& msg)> cerrPrintFunction() {
    return [this](const nlohmann::json& msg) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << msg.dump(2) << std::endl;
    };
  }

  void runOnce() {
    if (!availableThreads_ && masksDone_.empty()) return;
    syncInProgress_ = true;
    // This is needed in case termination is requested just before entering this function
    if (terminationRequested_) {
      syncInProgress_ = false;
      return;
    }
    std::optional<nlohmann::json> tasks;
    if (!(tasks = dropbox_.downloadJSON("tasks.json", nlohmann::json::array(), cerrPrintFunction()))) return;

    while (!dropbox_.lockFile(statusFilename, lockCode_, cerrPrintFunction())) {
      std::this_thread::sleep_for(sleepBetweenLockTries_);
    }
    auto status = dropbox_.downloadJSON(statusFilename, defaultStatus, cerrPrintFunction());
    if (isValidStatus(status)) {
      addMissingKeys(&status.value());
      recordDoneTasks(&status.value());
      startJobsIfNeeded(tasks.value(), &status.value());
      updateIdleThreads(&status.value());
      sortMasks(&status.value());
      while (!dropbox_.uploadJSON(statusFilename, status.value(), cerrPrintFunction())) {
        std::this_thread::sleep_for(sleepBetweenUploadTries_);
      };
    } else {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << "Cannot get " + statusFilename + " of expected version " << expectedStatusVersion_ << std::endl;
    }

    while (!dropbox_.unlockFile(statusFilename, cerrPrintFunction())) {
      std::this_thread::sleep_for(sleepBetweenUnlockTries_);
    }
    syncInProgress_ = false;
    if (terminationRequested_) terminationSync();
  }

  void terminationSync() {
    std::cout << "Notifying " + statusFilename + " we are no longer available." << std::endl;
    std::lock_guard<std::mutex> terminationLock(terminationSyncMutex_);

    while (!dropbox_.lockFile(statusFilename, lockCode_, cerrPrintFunction())) {
      std::this_thread::sleep_for(sleepBetweenLockTries_);
    }
    auto status = dropbox_.downloadJSON(statusFilename, defaultStatus, cerrPrintFunction());
    if (isValidStatus(status)) {
      addMissingKeys(&status.value());
      availableThreads_ = 0;
      updateIdleThreads(&status.value());
      removeOurMasks(&status.value());
      sortMasks(&status.value());
      while (!dropbox_.uploadJSON(statusFilename, status.value(), cerrPrintFunction())) {
        std::this_thread::sleep_for(sleepBetweenUploadTries_);
      };
    }
    while (!dropbox_.unlockFile(statusFilename, cerrPrintFunction())) {
      std::this_thread::sleep_for(sleepBetweenUnlockTries_);
    }

    terminated_ = true;
    std::cout << statusFilename + " termination update done." << std::endl;
  }

  bool isValidStatus(const std::optional<nlohmann::json>& status) {
    return status && status->count(versionKey) && status->at(versionKey).is_number_integer() &&
           status->at(versionKey) == expectedStatusVersion_;
  }

  void removeOurMasks(nlohmann::json* status) {
    std::lock_guard<std::mutex> ourMasksInProgressLock(ourMasksInProgressMutex_);

    if (!(*status)[masksInProgressKey].is_array()) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << masksInProgressKey << " in " << statusFilename << " must be an array." << std::endl;
      std::cerr << "Cannot stop running masks, " << statusFilename << " will be inconsistent." << std::endl;
      return;
    }

    std::unordered_set<std::string> newMasksInProgress;
    for (const auto& mask : (*status)[masksInProgressKey]) {
      if (!ourMasksInProgress_.count(mask)) newMasksInProgress.insert(std::string(mask));
    }
    (*status)[masksInProgressKey] = newMasksInProgress;
  }

  void addMissingKeys(nlohmann::json* status) {
    if (!(*status).count(idleCPUsKey)) (*status)[idleCPUsKey] = 0;
    if (!(*status).count(masksDoneKey)) (*status)[masksDoneKey] = nlohmann::json::array();
    if (!(*status).count(masksInProgressKey)) (*status)[masksInProgressKey] = nlohmann::json::array();
  }

  void recordDoneTasks(nlohmann::json* status) {
    std::lock_guard<std::mutex> masksDoneLock(masksDoneMutex_);

    if (!(*status)[masksDoneKey].is_array()) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << masksDoneKey << " in " << statusFilename << " is not an array." << std::endl;
      std::cerr << "Cannot update tasks as done." << std::endl;
      return;
    }
    if (!(*status)[masksInProgressKey].is_array()) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << masksInProgressKey << " in " << statusFilename << " is not an array." << std::endl;
      std::cerr << "Cannot update tasks as done." << std::endl;
      return;
    }

    std::unordered_set<std::string> newMasksInProgress;
    for (const auto& mask : (*status)[masksInProgressKey]) {
      newMasksInProgress.insert(std::string(mask));
    }
    while (!masksDone_.empty()) {
      (*status)[masksDoneKey].push_back(masksDone_.front());
      newMasksInProgress.erase(masksDone_.front());
      {
        std::lock_guard<std::mutex> lock(ourMasksInProgressMutex_);
        ourMasksInProgress_.erase(masksDone_.front());
      }
      {
        std::lock_guard<std::mutex> lock(printMutex_);
        std::cout << "Done: " << masksDone_.front() << std::endl;
      }
      ++availableThreads_;
      masksDone_.pop();
    }
    (*status)[masksInProgressKey] = newMasksInProgress;
  }

  void sortMasks(nlohmann::json* status) {
    auto comparison = [](const std::string& first, const std::string& second) {
      auto firstMask = parseMaskDescription(first);
      auto secondMask = parseMaskDescription(second);
      if (!firstMask || !secondMask) return first < second;
      if (firstMask->size.first != secondMask->size.first) return firstMask->size.first < secondMask->size.first;
      if (firstMask->size.second != secondMask->size.second) return firstMask->size.second < secondMask->size.second;
      return firstMask->id < secondMask->id;
    };
    std::sort((*status)[masksDoneKey].begin(), (*status)[masksDoneKey].end(), comparison);
    std::sort((*status)[masksInProgressKey].begin(), (*status)[masksInProgressKey].end(), comparison);
  }

  struct MaskSpec {
    std::pair<int, int> size;
    int id;
  };

  void startJobsIfNeeded(const nlohmann::json& tasks, nlohmann::json* status) {
    if (!availableThreads_) return;

    if (!(*status)[masksDoneKey].is_array()) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << masksDoneKey << " is not an array in " << statusFilename << std::endl;
      std::cerr << "Cannot work on new or resume masks." << std::endl;
      return;
    }
    std::unordered_set<std::string> masksDone;
    for (auto& task : (*status)[masksDoneKey]) {
      masksDone.insert(std::string(task));
    }
    if (!(*status)[masksInProgressKey].is_array()) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << masksInProgressKey << " is not an array in " << statusFilename << std::endl;
      std::cerr << "Cannot work on new or resume masks." << std::endl;
      return;
    }
    std::unordered_set<std::string> masksInProgress;
    for (auto& task : (*status)[masksInProgressKey]) {
      masksInProgress.insert(std::string(task));
    }
    if (!tasks.is_array()) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << "tasks.json is not an array." << std::endl;
      std::cerr << "Cannot work on new or resume masks." << std::endl;
      return;
    }
    for (const auto& task : tasks) {
      if (!availableThreads_ || masksDone.count(task) || masksInProgress.count(task)) continue;
      --availableThreads_;
      (*status)[masksInProgressKey].push_back(task);
      std::optional<MaskSpec> maskSpec;
      if (!(maskSpec = parseMaskDescription(task))) {
        std::lock_guard<std::mutex> lock(printMutex_);
        std::cerr << "Skipping invalid mask specification: " << task << std::endl;
        continue;
      }
      {
        std::lock_guard<std::mutex> lock(ourMasksInProgressMutex_);
        ourMasksInProgress_.insert(std::string(task));
      }
      {
        std::lock_guard<std::mutex> lock(printMutex_);
        std::cout << "Working on " << std::string(task) << "..." << std::endl;
      }
      threads_.push_back(std::thread([this, task, maskSpec] {
        Mask::LoggingParameters parameters;
        parameters.filename = std::string(task) + ".json";
        parameters.statusFilename = "." + std::string(task) + ".status.json";
        parameters.resultsSavingPeriod = loggingParameters_.resultsSavingPeriod;
        parameters.updateStatus = [](const nlohmann::json& status) { return; };
        parameters.logError = [this](const nlohmann::json& error) {
          std::lock_guard<std::mutex> lock(printMutex_);
          std::cerr << error.dump(2) << std::endl;
        };
        std::shared_ptr<Mask> mask = std::make_shared<Mask>(maskSpec->size, maskSpec->id, dropbox_, parameters);
        maskPtrs_.push_back(mask);
        mask->findMinimalSets();
        std::lock_guard<std::mutex> masksDoneLock(masksDoneMutex_);
        if (mask->isDone()) masksDone_.push(task);
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
    if (!(*status)[idleCPUsKey].is_number_integer() || (*status)[idleCPUsKey] < 0) {
      std::lock_guard<std::mutex> lock(printMutex_);
      std::cerr << idleCPUsKey << " must be a non-negative integer in " << statusFilename << std::endl;
      std::cerr << "IdleCPU count will be inconsistent." << std::endl;
      return;
    }
    (*status)[idleCPUsKey] =
        static_cast<int>((*status)[idleCPUsKey]) + (availableThreads_ - lastReportedAvailableThreads_);
    lastReportedAvailableThreads_ = availableThreads_;
  }
};

MaskManager::MaskManager(Dropbox& dropbox, const MaskManager::LoggingParameters& parameters)
    : implementation_(std::make_shared<Implementation>(dropbox, parameters)) {}

void MaskManager::run(int threadCount) { implementation_->run(threadCount); }

void MaskManager::requestTermination() { implementation_->requestTermination(); }

bool MaskManager::canBeSafelyTerminated() { return implementation_->canBeSafelyTerminated(); }
}  // namespace TilingSystem
