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
  const std::chrono::milliseconds sleepBetweenUnlockTries_ = std::chrono::milliseconds(1202);
  const std::chrono::milliseconds sleepBetweenUploadTries_ = std::chrono::milliseconds(1202);

  const std::string versionKey = "Version";
  const std::string idleCPUsKey = "IdleCPUs";
  const std::string masksDoneKey = "MasksDone";
  const std::string masksInProgressKey = "MasksInProgress";
  const std::string masksPausedKey = "MasksPaused";
  const std::string masksToDoKey = "MasksToDo";

  const nlohmann::json defaultStatus = {{versionKey, expectedStatusVersion_},
                                        {idleCPUsKey, 0},
                                        {masksDoneKey, nlohmann::json::object()},
                                        {masksInProgressKey, nlohmann::json::object()},
                                        {masksPausedKey, nlohmann::json::object()},
                                        {masksToDoKey, nlohmann::json::object()}};

  const std::function<void(const nlohmann::json& msg)> cerrPrint = [](const nlohmann::json& msg) {
    std::cerr << std::string(msg["Error"]) << std::endl;
    std::cerr << msg["Response"].dump(2) << std::endl;
  };

  Dropbox& dropbox_;
  LoggingParameters loggingParameters_;
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
  std::unordered_map<std::string, nlohmann::json> maskStatus_;

  std::vector<std::thread> threads_;

  std::vector<std::weak_ptr<Mask>> maskPtrs_;

  bool terminationStarted_ = false;
  bool terminationRequested_ = false;
  bool syncInProgress_ = false;
  bool terminated_ = false;
  std::mutex terminationSyncMutex_;

 public:
  Implementation(Dropbox& dropbox, const LoggingParameters& parameters)
      : dropbox_(dropbox), loggingParameters_(parameters) {}

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
        std::cout << "Cannot terminate yet due to mask data syncing..." << std::endl;
        return false;
      }
    }
    return true;
  }

 private:
  void runOnce() {
    syncInProgress_ = true;
    // This is needed in case termination is requested just before entering this function
    if (terminationRequested_) {
      syncInProgress_ = false;
      return;
    }
    std::optional<nlohmann::json> tasks;
    if (!(tasks = dropbox_.downloadJSON("tasks.json", nlohmann::json::array(), cerrPrint))) return;

    if (!dropbox_.lockFile(statusFilename, cerrPrint)) return;

    auto status = dropbox_.downloadJSON(statusFilename, defaultStatus, cerrPrint);
    if (isValidStatus(status)) {
      importTasks(tasks.value(), &status.value());
      recordDoneTasks(&status.value());
      startJobsIfNeeded(&status.value());
      updateIdleThreads(&status.value());
      updateMaskStatus(&status.value());
      // TODO: move mask status to MasksDone, MasksInProgress and MasksPaused, and make it machine readable
      // TODO: add masks todo.
      // TODO: defend against bad json files.
      // TODO: implement provision/setup scripts.
      // TODO: convert old result jsons to the new format.
      // TODO: make sure symmetrization is used for periods.
      // TODO: update with old results again.
      while (!dropbox_.uploadJSON(statusFilename, status.value(), cerrPrint)) {
        std::this_thread::sleep_for(sleepBetweenUploadTries_);
      };
    } else {
      std::cerr << "Cannot get " + statusFilename + " of expected version " << expectedStatusVersion_
                << ". Terminating now..." << std::endl;
      requestTermination();
    }

    while (!dropbox_.unlockFile(statusFilename, cerrPrint)) {
      std::this_thread::sleep_for(sleepBetweenUnlockTries_);
    }
    syncInProgress_ = false;
    if (terminationRequested_) terminationSync();
  }

  void terminationSync() {
    std::cout << "Notifying " + statusFilename + " we are no longer available." << std::endl;
    std::lock_guard<std::mutex> terminationLock(terminationSyncMutex_);

    if (!dropbox_.lockFile(statusFilename, cerrPrint)) return;
    auto status = dropbox_.downloadJSON(statusFilename, defaultStatus, cerrPrint);
    if (isValidStatus(status)) {
      availableThreads_ = 0;
      updateIdleThreads(&status.value());
      pauseOurMasks(&status.value());
      while (!dropbox_.uploadJSON(statusFilename, status.value(), cerrPrint)) {
        std::this_thread::sleep_for(sleepBetweenUploadTries_);
      };
    }
    while (!dropbox_.unlockFile(statusFilename, cerrPrint)) {
      std::this_thread::sleep_for(sleepBetweenUnlockTries_);
    }

    terminated_ = true;
    std::cout << statusFilename + " termination update done." << std::endl;
  }

  bool isValidStatus(const std::optional<nlohmann::json>& status) {
    return status && status->count(versionKey) && status->at(versionKey).is_number_integer() &&
           status->at(versionKey) == expectedStatusVersion_;
  }

  void pauseOurMasks(nlohmann::json* status) {
    std::lock_guard<std::mutex> ourMasksInProgressLock(ourMasksInProgressMutex_);
    for (const auto& mask : ourMasksInProgress_) {
      if ((*status)[masksInProgressKey].count(mask)) {
        (*status)[masksPausedKey][mask] = (*status)[masksInProgressKey][mask];
        (*status)[masksInProgressKey].erase(mask);
      }
    }
  }

  void importTasks(const nlohmann::json& tasks, nlohmann::json* status) {
    for (const auto& task : tasks) {
      if (!(*status)[masksDoneKey].count(task) && !(*status)[masksInProgressKey].count(task) &&
          !(*status)[masksPausedKey].count(task) && !(*status)[masksToDoKey].count(task)) {
        (*status)[masksToDoKey][std::string(task)] = nlohmann::json::object();
      }
    }
  }

  void recordDoneTasks(nlohmann::json* status) {
    std::lock_guard<std::mutex> masksDoneLock(masksDoneMutex_);
    while (!masksDone_.empty()) {
      (*status)[masksDoneKey][masksDone_.front()] = maskStatus_[masksDone_.front()];
      (*status)[masksInProgressKey].erase(masksDone_.front());
      {
        std::lock_guard<std::mutex> lock(ourMasksInProgressMutex_);
        ourMasksInProgress_.erase(masksDone_.front());
      }
      std::cout << "Done: " << masksDone_.front() << std::endl;
      ++availableThreads_;
      masksDone_.pop();
    }
  }

  void updateMaskStatus(nlohmann::json* status) {
    std::lock_guard<std::mutex> lock(maskStatusMutex_);
    for (const auto& [maskName, maskStatus] : maskStatus_) {
      if ((*status)[masksInProgressKey].count(maskName)) {
        (*status)[masksInProgressKey][maskName] = maskStatus;
      }
    }
  }

  struct MaskSpec {
    std::pair<int, int> size;
    int id;
  };

  void startJobsIfNeeded(nlohmann::json* status) {
    if (!availableThreads_) return;
    std::vector<std::string> availableTasks;
    for (const auto& task : (*status)[masksPausedKey].get<nlohmann::json::object_t>()) {
      availableTasks.push_back(task.first);
    }
    for (const auto& task : (*status)[masksToDoKey].get<nlohmann::json::object_t>()) {
      availableTasks.push_back(task.first);
    }
    for (const auto& task : availableTasks) {
      if (!availableThreads_) continue;
      --availableThreads_;
      (*status)[masksInProgressKey][std::string(task)] = {{"Message", "About to start..."},
                                                          {"Time", currentWallTimeString()}};
      if ((*status)[masksPausedKey].count(task)) (*status)[masksPausedKey].erase(task);
      if ((*status)[masksToDoKey].count(task)) (*status)[masksToDoKey].erase(task);
      std::optional<MaskSpec> maskSpec;
      if (!(maskSpec = parseMaskDescription(task))) {
        std::cerr << "Invalid mask specification in tasks.json: " << task << std::endl;
        continue;
      }
      {
        std::lock_guard<std::mutex> lock(ourMasksInProgressMutex_);
        ourMasksInProgress_.insert(std::string(task));
      }
      std::cout << "Working on " << std::string(task) << "..." << std::endl;
      threads_.push_back(std::thread([this, task, maskSpec] {
        Mask::LoggingParameters parameters;
        parameters.filename = std::string(task) + ".json";
        parameters.resultsSavingPeriod = loggingParameters_.resultsSavingPeriod;
        parameters.updateStatus = [this, &task](const nlohmann::json& message) {
          std::lock_guard<std::mutex> lock(maskStatusMutex_);
          maskStatus_[task] = message;
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
