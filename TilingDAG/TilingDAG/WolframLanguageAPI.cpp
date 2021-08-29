#include "WolframLanguageAPI.hpp"

#include <memory>
#include <unordered_map>

#include "TilingDAG.hpp"
#include "SparseTilingDAG.hpp"

using SystemID = mint;
std::unordered_map<SystemID, std::unique_ptr<TilingDAG>> tilingDAGs_;
std::unordered_map<SystemID, std::unique_ptr<SparseTilingDAG>> sparseTilingDAGs_;

void tilingDAGManageInstance([[maybe_unused]] WolframLibraryData libData, mbool mode, mint id) {
  if (mode == 0) {
    tilingDAGs_.emplace(id, nullptr);
  } else {
    tilingDAGs_.erase(id);
  }
}

void sparseTilingDAGManageInstance([[maybe_unused]] WolframLibraryData libData, mbool mode, mint id) {
  if (mode == 0) {
    sparseTilingDAGs_.emplace(id, nullptr);
  } else {
    sparseTilingDAGs_.erase(id);
  }
}

EXTERN_C mint WolframLibrary_getVersion() { return WolframLibraryVersion; }

EXTERN_C int WolframLibrary_initialize(WolframLibraryData libData) {
  int dagError = (*libData->registerLibraryExpressionManager)("TilingDAG", tilingDAGManageInstance);
  if (dagError != LIBRARY_NO_ERROR) return dagError;
  int sparseDAGError = (*libData->registerLibraryExpressionManager)("SparseTilingDAG", sparseTilingDAGManageInstance);
  if (sparseDAGError != LIBRARY_NO_ERROR) return sparseDAGError;
  return LIBRARY_NO_ERROR;
}

EXTERN_C void WolframLibrary_uninitialize(WolframLibraryData libData) {}

EXTERN_C int dagInitialize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    int bitCount = static_cast<int>(MArgument_getInteger(argv[1]));
    tilingDAGs_[thisSystemID] = std::make_unique<TilingDAG>(bitCount);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C DLLEXPORT int sparseDAGInitialize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    int bitCount = static_cast<int>(MArgument_getInteger(argv[1]));
    sparseTilingDAGs_[thisSystemID] = std::make_unique<SparseTilingDAG>(bitCount);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int bitCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    MArgument_setInteger(result, tilingDAGs_[thisSystemID]->bitCount());
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int sparseDAGBitCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    MArgument_setInteger(result, sparseTilingDAGs_[thisSystemID]->bitCount());
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int currentSise(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    MArgument_setInteger(result, sparseTilingDAGs_[thisSystemID]->currentSize());
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int setTileable(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    unsigned patternBits = static_cast<unsigned>(MArgument_getInteger(argv[1]));
    tilingDAGs_[thisSystemID]->setTileable(patternBits);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int sparseDAGSetTileable(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    unsigned patternBits = static_cast<unsigned>(MArgument_getInteger(argv[1]));
    sparseTilingDAGs_[thisSystemID]->setTileable(patternBits);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int setUntileable(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    unsigned patternBits = static_cast<unsigned>(MArgument_getInteger(argv[1]));
    tilingDAGs_[thisSystemID]->setUntileable(patternBits);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int setUntileableUpToSize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    int size = static_cast<int>(MArgument_getInteger(argv[1]));
    tilingDAGs_[thisSystemID]->setUntileableUpToSize(size);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int setRestUntileableAndIncrementSize(
    WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    sparseTilingDAGs_[thisSystemID]->setRestUntileableAndIncrementSize();
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int tileability(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    unsigned patternBits = static_cast<unsigned>(MArgument_getInteger(argv[1]));
    MArgument_setInteger(result, static_cast<int>(tilingDAGs_[thisSystemID]->tileability(patternBits)));
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int tileableCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    MArgument_setInteger(result, static_cast<int64_t>(tilingDAGs_[thisSystemID]->tileableCount()));
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int untileableCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    MArgument_setInteger(result, static_cast<int64_t>(tilingDAGs_[thisSystemID]->untileableCount()));
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int unknownCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    MArgument_setInteger(result, static_cast<int64_t>(tilingDAGs_[thisSystemID]->unknownCount()));
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int unknownSubsetsOfSize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 2) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    int size = static_cast<int>(MArgument_getInteger(argv[1]));
    const auto subsets = tilingDAGs_[thisSystemID]->unknownSubsetsOfSize(size);
    MTensor output;
    const mint dimensions[1] = {static_cast<mint>(subsets.size())};
    libData->MTensor_new(MType_Integer, 1, dimensions, &output);
    mint writeIndex = 0;
    mint position[1];
    for (const auto subset : subsets) {
      position[0] = ++writeIndex;
      libData->MTensor_setInteger(output, position, subset);
    }
    MArgument_setMTensor(result, output);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}

EXTERN_C int unknownSubsetsOfCurrentSize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result) {
  if (argc != 1) return LIBRARY_FUNCTION_ERROR;

  try {
    SystemID thisSystemID = MArgument_getInteger(argv[0]);
    const auto subsets = sparseTilingDAGs_[thisSystemID]->unknownSubsetsOfCurrentSize();
    MTensor output;
    // each uint64_t has to be converted into two mints, as LibraryLink does not support uints.
    const mint dimensions[1] = {static_cast<mint>(2 * subsets.size())};
    libData->MTensor_new(MType_Integer, 1, dimensions, &output);
    mint writeIndex = 0;
    mint position[1];
    for (const auto subset : subsets) {
      position[0] = ++writeIndex;
      libData->MTensor_setInteger(output, position, static_cast<uint32_t>(subset));
      position[0] = ++writeIndex;
      libData->MTensor_setInteger(output, position, subset >> 32);
    }
    MArgument_setMTensor(result, output);
  } catch (...) {
    return LIBRARY_FUNCTION_ERROR;
  }

  return LIBRARY_NO_ERROR;
}
