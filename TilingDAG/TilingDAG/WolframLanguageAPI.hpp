#ifndef WolframLanguageAPI_hpp
#define WolframLanguageAPI_hpp

#include "WolframHeaders/WolframLibrary.h"

EXTERN_C DLLEXPORT mint WolframLibrary_getVersion();

EXTERN_C DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData);

EXTERN_C DLLEXPORT int dagInitialize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int bitCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int setTileable(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int setUntileable(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int setUntileableUpToSize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int tileability(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int tileableCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int untileableCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int unknownCount(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

EXTERN_C DLLEXPORT int unknownSubsetsOfSize(WolframLibraryData libData, mint argc, MArgument* argv, MArgument result);

#endif /* WolframLanguageAPI_hpp */
