#!/usr/bin/env wolframscript

Check[
  currentFilename = $ScriptCommandLine[[2]];
  otherFilename = $ScriptCommandLine[[3]];
  finalFilename = $ScriptCommandLine[[4]];

  Print["Merging ", finalFilename];

  currentData = Import[currentFilename, "WL"];
  otherData = Import[otherFilename, "WL"];

  mergeResult = <|
    "CompletedSizes" -> Union[currentData["CompletedSizes"], otherData["CompletedSizes"]],
    "MinimalSets" -> Union[currentData["MinimalSets"], otherData["MinimalSets"]],
    "MinimalGridSize" -> Max[currentData["MinimalGridSize"], otherData["MinimalGridSize"]],
    "LongFiniteTilers" -> Join[currentData["LongFiniteTilers"], otherData["LongFiniteTilers"]]
  |>;

  Put[mergeResult, currentFilename];

  Print["+", Length[mergeResult["MinimalSets"]] - Length[currentData["MinimalSets"]], " minimal sets"];
,
  Print["Messages were generated during the merge. Exiting."];
  Exit[1];
];
