#!/usr/bin/env wolframscript

Check[
  currentFilename = $ScriptCommandLine[[2]];
  otherFilename = $ScriptCommandLine[[3]];
  finalFilename = $ScriptCommandLine[[4]];

  WriteString["stdout", "Merging ", finalFilename, ": "];

  currentData = Import[currentFilename, "WL"];
  otherData = Import[otherFilename, "WL"];

  mergeResult = <|
    "CompletedSizes" -> Union[currentData["CompletedSizes"], otherData["CompletedSizes"]],
    "MinimalSets" -> Union[currentData["MinimalSets"], otherData["MinimalSets"]],
    "MinimalGridSize" -> Max[currentData["MinimalGridSize"], otherData["MinimalGridSize"]],
    "LongFiniteTilers" -> Join[currentData["LongFiniteTilers"], otherData["LongFiniteTilers"]]
  |>;

  Put[mergeResult, currentFilename];

  WriteString["stdout",
    "+", Length[mergeResult["MinimalSets"]] - Length[currentData["MinimalSets"]], " minimal sets, ",
    "+", mergeResult["MinimalGridSize"] - currentData["MinimalGridSize"], " grid size",
    If[Length[mergeResult["CompletedSizes"]] > Length[currentData["CompletedSizes"]],
      ", and more completed sizes"
    ,
      ""
    ],
    "\n"];
,
  Print["Messages were generated during the merge. Exiting."];
  Exit[1];
];
