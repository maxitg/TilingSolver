(* Tiling solver *)

ClearAll[$statePattern];
$statePattern = {{(0 | 1 | Verbatim[_])...}...};

ClearAll[$patternsPattern];
$patternsPattern = {$statePattern ...};

ClearAll[TilingStatePlot];
Options[TilingStatePlot] = Options[ArrayPlot];
TilingStatePlot[state : $statePattern, opts : OptionsPattern[]] :=
  ArrayPlot[state, opts, Mesh -> True, PlotRangePadding -> 0, ColorRules -> {0 -> White, 1 -> Black, _ -> Gray}];

ClearAll[TilingPatternPlot];
Options[TilingPatternPlot] = Options[TilingStatePlot];
TilingPatternPlot[patterns : $patternsPattern, opts : OptionsPattern[]] :=
  TilingStatePlot[#, opts, ImageSize -> 50] & /@ patterns;

Tile::usage = "Tile[x, y] represents a tile at x, y coordinates.";

ClearAll[SingleTileLogicalExpression];
SingleTileLogicalExpression[patterns : $patternsPattern] := Module[{variables},
  variables = Table[Tile[i, j], {i, 1, Length @ patterns[[1]]}, {j, 1, Length @ patterns[[1, 1]]}];
  Or @@ And @@@ (
    Catenate /@ Map[Thread, Thread /@ (variables -> # & /@ patterns), {2}] /.
      {(cell_ -> 1) :> cell, (cell_ -> 0) :> !cell, (cell_ -> Verbatim[_]) :> Nothing})
];

displacedLogicalExpression[expr_, {dx_, dy_}] := expr /. {Tile[x_, y_] :> Tile[x + dx, y + dy]};

positionNormalize[rule_] := Module[{allCoordinates, offset},
  allCoordinates = Transpose[List @@@ Cases[rule, _Tile, All]];
  offset = Min /@ allCoordinates;
  displacedLogicalExpression[rule, 1 - offset]
];

Boundary::usage =
  "Boundary is an option for GenerateTiling specifying what should happen at the boundary. " <>
  "Possible values are \"Any\" and \"Periodic\".";

singlePatternExpressionToGrid[gridSize_][singleTileExpression_] := Module[{i, j},
  And @@ Catenate @ Table[
    displacedLogicalExpression[singleTileExpression, {i, j}],
    {i, 0, gridSize[[1]] - 1},
    {j, 0, gridSize[[2]] - 1}]
];

ClearAll[GenerateTiling];
Options[GenerateTiling] = {Boundary -> "Any"};
GenerateTiling[patterns : Except[{}, $patternsPattern],
               init : $statePattern,
               size : {_Integer, _Integer},
               count_Integer : Automatic,
               OptionsPattern[]] /;
      And @@ (And @@ Thread[size > Replace[Dimensions[#], {0} -> {0, 0}]] & /@ {patterns[[1]], init}) := Module[{
    patternSize, extendedPatternExpression, initExpression, variables, i, j, depth,
    boundaryExpression, solutionList, initX, initY, takeTop, takeBottom, takeLeft, takeRight},
  patternSize = Dimensions[patterns[[1]]];
  initX = Ceiling[(size[[1]] + 1) / 2] - Floor[Dimensions[init][[1]] / 2];
  initY = Ceiling[(size[[2]] + 1) / 2] - Floor[Dimensions[init][[-1]] / 2];
  extendedPatternExpression = singlePatternExpressionToGrid[size][SingleTileLogicalExpression[patterns]];
  initExpression = displacedLogicalExpression[SingleTileLogicalExpression[{init}], {initX, initY}];
  variables = Catenate @ Table[
    Tile[i, j], {i, size[[1]] + patternSize[[1]] - 1}, {j, size[[2]] + patternSize[[2]] - 1}];
  boundaryExpression = Switch[OptionValue[Boundary],
    "Any",
      True,
    "Periodic",
      And @@ Catenate @ Table[
        Tile[i, depth] && Tile[i, size[[2]] + depth] || !Tile[i, depth] && !Tile[i, size[[2]] + depth],
        {i, size[[1]] + patternSize[[1]] - 1},
        {depth, patternSize[[2]] - 1}] &&
      And @@ Catenate @ Table[
        Tile[depth, i] && Tile[size[[1]] + depth, i] || !Tile[depth, i] && !Tile[size[[1]] + depth, i],
        {i, size[[2]] + patternSize[[2]] - 1},
        {depth, patternSize[[1]] - 1}]];
  solutionList = SatisfiabilityInstances[
    extendedPatternExpression && initExpression && boundaryExpression,
    variables,
    Replace[count, Automatic -> 1],
    Method -> "SAT"];
  If[solutionList === {}, Return[Failure["NotTileable", <||>], Module]];
  takeTop = 1 + Quotient[patternSize[[1]] - 1, 2];
  takeBottom = takeTop - 1 - patternSize[[1]];
  takeLeft = 1 + Quotient[patternSize[[2]] - 1, 2];
  takeRight = takeLeft - 1 - patternSize[[2]];
  If[count === Automatic, First, Identity][
    Boole[Normal @ SparseArray[Thread[List @@@ positionNormalize[variables] -> #]]][[
        takeTop ;; takeBottom, takeLeft ;; takeRight]] & /@
      solutionList]
];

GenerateTiling[patterns_, init_, size_Integer, rest___] := GenerateTiling[patterns, init, {size, size}, rest];

GenerateTiling[{}, ___] := Failure["NotTileable", <||>];

(* Searcher *)

TilingPattern::usage = "TilingPattern[i] represents a pattern with index i.";

SingleTileAndPatternLogicalExpression[pattern_, patternVariable_, tileVariables_] := Module[{},
  And @@ Replace[Thread[tileVariables -> Catenate @ pattern],
          {(cell_ -> 1) :> cell, (cell_ -> 0) :> !cell, (cell_ -> Verbatim[_]) :> Nothing},
          {1}] && patternVariable
];

ClearAll[SingleTileFlaggedLogicalExpression];
SingleTileFlaggedLogicalExpression[patterns : $patternsPattern] := Module[{tileVariables},
  tileVariables = Catenate @ Table[Tile[i, j], {i, Length @ patterns[[1]]}, {j, Length @ patterns[[1, 1]]}];
  Or @@ MapIndexed[SingleTileAndPatternLogicalExpression[#, TilingPattern[#2[[1]]], tileVariables] &, patterns]
];

ClearAll[disallowMinimalSetExpression];
disallowMinimalSetExpression[patternCount_Integer, minimalSet_Integer] := Module[{},
  Not[And @@ TilingPattern /@ First /@ Position[IntegerDigits[minimalSet, 2, patternCount], 1]]
];

ClearAll[FindTileableSet];
FindTileableSet[patterns : $patternsPattern, knownMinimalSets : {_Integer...}, gridSize_Integer] /;
      And @@ Thread[gridSize > Dimensions[patterns[[1]]]] := Module[{
    patternSize, extendedPatternExpression,
    disableKnownMinimalSets, i, j, patternVariables, tileVariables, variables, solutionList},
  patternSize = Dimensions[patterns[[1]]];
  extendedPatternExpression = singlePatternExpressionToGrid[{gridSize, gridSize}][
    SingleTileFlaggedLogicalExpression[patterns]];
  disableKnownMinimalSets = And @@ (disallowMinimalSetExpression[Length @ patterns, #] & /@ knownMinimalSets);
  patternVariables = TilingPattern /@ Range @ Length @ patterns;
  tileVariables = Catenate @ Table[
    Tile[i, j], {i, gridSize + patternSize[[1]] - 1}, {j, gridSize + patternSize[[2]] - 1}];
  variables = Join[patternVariables, tileVariables];
  solutionList = SatisfiabilityInstances[
    disableKnownMinimalSets && extendedPatternExpression,
    variables,
    Method -> "SAT"];
  If[solutionList === {}, Return[Failure["NotTileable", <||>], Module]];
  FromDigits[Boole @ Take[First @ solutionList, Length @ patterns], 2]
];

MinUntileablePowerOfTwo[patterns_, maxGridSize_] :=
  SelectFirst[FailureQ[GenerateTiling[patterns, {}, #]] &][
    Select[# > Max[Dimensions[patterns[[1]]]] &][2^Range[Ceiling @ Log2[maxGridSize]]]];

ClearAll[smallerTileableSet];
smallerTileableSet[gridSize_][patterns_] :=
  SelectFirst[! FailureQ[GenerateTiling[#, {}, gridSize]] &] @ Subsets[patterns, {Length[patterns] - 1}];

ClearAll[ReduceToMinimalSet];
ReduceToMinimalSet[gridSize_][patterns_] :=
  FixedPoint[Replace[smallerTileableSet[gridSize][#], _ ? MissingQ -> #] &, patterns];

ReduceToMinimalSet[allPatterns_, gridSize_Integer][initialSet_Integer] :=
  PatternSetToNumber[allPatterns] @ ReduceToMinimalSet[gridSize] @ NumberToPatternSet[allPatterns][initialSet];

PatternSetToNumber[allPatterns_][set_] :=
  Total @ (2^(Map[First @ FirstPosition[Reverse @ allPatterns, #] &, set, {1}] - 1));

NumberToPatternSet[allPatterns_][number_] :=
  allPatterns[[First /@ Position[IntegerDigits[number, 2, Length[allPatterns]], 1]]];

patternTrim[pattern_] := FixedPoint[Replace[{
  {{Verbatim[_]...}, x___} :> {x},
  {x___, {Verbatim[_]...}} :> {x},
  x : {{Verbatim[_], ___}...} :> Rest /@ x,
  x : {{___, Verbatim[_]}...} :> Most /@ x
}], pattern];

shiftPatternRows[pattern_] :=
  MapIndexed[Join[Table[_, (#2[[1]] - 1)], #, Table[_, (Length[pattern] - #2[[1]])]] &, pattern];

$patternSymmetryGenerators = {Reverse, Transpose, shiftPatternRows, Replace[#, {0 -> 1, 1 -> 0}, {2}] &};

GetSymmetryPermutations[allPatterns_] := GetSymmetryPermutations[allPatterns] = Module[{transformedPatterns},
  transformedPatterns = Select[Sort[#] === Sort[allPatterns] &] @ FixedPoint[
    Union @ Join[
      Catenate[
        Function[patterns,
          Select[AllTrue[Max[Dimensions[#]] <= Max @ Dimensions[allPatterns[[1]]] &]] @
            (patternTrim /@ # /@ patterns & /@ $patternSymmetryGenerators)
        ] /@ #],
      #] &,
    {allPatterns}];
  Sort @ Map[First @ FirstPosition[allPatterns, #] &, transformedPatterns, {2}]
];

CanonicalPatternSet[symmetryPermutations_, subsetSize_][subsetInt_] := With[{
    digits = IntegerDigits[subsetInt, 2, subsetSize]},
  FromDigits[First[Sort[Permute[digits, #] & /@ symmetryPermutations]], 2]
];

CanonicalPatternSetQ[symmetryPermutations_, subsetSize_][subsetInt_] := With[{
    digits = IntegerDigits[subsetInt, 2, subsetSize]},
  First[Sort[Permute[digits, #] & /@ symmetryPermutations]] === digits
];

AddSymmetricPatterns[symmetryPermutations_, subsetSize_][numbers_] := Union[
  FromDigits[#, 2] & /@
    Catenate @ Outer[Permute, IntegerDigits[#, 2, subsetSize] & /@ numbers, symmetryPermutations, 1]];

maskName[size_, maskID_] := ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID];

maskFileName[size_, maskID_] := maskName[size, maskID] <> ".m";

ImportMinimalSets[size_, maskID_] := Import["minimal-sets/" <> maskFileName[size, maskID]]["MinimalSets"];

ImportCompletedSizes[size_, maskID_] := Import["minimal-sets/" <> maskFileName[size, maskID]]["CompletedSizes"];

ImportMinimalPeriods[size_, maskID_] := Import["periods/" <> maskFileName[size, maskID]];

CanonicalMinimalSets[size_, maskID_] := Module[{minimalSets, allPatterns, permutations},
  minimalSets = ImportMinimalSets[size, maskID];
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  permutations = GetSymmetryPermutations[allPatterns];
  Select[CanonicalPatternSetQ[permutations, Length[allPatterns]]] @ minimalSets;
];

(* Periodicity *)

MinimalPeriod[maxPeriod_][patterns_] := Module[{minPeriod},
  minPeriod = Max[Dimensions[First[patterns]]] + 1;
  SelectFirst[(
      WriteString["stdout", " ", #];
      !FailureQ[GenerateTiling[patterns, {}, #, Boundary -> "Periodic"]]
    ) &][Range[minPeriod, maxPeriod]]
];

MinimalPeriodCached[allPatterns_, maxPeriod_][setNumber_] := MinimalPeriodCached[allPatterns, maxPeriod][setNumber] =
  MinimalPeriod[maxPeriod][NumberToPatternSet[allPatterns][setNumber]];

(* Reporting *)

ProgressForMask[{size_, maskID_}] := Module[{},
  fileName = "minimal-sets/" <> maskFileName[size, maskID];
  If[FileExistsQ[fileName],
    progressData = Import[fileName];
    completedSizes = progressData["CompletedSizes"];
    sizeCounts = CountsBy[progressData["MinimalSets"], Count[IntegerDigits[#, 2], 1] &];
    sizeCountsList = Lookup[sizeCounts, #, 0] & /@ Range[Max[completedSizes] + 1];
  ,
    completedSizes = {0};
    sizeCountsList = {};
  ];
  ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ": " <>
    ToString[Max[completedSizes] + 1] <> ": " <> ToString[sizeCountsList]
];

ExportProgressForMasks[fileName_, masks_] := Export[fileName, Map[ProgressForMask, masks]];

(* Main - FindMinimalSets *)

idToMask[size_, maskID_] := Partition[IntegerDigits[maskID, 2, Times @@ size], size[[2]]];

maskToAllPatterns[mask_] := With[{
    functionBody = Module[{n = 0}, mask /. {0 -> _, 1 :> Slot[++n]}]},
  Function[functionBody] @@@ Tuples[{1, 0}, Count[Catenate[mask], 1]]
];

LoggingPeriod::usage = "Time period to do tiling between disk writes.";

logFindMinimalSetsStatus[maskName_, gridSize_, countsPerSize_, channel_] := WriteString[
  channel,
  maskName <> ": #" <> ToString[gridSize] <> ", " <> ToString[Total[countsPerSize]] <> ": " <>
  Replace[countsPerSize, {0, data___, Longest[0 ...]} :> (StringRiffle[ToString /@ {data}, " "] <> " <- " <> ToString[Length[{data}]])] <>
  "\n"];

ExportMinimalSets[fileName_, completedSizes_, minimalSets_, minimalGridSize_, longFiniteTilers_] := Put[<|
  "CompletedSizes" -> Sort @ completedSizes,
  "MinimalSets" -> Sort @ minimalSets,
  "MinimalGridSize" -> minimalGridSize,
  "LongFiniteTilers" -> longFiniteTilers|>, fileName];

ClearAll[FindMinimalSets];
Options[FindMinimalSets] = {LogChannel -> "stdout", LoggingPeriod -> Quantity[1, "Minutes"]};
FindMinimalSets[patterns : $patternsPattern, gridSize_Integer, maskName_String, fileName_String, OptionsPattern[]] /;
      And @@ Thread[gridSize > Dimensions[patterns[[1]]]] := Module[{
    minimalSets, completedSizes, currentSet, minimalSet, countsPerSize, currentGridSize, latestDiskOperation,
    symmetries, newSets, longFiniteTilers},
  symmetries = GetSymmetryPermutations[patterns];
  If[FileExistsQ[fileName],
    minimalSets = AddSymmetricPatterns[symmetries, Length @ patterns] @ Import[fileName]["MinimalSets"];
    completedSizes = Lookup[Import[fileName], "CompletedSizes", {0}];
    currentGridSize = Lookup[Import[fileName], "MinimalGridSize", Max @ Dimensions[patterns[[1]]] + 1];
    longFiniteTilers = Lookup[Import[fileName], "LongFiniteTilers", <||>];
  ,
    minimalSets = {};
    completedSizes = {0};
    currentGridSize = Max @ Dimensions[patterns[[1]]] + 1;
    longFiniteTilers = <||>;
  ];
  latestDiskOperation = Now;
  countsPerSize = KeySort @ Join[
    Association @ Thread[Range[0, Length @ patterns] -> 0], CountsBy[minimalSets, Count[IntegerDigits[#, 2], 1] &]];
  While[!FailureQ[currentSet = FindTileableSet[patterns, minimalSets, currentGridSize]],
    minimalSet = ReduceToMinimalSet[patterns, currentGridSize][currentSet];
    If[FailureQ[GenerateTiling[NumberToPatternSet[patterns][minimalSet], {}, gridSize]],
      longFiniteTilers[currentGridSize] = minimalSet;
      ++currentGridSize;
      logFindMinimalSetsStatus[maskName, currentGridSize, Values @ countsPerSize, OptionValue[LogChannel]];
    ,
      newSets = AddSymmetricPatterns[symmetries, Length @ patterns][{minimalSet}];
      minimalSets = Join[minimalSets, newSets];
      If[Now > latestDiskOperation + OptionValue[LoggingPeriod],
        ExportMinimalSets[fileName, completedSizes, minimalSets, currentGridSize, longFiniteTilers];
        latestDiskOperation = Now;
      ];
      countsPerSize[Count[IntegerDigits[minimalSet, 2], 1]] += Length[newSets];
      logFindMinimalSetsStatus[maskName, currentGridSize, Values @ countsPerSize, OptionValue[LogChannel]];
    ];
  ];
  ExportMinimalSets[fileName, Range[0, Length[patterns]], minimalSets, currentGridSize, longFiniteTilers];
  minimalSets
];

$largeGridSize = 32;

FindMinimalSets[size_, maskID_, opts : OptionsPattern[]] := Block[{
    $currentMaskSize = size, $currentMaskID = maskID}, Module[{allPatterns},
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  FindMinimalSets[
    allPatterns, $largeGridSize, maskName[size, maskID], "minimal-sets/" <> maskFileName[size, maskID], opts]
]];

ParallelFindMinimalSets[maskIDs_] := Module[{},
  If[!DirectoryQ["log"], CreateDirectory["log"]];
  fileNames = "log/minimal-sets/" <> maskFileName[##] & @@@ maskIDs;
  If[!FileExistsQ[#], CreateFile[#]] & /@ fileNames;
  ParallelMap[
    FindMinimalSets[#[[1]], #[[2]], LogChannel -> File["log/minimal-sets/" <> maskFileName[#[[1]], #[[2]]]]] &, maskIDs]
];

(* Main - FindMinimalPeriods *)

FindMinimalPeriods[maxPeriod_][size_, maskID_] := Module[{
    allPatterns, minimalSetsFlat, groupedMinimalSets, minimalSets, permutations, setsAlreadyDone, minimalPeriods,
    resultAssociation},
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  minimalSetsFlat = ImportMinimalSets[size, maskID];
  groupedMinimalSets = GroupBy[minimalSetsFlat, Count[IntegerDigits[#, 2], 1] &];
  minimalSets = Lookup[groupedMinimalSets, #, {}] & /@ Range[Length[allPatterns]];
  permutations = GetSymmetryPermutations[allPatterns];
  If[FileExistsQ["periods/" <> maskFileName[size, maskID]],
    setsAlreadyDone = Import["periods/" <> maskFileName[size, maskID]];
    minimalSets = Select[MissingQ[setsAlreadyDone[#]] &] /@ minimalSets;
  ,
    setsAlreadyDone = <||>;
  ];
  minimalPeriods = Table[(
    WriteString["stdout",
                "Tiling ",
                currentSize,
                "/",
                Length[minimalSets],
                " : ",
                currentSet,
                "/",
                Length[minimalSets[[currentSize]]],
                " :"];
    WithCleanup[
      MinimalPeriodCached[allPatterns, maxPeriod][
        CanonicalPatternSet[permutations, Length[allPatterns]][minimalSets[[currentSize, currentSet]]]]
    ,
      WriteString["stdout", "\n"];
    ]
  ), {currentSize, Length[minimalSets]}, {currentSet, Length[minimalSets[[currentSize]]]}];
  resultAssociation = KeySort @ Join[
    setsAlreadyDone, Association @ Thread[Catenate[minimalSets] -> Catenate[minimalPeriods]]];
  Put[resultAssociation, "periods/" <> maskFileName[size, maskID]];
  Print["Periods exceeding the limit: ", Count[Values[resultAssociation], _ ? MissingQ]];
  Print["Max: ", Max @ Cases[Values[resultAssociation], Except[_ ? MissingQ]]];
  resultAssociation
];
