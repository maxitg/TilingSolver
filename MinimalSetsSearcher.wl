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

ImportMinimalSets[size_, maskID_] := With[{imported = Import["minimal-sets/" <> maskFileName[size, maskID]]},
  If[FailureQ[imported], imported, imported["MinimalSets"]]
];

ImportCompletedSizes[size_, maskID_] := With[{imported = Import["minimal-sets/" <> maskFileName[size, maskID]]},
  If[FailureQ[imported], imported, imported["CompletedSizes"]]
];

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

$largeGridSize = 45;

FindMinimalSets[size_, maskID_, opts : OptionsPattern[]] := Block[{
    $currentMaskSize = size, $currentMaskID = maskID}, Module[{allPatterns},
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  FindMinimalSets[
    allPatterns, $largeGridSize, maskName[size, maskID], "minimal-sets/" <> maskFileName[size, maskID], opts]
]];

ParallelFindMinimalSets[maskIDs_, shouldLogToFiles_ : True] := Module[{},
  If[shouldLogToFiles,
    If[!DirectoryQ["log"], CreateDirectory["log"]];
    fileNames = AbsoluteFileName /@ ("log/minimal-sets/" <> maskFileName[##] & @@@ maskIDs);
    If[!FileExistsQ[#], CreateFile[#]] & /@ fileNames;
  ];
  ParallelMap[
    FindMinimalSets[
      #[[1]],
      #[[2]],
      LogChannel -> If[shouldLogToFiles, File["log/minimal-sets/" <> maskFileName[#[[1]], #[[2]]]], "stdout"]] &,
    maskIDs,
    Method -> "FinestGrained"]
];

$masks32$4 = {
  {{2, 4}, 31}, {{2, 4}, 59}, {{2, 4}, 91}, {{2, 4}, 121}, {{2, 4}, 155}, {{3, 4}, 283}, {{3, 4}, 285}, {{3, 4}, 299},
  {{3, 4}, 301}, {{3, 4}, 313}, {{3, 4}, 331}, {{3, 4}, 333}, {{3, 4}, 345}, {{3, 4}, 361}, {{3, 4}, 391},
  {{3, 4}, 395}, {{3, 4}, 397}, {{3, 4}, 398}, {{3, 4}, 403}, {{3, 4}, 405}, {{3, 4}, 406}, {{3, 4}, 410},
  {{3, 4}, 412}, {{3, 4}, 419}, {{3, 4}, 422}, {{3, 4}, 425}, {{3, 4}, 426}, {{3, 4}, 433}, {{3, 4}, 434},
  {{3, 4}, 436}, {{3, 4}, 440}, {{3, 4}, 451}, {{3, 4}, 453}, {{3, 4}, 454}, {{3, 4}, 457}, {{3, 4}, 458},
  {{3, 4}, 465}, {{3, 4}, 466}, {{3, 4}, 468}, {{3, 4}, 481}, {{3, 4}, 482}, {{3, 4}, 651}, {{3, 4}, 653},
  {{3, 4}, 665}, {{3, 4}, 666}, {{3, 4}, 681}, {{3, 4}, 713}, {{3, 4}, 793}, {{3, 4}, 809}, {{3, 4}, 899},
  {{3, 4}, 901}, {{3, 4}, 902}, {{3, 4}, 905}, {{3, 4}, 906}, {{3, 4}, 1305}, {{3, 4}, 1306}, {{3, 4}, 1321},
  {{3, 4}, 1322}, {{3, 4}, 1353}, {{3, 4}, 1417}, {{3, 4}, 1561}, {{3, 4}, 1577}, {{3, 4}, 2345}, {{4, 4}, 4123},
  {{4, 4}, 4125}, {{4, 4}, 4139}, {{4, 4}, 4185}, {{4, 4}, 4231}, {{4, 4}, 4235}, {{4, 4}, 4237}, {{4, 4}, 4238},
  {{4, 4}, 4243}, {{4, 4}, 4245}, {{4, 4}, 4246}, {{4, 4}, 4250}, {{4, 4}, 4252}, {{4, 4}, 4259}, {{4, 4}, 4261},
  {{4, 4}, 4262}, {{4, 4}, 4265}, {{4, 4}, 4266}, {{4, 4}, 4273}, {{4, 4}, 4274}, {{4, 4}, 4276}, {{4, 4}, 4280},
  {{4, 4}, 4291}, {{4, 4}, 4293}, {{4, 4}, 4294}, {{4, 4}, 4297}, {{4, 4}, 4305}, {{4, 4}, 4306}, {{4, 4}, 4308},
  {{4, 4}, 4312}, {{4, 4}, 4321}, {{4, 4}, 4322}, {{4, 4}, 4483}, {{4, 4}, 4485}, {{4, 4}, 4486}, {{4, 4}, 4490},
  {{4, 4}, 4500}, {{4, 4}, 4513}, {{4, 4}, 4545}, {{4, 4}, 4546}, {{4, 4}, 4739}, {{4, 4}, 4741}, {{4, 4}, 4745},
  {{4, 4}, 4753}, {{4, 4}, 4756}, {{4, 4}, 4769}, {{4, 4}, 4801}, {{4, 4}, 4993}, {{4, 4}, 5131}, {{4, 4}, 5133},
  {{4, 4}, 5146}, {{4, 4}, 5148}, {{4, 4}, 5193}, {{4, 4}, 5251}, {{4, 4}, 5253}, {{4, 4}, 5257}, {{4, 4}, 5265},
  {{4, 4}, 5281}, {{4, 4}, 5313}, {{4, 4}, 5505}, {{4, 4}, 5761}, {{4, 4}, 6151}, {{4, 4}, 6155}, {{4, 4}, 6157},
  {{4, 4}, 6158}, {{4, 4}, 6163}, {{4, 4}, 6165}, {{4, 4}, 6166}, {{4, 4}, 6172}, {{4, 4}, 6179}, {{4, 4}, 6181},
  {{4, 4}, 6182}, {{4, 4}, 6185}, {{4, 4}, 6186}, {{4, 4}, 6194}, {{4, 4}, 6196}, {{4, 4}, 6200}, {{4, 4}, 6211},
  {{4, 4}, 6213}, {{4, 4}, 6214}, {{4, 4}, 6217}, {{4, 4}, 6228}, {{4, 4}, 6242}, {{4, 4}, 6275}, {{4, 4}, 6277},
  {{4, 4}, 6278}, {{4, 4}, 6290}, {{4, 4}, 6292}, {{4, 4}, 6305}, {{4, 4}, 6306}, {{4, 4}, 6337}, {{4, 4}, 6403},
  {{4, 4}, 6405}, {{4, 4}, 6406}, {{4, 4}, 6410}, {{4, 4}, 6418}, {{4, 4}, 6420}, {{4, 4}, 6434}, {{4, 4}, 6466},
  {{4, 4}, 6530}, {{4, 4}, 6659}, {{4, 4}, 6661}, {{4, 4}, 6662}, {{4, 4}, 6665}, {{4, 4}, 6676}, {{4, 4}, 6690},
  {{4, 4}, 6914}, {{4, 4}, 7171}, {{4, 4}, 7173}, {{4, 4}, 7177}, {{4, 4}, 7186}, {{4, 4}, 7188}, {{4, 4}, 7202},
  {{4, 4}, 7426}, {{4, 4}, 8339}, {{4, 4}, 8341}, {{4, 4}, 8345}, {{4, 4}, 8585}, {{4, 4}, 10261}, {{4, 4}, 10262},
  {{4, 4}, 10265}, {{4, 4}, 10266}, {{4, 4}, 10292}, {{4, 4}, 10386}, {{4, 4}, 10505}, {{4, 4}, 10506}, {{4, 4}, 12425},
  {{4, 4}, 14345}, {{4, 4}, 22537}};

ParallelFindMinimalSets32$4[idx___] := ParallelFindMinimalSets[$masks32$4[[idx]], False];

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

  Do[
    Do[
      WriteString[
        "stdout",
        "Tiling ", currentSize, "/", Length[minimalSets], " : ",
        currentSet, "/", Length[minimalSets[[currentSize]]], " :"];
      setsAlreadyDone[minimalSets[[currentSize, currentSet]]] = MinimalPeriodCached[allPatterns, maxPeriod][
        CanonicalPatternSet[permutations, Length[allPatterns]][minimalSets[[currentSize, currentSet]]]];
      WriteString["stdout", "\n"];
    , {currentSet, Length[minimalSets[[currentSize]]]}];
    resultAssociation = KeySort @ setsAlreadyDone;
    Put[resultAssociation, "periods/" <> maskFileName[size, maskID]];
  , {currentSize, Length[minimalSets]}];

  Print["Periods exceeding the limit: ", Count[Values[resultAssociation], _ ? MissingQ]];
  Print["Max: ", Max @ Cases[Values[resultAssociation], Except[_ ? MissingQ]]];
  resultAssociation
];
