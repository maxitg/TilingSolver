(* ParallelMapMonitored *)

Options[ParallelMapMonitored] = Options[MapMonitored] = Options[mapFuncMonitored] = Join[{
  "Label" -> Automatic,
  "UpdateInterval" -> 1,
  "Debug" -> False,
  "Profile" -> False,
  TimeConstraint -> \[Infinity],
  MemoryConstraint -> \[Infinity]
}, Options[ParallelMap]];

ParallelMapMonitored::warn = "Warning generated for input `1` of `2`.";

ParallelMapMonitored::timo = "Evaluation timed out for input `1` of `2`.";

ParallelMapMonitored::memo = "Evaluation ran out of memory for input `1` of `2`.";

printStatusUpdate[label_, exprLength_, startTime_][globalCounter_] := Print[If[globalCounter > 0,
  StringTemplate["`1`: .`2` [`3` / `4`] ETA: `5`"][
    label,
    IntegerString[Floor[10^3 globalCounter / exprLength], 10, 3],
    globalCounter,
    exprLength,
    DateString[startTime + (AbsoluteTime[] - startTime) exprLength/globalCounter]
  ],
  StringTemplate["`1`: .`2` [`3` / `4`]"][label, "000", 0, exprLength]
]];

mapFuncMonitored[mapFunc_, f_, expr_, o : OptionsPattern[]] := Module[{
    globalCounter = 0, localCounter = 0, globalStartTime = Infinity, localStartTime = Infinity, exprLength,
    lastCheckedTime, singleOutput, parallelLock, label, updateInterval, warningQ},
  SetSharedVariable[globalCounter, globalStartTime];
  exprLength = Length[expr];
  label = If[StringQ[OptionValue["Label"]], OptionValue["Label"], "Evaluating ParallelMap"];
  lastCheckedTime = AbsoluteTime[];
  If[mapFunc === ParallelMap,
    ParallelEvaluate[lastCheckedTime = AbsoluteTime[], DistributedContexts -> Automatic]
  ];
  updateInterval = OptionValue["UpdateInterval"] * If[mapFunc === ParallelMap, $KernelCount, 1];
  If[mapFunc === ParallelMap,
    ParallelEvaluate[updateInterval = OptionValue["UpdateInterval"] * $KernelCount + $KernelID]
  ];
  printStatusUpdate[label, exprLength, globalStartTime][globalCounter];
  mapFunc[(
    If[localStartTime === Infinity, localStartTime = AbsoluteTime[]];
    warningQ = False;
    singleOutput = MemoryConstrained[
      TimeConstrained[
        If[OptionValue["Profile"], Timing, Identity][If[OptionValue["Debug"],
          Quiet[Check[f[#], warningQ = True; f[#]]],
          f[#]
        ]],
        OptionValue[TimeConstraint],
        Message[ParallelMapMonitored::timo, #, label];
        $Aborted
      ],
      OptionValue[MemoryConstraint],
      Message[ParallelMapMonitored::memo, #, label];
      $Aborted
    ];
    If[warningQ, Message[ParallelMapMonitored::warn, #, label]];
    localCounter++;
    If[AbsoluteTime[] - lastCheckedTime > updateInterval,
      CriticalSection[{parallelLock},
        globalStartTime = Min[globalStartTime, localStartTime];
        globalCounter += localCounter;
        printStatusUpdate[label, exprLength, globalStartTime][globalCounter];
      ];
      localCounter = 0;
      lastCheckedTime = AbsoluteTime[];
    ];
    singleOutput
  ) &, expr, FilterRules[{DistributedContexts -> Automatic, o, Options[ParallelMapMonitored]}, Options[mapFunc]]]
];

ParallelMapMonitored[f_, expr_, o : OptionsPattern[]] := Module[{
    globalCounter = 0, localCounter = 0, globalStartTime = Infinity, localStartTime = Infinity, exprLength,
    lastCheckedTime, singleOutput, parallelLock, label, updateInterval, warningQ, result},
  SetSharedVariable[globalCounter, globalStartTime];
  exprLength = Length[expr];
  label = If[StringQ[OptionValue["Label"]], OptionValue["Label"], "Evaluating ParallelMap"];
  lastCheckedTime = AbsoluteTime[];
  ParallelEvaluate[lastCheckedTime = AbsoluteTime[], DistributedContexts -> Automatic];
  updateInterval = OptionValue["UpdateInterval"] * If[mapFunc === ParallelMap, $KernelCount, 1];
  ParallelEvaluate[updateInterval = OptionValue["UpdateInterval"] * $KernelCount + $KernelID];
  printStatusUpdate[label, exprLength, globalStartTime][globalCounter];
  CreateDirectory["parallel-tasks"];
  Table[
    Put[expr[[Min[kernel, Length @ expr] ;; -1 ;; $KernelCount]], "parallel-tasks/task-" <> ToString[kernel] <> ".m"],
    {kernel, $KernelCount}];
  result = OperatorApplied[Take][Length @ expr] @ Catenate @ Transpose @ PadRight @ ParallelEvaluate[
    myTasks = Import["parallel-tasks/task-" <> ToString[$KernelID] <> ".m"];
    Map[(
      If[localStartTime === Infinity, localStartTime = AbsoluteTime[]];
      warningQ = False;
      singleOutput = MemoryConstrained[
        TimeConstrained[
          If[OptionValue["Profile"], Timing, Identity][If[OptionValue["Debug"],
            Quiet[Check[f[#], warningQ = True; f[#]]],
            f[#]
          ]],
          OptionValue[TimeConstraint],
          Message[ParallelMapMonitored::timo, #, label];
          $Aborted
        ],
        OptionValue[MemoryConstraint],
        Message[ParallelMapMonitored::memo, #, label];
        $Aborted
      ];
      If[warningQ, Message[ParallelMapMonitored::warn, #, label]];
      localCounter++;
      If[AbsoluteTime[] - lastCheckedTime > updateInterval,
        CriticalSection[{parallelLock},
          globalStartTime = Min[globalStartTime, localStartTime];
          globalCounter += localCounter;
          printStatusUpdate[label, exprLength, globalStartTime][globalCounter];
        ];
        localCounter = 0;
        lastCheckedTime = AbsoluteTime[];
      ];
      singleOutput
    ) &, myTasks]
  ];
  DeleteDirectory["parallel-tasks", DeleteContents -> True];
  result
];

MapMonitored[args___] := mapFuncMonitored[Map, args];

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

ClearAll[GenerateTiling];
Options[GenerateTiling] = {Boundary -> "Any"};
GenerateTiling[patterns : $patternsPattern,
               init : $statePattern,
               size_Integer,
               count_Integer : Automatic,
               opts : OptionsPattern[]] := Module[{
    patternSize, verticalExtension, horizontalExtension, extendedPatternExpression, initExpression, i, j, variables,
    boundaryExpression, solutionList, initX, initY, singleTileLogicalExpression},
  patternSize = Dimensions[patterns[[1]]];
  verticalExtension = size - patternSize[[1]] + 2;
  horizontalExtension = size - patternSize[[2]] + 2;
  initX = Ceiling[(size + 1) / 2] - Floor[Dimensions[init][[1]] / 2];
  initY = Ceiling[(size + 1) / 2] - Floor[Dimensions[init][[-1]] / 2];
  singleTileLogicalExpression = SingleTileLogicalExpression[patterns];
  extendedPatternExpression = And @@ Catenate @ Table[
    displacedLogicalExpression[singleTileLogicalExpression, {i, j}],
    {i, 0, verticalExtension},
    {j, 0, horizontalExtension}];
  initExpression = displacedLogicalExpression[SingleTileLogicalExpression[{init}], {initX, initY}];
  variables = Catenate @ Table[Tile[i, j], {i, size + 2}, {j, size + 2}];
  boundaryExpression = Switch[OptionValue[Boundary],
    "Any",
      True,
    "Periodic",
      And @@ Table[
        (Tile[i, 2] && Tile[i, size + 2] || !Tile[i, 2] && !Tile[i, size + 2]) &&
          (Tile[i, 1] && Tile[i, size + 1] || !Tile[i, 1] && !Tile[i, size + 1]) &&
          (Tile[2, i] && Tile[size + 2, i] || !Tile[2, i] && !Tile[size + 2, i]) &&
          (Tile[1, i] && Tile[size + 1, i] || !Tile[1, i] && !Tile[size + 1, i]),
        {i, 1, size + 2}]];
  solutionList = SatisfiabilityInstances[
    extendedPatternExpression && initExpression && boundaryExpression,
    variables,
    Replace[count, Automatic -> 1],
    Method -> "SAT"];
  If[solutionList === {}, Return[Failure["NotTileable", <||>], Module]];
  If[count === Automatic, First, Identity][
    Boole[Normal @ SparseArray[Thread[List @@@ positionNormalize[variables] -> #]]][[2 ;; -2, 2 ;; -2]] & /@
      solutionList]
];

(* TilingDAG *)

Needs["CCompilerDriver`"]

lib = CCompilerDriver`CreateLibrary[FileNames["~/git/TilingSolver/TilingDAG/TilingDAG/*pp"],
                                    "TilingDAG",
                                    "CleanIntermediate" -> True,
                                    "Language" -> "C++",
                                    "CompileOptions" -> "-std=c++17"];

dagInitialize = LibraryFunctionLoad[lib, "dagInitialize", {Integer, Integer}, "Void"];

bitCount = LibraryFunctionLoad[lib, "bitCount", {Integer}, Integer];

setTileable = LibraryFunctionLoad[lib, "setTileable", {Integer, Integer}, "Void"];

setUntileable = LibraryFunctionLoad[lib, "setUntileable", {Integer, Integer}, "Void"];

setUntileableUpToSize = LibraryFunctionLoad[lib, "setUntileableUpToSize", {Integer, Integer}, "Void"];

tileability = LibraryFunctionLoad[lib, "tileability", {Integer, Integer}, Integer];

tileableCount = LibraryFunctionLoad[lib, "tileableCount", {Integer}, Integer];

untileableCount = LibraryFunctionLoad[lib, "untileableCount", {Integer}, Integer];

unknownCount = LibraryFunctionLoad[lib, "unknownCount", {Integer}, Integer];

unknownSubsetsOfSize = LibraryFunctionLoad[lib, "unknownSubsetsOfSize", {Integer, Integer}, {Integer, 1}];

TilingDAG::usage = "TilingDAG[$$] represents tileability information in a DAG of subsets.";

CreateTilingDAG[bitCount_] := Module[{expr},
  expr = CreateManagedLibraryExpression["TilingDAG", TilingDAG];
  dagInitialize[First[expr], bitCount];
  expr
];

$tilingDAGIcon = Graphics[
  GraphicsComplex[
    {{0.1, -3.31951456589972}, {-0.14816751450286603`, -2.625037331552915}, {0.6310524421714278, -1.3},
     {0.9405108616213151, -2.8841601437046225`}, {0.4967448863824806, -2.092358403567382},
     {-0.846735323402297, -1.466588600696043}, {0.8846460183439665, -0.5107506168284197},
     {1.8939086566530445`, -2.50980168725566}, {1.756629266633539, -3.4622764737192444`},
     {2.119361963550152, -2.99}, {-0.5709741939515942, -4.632295267644082},
     {0.20977925607671288`, -4.647162049737781}, {-1.0861820131541373`, -4.047493574735101},
     {-1.2223073729506904`, -2.2040562174063485`}},
    {Hue[0.6, 0.7, 0.5],
     Opacity[0.7],
     Arrowheads[0.],
     Arrow[
       {{1, 2}, {1, 4}, {1, 11}, {1, 12}, {1, 13}, {2, 3}, {2, 4}, {2, 5}, {2, 6}, {2, 14}, {3, 4}, {3, 7}, {4, 5},
        {4, 8}, {4, 9}, {8, 10}, {9, 10}},
       0.0378698213750627],
     {Hue[0.6, 0.2, 0.8], EdgeForm[{GrayLevel[0], Opacity[0.7]}], Disk[1, 0.05], Disk[2, 0.05], Disk[3, 0.05],
      Disk[4, 0.05], Disk[5, 0.05], Disk[6, 0.05], Disk[7, 0.05], Disk[8, 0.05], Disk[9, 0.05], Disk[10, 0.05],
      Disk[11, 0.05], Disk[12, 0.05], Disk[13, 0.05], Disk[14, 0.05]}}],
  AspectRatio -> 1,
  Background -> GrayLevel[0.93],
  Frame -> True,
  FrameStyle -> Directive[Opacity[0.5], Thickness[Tiny], RGBColor[0.368417, 0.506779, 0.709798]],
  FrameTicks -> None,
  ImagePadding -> 0,
  ImageSize -> Dynamic[{Automatic, 3.5 (CurrentValue["FontCapHeight"]/AbsoluteCurrentValue[Magnification])}],
  PlotRange -> {{-1.1, 2.4}, {-4.4, -0.7}}];

TilingDAG /: MakeBoxes[object : TilingDAG[id_], format_] := ModuleScope[
  BoxForm`ArrangeSummaryBox[
    TilingDAG,
    object,
    $tilingDAGIcon,
    (* Always displayed *)
    {{BoxForm`SummaryItem[{"Bit count: ", bitCount[id]}]},
     {BoxForm`SummaryItem[{"Tileable: ",
                           Row[{tileableCount[id], "(", 100. * tileableCount[id] / 2^bitCount[id], "%)"}]}]},
     {BoxForm`SummaryItem[{"Untileable: ",
                           Row[{untileableCount[id], "(", 100. * untileableCount[id] / 2^bitCount[id], "%)"}]}]},
     {BoxForm`SummaryItem[{"Unknown: ",
                           Row[{unknownCount[id], "(", 100. * unknownCount[id] / 2^bitCount[id], "%)"}]}]}},
    (* Displayed on request *)
    {},
    format,
    "Interpretable" -> Automatic
  ]
];

SetTileable[obj : TilingDAG[id_], bitPattern_] := (
  setTileable[id, bitPattern];
  obj
);

SetUntileable[obj : TilingDAG[id_], bitPattern_] := (
  setUntileable[id, bitPattern];
  obj
);

SetUntileableUpToSize[obj : TilingDAG[id_], size_] := (
  setUntileableUpToSize[id, size];
  obj
);

UnknownSubsets[obj : TilingDAG[id_], size_] := unknownSubsetsOfSize[id, size];

sparseDagInitialize = LibraryFunctionLoad[lib, "sparseDAGInitialize", {Integer, Integer}, "Void"];

sparseDAGBitCount = LibraryFunctionLoad[lib, "sparseDAGBitCount", {Integer}, Integer];

currentSize = LibraryFunctionLoad[lib, "currentSize", {Integer}, Integer];

unknownSubsetsOfCurrentSize = LibraryFunctionLoad[lib, "unknownSubsetsOfCurrentSize", {Integer}, {Integer, 1}];

sparseDAGSetTileable = LibraryFunctionLoad[lib, "sparseDAGSetTileable", {Integer, Integer, Integer}, "Void"];

setRestUntileableAndIncrementSize = LibraryFunctionLoad[lib, "setRestUntileableAndIncrementSize", {Integer}, "Void"];

SparseTilingDAG::usage = "SparseTilingDAG[$$] represents tileability information in a DAG of subsets.";

CreateSparseTilingDAG[bitCount_] := Module[{expr},
  expr = CreateManagedLibraryExpression["SparseTilingDAG", SparseTilingDAG];
  sparseDagInitialize[First[expr], bitCount];
  expr
];

SparseTilingDAG /: MakeBoxes[object : SparseTilingDAG[id_], format_] := ModuleScope[
  BoxForm`ArrangeSummaryBox[
    SparseTilingDAG,
    object,
    $tilingDAGIcon,
    (* Always displayed *)
    {{BoxForm`SummaryItem[{"Bit count: ", sparseDAGBitCount[id]}]},
     {BoxForm`SummaryItem[{"Current size: ", currentSize[id]}]}},
    (* Displayed on request *)
    {},
    format,
    "Interpretable" -> Automatic
  ]
];

BitCount[obj : SparseTilingDAG[id_]] := sparseDAGBitCount[id];

CurrentSize[obj : SparseTilingDAG[id_]] := currentSize[id];

UnknownSubsetsOfCurrentSize[obj : SparseTilingDAG[id_]] := Module[{halfLengthInts},
  halfLengthInts = unknownSubsetsOfCurrentSize[id];
  #[[1]] + #[[2]] * 2^32 & /@ Partition[halfLengthInts, {2}]
];

SetTileable[obj : SparseTilingDAG[id_], bitPattern_] := (
  sparseDAGSetTileable[id, BitAnd[bitPattern, 2^32 - 1], BitShiftRight[bitPattern, 32]];
  obj
);

SetRestUntileableAndIncrementSize[obj : SparseTilingDAG[id_]] := (
  setRestUntileableAndIncrementSize[id];
  obj
);

(* Searcher *)

PatternSetToNumber[allPatterns_][set_] := Total @ (2^(Map[First @ FirstPosition[allPatterns, #] &, set, {1}] - 1));

NumberToPatternSet[allPatterns_][number_] :=
  allPatterns[[First /@ Position[Reverse @ IntegerDigits[number, 2, Length[allPatterns]], 1]]];

(* Zero implies its tileable to maxGridSize *)
MinUntileablePowerOfTwo[allPatterns_, patternNumber_, init_, maxGridSize_, patternSize_] := With[{
    patterns = NumberToPatternSet[allPatterns][patternNumber]},
  SelectFirst[
    Select[# > patternSize &][2^Range[Ceiling @ Log2[maxGridSize]]],
    FailureQ[GenerateTiling[patterns, init, #]] &,
    0]
];

$minMinUntileableSizeToLog = 32;

SuccessfulTilings[allPatterns_, patternNumbers_, maxGridSize_, init_, patternSize_] := Module[{
    minUntileablePowersOfTwo, sizesSoFar, sizesFilename, maskSizeString, sizesToWrite,
    maskString = StringRiffle[Join[ToString /@ $currentMaskSize, {$currentMaskID}], "-"]},
  maskSizeString = maskString <> "-" <> ToString[$currentSetSize];
  minUntileablePowersOfTwo = ParallelMapMonitored[
    MinUntileablePowerOfTwo[allPatterns, #, init, maxGridSize, patternSize] &,
    patternNumbers,
    "Label" -> ("Tiling " <> maskSizeString)];
  If[!DirectoryQ["untileable-sizes"], CreateDirectory["untileable-sizes"]];
  sizesFilename = "untileable-sizes/" <> maskString <> ".m";
  sizesSoFar = If[FileExistsQ[sizesFilename], Import[sizesFilename], <||>];
  sizesToWrite = Join[
    sizesSoFar,
    <|$currentSetSize -> KeyMap[NumberToPatternSet[allPatterns]] @ Select[# >= $minMinUntileableSizeToLog &] @
      Association @ Thread[patternNumbers -> minUntileablePowersOfTwo]|>];
  Put[sizesToWrite, sizesFilename];
  Pick[patternNumbers, EqualTo[0] /@ minUntileablePowersOfTwo]
];

patternTrim[pattern_] := FixedPoint[Replace[{
  {{Verbatim[_]...}, x___} :> {x},
  {x___, {Verbatim[_]...}} :> {x},
  x : {{Verbatim[_], ___}...} :> Rest /@ x,
  x : {{___, Verbatim[_]}...} :> Most /@ x
}], pattern];

$symmetryTransforms = Composition @@@ Tuples[{
  {Identity, Reverse},
  {Identity, Map[Reverse]},
  {Identity, Transpose},
  {Identity, Replace[#, {0 -> 1, 1 -> 0}, {2}] &}}];

shiftPatternRows[pattern_] :=
  MapIndexed[Join[Table[_, (#2[[1]] - 1)], #, Table[_, (Length[pattern] - #2[[1]])]] &, pattern];

$patternSymmetryGenerators = {Reverse, Transpose, shiftPatternRows, Replace[#, {0 -> 1, 1 -> 0}, {2}] &};

GetSymmetryPermutations[allPatterns_] := GetSymmetryPermutations[allPatterns] = Module[{transformedPatterns},
  transformedPatterns = Select[Sort[#] === Sort[allPatterns] &] @ FixedPoint[
    Union @ Join[
      Catenate[
        Function[patterns,
          Select[AllTrue[Max[Dimensions[#]] <= Times @@ Dimensions[allPatterns[[1]]] &]] @
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

TilingsIntsOfSize[inputPatterns_, tilingDAG_] := Module[{
    allSubsetInts, symmetryPermutations, allSubsets, canonicalQ, printCell},
  (* Generate all subsets *)
  allSubsetInts = UnknownSubsetsOfCurrentSize[tilingDAG];
  symmetryPermutations = GetSymmetryPermutations[inputPatterns];
  Print["Found symmetries: ", Length @ symmetryPermutations];
  canonicalQ = ParallelMapMonitored[
    CanonicalPatternSetQ[symmetryPermutations, Length[inputPatterns]], allSubsetInts, "Label" -> "Canonicalizing"];
  Pick[allSubsetInts, canonicalQ]
];

AddSymmetricPatterns[symmetryPermutations_, subsetSize_][numbers_] := Union[
  FromDigits[#, 2] & /@
    Catenate @ Outer[Permute, IntegerDigits[#, 2, subsetSize] & /@ numbers, symmetryPermutations, 1]];

FindMinimalPatterns[allPatterns_, tilingDAG_, maxGridSize_, init_ : {}] := Block[{
    newPatternsAsNumbers, successfulPatternsAsNumbers, $currentSetSize = CurrentSize[tilingDAG]},
  Print["Set size: ", $currentSetSize];
  newPatternsAsNumbers = TilingsIntsOfSize[allPatterns, tilingDAG];
  Print["Pattern sets to tile: ", Length @ newPatternsAsNumbers];
  successfulPatternsAsNumbers = AddSymmetricPatterns[GetSymmetryPermutations[allPatterns], Length[allPatterns]][
    SuccessfulTilings[allPatterns, newPatternsAsNumbers, maxGridSize, init, Max[Dimensions[First[allPatterns]]]]];
  Print["Successful count: ", Length @ successfulPatternsAsNumbers];
  MapMonitored[SetTileable[tilingDAG, #] &, successfulPatternsAsNumbers, "Label" -> "Writing to tilingDAG"];
  SetRestUntileableAndIncrementSize[tilingDAG];
  NumberToPatternSet[allPatterns] /@ successfulPatternsAsNumbers
];

FindTilingsSeq[allPatterns_, maxSetSize_Integer, filename_, maxGridSize_ : 64, init_ : {}] := Module[{
    tilingDAG = CreateSparseTilingDAG[Length[allPatterns]], minimalSetsSoFar},
  SetRestUntileableAndIncrementSize[tilingDAG]; (* go to size 1 *)
  If[FileExistsQ[filename],
    minimalSetsSoFar = Import @ filename;
    MapMonitored[
      Function[{minimalSets},
        Scan[SetTileable[tilingDAG, PatternSetToNumber[allPatterns][#]] &, minimalSets];
        SetRestUntileableAndIncrementSize[tilingDAG]
      ],
      minimalSetsSoFar,
      "Label" -> "Initializing tiling DAG"];
  ,
    Put[{}, filename];
  ];
  Map[Module[{result},
    result = FindMinimalPatterns[allPatterns, tilingDAG, maxGridSize, init];
    Put[Append[Import[filename], result], filename];
    result
  ] &,
    Range[CurrentSize[tilingDAG], maxSetSize]
  ]
];

ReverseEngineerTiles[tilegraphic_] :=
  Reverse /@ Cases[tilegraphic, Raster[x_SparseArray, ___] :> (Normal[x] /. {0.5 -> _, 1. -> 1, 0. -> 0}), Infinity];

ImportMinimalSets[size_, maskID_] :=
  Import[ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ".m"];

ImportMinimalPeriods[size_, maskID_] :=
  Import["periods-" <> ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ".m"];

CanonicalMinimalSets[size_, maskID_] := Module[{minimalSets, allPatterns, permutations},
  minimalSets = ImportMinimalSets[size, maskID];
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  permutations = GetSymmetryPermutations[allPatterns];
  canonicalSetsAsNumbers = Select[CanonicalPatternSetQ[permutations, Length[allPatterns]]] /@
    Map[PatternSetToNumber[allPatterns], minimalSets, {2}];
  Map[NumberToPatternSet[allPatterns], canonicalSetsAsNumbers, {2}]
];

(* Periodicity *)

MinimalPeriod[maxPeriod_][patterns_] := Module[{minPeriod, currentPeriod},
  minPeriod = Max[Dimensions[First[patterns]]] + 1;
  SelectFirst[(
      WriteString["stdout", " ", #];
      !FailureQ[GenerateTiling[patterns, {}, #, Boundary -> "Periodic"]]
    ) &][Range[minPeriod, maxPeriod]]
];

MinimalPeriodCached[allPatterns_, maxPeriod_][setNumber_] := MinimalPeriodCached[allPatterns, maxPeriod][setNumber] =
  MinimalPeriod[maxPeriod][NumberToPatternSet[allPatterns][setNumber]];

(* Main - FindMinimalSets *)

idToMask[size_, maskID_] := Partition[IntegerDigits[maskID, 2, Times @@ size], size[[2]]];

maskToAllPatterns[mask_] := With[{
    functionBody = Module[{n = 0}, mask /. {0 -> _, 1 :> Slot[++n]}]},
  Function[functionBody] @@@ Tuples[{1, 0}, Count[Catenate[mask], 1]]
];

FindMinimalSets[size_, maskID_] := Block[{$currentMaskSize = size, $currentMaskID = maskID}, Module[{allPatterns},
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  FindTilingsSeq[allPatterns,
                 Length[allPatterns],
                 ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ".m"]
]];


(* Main - FindMinimalPeriods *)

FindMinimalPeriods[maxPeriod_][size_, maskID_] := Module[{
    allPatterns, minimalSets, minimalSetsAsNumbers, permutations, currentSize, currentSet, minimalPeriods},
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  minimalSets = ImportMinimalSets[size, maskID];
  minimalSetsAsNumbers = Map[PatternSetToNumber[allPatterns], minimalSets, {2}];
  permutations = GetSymmetryPermutations[allPatterns];
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
        CanonicalPatternSet[permutations, Length[allPatterns]][minimalSetsAsNumbers[[currentSize, currentSet]]]]
    ,
      WriteString["stdout", "\n"];
    ]
  ), {currentSize, Length[minimalSets]}, {currentSet, Length[minimalSets[[currentSize]]]}];
  Put[
    minimalPeriods, "periods-" <> ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ".m"];
  Print["Periods exceeding the limit: ", Count[Catenate @ minimalPeriods, _ ? MissingQ]];
  Print["Max: ", Max @ Cases[Catenate @ minimalPeriods, Except[_ ? MissingQ]]];
  minimalPeriods
];

(* Maximal Sets *)

maximalSetsFileName[size_, maskID_] :=
  "maximal-sets/" <> ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ".m";

ImportMaximalSets[size_, maskID_] :=
  Map[NumberToPatternSet[maskToAllPatterns[idToMask[size, maskID]]], Import @ maximalSetsFileName[size, maskID], {2}];

FindMaximalSets[bitCount_, minimalSets : {{___Integer}...}] := Module[{dag},
  dag = CreateTilingDAG[bitCount];
  MapMonitored[SetTileable[dag, #] &, Catenate @ minimalSets, "Label" -> "Writing tileable to DAG"];
  MapMonitored[
    WithCleanup[
      UnknownSubsets[dag, #]
    ,
      SetUntileable[dag, #] & /@ UnknownSubsets[dag, #]
    ] &,
    Range[bitCount, 0, -1],
    "Label" -> "Extracting untileable"]
];

FindMaximalSets[size_, maskID_] := Module[{allPatterns, minimalSets, maximalSets},
  allPatterns = maskToAllPatterns[idToMask[size, maskID]];
  minimalSets = ImportMinimalSets[size, maskID];
  maximalSets = Map[NumberToPatternSet[allPatterns],
                    FindMaximalSets[Length[allPatterns], Map[PatternSetToNumber[allPatterns], minimalSets, {2}]],
                    {2}];
  WriteString["stdout", "Writing to disk..."];
  If[!DirectoryQ["maximal-sets"], CreateDirectory["maximal-sets"]];
  Put[maximalSets, maximalSetsFileName[size, maskID]];
  WriteString["stdout", " done\n"];
  maximalSets
];

(* Max tileable sizes *)

maxTileableSizesFileName[size_, maskID_] :=
  "max-tileable-sizes/" <> ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ".m";

ImportMaxTileableSizes[size_, maskID_] := Import @ maxTileableSizesFileName[size, maskID];

TileableQ[patternSet_, size_] := Module[{result}, WithCleanup[
  WriteString["stdout", " ", size],
  result = Not @ FailureQ @ GenerateTiling[patternSet, {}, size],
  WriteString["stdout", If[result, ">", "<"]]
]];

MaxTileableSize[patternSet_] := Module[{left = 0, right = 1, middle},
  While[TileableQ[patternSet, right],
    left = right;
    right *= 2
  ];
  While[right - left > 1,
    middle = Quotient[left + right, 2];
    If[TileableQ[patternSet, middle], left = middle, right = middle];
  ];
  WriteString["stdout", " ", left];
  left
];

MaxTileableSizeCached[allPatterns_][setNumber_] := MaxTileableSizeCached[allPatterns][setNumber] =
  MaxTileableSize[NumberToPatternSet[allPatterns][setNumber]];

FindMaxTileableSizes[size_, maskID_] := Module[{maximalSets},
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  maximalSets = ImportMaximalSets[size, maskID];
  maximalSetsAsNumbers = Map[PatternSetToNumber[allPatterns], maximalSets, {2}];
  permutations = GetSymmetryPermutations[allPatterns];
  maximalSizes = Table[(
    WriteString["stdout",
                "Tiling ",
                currentSize,
                "/",
                Length[maximalSets],
                " : ",
                currentSet,
                "/",
                Length[maximalSets[[currentSize]]],
                " :"];
    WithCleanup[
      MaxTileableSizeCached[allPatterns][
        CanonicalPatternSet[permutations, Length[allPatterns]][maximalSetsAsNumbers[[currentSize, currentSet]]]]
    ,
      WriteString["stdout", "\n"];
    ]
  ), {currentSize, Length[maximalSets]}, {currentSet, Length[maximalSets[[currentSize]]]}];

  If[!DirectoryQ["max-tileable-sizes"], CreateDirectory["max-tileable-sizes"]];
  Put[maximalSizes, maxTileableSizesFileName[size, maskID]];
  Print["Max: ", Max @ maximalSizes];
  maximalSizes
];
