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
  updateInterval = OptionValue["UpdateInterval"];
  lastCheckedTime = AbsoluteTime[];
  ParallelEvaluate[lastCheckedTime = AbsoluteTime[], DistributedContexts -> Automatic];
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

ParallelMapMonitored[args___] := mapFuncMonitored[ParallelMap, args];

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
               opts : OptionsPattern[]] /;
      And @@ (And @@ (size > # & /@ Dimensions[#]) & /@ {patterns[[1]], init}) := Module[{
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

TilingDAG /: MakeBoxes[object : TilingDAG[id_], format_] := ModuleScope[
  BoxForm`ArrangeSummaryBox[
    TilingDAG,
    object,
    Graphics[
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
      PlotRange -> {{-1.1, 2.4}, {-4.4, -0.7}}],
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

(* Searcher *)

PatternSetToNumber[allPatterns_][set_] := Total @ (2^(Map[First @ FirstPosition[allPatterns, #] &, set, {1}] - 1));

NumberToPatternSet[allPatterns_][number_] :=
  allPatterns[[First /@ Position[IntegerDigits[number, 2, Length[allPatterns]], 1]]];

GenerateTilingSequence[allPatterns_, patternNumber_, init_, size_, patternSize_] := With[{
    patterns = NumberToPatternSet[allPatterns][patternNumber]},
  Last[
    Replace[GenerateTiling[patterns, init, #], failure_ ? FailureQ :> Return[failure]] & /@
      {patternSize + 1, patternSize + 2, patternSize + 3, size}]
];

SuccessfulTilings[allPatterns_, patternNumbers_, size_, init_, patternSize_] := Module[{
    tileableQ},
  tileableQ = ParallelMapMonitored[
    Not[FailureQ @ GenerateTilingSequence[allPatterns, #, init, size, patternSize]] &,
    patternNumbers,
    "Label" -> "Tiling"];
  Pick[patternNumbers, tileableQ]
];

$symmetryTransforms = Composition @@@ Tuples[{
  {Identity, Reverse},
  {Identity, Map[Reverse]},
  {Identity, Transpose},
  {Identity, Replace[#, {0 -> 1, 1 -> 0}, {2}] &}}];

CanonicalPatternSetQ[symmetryPermutations_, subsetSize_][subsetInt_] := With[{
    digits = IntegerDigits[subsetInt, 2, subsetSize]},
  First[Sort[Permute[digits, #] & /@ symmetryPermutations]] === digits
];

GetSymmetryPermutations[patterns_] := DeleteCases[{0...}] @ Map[
  First @ FirstPosition[patterns, #, {0}] &, Function[transform, transform /@ patterns] /@ $symmetryTransforms, {2}];

TilingsIntsOfSize[inputPatterns_, size_, tilingDAG_] := Module[{
    allSubsetInts, symmetryPermutations, allSubsets, canonicalQ, printCell},
  (* Generate all subsets *)
  allSubsetInts = UnknownSubsets[tilingDAG, size];
  symmetryPermutations = GetSymmetryPermutations[inputPatterns];
  Print["Found symmetries: ", Length @ symmetryPermutations];
  canonicalQ = ParallelMapMonitored[
    CanonicalPatternSetQ[symmetryPermutations, Length[inputPatterns]], allSubsetInts, "Label" -> "Canonicalizing"];
  Pick[allSubsetInts, canonicalQ]
];

AddSymmetricPatterns[symmetryPermutations_, subsetSize_][numbers_] := Union[
  FromDigits[#, 2] & /@
    Catenate @ Outer[Permute, IntegerDigits[#, 2, subsetSize] & /@ numbers, symmetryPermutations, 1]];

FindMinimalPatterns[allPatterns_, tilingDAG_, setSize_Integer, size_ : 20, init_ : {}] := Block[{
    newPatternsAsNumbers, successfulPatternsAsNumbers},
  Print["Set size: ", setSize];
  newPatternsAsNumbers = TilingsIntsOfSize[allPatterns, setSize, tilingDAG];
  Print["Pattern sets to tile: ", Length @ newPatternsAsNumbers];
  successfulPatternsAsNumbers = AddSymmetricPatterns[GetSymmetryPermutations[allPatterns], Length[allPatterns]][
    SuccessfulTilings[allPatterns, newPatternsAsNumbers, size, init, Max[Dimensions[First[allPatterns]]]]];
  Print["Successful count: ", Length @ successfulPatternsAsNumbers];
  MapMonitored[SetTileable[tilingDAG, #] &, successfulPatternsAsNumbers, "Label" -> "Writing to tilingDAG"];
  SetUntileableUpToSize[tilingDAG, setSize];
  NumberToPatternSet[allPatterns] /@ successfulPatternsAsNumbers
];

FindTilingsSeq[allPatterns_, maxSetSize_Integer, filename_, size_ : 20, init_ : {}] := Module[{
    tilingDAG = CreateTilingDAG[Length[allPatterns]]},
  Put[{}, filename];
  Map[Module[{result},
    result = FindMinimalPatterns[allPatterns, tilingDAG, #, size, init];
    Put[Append[Import[filename], result], filename];
    result
  ] &,
    Range[1, maxSetSize]
  ]
];

ReverseEngineerTiles[tilegraphic_] :=
  Reverse /@ Cases[tilegraphic, Raster[x_SparseArray, ___] :> (Normal[x] /. {0.5 -> _, 1. -> 1, 0. -> 0}), Infinity];

(* Main *)

idToMask[size_, maskID_] := Partition[IntegerDigits[maskID, 2, Times @@ size], size[[1]]];

maskToAllPatterns[mask_] := With[{
    functionBody = Module[{n = 0}, mask /. {0 -> _, 1 :> Slot[++n]}]},
  Function[functionBody] @@@ Tuples[{1, 0}, Count[Catenate[mask], 1]]
];

FindMinimalSets[size_, maskID_] := Module[{},
  allPatterns = maskToAllPatterns @ idToMask[size, maskID];
  FindTilingsSeq[allPatterns,
                 Length[allPatterns],
                 ToString[size[[1]]] <> "-" <> ToString[size[[2]]] <> "-" <> ToString[maskID] <> ".m"]
];
