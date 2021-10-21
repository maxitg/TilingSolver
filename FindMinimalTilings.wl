ClearAll[$statePattern];
$statePattern = {{(_Integer ? (GreaterEqualThan[0]) | Verbatim[_])...}...};

ClearAll[$patternsPattern];
$patternsPattern = {$statePattern ...};

ClearAll[tile];
ClearAll[tilingPattern];

ClearAll[displacedLogicalExpression];
displacedLogicalExpression[expr_, {dx_, dy_}] := expr /. {
  tile[x_, y_, b_] :> tile[x + dx, y + dy, b],
  tile[x_, y_] :> tile[x + dx, y + dy]};

ClearAll[singlePatternExpressionToGrid];
singlePatternExpressionToGrid[gridSize_][singleTileExpression_] := Module[{i, j},
  And @@ Catenate @ Table[
    displacedLogicalExpression[singleTileExpression, {i, j}],
    {i, 0, gridSize[[1]] - 1},
    {j, 0, gridSize[[2]] - 1}]
];

ClearAll[singleTileAndPatternLogicalExpression];
singleTileAndPatternLogicalExpression[pattern_, patternVariable_, tileVariables_] := Module[{},
  And @@ Replace[Thread[tileVariables -> Catenate @ pattern],
          {(cell_ -> 1) :> cell, (cell_ -> 0) :> !cell, (cell_ -> Verbatim[_]) :> Nothing},
          {1}] && patternVariable
];

ClearAll[singleTileFlaggedLogicalExpression];
singleTileFlaggedLogicalExpression[patterns_] := Module[{tileVariables},
  tileVariables = Catenate @ Table[tile[i, j], {i, Length @ patterns[[1]]}, {j, Length @ patterns[[1, 1]]}];
  Or @@ MapIndexed[singleTileAndPatternLogicalExpression[#, tilingPattern[#2[[1]]], tileVariables] &, patterns]
];

ClearAll[disallowMinimalSetExpression];
disallowMinimalSetExpression[patternCount_, minimalSet_] := Module[{},
  Not[And @@ tilingPattern /@ First /@ Position[IntegerDigits[minimalSet, 2, patternCount], 1]]
];

ClearAll[findTileableSet];
findTileableSet[patterns_, knownMinimalSets_, gridSize_, tilesetSize_] := Module[{
    patternSize, extendedPatternExpression,
    disableKnownMinimalSets, i, j, patternVariables, tileVariables, variables, maxSetSizeExpression, solutionList},
  patternSize = Dimensions[patterns[[1]]];
  extendedPatternExpression = singlePatternExpressionToGrid[{gridSize, gridSize}][
    singleTileFlaggedLogicalExpression[patterns]];
  disableKnownMinimalSets = And @@ (disallowMinimalSetExpression[Length @ patterns, #] & /@ knownMinimalSets);
  patternVariables = tilingPattern /@ Range @ Length @ patterns;
  tileVariables = Catenate @ Table[
    tile[i, j], {i, gridSize + patternSize[[1]] - 1}, {j, gridSize + patternSize[[2]] - 1}];
  variables = Join[patternVariables, tileVariables];
  maxSetSizeExpression = BooleanCountingFunction[{tilesetSize}, Length @ patterns] @@ patternVariables;
  solutionList = SatisfiabilityInstances[
    maxSetSizeExpression && disableKnownMinimalSets && extendedPatternExpression,
    variables,
    Method -> "SAT"];
  If[solutionList === {}, Return[Failure["NotTileable", <||>], Module]];
  FromDigits[Boole @ Take[First @ solutionList, Length @ patterns], 2]
];

ClearAll[toBinaryTiles];
toBinaryTiles[bitCount_][x_, y_, value_] := With[{
    digits = IntegerDigits[value, 2, bitCount]},
  And @@ MapIndexed[If[# == 1, Identity, Not][tile[x, y, #2[[1]]]] &, digits]
];

ClearAll[singleTileLogicalExpression];
singleTileLogicalExpression[bitCount_][patterns : $patternsPattern] := Module[{variables},
  variables = Table[tile[i, j], {i, 1, Length @ patterns[[1]]}, {j, 1, Length @ patterns[[1, 1]]}];
  Or @@ And @@@ (
    Catenate /@ Map[Thread, Thread /@ (variables -> # & /@ patterns), {2}] /. {
      (tile[x_, y_] -> value_Integer) :> toBinaryTiles[bitCount][x, y, value],
      (tile[x_, y_] -> Verbatim[_]) :> Nothing})
];

ClearAll[generateTiling];
generateTiling[patterns : Except[{}, $patternsPattern],
               init : $statePattern,
               size : {_Integer, _Integer}] := Module[{
    patternSize, bitCount, extendedPatternExpression, initExpression, variables, i, j, depth,
    solutionList, initX, initY, takeTop, takeBottom, takeLeft, takeRight},
  patternSize = Dimensions[patterns[[1]]];
  bitCount = Max[Ceiling[Log2[Max[{Cases[patterns, _ ? NumericQ, {3}], Cases[init, _ ? NumericQ, {2}]}] + 1]], 1];
  initX = Ceiling[(size[[1]] + 1) / 2] - Floor[Dimensions[init][[1]] / 2];
  initY = Ceiling[(size[[2]] + 1) / 2] - Floor[Dimensions[init][[-1]] / 2];
  extendedPatternExpression = singlePatternExpressionToGrid[size][singleTileLogicalExpression[bitCount][patterns]];
  initExpression = displacedLogicalExpression[singleTileLogicalExpression[bitCount][{init}], {initX, initY}];
  variables = Catenate @ Catenate @ Table[
    tile[i, j, b], {i, size[[1]] + patternSize[[1]] - 1}, {j, size[[2]] + patternSize[[2]] - 1}, {b, bitCount}];
  solutionList = SatisfiabilityInstances[
    extendedPatternExpression && initExpression,
    variables,
    1,
    Method -> "SAT"];
  If[solutionList === {}, Return[Failure["NotTileable", <||>], Module]];
  takeTop = 1 + Quotient[patternSize[[1]] - 1, 2];
  takeBottom = takeTop - 1 - patternSize[[1]];
  takeLeft = 1 + Quotient[patternSize[[2]] - 1, 2];
  takeRight = takeLeft - 1 - patternSize[[2]];

  First[
    Partition[FromDigits[#, 2] & /@ Boole[Partition[#, bitCount]], size[[2]] + patternSize[[2]] - 1][[
        takeTop ;; takeBottom, takeLeft ;; takeRight]] & /@
      solutionList]
];

generateTiling[patterns_, init_, size_Integer, rest___] := generateTiling[patterns, init, {size, size}, rest];

generateTiling[{}, ___] := Failure["NotTileable", <||>];

ClearAll[getSymmetryPermutations];
getSymmetryPermutations[allPatterns_] := Module[{transformedPatterns},
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

ClearAll[patternTrim];
patternTrim[pattern_] := FixedPoint[Replace[{
  {{Verbatim[_]...}, x___} :> {x},
  {x___, {Verbatim[_]...}} :> {x},
  x : {{Verbatim[_], ___}...} :> Rest /@ x,
  x : {{___, Verbatim[_]}...} :> Most /@ x
}], pattern];

(* Note: shift symmetry is removed because it makes results depend on the grid size *)

ClearAll[$patternSymmetryGenerators];
$patternSymmetryGenerators = {Reverse, Transpose, Replace[#, {0 -> 1, 1 -> 0}, {2}] &};

patternSetToNumber[allPatterns_][set_] :=
  Total @ (2^(Map[First @ FirstPosition[Reverse @ allPatterns, #] &, set, {1}] - 1));

numberToPatternSet[allPatterns_][number_] :=
  allPatterns[[First /@ Position[IntegerDigits[number, 2, Length[allPatterns]], 1]]];

ClearAll[smallerTileableSet];
smallerTileableSet[gridSize_][patterns_] :=
  SelectFirst[!FailureQ[generateTiling[#, {}, gridSize]] &] @ Subsets[patterns, {Length[patterns] - 1}];

ClearAll[reduceToMinimalSet];
reduceToMinimalSet[gridSize_][patterns_] :=
  FixedPoint[Replace[smallerTileableSet[gridSize][#], _ ? MissingQ -> #] &, patterns];

reduceToMinimalSet[allPatterns_, gridSize_][initialSet_] :=
  patternSetToNumber[allPatterns] @ reduceToMinimalSet[gridSize] @ numberToPatternSet[allPatterns][initialSet];

addSymmetricPatterns[symmetryPermutations_, subsetSize_][numbers_] := Union[
  FromDigits[#, 2] & /@
    Catenate @ Outer[Permute, IntegerDigits[#, 2, subsetSize] & /@ numbers, symmetryPermutations, 1]];

(* Note: this is a pretty bad API. tilesetSize does not make anything faster because the SAT problem is simpler if the
         size is not specified. Limiting gridSize means some of the found candidates might not be minimal sets. *)

ClearAll[FindMinimalTilings];
FindMinimalTilings[allTiles : $patternsPattern ? (SameQ @@ (Dimensions /@ #) &),
                   tilesetSize_Integer /; tilesetSize >= 0,
                   gridSize_Integer /; gridSize > 0] := Module[{
    minimalSets, candidateSet, minimalSet, currentGridSize, symmetries, newSets, results, currentGridState},
  symmetries = getSymmetryPermutations[allTiles];
  minimalSets = {};
  results = <||>;
  currentGridSize = 2;
  While[!FailureQ[candidateSet = findTileableSet[allTiles, minimalSets, currentGridSize, tilesetSize]],
    minimalSet = reduceToMinimalSet[allTiles, currentGridSize][candidateSet];
    If[FailureQ[currentGridState = generateTiling[numberToPatternSet[allTiles][minimalSet], {}, gridSize]],
      ++currentGridSize;
    ,
      newSets = addSymmetricPatterns[symmetries, Length @ allTiles][{minimalSet}];
      minimalSets = Join[minimalSets, newSets];
      (* TODO: needs to be optimized to make use of symmetries. *)
      If[Count[IntegerDigits[minimalSet, 2], 1] === tilesetSize,
        results = Join[
          results, AssociationMap[generateTiling[#, {}, gridSize] &, numberToPatternSet[allTiles] /@ newSets]]
      ];
    ];
  ];
  results
];

ClearAll[maskToAllPatterns];
maskToAllPatterns[mask_] := With[{
    functionBody = Module[{n = 0}, mask /. {0 -> _, 1 :> Slot[++n]}]},
  Function[functionBody] @@@ Tuples[{1, 0}, Count[Catenate[mask], 1]]
];

ClearAll[templateToAllPatterns];
templateToAllPatterns[coordinates_] := maskToAllPatterns[Normal @ SparseArray[Thread[coordinates -> 1]]];

FindMinimalTilings[template : {{_Integer ? (# > 0 &), _Integer ? (# > 0 &)}..},
                   tilesetSize_Integer /; tilesetSize >= 0,
                   gridSize_Integer /; gridSize > 0] :=
  FindMinimalTilings[templateToAllPatterns[template], tilesetSize, gridSize];
