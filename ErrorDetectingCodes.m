(* ::Package:: *)

BeginPackage["ErrorDetectingCodes`"];

Needs["HypergraphFunctions`"];

isCorrectBase[baseH_, numQubits_] := Module[{isCorrect = True, qubit},
	For[qubit = 1, qubit <= numQubits && isCorrect, qubit++,
		If[!isOrthogonal[compactProjection[baseH, qubit, numQubits], numQubits - 1],
			isCorrect = False;
		];
	];
	isCorrect
];

isCorrectDiff[diffH_, numQubits_] := Module[{isCorrect = True, qubit},
	If[!isOrthogonal[diffH, numQubits],
		isCorrect = False;
	];
	For [qubit = 1, qubit <= numQubits && isCorrect, qubit++,
		If[!areOrthogonal[diffH, {{qubit}}, numQubits],
			isCorrect = False;
		];
	];
	isCorrect
];

isCorrectPair[baseH_, diffH_, numQubits_] := Module[{isCorrect = True, qubit},
	For [qubit = 1, qubit <= numQubits && isCorrect, qubit++,
		With[{undecoratedProjection = overlay[projection[baseH, qubit], diffH]},
			If[!isOrthogonal[undecoratedProjection, numQubits],
				isCorrect = False;
			];
			If[!areOrthogonal[undecoratedProjection, {{qubit}}, numQubits],
				isCorrect = False;
			];
		];
	];
	isCorrect
];

findAllCodes[baseH_, numQubits_, dHs_, dimension_, foundSoFar_, searchedUpTo_] := Module[{lastInd, prevDiffInd, lastBase, diffH, Flag},
	If[Length[foundSoFar] == dimension - 1,
		Sow[foundSoFar];,
		For[lastInd = searchedUpTo, lastInd <= Length[dHs] + 1 - (dimension - 1 - Length[foundSoFar]), lastInd++,
			lastBase = overlay[baseH, dHs[[lastInd]]];
			Flag = 1;
			For[prevDiffInd = 1, prevDiffInd <= Length[foundSoFar] && Flag == 1, prevDiffInd++,
				diffH = overlay[dHs[[foundSoFar[[prevDiffInd]]]], dHs[[lastInd]]];
				If[!isCorrectDiff[diffH, numQubits],
					Flag = 0;
				];
				If[!isCorrectPair[lastBase, diffH, numQubits],
					Flag = 0;
				];
			];
			If[Flag == 1,
				findAllCodes[baseH, numQubits, dHs, dimension, Append[foundSoFar, lastInd], lastInd + 1];
			];
		];
	];
];

getAllCodes[baseH_, numQubits_, dHs_, dimension_] := Module[{codeData},
	codeData = Reap[
		findAllCodes[baseH, numQubits, dHs, dimension, {}, 1];
	][[2]];
	If[Length[codeData] > 0,
		codeData[[1]],
		{}
	]
];

hasCodeRecursive[baseH_, numQubits_, dHs_, dimension_, foundSoFar_, searchedUpTo_] := Module[{lastInd, prevDiffInd, lastBase, diffH, Flag, foundCode = False},
	If[Length[foundSoFar] == dimension - 1,
		foundCode = True;,
		For[lastInd = searchedUpTo, !foundCode && lastInd <= Length[dHs] + 1 - (dimension - 1 - Length[foundSoFar]), lastInd++,
			lastBase = overlay[baseH, dHs[[lastInd]]];
			Flag = 1;
			For[prevDiffInd = 1, prevDiffInd <= Length[foundSoFar] && Flag == 1, prevDiffInd++,
				diffH = overlay[dHs[[foundSoFar[[prevDiffInd]]]], dHs[[lastInd]]];
				If[!isCorrectDiff[diffH, numQubits],
					Flag = 0;
				];
				If[!isCorrectPair[lastBase, diffH, numQubits],
					Flag = 0;
				];
			];
			If[Flag == 1,
				foundCode = hasCodeRecursive[baseH, numQubits, dHs, dimension, Append[foundSoFar, lastInd], lastInd + 1];
			];
		];
	];
	foundCode
];

hasCode[baseH_, numQubits_, dHs_, dimension_] := hasCodeRecursive[baseH, numQubits, dHs, dimension, {}, 1];

EndPackage[];
