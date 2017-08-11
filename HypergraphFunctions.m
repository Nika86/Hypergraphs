(* ::Package:: *)

BeginPackage["HypergraphFunctions`"];

unsortedOverlay[h1_, h2_] := Complement[Union[h1, h2], Intersection[h1, h2]];
overlay[h1_, h2_] := Sort[unsortedOverlay[h1, h2]];
overlay[h1_, h2_, ho__] := overlay[overlay[h1, h2], ho];

delete[h_, qubit_] := Select[h, !MemberQ[#, qubit]&];
projection[h_, qubit_] := DeleteCases[#, qubit]& /@ Select[h, MemberQ[#, qubit]&];
unsortedShrink[h_, qubit_] := unsortedOverlay[delete[h, qubit], projection[h, qubit]];
shrink[h_, qubit_] := overlay[delete[h, qubit], projection[h, qubit]];

compactify[h_, qubit_, numQubits_] := h /. Table[i -> (i - 1), {i, qubit + 1, numQubits}];

compactDelete[h_, qubit_, numQubits_] := compactify[delete[h, qubit], qubit, numQubits];
compactProjection[h_, qubit_, numQubits_] := compactify[projection[h, qubit], qubit, numQubits];
compactShrink[h_, qubit_, numQubits_] := compactify[shrink[h, qubit], qubit, numQubits];

memThreshold = 7;
memWeight[h_, numQubits_] := memWeight[h, numQubits] = If[numQubits == 0,
	1 - 2 Boole[MemberQ[h, {}]],
	With[{d = delete[h, numQubits], s = unsortedShrink[h, numQubits]},
		memWeight[d, numQubits - 1] + memWeight[s, numQubits - 1]
	]
];

weight[h_, numQubits_] := Which[
	Length[h] == 0,
	2^numQubits,

	Length[h] == 1,
	2^numQubits - 2^(1 + numQubits - Length[h[[1]]]),

	Max[h] < numQubits,
	2^(numQubits - Max[h]) weight[h, Max[h]],

	numQubits <= memThreshold,
	memWeight[h, numQubits],

	True,
	With[{d = delete[h, numQubits], s = unsortedShrink[h, numQubits]},
		weight[d, numQubits - 1] + weight[s, numQubits - 1]
	]
];

isOrthogonal[h_, numQubits_] := weight[h, numQubits] == 0;
areOrthogonal[h1_, h2_, numQubits_] := isOrthogonal[overlay[h1, h2], numQubits];

degree[h_, i_] := Length[projection[h, i]];
degreeSequence[h_, numQubits_] := Table[degree[h, i], {i, numQubits}];

PermuteVertices[h_, permutation_, numQubits_] := Sort[Sort /@ (h /. Table[k -> permutation[[k]], {k, numQubits}])];
removePermutations[hList_, numQubits_] := Module[{permutations = Permutations[Range[numQubits]],  alreadyFound, uniqueData, i},
	Clear[alreadyFound];
	For[i = 1, i <= Length[hList], i++,
		alreadyFound[hList[[i]]] = False;
	];
	uniqueData = Reap[
		For[i = 1, i <= Length[hList], i++,
			If[!alreadyFound[hList[[i]]],
				Sow[hList[[i]]];
				For[j = 1, j <= Length[permutations], j++,
					alreadyFound[PermuteVertices[hList[[i]], permutations[[j]], numQubits]] = True;
				];
			];
		];
	][[2]];
	If[Length[uniqueData]>0,
		uniqueData[[1]],
		{}
	]
];

generateKReductions[h_, indexList_] := Module[{reductionList = {}},
	If[Length[indexList] == 0,
		{h},
		Join[
			generateKReductions[delete[h, indexList[[1]]], indexList[[2 ;;]]],
			generateKReductions[shrink[h, indexList[[1]]], indexList[[2 ;;]]]
		]
	]
];

checkKRDMHG[h_, indexList_, numQubits_] := Module[{reductionList, Flag = True, i, j},
	reductionList = generateKReductions[h, indexList];
	For [i = 1, i <= Length[reductionList] && Flag, i++,
		For [j = i + 1, j <= Length[reductionList] && Flag, j++,
			If[!areOrthogonal[reductionList[[i]], reductionList[[j]], numQubits],
				Flag = False;
			];
		];
	];
	Flag
];

EndPackage[];
