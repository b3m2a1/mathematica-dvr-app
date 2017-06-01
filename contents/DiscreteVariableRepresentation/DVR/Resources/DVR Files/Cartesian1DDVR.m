(* ::Package:: *)

(* ::Section:: *)
(*Cartesian 1D DVR*)


(* ::Text:: *)
(*A one dimensional DVR as reported by Colbert and Miller for the (-\[Infinity],\[Infinity]) range*)


BeginDVRExtension["Cartesian1DDVR"];


(* ::Subsection:: *)
(*Exposed Code*)


(* ::Subsubsection:: *)
(*Exposed Functions*)


Cartesian1DDVRFormatGrid::usage=""
Cartesian1DDVRPoints::usage=""
Cartesian1DDVRKineticMatrix::usage=""
Cartesian1DDVRPotentialMatrix::usage=""
Cartesian1DDVRWavefunctions::usage=""
Cartesian1DDVRPlotFunction::usage=""


(* ::Subsubsection:: *)
(*Exposed Options*)


(*Options for various functions*)
EnergyDigits::usage="(integer) Option name for Cartesian1DDVRPlot"
SquareWavefunction::usage="(boolean) Option name for Cartesian1DDVRPlot"
ZeroPointEnergy::usage="(Number) Option for Cartesian1DDVRPlot"
WavefunctionSelection::usage="(All,Integer,List) Option name for Cartesian1DDVRPlot"


(* ::Subsection:: *)
(*Internal Code*)


Begin["`Private`"];


(*Import files to reuse (see SphericalDVR for example)*)


(* ::Subsubsection::Closed:: *)
(*Grid Formatting Function*)


(* ::Text:: *)
(*This function should take a grid and the grid points used to generate the grid.*)


Cartesian1DDVRFormatGrid[grid_,points_]:=grid;


(* ::Subsubsection::Closed:: *)
(*Grid Points Function*)


(* ::Text:: *)
(*This function should take the number of grid points for each coordinate*)


Cartesian1DDVRPoints[points_,X:_?(MatchQ[#,{_?NumberQ,_?NumberQ}]&):{-10,10}]:=
	With[{xmin=X[[1]],xmax=X[[2]]},
	With[{\[CapitalDelta]x=(xmax-xmin)/(points-1)},
		Table[1.xmin+(i-1) \[CapitalDelta]x,{i,points}]
	]
	];


(* ::Subsubsection::Closed:: *)
(*Kinetic Matrix Function*)


(* ::Text:: *)
(*This should take the grid generated in grid points function and turn it into the kinetic energy matrix for the DVR*)


Cartesian1DDVRKineticMatrix[grid_,OptionsPattern@{M->1,H->1}]:=
	With[{xmin=Min@grid,xmax=Max@grid,points=Length@grid},
		With[{dx=(xmax-xmin)/points,m=OptionValue@M,\[HBar]=OptionValue@H},
			Table[(1.)*(\[HBar] (-1)^(i-j))/(2m dx^2) If[i==j,\[Pi]^2/3,2/(i-j)^2],{i,points},{j,points}]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Potential Matrix Function*)


(* ::Text:: *)
(*This should take the grid generated in grid points function and turn it into the potential energy matrix for the DVR*)


Options[Cartesian1DDVRPotentialMatrix]={Function->((#/2)^2&)};
Cartesian1DDVRPotentialMatrix[grid_,ops:OptionsPattern[]]:=
	With[{func=OptionValue@Function},
			With[{A=func/@grid},
			(*SparseArray[Band[{1,1}]\[Rule]A,{Length@grid,Length@grid},0]*)
			DiagonalMatrix@A
			]
		]


(* ::Subsubsection::Closed:: *)
(*Wavefunctions Function*)


(* ::Text:: *)
(*Should take the kinetic and potential energy matrices generated and turn it into an ordered array of energies and wavefunction vectors*)


Cartesian1DDVRWavefunctions[T_,V_]:=
	With[{S=Eigensystem@(T+V)},
		With[{E=S[[1]],\[CapitalPsi]=S[[2]]},
			With[{o=Ordering@E},
				{E[[o]],With[{\[Psi]=\[CapitalPsi][[o]]},
							With[{phase=Sign/@\[Psi][[1]]},
							phase*\[Psi]
							]
							]}
			]
		]
	]


(* ::Subsubsection::Closed:: *)
(*Plotting Function*)


(* ::Text:: *)
(*Should take the wavefunctions and a scad of options to make a nice plot. Generally reusing some prewritten code works best*)


Options[Cartesian1DDVRPlotFunction]=
	{WavefunctionSelection->All,
	AxesOrigin->{0,0},
	EnergyDigits->3,
	ZeroPointEnergy->0,
	LabelingFunction->Automatic,
	CutOff->10^-5,
	Manipulate->True,
	PlotRange->Automatic,
	SquareWavefunction->False}~Join~FilterRules[Options[Plot],Except[AxesOrigin|PlotRange|LabelingFunction]];
Cartesian1DDVRPlotFunction[solutions_,grid_,potentialMatrix_,ops:OptionsPattern[]]:=
	Module[
		{\[CapitalLambda],X,\[Psi],num=OptionValue[WavefunctionSelection],
		dataRange,
		potential=If[potentialMatrix//MatrixQ,Diagonal@potentialMatrix,potentialMatrix],
		squared=OptionValue@SquareWavefunction,
		lf=Replace[OptionValue@LabelingFunction,{
				Automatic->(Row@{Subscript["\[Psi]",#1],": E=",NumberForm[#2,OptionValue[EnergyDigits]]}&),
				None->False}],
		plotWave,waveSet,Ec=OptionValue[CutOff],
		wavePlot,potentialPlot,
		\[Lambda]Plot,plotRange
		},
		{\[CapitalLambda],X}=solutions;
		num=Replace[num,{All:>Range[1,Length[\[CapitalLambda]]],_List:>num,_?IntegerQ:>Range[1,num],_->1}];
		\[Psi][n_]:=MapThread[{#1,If[squared,#2^2,#2]}&,{grid,X[[n]]}];
		waveSet=\[Psi]/@num;
		dataRange=With[{\[CurlyPhi]=Select[Flatten[waveSet,1],Abs[#[[2]]]>=Ec&]},
					{Min[\[CurlyPhi][[All,1]]],Max[\[CurlyPhi][[All,1]]]}
					];
		plotWave[n_]:=With[{\[Lambda]={0,\[CapitalLambda][[n]]}},
				\[Lambda]+#&/@\[Psi][n]
				];
		
		waveSet=Select[#,dataRange[[1]]<=#[[1]]<=dataRange[[2]]&]&/@(plotWave/@num);
		(*Print@waveSet;*)
		wavePlot[sel_,lFunc_:lf]:=ListLinePlot[ 
			waveSet[[sel]],
			PlotRange->{Min@{Min@waveSet[[sel,All,2]],0},Max@waveSet[[sel,All,2]]},
			PlotLegends->(If[lFunc===False,None,(lFunc@@#)&/@Thread[{num[[If[Length@sel===0,{sel},sel]]],\[CapitalLambda][[num[[If[Length@sel===0,{sel},sel]]]]]}]])
			];
		(*Print@\[CapitalLambda][[2]];*)
		potentialPlot=ListLinePlot[Thread[{grid,potential}],PlotStyle->{Gray,Dashing[{.01,.025,.02,.025}]}];
		\[Lambda]Plot[sel_]:=ListLinePlot[Thread[{grid,#}],PlotStyle->{Red,Dotted}]&/@\[CapitalLambda][[num[[sel]]]];
		plotRange=Automatic;(*Replace[OptionValue@PlotRange,Automatic:>Max@Flatten@waveSet[[All,2]]];*)
		If[Length@num>1,
			If[OptionValue@Manipulate,
				With[{wp=wavePlot,N=num,lF=With[{f=lf[#1,#2]},f&],potPlot=potentialPlot,L=\[CapitalLambda],\[Lambda]P=\[Lambda]Plot,pR=plotRange},
				Manipulate[
					Show[wp[N[[i]],lF],{potPlot,\[Lambda]P[{i}]},FilterRules[{ops,PlotRange->pR},Options[Plot]]],
					{{i,1,""},1,Length@N,1}
					]
				],
				Show[wavePlot[All],{potentialPlot,\[Lambda]Plot[All]},FilterRules[{ops,PlotRange->plotRange},Options[Plot]]]
			],
			Show[wavePlot[All],{potentialPlot,\[Lambda]Plot[All]},FilterRules[{ops,PlotRange->plotRange},Options[Plot]]]
			]
	];


(* ::Subsubsection::Closed:: *)
(*End*)


End[];


(* ::Subsection:: *)
(*DVR Set Up*)


`$DVRDimension=1;
`$FormatGrid=Cartesian1DDVRFormatGrid;
`$PointLabels={"x"|"y"|"z"};
`$GridPointsFunction=Cartesian1DDVRPoints;
`$KineticMatrixFunction=Cartesian1DDVRKineticMatrix;
`$PotentialMatrixFunction=Cartesian1DDVRPotentialMatrix;
`$WavefunctionsFunction=Cartesian1DDVRWavefunctions;
`$PlotFunction=Cartesian1DDVRPlotFunction;


EndDVRExtension[];
