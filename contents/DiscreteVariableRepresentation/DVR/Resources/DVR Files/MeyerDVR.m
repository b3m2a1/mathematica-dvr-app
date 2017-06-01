(* ::Package:: *)

(* ::Section:: *)
(*Meyer DVR (for use on a grid in \[CurlyPhi])*)


BeginDVRExtension@"MeyerDVR";


(* ::Subsection:: *)
(*Exposed Code*)


(* ::Subsubsection::Closed:: *)
(*Options For Plotting*)


MeyerDVRPoints::usage=""
MeyerDVRK::usage=""
MeyerDVRV::usage=""
MeyerDVRWavefunctions::usage=""
MeyerDVRPlot::usage=""


(* ::Subsection:: *)
(*Internal Code*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Grid Point Function*)


Clear["MeyerDVRPoints"]
MeyerDVRPoints[points_,min_:0,max_:2\[Pi],OptionsPattern[]]:=
(*Units: Radians?*)
Module[
{L,
\[Delta]=KroneckerDelta,Null,
\[CapitalDelta]\[Phi],
X},
\[CapitalDelta]\[Phi]=(max-min)/(points);
X=Array[1.(min+(#-1)\[CapitalDelta]\[Phi])&,points]
]


(* ::Subsubsection:: *)
(*Kinetic Energy Function*)


Clear["MeyerDVRK"]
Options[MeyerDVRK]={ScalingFactor->1,DVR\[HBar]->1};
MeyerDVRK[gridpoints_,ops:OptionsPattern[]]:=
	(*Units are...?*)
	Module[
		{X=gridpoints,i,p,\[HBar]=OptionValue@DVR\[HBar],Kel,K},
		p=Length[X];
		Kel[i_,j_]:=(1.\[HBar])If[i==j,(p^2/2+1)*1/6,((-1)^(i-j))/(2*Sin[(\[Pi]*(i-j))/p]^2)];
		Array[Kel,{p,p}]
	]


(* ::Subsubsection:: *)
(*Potential Energy Function*)


Clear["MeyerDVRV"]
Options[MeyerDVRV]={PotentialFunction->Function[\[Phi],Sin[\[Phi]]]};
MeyerDVRV[gridpoints_,ops:OptionsPattern[]]:=
	With[{P=OptionValue[PotentialFunction],X=gridpoints,p=Length@gridpoints},
		SparseArray[Band[{1,1}]->P/@X,{p,p}]
		]


(* ::Subsubsection::Closed:: *)
(*Wavefunctions function*)


Clear["MeyerDVRWavefunctions"]
MeyerDVRWavefunctions[kmat_,vmat_,ops:OptionsPattern[{KineticMatrix->None}]]:=
Module[
{\[CapitalLambda],\[CapitalPsi],K,V,phaseMap},

K=kmat;
V=vmat;
{\[CapitalLambda],\[CapitalPsi]}=Eigensystem[K+V];
{\[CapitalLambda],\[CapitalPsi]}=With[{o=Ordering[\[CapitalLambda]]},
{\[CapitalLambda][[o]],\[CapitalPsi][[o]]}
];
phaseMap=Sign[#]&/@\[CapitalPsi][[1]];
{\[CapitalLambda],MapThread[#1*#2&,{phaseMap,#}]&/@\[CapitalPsi]}
]


(* ::Subsubsection::Closed:: *)
(*Plotting Function*)


Clear["MeyerDVRPlot"]
Options[MeyerDVRPlot]={
FilterRules[Options[ListPolarPlot],Except[Joined]],
WaveEigensystem->False,
WaveFunctions->All,
Labels->True,
EnergyDigits->3,
pointLabel->True,
ShowPotential->True,
ShowEnergies->False,
CutOff->10^-5,
PlotSquare->False,
EnergyShift->False,
Joined->True,
LineThickness->Thick,
List->True
};
MeyerDVRPlot[solutions_,gridpoints_,potential_,ops:OptionsPattern[]]:=
Module[
{X,V,\[CapitalLambda],\[CapitalPsi],\[Psi],
waveSet,wavePlot,potentialPlot,
runHere,
num=OptionValue[WaveFunctions]},
X=gridpoints;
V=potential;
{\[CapitalLambda],\[CapitalPsi]}=solutions;
num=Which[
TrueQ[num==All],Range[Length[\[CapitalLambda]]],
TrueQ[Head[num]==List],num,
True,Range[num]];
\[Psi][n_]:=Sort[With[
{S=OptionValue[EnergyShift],Q=OptionValue[PlotSquare]},MapThread[{#1,If[S,\[CapitalLambda][[n]],0]+If[Q,#2\[Conjugate]#2,#2]}&,{X,\[CapitalPsi][[n]]}]
],#1[[1]]>#2[[1]]&];
waveSet=\[Psi]/@num;
wavePlot=With[
{N=Length[num]},
Array[ListPolarPlot[waveSet[[#]],
PlotLegends->If[OptionValue[Labels],
With[{n=num[[#]]},
Row[{
Subscript["\[Psi]",ToString[n]],
": E=",NumberForm[\[CapitalLambda][[n]],{5,OptionValue[EnergyDigits]}]
}]
],
None
],
FilterRules[Flatten[{ops,Joined->OptionValue[Joined]}],Options[ListPolarPlot]],
ColorFunctionScaling->False,
ColorFunction->With[{m=Min[X],M=Max[X],c=ColorData["BrightBands"][#/N]},Function[{x,y,\[Theta],r},Darker[c,(\[Theta]-m)/(1.5(M-m))]]
],
PlotStyle->Flatten[{OptionValue[PlotStyle],OptionValue[LineThickness]}]
]&,N]
];
potentialPlot=
If[OptionValue[ShowPotential],With[{
pset=Module[
{D=Diagonal[V],M,m},
M=Max[D];
m=Max[waveSet[[All,All,2]]];
Sort[MapThread[{#1,Abs[m/M]#2}&,{X,D}],#1[[1]]>#2[[1]]&]
]},
Show[
With[{p=2,M=Max[pset[[All,2]]],m=Min[pset[[All,2]]]},Graphics[{Arrowheads[Small],Array[Function[i,{GrayLevel[.5],Arrow[{#2 Cos[#1],#2 Sin[#1]}&@@@pset[[p i;;p i+1]]]}],Floor[Length[pset]/p]-1]}
]
]
]
]
,
{}]
;
If[OptionValue[List],
Module[{D=Diagonal[V],W=waveSet[[All,All,2]],\[Omega],\[CapitalOmega],M,m},
M=Max[D];m=Min[D];
\[CapitalOmega]=Max[W];\[Omega]=Min[W];
Manipulate[Show[wavePlot[[n]],potentialPlot],{{n,1,"Wavefunction"},1,Length[wavePlot],1},
FrameLabel->{"",Column[{"Potential",BarLegend[{GrayLevel,{m,M}}]}]}
]
],
Show[Append[wavePlot,potentialPlot]]
]
]


End[]


(* ::Subsection:: *)
(*DVR Set Up*)


(* ::Text:: *)
(*Gives values to important symbols that the DVR tries to find*)


`$DVRDimension=1;
`$FormatGrid=Function[{G,P},G];
`$PointLabels={("\[CurlyPhi]"|"\[Phi]"|"phi"|"Phi"|"Angular"|"angular")};
`$GridPointsFunction=MeyerDVRPoints;
`$KineticMatrixFunction=MeyerDVRK;
`$PotentialMatrixFunction=MeyerDVRV;
`$WavefunctionsFunction=MeyerDVRWavefunctions;
`$PlotFunction=MeyerDVRPlot;


EndDVRExtension[];
