(* ::Package:: *)

(* ::Section:: *)
(*Legendre DVR (uses Legendre polynomials to construct the grid)*)


BeginDVRExtension@"LegendreDVR";


(* ::Subsection::Closed:: *)
(*Exposed Code*)


LegendreDVRPoints::usage=""
LegendreDVRK::usage=""
LegendreDVRV::usage=""
LegendreDVRWavefunctions::usage=""
LegendreDVRPlot::usage=""


(* ::Subsection:: *)
(*Internal Code*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Grid Formatting*)


LegendreDVRFormatGrid[G_,P_:None]:=With[{N=If[P==None//TrueQ,Length@G,P]},{G,LegendreDVRPoints[P][[2]]}]


(* ::Subsubsection::Closed:: *)
(*Grid Points*)


Clear["LegendreDVRPoints"]
Options[LegendreDVRPoints]={Verbose->False};
LegendreDVRPoints[\[Theta]Points_,OptionsPattern[]]:=
Module[
{p=\[Theta]Points,
L,
LegendreIntegration,
\[Delta]=KroneckerDelta,
\[CapitalLambda],T},
(*
Using Subscript[x, ij]=\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(\[Pi]\)]\(
\*SubscriptBox[\(P\), \(i\)]\((cos\ \[Theta])\)cos\ \[Theta]\ 
\*SubscriptBox[\(P\), \(j\)]\((cos\ \[Theta])\)sin\[Theta]\ \[DoubleStruckD]\[Theta]\)\),
Subscript[x, ij]=Sqrt[(i+1)^2/((2i+1)(2i+3))]Subscript[\[Delta], j,i+1]+Sqrt[i^2/((2i+1)(2i-1))]Subscript[\[Delta], j,i-1]
*)
LegendreIntegration[n_,m_]:=With[{i=n-1,j=m-1},
Sqrt[(i+1)^2/((2.i+1)(2i+3))]\[Delta][j,i+1]+Sqrt[i^2/((2.i+1)(2i-1))]\[Delta][j,i-1]
];
L=Array[LegendreIntegration,{p,p}];
{\[CapitalLambda],T}=Eigensystem[L];
With[{o=Ordering[\[CapitalLambda]]},
{\[CapitalLambda][[o]],T[[o]]}
]
]


(* ::Subsubsection::Closed:: *)
(*Kinetic Energy Matrix*)


Clear["LegendreDVRK"]
Options[LegendreDVRK]=Flatten[{DVR\[HBar]->1,m->1,LegendreEigensystem->False,
\[Theta]PointTransformation->Function[cos\[Theta],cos\[Theta]],Options[LegendreDVRPoints]}];
LegendreDVRK[\[Theta]PointsTransformation_,ops:OptionsPattern[]]:=
Module[
{\[CapitalLambda],T,i,T\[Theta]=OptionValue[\[Theta]PointTransformation],
	m=OptionValue[m],M,\[HBar]=OptionValue@DVR\[HBar],
	K},
(*
Using Subscript[x, ij]=\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(0\), \(\[Pi]\)]\(
\*SubscriptBox[\(P\), \(i\)]\((cos\ \[Theta])\)cos\ \[Theta]\ 
\*SubscriptBox[\(P\), \(j\)]\((cos\ \[Theta])\)sin\[Theta]\ \[DoubleStruckD]\[Theta]\)\),
Subscript[x, ij]=(Sqrt[(1+i)^2/((1+2 i) (3+2 i))])Subscript[\[Delta], j,1+i]+Sqrt[i^2/((-1+2 i) (1+2 i))] Subscript[\[Delta], j,-1+i]*)
{\[CapitalLambda],T}=\[Theta]PointsTransformation;
	M[n_]:=m^2/(1- T\[Theta][\[CapitalLambda][[n]]]^2);
	K=T.DiagonalMatrix[Array[(#-1)(#)&,Length[\[CapitalLambda]]]].(T\[Transpose]);
	For[i=1,i<=Length[\[CapitalLambda]],i++,
		If[OptionValue[Verbose],Print[T\[Theta][\[CapitalLambda][[i]]]]];
		K[[i,i]]+=M[i]
		];
	\[HBar]*K
]


(* ::Subsubsection::Closed:: *)
(*Potential Energy Matrix*)


Clear["LegendreDVRV"]
Options[LegendreDVRV]=Flatten[{Potential->Function[\[Theta],Sin[\[Theta]]],Options[LegendreDVRK]}];
LegendreDVRV[\[Theta]PointsTransformation_,ops:OptionsPattern[]]:=
	Module[
	{\[CapitalLambda],T,V,P=OptionValue[Potential]},
		{\[CapitalLambda],T}=\[Theta]PointsTransformation;
		V=SparseArray[{i_,j_}/;i==j:>P[ArcCos[\[CapitalLambda][[i]]]],{Length[\[CapitalLambda]],Length[\[CapitalLambda]]}]
	]


(* ::Subsubsection::Closed:: *)
(*Wavefunctions Function*)


Clear["LegendreDVRWavefunctions"]
LegendreDVRWavefunctions[kmat_,vmat_,ops:OptionsPattern[{KineticMatrix->None}]]:=
Module[
{\[CapitalLambda],\[CapitalPsi],K=kmat,V=vmat,phaseMap},
{\[CapitalLambda],\[CapitalPsi]}=Eigensystem[K+V];
{\[CapitalLambda],\[CapitalPsi]}=With[{o=Ordering[\[CapitalLambda]]},
{\[CapitalLambda][[o]],\[CapitalPsi][[o]]}
];
phaseMap=Sign[#]&/@\[CapitalPsi][[1]];
{\[CapitalLambda],MapThread[#1*#2&,{phaseMap,#}]&/@\[CapitalPsi]}
]


(* ::Subsubsection::Closed:: *)
(*Plotting Function*)


Clear["LegendreDVRPlot"]
Options[LegendreDVRPlot]={
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
List->True,
ColorScheme->"BrightBands"
};
LegendreDVRPlot[solutions_,gridpoints_,potentialmatrix_,ops:OptionsPattern[]]:=
Module[
{X,T,V=potentialmatrix,\[CapitalLambda],\[CapitalPsi],\[Psi],
waveSet,wavePlot,potentialPlot,
runHere,volEl,volEls,
num=OptionValue[WaveFunctions]},
{X,T}=gridpoints;
{\[CapitalLambda],\[CapitalPsi]}=solutions;
num=Which[
TrueQ[num==All],Range[1,Length[\[CapitalLambda]]] ,
TrueQ[Head[num]==List],num,
TrueQ[Head[num]==Integer],Range[1,num],
True,Range[1,Floor[Length[\[CapitalLambda]]/2]]];
volEl[x_]:=Sqrt[1-x^2](*(*Abs[(ArcCos[#1]-ArcCos[#2])]*))&@@{X[[n]],X[[If[n+1>Length[X],n-1,n+1]]]};*)
(*volEls=Array[volEl,Length[X]]*);
\[Psi][n_]:=Sort[With[
{S=OptionValue[EnergyShift],Q=OptionValue[PlotSquare]},MapThread[{ArcCos[#1],If[S,\[CapitalLambda][[n]],0]+If[Q,(#2\[Conjugate]#2)/1(*volEl[#1]^2*),#2/1(*volEl[#1]*)]}&,{X,\[CapitalPsi][[n]]}]
],#1[[1]]>#2[[1]]&];
waveSet=\[Psi]/@num;
wavePlot=With[
{N=Length[num]},
Array[ListPolarPlot[waveSet[[#]],
PlotLegends->If[OptionValue[Labels],
With[{n=num[[#]]},
Row[{
Subscript["\[Psi]",ToString[n]],
": E=",Round[\[CapitalLambda][[n]],1.0*10^(-1*OptionValue[EnergyDigits])]
}]
],
None
],
FilterRules[Flatten[{ops,Joined->OptionValue[Joined]}],Options[ListPolarPlot]],
ColorFunctionScaling->False,
ColorFunction->With[{c=ColorData[OptionValue[ColorScheme]][#/N]},Function[{x,y,\[Theta],r},Darker[c,\[Theta]/(1.5\[Pi])]]
],
PlotStyle->Flatten[{OptionValue[PlotStyle],OptionValue[LineThickness]}]
]&,N]
];
potentialPlot=
If[OptionValue[ShowPotential],
With[{
pset=Module[
{D=Diagonal[V],M,m},
M=Max[D];
m=Max[waveSet[[All,All,2]]];
Sort[MapThread[{#1,Abs[m/M]#2}&,{X,D}],#1[[1]]>#2[[1]]&]
]},
Show[
With[{p=1(*+Floor[OptionValue[\[Theta]Points]/50]*),M=Max[pset[[All,2]]],m=Min[pset[[All,2]]]},Graphics[{Arrowheads[Small],Array[Function[i,{GrayLevel[.5],Arrow[{#2 Sin[#1],#2 Cos[#1]}&@@@pset[[p i;;p i+1]]]}],Floor[Length[pset]/p]-1]}
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


`$DVRDimension=1;
`$FormatGrid=LegendreDVRFormatGrid;
`$PointLabels={("\[Theta]"|"theta"|"Theta"|"Azimuthal"|"azimuthal")};
`$GridPointsFunction=LegendreDVRPoints;
`$KineticMatrixFunction=LegendreDVRK;
`$PotentialMatrixFunction=LegendreDVRV;
`$WavefunctionsFunction=LegendreDVRWavefunctions;
`$PlotFunction=LegendreDVRPlot;


EndDVRExtension[];
