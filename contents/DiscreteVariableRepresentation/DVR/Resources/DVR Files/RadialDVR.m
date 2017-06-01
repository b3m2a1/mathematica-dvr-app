(* ::Package:: *)

(* ::Section:: *)
(*Radial DVR (Colbert and Miller (0,\[Infinity]) range DVR)*)


BeginDVRExtension@"RadialDVR";


(* ::Subsection::Closed:: *)
(*Exposed Code*)


(* ::Subsubsection::Closed:: *)
(*Exposed Functions*)


RadialDVRPoints::usage="RadialDVRPoints[Option Set] returns the radial DVR grid/array"
RadialDVRK::usage="Returns the radial DVR kinetic energy matrix"
RadialDVRV::usage="Returns the radial DVR potential energy matrix"
RadialDVRWavefunctions::usage="Returns the radial DVR Hamiltonian eigensystem"
RadialDVRPlot::usage="Plots a radial DVR eigensystem"


(* ::Subsection:: *)
(*Internal Code*)


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Grid Points*)


Clear["RadialDVRPoints"]
RadialDVRPoints[points_,min_:.2,max_:2,OptionsPattern[]]:=
Module[
{
L,
\[Delta]=KroneckerDelta,Null,
\[CapitalDelta]r,
X},
\[CapitalDelta]r=(max-min)/points;
X=Array[1.(min+# \[CapitalDelta]r)&,points]
]


(* ::Subsubsection::Closed:: *)
(*Kinetic Energy Matrix*)


Clear["RadialDVRK"]
Options[RadialDVRK]={DVR\[HBar]->1};
RadialDVRK[gridpoints_,ops:OptionsPattern[]]:=
	Module[
	{
	X,i,p,\[HBar]=OptionValue@DVR\[HBar],
	Kel,
	K},
	X=gridpoints;
	p=Length[X];
	Kel[i_,j_]:=(1.\[HBar])If[i==j,\[Pi]^2/3,(2*(-1)^(i-j))/(i-j)^2];
	Array[Kel,{p,p}]
	]


(* ::Subsubsection:: *)
(*Potential Energy Matrix*)


Clear["RadialDVRV"]
	Options[RadialDVRV]={PotentialFunction->Function[r,-(1/r^2)]};
	RadialDVRV[gridpoints_,ops:OptionsPattern[]]:=
	With[{P=OptionValue[PotentialFunction],X=gridpoints,p=Length@gridpoints},
		SparseArray[Band[{1,1}]->P/@X,{p,p}]
		];


(* ::Subsubsection::Closed:: *)
(*Wavefunctions Function*)


Clear["RadialDVRWavefunctions"]
RadialDVRWavefunctions[kmat_,vmat_,ops:OptionsPattern[{KineticMatrix->None}]]:=
Module[
{\[CapitalLambda],\[CapitalPsi],K=kmat,V=vmat,phaseMap,scale,O},
{\[CapitalLambda],\[CapitalPsi]}=Eigensystem[K+V];
O=Ordering@\[CapitalLambda];
{\[CapitalLambda],\[CapitalPsi]}={\[CapitalLambda][[O]],\[CapitalPsi][[O]]};
scale=Module[{Y,M,i},
	Y=Abs/@\[CapitalPsi][[1]];
	M=Max[Y];
	i=Position[Y,M][[1,1]];
	Sign[\[CapitalPsi][[1,i]]]
	];
{\[CapitalLambda],(scale*#&/@\[CapitalPsi])}
]


(* ::Subsubsection::Closed:: *)
(*Plotting Function*)


Clear["RadialDVRPlot"]
	Options[RadialDVRPlot]=Flatten[{
	Options[RadialDVRWavefunctions],
	Options[ListLinePlot],
	WaveEigensystem->False,
	WaveFunctions->5,
	Labels->True,
	EnergyDigits->3,
	pointLabel->True,
	ShowPotential->True,
	ShowEnergies->False,
	CutOff->10^-5,
	PlotSquare->False,
	EnergyShift->True,
	Joined->True,
	LineThickness->Thick,
	List->True
	}];
RadialDVRPlot[solutions_,gridpoints_,potential_,ops:OptionsPattern[]]:=
Module[
	{X,V,\[CapitalLambda],\[CapitalPsi],\[Psi],
	waveSet,wavePlot,potentialPlot,dataRange,
	runHere,Ec=OptionValue[CutOff],
	num=OptionValue[WaveFunctions]},
	runHere[f_,OptionsPattern[{others->{}}]]:=f[FilterRules[Flatten[{OptionValue[others],ops}],Options[f]]];
	X=gridpoints;
	V=potential;
	{\[CapitalLambda],\[CapitalPsi]}=solutions;
	num=If[num==All,Range[1,Length[\[CapitalLambda]]],False,If[Head[num]==List,num,False,Range[1,num]]];
	\[Psi][n_]:=Sort[With[{Q=OptionValue[PlotSquare]},MapThread[{#1,If[Q,#2\[Conjugate]#2,#2]}&,{X,\[CapitalPsi][[n]]}]],#1[[1]]>#2[[1]]&];
	waveSet=\[Psi]/@num;
	dataRange=With[{\[CurlyPhi]=Select[Flatten[waveSet,1],Abs[#[[2]]]>=Ec&]},Max[\[CurlyPhi][[All,1]]]];
	waveSet=With[
		{S=OptionValue[EnergyShift],set=Select[#,#[[1]]<=dataRange&]&/@waveSet},MapThread[Function[{a,b},(#+{0,If[S,b,0]})&/@a],{set,\[CapitalLambda][[num]]}]
	];
	wavePlot=With[
		{N=Length[num]},
		Array[
			ListLinePlot[
				waveSet[[#]],
				PlotLegends->
					If[OptionValue[Labels],
						With[
							{n=num[[#]]},
							Row[{
								Subscript["\[Psi]",ToString[n]],
								": E=",NumberForm[\[CapitalLambda][[n]],{5,OptionValue[EnergyDigits]}]
							}]
						],
						None
					],
				PlotRange->All,
				FilterRules[Flatten[{ops,Joined->OptionValue[Joined]}],Options[ListLinePlot]],
				ColorFunction->With[{c=ColorData["BrightBands"][#/N]},Function[y,c]],
				AxesOrigin->{0,0},
				PlotStyle->Flatten[{OptionValue[PlotStyle],OptionValue[LineThickness]}]
				]&,N]
		];
	potentialPlot=
		If[OptionValue[ShowPotential],
			ListLinePlot[
				With[
					{D=Diagonal[V]},
					Sort[MapThread[{#1,#2}&,{X,D}],#1[[1]]>#2[[1]]&]
				],
				Joined->True,
				PlotStyle->{Dashing[{Small,Tiny}],Gray,Thick}
			],
		{}];
		If[OptionValue[List],
			Module[{D=Diagonal[V],W=waveSet[[All,All,2]],\[Omega],\[CapitalOmega],M,m},
				M=Max[D];m=Min[D];
				\[CapitalOmega]=Max[W];\[Omega]=Min[W];
					Manipulate[Show[wavePlot[[n]],potentialPlot],{{n,1,"Wavefunction"},1,Length[wavePlot],1},
						FrameLabel->{"",Column[{"Potential",BarLegend[{GrayLevel,{m,M}}]}]}
					]
				],
			Show[Flatten[{potentialPlot,wavePlot}],FilterRules[{ops},Options@ListLinePlot]]
		]
]


End[]


(* ::Subsection:: *)
(*DVR Set Up*)


`$DVRDimension=1;
`$FormatGrid=Function[{G,P},G];
`$PointLabels={("r"|"R"|"radial")};
`$GridPointsFunction=RadialDVRPoints;
`$KineticMatrixFunction=RadialDVRK;
`$PotentialMatrixFunction=RadialDVRV;
`$WavefunctionsFunction=RadialDVRWavefunctions;
`$PlotFunction=RadialDVRPlot;


EndDVRExtension[]
