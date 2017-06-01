(* ::Package:: *)

(* ::Section:: *)
(*Spherical DVR*)


BeginDVRExtension["SphericalDVR"]


(* ::Subsection:: *)
(*Exposed Code*)


(* ::Subsubsection::Closed:: *)
(*Functions*)


SphericalDVRFormatGrid::usage="Formats a grid appropriately for a DVR"
SphericalDVRPoints::usage="Constructs the spherical DVR grid and packages the appropriate Legendre tranformation"
SphericalDVRK::usage="Constructs the kinetic matrix appropriate for a spherical DVR, given the input values"
SphericalDVRV::usage="Constructs the potential energy matrix from either a potential function or an array of potential energy values at the gridpoints"
SphericalDVRWavefunctions::usage="Using the appropriate K and V, which may be given as inputs to speed up calculations, calculates the eigenvalues and eigenvectors for the constructed Hamiltonian.
If no kinetic matrix is given and it can't import the right kinetic matrix, it will calculate and save it."
PotentialObjectFunction::usage="Returns a function to generate the potential plot object
	PotentialObjectFunction[Gridpoints_,PotentialArray_,radialGridpoints_:None,angularGridpoints_:None,azimuthalGridpoints_:None,DefaultDrawMode_:Point,DefaultColorFunction_:\"FuschiaTones\",DefaultSize_:Small]"
WavefunctionPlotFunction::usage="Returns a function to plot the wave function and potential
	WavefunctionPlotFunction[Gridpoints_,WavefunctionFunction_,EnergyLevelFunction_,PointColoringFunction_,PointFunction_,FieldPoints_,radialGridpoints_:None,angularGridpoints_:None,azimuthalGridpoints_:None,radialSelectionDefault_:All,angularSelectionDefault_:All,azimuthalSelectionDefault_:All,shiftEnergies_:False,waveFunctionDigits_:10,boxSize_:None,axes_:{},pointSize_:Small,ops:OptionsPattern[]]
	->PlottingFunction["
SphericalDVRPlot::usage="Either provides plotting functions or a plot of the wavefunctions, depending on the value of Return given"


(*VariableTrace::usage="Takes a single, specific trace out of a wave-grid"*)


(*TraceViewer::usage="Provides a manipulator to look at the various trace functions"*)


(* ::Subsubsection::Closed:: *)
(*Plotting Options*)


SynchronousAdjustment::usage="(boolean) Option name for SphericalDVRPlot"
PotentialSize::usage="(graphics size specifier) Option name for SphericalDVRPlot"

EnergyDigits::usage="(integer) Option name for SphericalDVRPlot"
Interactivity::usage="(None,Less,Normal,More) Option name for SphericalDVRPlot"
PotentialMode::usage="(True,False,Point,Line,'Dispersed',Blend) Option name for SphericalDVRPlot"
WavefunctionValueMin::usage="(0<x<=1) Option name for SphericalDVRPlot"
WavefunctionValueMax::usage="(0<x<=1) Option name for SphericalDVRPlot"

WavefunctionSize::usage="(graphics size specifier) Option name for SphericalDVRPlot"
WavefunctionShape::usage="(Point,Line) Option name for SphericalDVRPlot"
Manipulator\[Psi]ValueStep::usage="Option name for SphericalDVRPlot"
WavefunctionSelection::usage="(All,Integer,List) Option name for SphericalDVRPlot"
RadialPointSelection::usage="(All,Integer,List) Option name for SphericalDVRPlot"
AzimuthalPointSelection::usage="(All,Integer,List) Option name for SphericalDVRPlot"
PolarPointSelection::usage="(All,Integer,List) Option name for SphericalDVRPlot"
ZeroPointEnergy::usage="(Number) Option for SphericalDVRPlot"
PotentialSelection::usage="(All,Integer,'Radial','Azimuthal','Polar',List) Option name for SphericalDVRPlot"


(* ::Subsubsection::Closed:: *)
(*Complex Options*)


RotationalConstantA::usage="Specifies the A rotational constant for a PlanePointDVR"
RotationalConstantB::usage="Specifies the B rotational constant for a PlanePointDVR"
RotationalConstantC::usage="Specifies the C rotational constant for a PlanePointDVR"
ReducedMass::usage="Specifies the reduced mass of the complex"


(* ::Subsubsection::Closed:: *)
(*Plotting Option Values*)


Global`More::usage="Option value for Interactivity in SphericalDVRPlot"
Global`Sparse::usage="Option value for WavefunctionSize in SphericalDVRPlot"
Global`Blobby::usage="Option value for WavefunctionSize in SphericalDVRPlot"
Global`Dense::usage="Option value for WavefunctionSize in SphericalDVRPlot"
Global`Ultrafine::usage="Option value for Manipulator\[Psi]ValueStep in SphericalDVRPlot"


(* ::Subsection:: *)
(*Internal Code*)


LoadDVR@"LegendreDVR";
LoadDVR@"MeyerDVR";
LoadDVR@"RadialDVR";


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Useful functions*)


type[ob_]:=Switch[ob,
Null,"Null",
None,"None",
All,"All",
False,"Boolean",
True,"Boolean",
_,ToString[Head[ob]]]
sph2orth[r_,\[Phi]_,\[Theta]_]:={r Cos[\[Phi]] Sin[\[Theta]],r Sin[\[Phi]] Sin[\[Theta]],r Cos[\[Theta]]};
orth2sph[x_,y_,z_]:={Sqrt[x^2+y^2+z^2],180/\[Pi]*ArcTan[z,Sqrt[x^2+y^2]],180/\[Pi]*ArcTan[x,y]};


(* ::Text:: *)
(*Indexing functions for constructing the kinetic energy matrix*)


rIndex[rN_,\[Phi]N_,\[Theta]N_]:=Function[{i,j},
With[{f=Function[k,1+Mod[Floor[(k-1)/\[Phi]N]+1,rN]]},
{f[i],f[j]}]];
\[Phi]Index[rN_,\[Phi]N_,\[Theta]N_]:=Function[{i,j},
With[{f=Function[k,1+Mod[k-1,\[Phi]N]]},
{f[i],f[j]}]];
\[Theta]Index[rN_,\[Phi]N_,\[Theta]N_]:=Function[{i,j},
With[{f=Function[k,1+Floor[(k-1)/(rN*\[Phi]N)]]},
{f[i],f[j]}];]


(* ::Subsubsection::Closed:: *)
(*Grid Formatting*)


(* ::Text:: *)
(*Takes the grid and maybe the gridpoints to return the grid and the transformation matrix to the DVR basis in the Legendre term*)


SphericalDVRFormatGrid[grid_,points_:None]:=
	Module[{\[CapitalLambda]=points},
		\[CapitalLambda]=If[\[CapitalLambda]===None,Length[DeleteDuplicates@Flatten[grid,Depth@grid-3][[All,3]]],\[CapitalLambda][[3]]];
		{grid,LegendreDVR`LegendreDVRPoints[\[CapitalLambda]][[2]]}
	]


(* ::Subsubsection:: *)
(*Grid Points*)


(* ::Text:: *)
(*Takes the number of radial, angular, and azimuthal points and optionally their ranges and generates a 3D grid*)


Clear["SphericalDVRPoints"]
Options[SphericalDVRPoints]={DVRUnits->{1,1,1}};
SphericalDVRPoints[rpoints_Integer,\[Phi]points_Integer,\[Theta]points_Integer,
					rrange:{_?NumericQ,_?NumericQ}:{1,2},
					\[Theta]range:{_?NumericQ,_?NumericQ}:{0,\[Pi]},
					\[Phi]range:{_?NumericQ,_?NumericQ}:{0,2\[Pi]},ops:OptionsPattern[]]:=
	With[{R=RadialDVRPoints[rpoints,Sequence@@rrange],
			\[CapitalPhi]=MeyerDVRPoints[\[Phi]points,Sequence@@\[Phi]range],units=Replace[OptionValue@DVRUnits,{u:{_,_,_}:>u,
									o:(_?NumberQ|_Quantity|DVRUnits[_?NumberQ|_Quantity]):>{o,o,o},
									_:>{1,1,1}}]},
		With[{LTransfANDgrid=With[{p=LegendreDVRPoints[\[Theta]points]},With[{o=Ordering@p[[1]]},{p[[1,o]],p[[2,o]]}]]},
			{Table[{units[[1]]*r,units[[2]]*\[Phi],units[[3]]*\[Theta]},{r,R},{\[Phi],\[CapitalPhi]},{\[Theta],LTransfANDgrid[[1]]}],LTransfANDgrid[[2]]}
			]
	];


(* ::Subsubsection::Closed:: *)
(*Kinetic Energy Matrix (not implemented)*)


(* ::Text:: *)
(*Should, eventually, take the DVR grid and turn it into a kinetic energy matrix. But this involves getting a matrix for the standard spherical system*)


Options[SphericalDVRK]=Flatten[{\[HBar]->1,m->1,Options[LegendreDVRK],Options[RadialDVRK],Options[MeyerDVRK]}];
(*Clear["SphericalDVRK"]
	SphericalDVRK[grid_,ops:OptionsPattern[]]:=
		Module[
			{T,X,
				rX,\[Phi]X,\[Theta]X,
				rK,\[Phi]K,\[Theta]K,
				rN,\[Phi]N,\[Theta]N,
				rind,\[Phi]ind,\[Theta]ind,
				\[Delta]r,\[Delta]\[Phi],\[Delta]\[Theta],
				Kel,K
			},
			{X,T}=grid;
			X=Flatten[X,2];
			{rX,\[Phi]X,\[Theta]X}=DeleteDuplicates/@Array[X[[All,#]]&,3];
			rK=RadialDVRK[rX];\[Phi]K=MeyerDVRK[\[Phi]X];\[Theta]K=LegendreDVRK[{\[Theta]X,T}];
			{rN,\[Phi]N,\[Theta]N}=Length/@{rK,\[Phi]K,\[Theta]K};rind[i_,j_]:=With[{f=Function[k,1+Mod[Floor[(k-1)/\[Phi]N],rN]]},{f[i],f[j]}];\[Phi]ind[i_,j_]:=With[{f=Function[k,1+Mod[k-1,\[Phi]N]]},{f[i],f[j]}];
			\[Theta]ind[i_,j_]:=With[{f=Function[k,1+Floor[(k-1)/(rN*\[Phi]N)]]},{f[i],f[j]}];\[Delta]r[i_,j_]:=KroneckerDelta@@rind[i,j];\[Delta]\[Phi][i_,j_]:=KroneckerDelta@@\[Phi]ind[i,j];\[Delta]\[Theta][i_,j_]:=KroneckerDelta@@\[Theta]ind[i,j];Kel[i_,j_]:=(
				((\[Delta]\[Phi][i,j]*\[Delta]\[Theta][i,j])rK[[ Sequence@@rind[i,j] ]])+
				((\[Delta]r[i,j]*\[Delta]\[Theta][i,j])\[Phi]K[[ Sequence@@\[Phi]ind[i,j] ]])+
				((\[Delta]\[Phi][i,j]*\[Delta]r[i,j])\[Theta]K[[ Sequence@@\[Theta]ind[i,j] ]])
				);
			K=Array[Kel,{rN*\[Phi]N*\[Theta]N,rN*\[Phi]N*\[Theta]N}]
		]*)


(* ::Subsubsection:: *)
(*Potential Energy Matrix*)


(* ::Text:: *)
(*Takes the grid points and options (critically a PotentialFunction) to create a potential energy matrix.*)


Clear["SphericalDVRV"]
Options[SphericalDVRV]=Flatten[{
	PotentialFunction->Function[{r,\[Phi],\[Theta]},If[r<10^-10,1/r^2,-(1/r^2)]],
	DVRUnits->1.}];
SphericalDVRV[gridpoints_,ops:OptionsPattern[]]:=
	Module[
		{P=OptionValue[PotentialFunction],X,T,u=Replace[OptionValue@DVRUnits,Automatic->1.]},
		(*Print@u;*)
		{X,T}=gridpoints;
		X={#1,#2,ArcCos[#3]}&@@@Cases[X,{_?NumericQ,_?NumericQ,_?NumericQ},\[Infinity]];
		X=SortBy[X,Last];
		With[{L=Length[X]},SparseArray[{i_,j_}/;i==j:>u*(P@@X[[i]]),{L,L}]]
	]


(* ::Subsubsection::Closed:: *)
(*Wavefunctions*)


(* ::Text:: *)
(*Generates the wavefunctions from the Kinetic and Potential matrices*)


Clear["SphericalDVRWavefunctions"]
SphericalDVRWavefunctions[kmat_,vmat_]:=
	With[{eigenSystem=Eigensystem[kmat+vmat]},
		With[{O=Ordering[eigenSystem[[1]]]},
			With[{\[CapitalLambda]=eigenSystem[[1,O]],\[CapitalPsi]=eigenSystem[[2,O]]},
				With[{phaseMap=Sign/@\[CapitalPsi][[1]]},
					{\[CapitalLambda],phaseMap*#&/@\[CapitalPsi]}
					]
				]
			]
		];


(* ::Subsubsection::Closed:: *)
(*Wavefunction Plotting Function*)


Options[WavefunctionPlotFunction]=Flatten[{Verbose->False,AddTo->{},Options[Graphics3D]}];

WavefunctionPlotFunction[(*Should have 16 arguments*)
	Gridpoints_,WavefunctionFunction_,EnergyLevelFunction_,
	PointColoringFunction_,PointFunction_,FieldPoints_,
	radialGridpoints_:None,angularGridpoints_:None,azimuthalGridpoints_:None,
	radialSelectionDefault_:All,angularSelectionDefault_:All,azimuthalSelectionDefault_:All,
	waveFunctionDigits_:10,boxSize_:None,pointSpec_:Small,axes_:{},ops:OptionsPattern[]]:=
	Module[{WavefunctionPlotFunction,X=Gridpoints,\[Psi]=WavefunctionFunction,\[CapitalLambda]=EnergyLevelFunction,
				coFunc=PointColoringFunction,pointFunc=PointFunction,verb=OptionValue@Verbose,epi=OptionValue@AddTo,
				rX=radialGridpoints,\[Phi]X=angularGridpoints,\[Theta]X=azimuthalGridpoints},
		If[rX==None,rX=Gridpoints[[All,1]]];
		If[\[Phi]X==None,\[Phi]X=Gridpoints[[All,2]]];
		If[\[Theta]X==None,\[Theta]X=Gridpoints[[All,3]]];
		(*Print[{radialSelectionDefault,angularSelectionDefault,azimuthalSelectionDefault}];*)
		WavefunctionPlotFunction[wavefunctionNumber_,minimumValue_,maximumValue_,
				potentialBarLegend_:None,wavefunctionValueBarLegend_:None,
				showFieldPoints_:True,showWaveFunction_:True,
				radialSelection_:radialSelectionDefault,
				angularSelection_:angularSelectionDefault,
				azimuthalSelection_:azimuthalSelectionDefault,
				pointSize_:pointSpec,axThing_:axes,
				fieldPoints_:FieldPoints,verbose_:verb,epilog_:epi]:=
				With[{n=wavefunctionNumber,c=minimumValue,cs=maximumValue,pot=potentialBarLegend,
							wvv=wavefunctionValueBarLegend,cpF=showFieldPoints,grF=showWaveFunction,
							colorPoints=fieldPoints},
				Module[
					{
					\[Phi]=\[Psi][n],\[Lambda]=0,
					graphic,R=radialSelection,F=angularSelection,Q=azimuthalSelection,
					coP=With[{csn=If[cs===0,1,cs]},Function[x,coFunc[x/csn]]],
					\[Psi]m=None,\[CapitalPsi]M=None,\[Psi]T=0
					},
					If[MemberQ[R,All],R=rX[[All]],R=rX[[R]]];
					If[MemberQ[F,All],F=\[Phi]X[[All]],F=\[Phi]X[[F]]];
					If[MemberQ[Q,All],Q=Sort[\[Theta]X][[All]],Q=Sort[\[Theta]X][[Q]]];
					(*If[verbose,Print[Gridpoints//MatrixForm]];*)
					graphic=Graphics3D[{
								(*The fields points*)
								If[TrueQ@cpF,
									{PointSize[Small],
										colorPoints[[2]]
									},
									{}
								],
								(*The axes*)
								axThing,
								(*A line to ensure the size of the box does not change*)
								{Opacity[0],
									Line[boxSize{-{1,1,1},{1,1,1}}]
								},
								(*The actual wavefunction points*)
								If[Not@grF,{},
									With[{
										ps=Switch[pointSize,_?NumberQ,PointSize@pointSize,_,pointSize]
										},
										\[Psi]m=\[Infinity];\[CapitalPsi]M=-\[Infinity];\[Psi]T=0;
										Table[
										With[{p=\[CurlyPhi][[-1]]},(*\[Psi]T+=p;*)\[Psi]m=Min[{\[Psi]m,p}];\[CapitalPsi]M=Max[{\[CapitalPsi]M,p}]];
										Which[
											(Not@MemberQ[R,\[CurlyPhi][[1]]]||Not@MemberQ[F,\[CurlyPhi][[2]]]||Not@MemberQ[Q,\[CurlyPhi][[3]]]),
												(*If the point shouldn't be displayed*)
												Null,
											Abs[\[CurlyPhi][[-1]]-\[Lambda]]>cs,
												(*If the wavefunction value is greater than the max there*)
												{Opacity[.5],ps,Black,Point[sph2orth@@\[CurlyPhi][[1;;3]]]},
											Abs[\[CurlyPhi][[-1]]-\[Lambda]]>=c,
												(*If the wavefunction value is greater than or equal to the minimum there*)
												{ps,coP[Abs[\[CurlyPhi][[-1]]-\[Lambda]]],pointFunc[\[CurlyPhi],\[Lambda]]},
											True,
												(*Otherwise have no point*)
												Null]
										,{\[CurlyPhi],\[Phi]}]
										]
									],
								Replace[epilog,Graphics3D[l_]:>l]
								},
								(*Graphics options*)
								FilterRules[{ops},Except[AddTo|Verbose|Axes]],
								Axes->If[axThing==={},False,True]
								]//Legended[#,(Panel[Column[{
										Row[{Subscript["E",ToString[n]],": ",1.*Round[\[CapitalLambda][n],10^(-waveFunctionDigits)]}],
										Row[{If[grF,Column[{"\[Psi]-value",wvv}],""],If[Not@TrueQ[pot==Null]&&cpF,Column[{" Potential",pot}],""]}],
										Row@{{Subscript["\[Psi]","min"],NumberForm[\[Psi]m,waveFunctionDigits]}//Row,Spacer[10],{Subscript["\[Psi]","max"],NumberForm[\[CapitalPsi]M,waveFunctionDigits]}//Row(*,Spacer[10],{Subscript["\[Psi]","total"],NumberForm[\[Psi]T,waveFunctionDigits]}//Row*)}
										},Dividers->Center]](*//Inset[#,{Left,Top},{Left,Top}]&*))]&(*;

					Row[{
					(*Graphic*)
					Column[{
						graphic
						,
						(*Manipulate options*)
						 
						}],
					(*Wave values*)
					If[grF,
						Column[{"\[Psi]-value",wvv}],
						""],
					(*Potential*)
					
					}]*)
				]];
		WavefunctionPlotFunction
		];


(* ::Subsubsection::Closed:: *)
(*Potential Graphics Objects*)


PotentialObjectFunction[Gridpoints_,PotentialArray_,
		radialGridpoints_:None,angularGridpoints_:None,azimuthalGridpoints_:None,
		DefaultDrawMode_:Point,DefaultColorFunction_:"FuschiaTones",
		DefaultSize_:Small,DefaultMode_:All,DefaultCutoff_:(True&)]:=
	Module[{PotentialObjectFunction,pointF,lineF,blendF,blobF,
				rGrid=radialGridpoints,fGrid=angularGridpoints,qGrid=azimuthalGridpoints},
			If[rGrid==None,rGrid=Gridpoints[[All,1]]];
			If[fGrid==None,fGrid=Gridpoints[[All,2]]];
			If[qGrid==None,qGrid=Gridpoints[[All,3]]];
			pointF[A_,F_,X_:Gridpoints,rX_:rGrid,\[Phi]X_:fGrid,\[Theta]X_:qGrid,mode_:All,function_:({Point[#1],PointSize[#2]}&),cutOff_:DefaultCutoff,op_:1,size_:DefaultSize]:=
						Module[{selection,ordering={rX,Sort@\[Phi]X,Sort@\[Theta]X},choices=mode},
							If[MatchQ[choices,_List|_Integer|_String|_Rule]//Not,choices={"Points","Radial","Azimuthal","Polar"}];
							selection={{{},{},{}},{{},{},{}},{{},{},{}}};
							(*Print@choices;*)
							With[{repPattern=Function[{n,v},
								With[{y=Replace[v,{_Integer:>Range[v],_List:>v,_:>{v}}]},
									{1,2,3}/.{n:>(Cases[y,_Integer|All|None|_Spacings|_Select]/.{
													l:{_Integer..}:>Range[Length@ordering[[n]]][[l]],
													{___,None,___}->{},
													o:{Select[r_/;0<r<=1]|Select[{rm_,rM_}/;(0<rm<1&&rm<rM<=1)]}:>(Range@@(Floor/@(Flatten@{o[[1,1]]*Length@ordering[[n]]}))),
													{Spacings[i_]}:>With[{R=Range[Length@ordering[[n]]]},Table[Floor[i*k],{k,Floor[R/i]}]],
													o_:>(Range[Length@ordering[[n]]])}),
												n2_Integer:>Replace[Cases[y,(n2->l_):>l],{({}|{All})->Range[Length@ordering[[n2]]],
																				o:{Select[r_/;0<r<=1]|Select[{rm_,rM_}/;(0<rm<1&&rm<rM<=1)]}:>Range@@(Floor/@(Flatten@{o[[1,1]]*Length@ordering[[n2]]})),
																				{Spacings[i_]}:>With[{R=Range[Length@ordering[[n2]]]},Table[Floor[i*k],{k,Floor[R/i]}]],
																				c_:>Range[Length@ordering[[n2]]][[Flatten@{c}]]}]}]]},
							ReplaceAll[choices/.{"Radial"->1,"Azimuthal"->2,"Polar"->3},
									{(i2:(1|2|3)->x_):>(selection[[i2]]=repPattern[i2,x]),
									c:(1|2|3):>(selection[[c]]={Range[Length@ordering[[1]]],Range[Length@ordering[[2]]],Range[Length@ordering[[3]]]})
									}]
							];
							selection=MapThread[Intersection@@({#1,#2,#3}/.{}->Range[Max@Flatten[{#1,#2,#3}]])&,selection];
							selection=MapThread[#1[[#2]]&,{ordering,selection}];
							(*Print@{cutOff[A[[2]]],A[[2]]};*)
							(*Print@function;*)
							MapThread[
								If[(cutOff[#1]=!=False) && And@@Table[MemberQ[selection[[i]],#2[[i]]],{i,3}],
								{Opacity[op],F[#1],function[sph2orth@@#2,size]},
								{}]&,{A,X}]
							];
			blobF[A_,F_,X_:Gridpoints,mode_:All,cutOff_:DefaultCutoff]:=pointF[A,F,X,rGrid,fGrid,qGrid,mode,({Point[#1],PointSize[#2]}&),cutOff,.1,.2];
			blendF[A_,F_,X_:Gridpoints,rX_:rGrid,\[Phi]X_:fGrid,\[Theta]X_:qGrid,mode_:All,function_:Line,cutOff_:DefaultCutoff]:=lineF[A,F,X,rX,\[Phi]X,\[Theta]X,mode,function,cutOff,1,.01];
			lineF[A_,F_,X_:Gridpoints,rX_:rGrid,\[Phi]X_:fGrid,\[Theta]X_:qGrid,mode_:All,function_:Line,cutOff_:DefaultCutoff,op_:1,size_:DefaultSize]:=
					Module[{switchPattern={-1,-2,-2,-3,-3,-1},choices=mode,points=False,
								basecount,ordering,selection,lengths,angles},
						ordering={
								rX(*Sort@DeleteDuplicates[X[[All,1]]]*),
								\[Phi]X(*Sort@DeleteDuplicates[X[[All,2]]]*),
								Sort@\[Theta]X(*Sort@DeleteDuplicates[X[[All,3]]]*)
								};
							lengths=Length/@ordering;
							angles={2\[Pi]/lengths[[2]],\[Pi]/lengths[[3]]};
							If[MatchQ[choices,_List|_Integer|_String|_Rule]//Not,choices={"Points","Radial","Azimuthal","Polar"}];
							selection={{{},{},{}},{{},{},{}},{{},{},{}}};
							(*Print@choices;*)
							With[{repPattern=Function[{n,v},
								With[{y=Replace[v,{_Integer:>Range[v],_List:>v,_:>{v}}]},
									{1,2,3}/.{n:>(Cases[y,_Integer|All|None|_Spacings|_Select]/.{
													l:{_Integer..}:>Range[Length@ordering[[n]]][[l]],
													{___,None,___}->{},
													o:{Select[r_/;0<r<=1]|Select[{rm_,rM_}/;(0<rm<1&&rm<rM<=1)]}:>(Range@@(Floor/@(Flatten@{o[[1,1]]*Length@ordering[[n]]}))),
													{Spacings[i_]}:>With[{R=Range[Length@ordering[[n]]]},Table[Floor[i*k],{k,Floor[R/i]}]],
													o_:>(Range[Length@ordering[[n]]])}),
												n2_Integer:>Replace[Cases[y,(n2->l_):>l],{({}|{All})->Range[Length@ordering[[n2]]],
																				o:{Select[r_/;0<r<=1]|Select[{rm_,rM_}/;(0<rm<1&&rm<rM<=1)]}:>Range@@(Floor/@(Flatten@{o[[1,1]]*Length@ordering[[n2]]})),
																				{Spacings[i_]}:>With[{R=Range[Length@ordering[[n2]]]},Table[Floor[i*k],{k,Floor[R/i]}]],
																				c_:>Range[Length@ordering[[n2]]][[Flatten@{c}]]}]}]]},
							ReplaceAll[choices/.{"Radial"->1,"Azimuthal"->2,"Polar"->3},
									{(i2:(1|2|3)->x_):>(selection[[i2]]=repPattern[i2,x]),
									c:(1|2|3):>(selection[[c]]={Range[Length@ordering[[1]]],Range[Length@ordering[[2]]],Range[Length@ordering[[3]]]})
									}]
							];
							(*Print@selection;*)
							(*basecount=switchPattern[[1]];*)
							(*switchPattern=Sequence@@switchPattern;*)
							(*Global`anglestest={};*)
							points=MemberQ[choices,("Points"|0)];
							With[{R=Reap[
								(*Global`$TestSymbol=<|0\[Rule]{},1\[Rule]{},2\[Rule]{},3->{}|>;*)
								MapThread[With[{Px=#1,r=#2[[1]],f=#2[[2]],q=#2[[3]],x=sph2orth@@#2,tx={
																Position[ordering[[1]],#2[[1]]][[1,1]],
																	Position[ordering[[2]],#2[[2]]][[1,1]],
																	Position[ordering[[3]],#2[[3]]][[1,1]]
																		}},
											If[cutOff[Px]=!=False,
											Module[{p1,p2,p3,p4,p5,p6,counter=1},
													p1=tx+{1,0,0};p2=tx+{0,1,0};p3=tx+{0,0,1};
													p4=tx-{1,0,0};p5=tx-{0,1,0};p6=tx-{0,0,1};
													If[p1[[1]]>lengths[[1]],p1[[1]]=lengths[[1]]];
													(*If[p2[[2]]>lengths[[2]],p2[[2]]=lengths[[2]]];*)
													If[p3[[3]]>lengths[[3]],p3[[3]]=lengths[[3]]];
													If[p4[[1]]==0,p4[[1]]=1];
													(*If[p5[[2]]==0,p5[[2]]=1];*)
													If[p6[[3]]==0,p6[[3]]=1];
													If[points,Sow[{F[Px],PointSize[Switch[size,_?NumberQ,size+.05,_,size]],Point[x]}]];
													Do[
														With[{reps={1:>With[{y=sph2orth@@(ordering[[#,p[[#]]]]&/@{1,2,3})},With[{cp=x+Normalize[y-x]*Norm[y-x]/2},
																	
																	Sow[{CapForm["Butt"],Thickness[size],Opacity[op],{F[Px],Line[{x,cp}]}}]
																	]],
																2:>With[{ y=splineCircle[{0,0,x[[3]]},Norm@x[[1;;2]], With[{a=angles[[1]],n=tx[[2]],m=p[[2]]},Sort@{f,f+a/2*Sign[m-n]}] ]},
																	(*AppendTo[Global`anglestest,With[{a=angles[[1]],n=tx[[2]],m=p[[2]]},{f,f+a*(m-n)}] ];*)
																	
																	Sow[{CapForm["Butt"],Thickness[size],Opacity[op],{F[Px],y}}]
																	],
																3:>With[{ y=splineCircle[{0,0,0},r,Sort@{q,q+(p[[3]]-tx[[3]]) angles[[2]]/2}]},With[{r1=(GeometricTransformation[#,RotationTransform[-\[Pi]/2,{0,1,0}]]&),r2=(GeometricTransformation[#,RotationTransform[f-angles[[1]]*lengths[[2]]/4,{0,0,1}]]&)},
																	
																	Sow[{CapForm["Butt"],Thickness[size],Opacity[op],{F[Px],r2[r1[y]]}}]]
																	]}},
														If[p!=tx,
															Hold@Print[StringForm["Counter:``|Base:``|R=``,\[CapitalPhi]=``,\[CapitalTheta]=``",counter,tx,Sequence@@p]];
															(*Replace[counter,{
																{i:(1|2|3),v_}:>If[MemberQ[v/.((x_?(#<0&)):>(Length@ordering[[i]]+x+1)),p[[i]]],Replace[i,reps]],
																i:(1|2|3):>Replace[i,reps],
																_:>Null
																}
																];*)
															If[And@@Table[MemberQ[selection[[counter,i]],p[[i]]],{i,3}],Replace[counter,reps]]
															];
															];
															counter=Mod[++counter,3,1];,
															{p,{p1,p2,p3,p4,p5,p6}}
														]													
												]
												]
											]&,
							{A,X}];
							][[2]]},
							If[Length@r>0,R[[1]],R]
							]
						];
			PotentialObjectFunction[X_:Gridpoints,V_:PotentialArray,FP_:DefaultDrawMode,potCo_:DefaultColorFunction,
											rX_:rGrid,fX_:fGrid,qX_:qGrid,mode_:DefaultMode,cutOffFunction_:DefaultCutoff]:=
				With[{coF=Function[P,
					Module[{m,M,A,F},
						A=Diagonal[V];
						m=Min[A];M=Max[A];If[M==m,If[M==0,M=1];m=0];
						F=Function[x,potCo[(x-m)/(M-m)]];
						{BarLegend[{F,{m,M}}],
						Replace[FP,{
							Point:>pointF[A,F,X,rX,fX,qX,mode,({Point[#1],PointSize[#2]}&),cutOffFunction],
							Line:>lineF[A,F,X,rX,fX,qX,mode,Line,cutOffFunction],
							Blend:>blendF[A,F,X,rX,fX,qX,mode,Line,cutOffFunction],
							Blobby:>blobF[A,F,X,rX,fX,qX,mode,cutOffFunction],
							{Point,f_}:>(pointF[A,F,X,rX,fX,qX,mode,f,cutOffFunction]),
							f_:>f[A,F,X,rX,fX,qX,mode,cutOffFunction]
								}](*,
							_,pointF[A,F,X]*)
						}]
					]},
					If[FP===False,{Null,{}},coF[FP]]
				];
		PotentialObjectFunction		
	]


(* ::Subsubsection::Closed:: *)
(*Graphics Code*)


splineCircle::usage="Constructs a circle based on spline points. Comes from StackOverflow"
VariableSetBar::usage="A SetterBar which has a paired slider so that it can display many values compactly"
LabeledInterval::usage="An IntervalSlider with editable label"


Begin["`graphicsStuff`"];


ClearAll[splineCircle];
splineCircle[m_List,r_,angles_List: {0,2 \[Pi]}]:=Module[{seg,\[Phi],start,end,pts,w,k},{start,end}=Mod[angles//N,2 \[Pi]];
If[end<=start,end+=2 \[Pi]];
seg=Quotient[end-start//N,\[Pi]/2];
\[Phi]=Mod[end-start//N,\[Pi]/2];
If[seg==4,seg=3;\[Phi]=\[Pi]/2];
pts=r RotationMatrix[start].#&/@Join[Take[{{1,0},{1,1},{0,1},{-1,1},{-1,0},{-1,-1},{0,-1}},2 seg+1],RotationMatrix[seg \[Pi]/2].#&/@{{1,Tan[\[Phi]/2]},{Cos[\[Phi]],Sin[\[Phi]]}}];
If[Length[m]==2,pts=m+#&/@pts,pts=m+#&/@Transpose[Append[Transpose[pts],ConstantArray[0,Length[pts]]]]];
w=Join[Take[{1,1/Sqrt[2],1,1/Sqrt[2],1,1/Sqrt[2],1},2 seg+1],{Cos[\[Phi]/2],1}];
k=Join[{0,0,0},Riffle[#,#]&@Range[seg+1],{seg+1}];
BSplineCurve[pts,SplineDegree->2,SplineKnots->k,SplineWeights->w]]/;Length[m]==2||Length[m]==3


Options[LabeledInterval]=Flatten@{
	Options@IntervalSlider,
	Options@Labeled,
	FilterRules[Options@InputField,Except[FieldSize]],
	FieldSize->3
	};
SetAttributes[LabeledInterval,HoldFirst];
LabeledInterval[dynamicVar_,variableRange_List,(labelpos__?(Head[#]=!=Rule&)|___),ops:OptionsPattern[]]:=
	DynamicModule[
		{sl,lab,f1,f2,dynamset,variable=If[Head@dynamicVar===Dynamic,dynamicVar,Dynamic@dynamicVar]},
		dynamset[var_Dynamic,val_,part__:None]:=
			With[{hp=Block[{Dynamic=HoldPattern},var]},
				If[part===None,
					Set[hp,val],
					Block[{v=ReleaseHold@hp},Part[v,part]=val;hp=v]
				]
			];
		sl=IntervalSlider[variable,variableRange[[;;Min@{3,Length@variableRange}]],FilterRules[{ops},Options@IntervalSlider]];
		If[Length[variableRange]<4,
			f1=variableRange[[1]]+.25*(variableRange[[2]]-variableRange[[1]]);
			f2=variableRange[[2]]-.25*(variableRange[[2]]-variableRange[[1]]);,
			{f1,f2}=variableRange[[4]]
		];
		lab=Row[{
			InputField[Dynamic[dynamicVar[[1]],
				Function[{v},dynamset[variable,v,1]]],
				FieldSize->OptionValue@FieldSize,
				FilterRules[{ops},Options[InputField]]],
			InputField[Dynamic[dynamicVar[[2]],
				Function[{v},dynamset[variable,v,2]]],
				FieldSize->OptionValue@FieldSize,
				FilterRules[{ops},Options[InputField]]]
			}];
		Labeled[sl,lab,labelpos,FilterRules[{ops},Options@Labeled]]
	]


Clear["VariableSetBar"]
Options[VariableSetBar]=Flatten[{
Options[SetterBar],Options[Animator],Context->Automatic,RealVarName->Automatic,
AdjustmentRange->Automatic,Choices->10,SynchronousAdjustment->True}];
SetAttributes[VariableSetBar,HoldFirst];
vsbcontext=$Context;
VariableSetBar[variable_,variableRange_List,ops:OptionsPattern[]]:=(
	(*Begin["variableSetBarContext`"];*)
	(*variableSetBarContext`dm=*)DynamicModule[
		{S1,S2,adjustrange,
			varrange={1,OptionValue[Choices]},c=0,
			var,
			n=variableRange[[1]],valchoices,setvar,stringAssign,RET,
			cont=Switch[OptionValue[Context],
						Automatic,$Context,
						_,OptionValue[Context]]
		},
			
			adjustrange=Switch[OptionValue[AdjustmentRange],
							Automatic,If[Head@variableRange[[1]]==List//TrueQ,
											{0,Max[{Length[variableRange[[1]]]-OptionValue[Choices],0}]},
											{0,variableRange[[-1]]-OptionValue[Choices]}
										],
							_Integer,{0,OptionValue[AdjustmentRange]},
							_,OptionValue[AdjustmentRange]];
			stringAssign[strVar_,val_]:=ToExpression@(strVar<>"="<>ToString@val);
			SetAttributes[stringAssign,HoldFirst];
			valchoices=Switch[variableRange,
								({_Integer,_Integer,_Integer}|{_Integer,_Integer}),Range@@variableRange,
								_,variableRange];
			varrange[[-1]]=Min@{varrange[[-1]],Length[valchoices]};
			var=If[Head[variable]==Dynamic//TrueQ,
					variable,
					Dynamic[variable]
					];
			S2=If[OptionValue[SynchronousAdjustment],
					Animator[Dynamic[c],Append[adjustrange,1],4,AnimationRunning->False,
						FilterRules[{ops},Options[Animator]]],
					Slider[Dynamic[c],Append[adjustrange,1]],FilterRules[{ops},Options[Slider]]];
			(*setvar=StringReplace[ToString@var,{"Dynamic["->"","]"->""}];*)
			S1=Row[{
				Button["\[LeftArrow]",
					n=ToExpression@setvar;
					If[n-1>=adjustrange[[1]]+varrange[[1]],n+=-1];
					stringAssign[setvar,n];
					If[n<c+varrange[[1]],c+=-1],
					ImageSize->{Small,Medium},
					Appearance->"Frameless"
					],
				Dynamic[
					If[OptionValue[SynchronousAdjustment],
						Which[
							(*n<=varrange[[1]],n=varrange[[1]]+1,
							n>adjustrange[[2]]+varrange[[ 2]],n=adjustrange[[2]]+varrange[[2]],*)
							n<c+varrange[[1]],n+=1;stringAssign[setvar,n],
							n>c+varrange[[-1]],n+=-1;stringAssign[setvar,n]
							];
					n=ToExpression[setvar]
					];
					If[Head@setvar==Symbol,setvar=StringReplace[ToString@var,{"Dynamic["->"","]"->""}]];
					SetterBar[var,valchoices[[c+ varrange[[1]];;c+varrange[[2]] ]](*,Appearance\[Rule]*),FilterRules[{ops},Options[SetterBar]]]
					],
				Button["\[Rule]",
					n=ToExpression@setvar;
					If[n+1<=adjustrange[[2]]+varrange[[-1]],n+=1];
					stringAssign[setvar,n];
					If[n>c+varrange[[-1]],c+=1]
					,ImageSize->{Small,Medium},
					Appearance->"Frameless"
					]
				}];
		Column[{S1,S2}]
		](*;
		End[]
		variableSetBarContext`dm*)
)


End[];


VariableSetBar=`graphicsStuff`VariableSetBar
LabeledInterval=`graphicsStuff`LabeledInterval
splineCircle=`graphicsStuff`splineCircle


(* ::Subsubsection:: *)
(*DVR Plotting Function*)


(* ::Text:: *)
(*Takes the solutions, gridpoints, and the array of potential values to generate a plot of the solutions. The options are too numerous to document them all here.*)


Clear["SphericalDVRPlot"]
Options[SphericalDVRPlot]=Flatten[{
	Return->Manipulator,
	Options[Manipulate],
	FilterRules[Options[Graphics3D],
		Except[(Axes|AxesOrigin|ImageSize|Boxed|SphericalRegion|Ticks|Lighting)]],
	Lighting->"Neutral",
	ImageSize->{350,350},
	Axes->True,
	AxesOrigin->{0,0,0},
	Boxed->False,
	SphericalRegion->True,
	Ticks->{Automatic,None,None},
	ColorFunction->"Rainbow",
	PotentialColoring->"TemperatureMap",
	PotentialSize->Small,
	EnergyDigits->3,
	Interactivity->Normal,
	PotentialMode->True,
	PotentialCutOff->(True&),
	WavefunctionValueMin->10^-10,
	SquareWavefunction->False,
	Joined->True,
	List->False,
	WavefunctionSize->Normal,
	WavefunctionShape->Point,
	Opacity->1,
	WavefunctionValueMax->Automatic,
	Manipulator\[Psi]ValueStep->Small,
	WavefunctionSelection->All,
	RadialPointSelection->All,
	AzimuthalPointSelection->All,
	PolarPointSelection->All,
	PotentialSelection->All,
	Paneled->True,
	Tooltip->True,
	Verbose->False,
	ZeroPointEnergy->Automatic,
	AddTo->{}
}];
SphericalDVRPlot[solutions:{{(_?NumericQ)..},_}|None,gridpoints_List,potential:_List|None:None,ops:OptionsPattern[]]:=
	Module[
		{wavefunctionPlotFunction,wavefunctionPlotWrapper,potentialPlotObjectFunction,potentialPlotObjectWrapper,
			rX,\[Phi]X,\[Theta]X,
			X,T,V,\[CapitalLambda],\[CapitalPsi],\[Psi],axesThing,plotLab,
			waveSet,wavePlot,potentialPlot,maxProb=OptionValue[WavefunctionValueMax],
			runHere,Ec=Max[{10^-30,OptionValue[WavefunctionValueMin]}],
			FP,potentialMode,ps,colorPoints,boxsize,volElFactor,bg=OptionValue[Background],
			pointFunc,coFunc=OptionValue[ColorFunction],potCo=OptionValue[PotentialColoring],
			zpe=OptionValue@ZeroPointEnergy,
			num=OptionValue[WavefunctionSelection],default},default[option_,else_]:=With[{val=OptionValue[option]},
					If[Not[val],else,val,val]
				];
		(*-----------------------------------------------------------------------------*)
		(*Calculates box size and figures out what axes elements to put in*)
		{X,T}=gridpoints;
		(*Return[X];*)
		X={#1,#2,ArcCos[#3]}&@@@Cases[X,{_?NumericQ,_?NumericQ,_?NumericQ},\[Infinity]];
		X=SortBy[X,Last];
		V=Replace[potential,None->ConstantArray[0,Length@X]];
		(*
		Print@V;
		X=SortBy[MapThread[Append[#1,#2]&,{X,V}],#[[3]]&];
		{X,V}={X[[All,;;3]],X[[All,4]]};
		*)
		{rX,\[Phi]X,\[Theta]X}=DeleteDuplicates[X[[All,#]]]&/@{1,2,3};
		plotLab=OptionValue[PlotLabel];
		(*Gets the label for the plot*)
		
		(*the potential is assumed to be an array or sparse array representing the potential at the gridpoints*)
		{\[CapitalLambda],\[CapitalPsi]}=Replace[solutions,None:>{ConstantArray[0,Length@X],ConstantArray[ConstantArray[0,Length@X],Length@X]}];
		Switch[zpe,Automatic,zpe=-\[CapitalLambda][[1]],None,0];
		\[CapitalLambda]=zpe+\[CapitalLambda];
		(*These are the components which come out of SphericalDVRWavefunctions*)
		
		(*Gets the actual values of r,\[Phi], and \[Theta] for use later in the variable trace*)
		boxsize=Max[X[[All,1]]];
		(*The box has a size of max r*)
		axesThing=Which[
			TrueQ[Not[OptionValue[Axes]]],{},
			(*If "Axes" is False*)
			TrueQ[OptionValue[Axes]==None],{},
			(*If "Axes" is None*)
			True,
			(*else*)
				With[{u=(*(.5*Mean[X[[All,1]]])/*)1+boxsize,shift=1.5},
					With[{c=splineCircle[{0,0,0},u],text=Inset[Text["\[Pi]"],{-u,shift,0},{0,0}]},
						{c,text,
						Inset[Text["\[CurlyPhi]=0"],{u,shift,0},{0,0}],
						Inset[Text["\[Theta]=0"],{0,shift,u},{0,0}],
						Inset[Text["\!\(\*FractionBox[\(\[Pi]\), \(2\)]\)"],{0,u,shift},{0,0}],
						Inset[Text["3\!\(\*FractionBox[\(\[Pi]\), \(2\)]\)"],{0,-u,shift},{0,0}],
						GeometricTransformation[{c,text},RotationTransform[-(\[Pi]/2),{0,1,0}]]
						}
					]
				]
			];
		(*-----------------------------------------------------------------------------*)
		(*Puts the wave functions in plottable form*)
		num=Switch[type[num],
			"All",Range[1,Length[\[CapitalLambda]]],
			"Integer",Range[1,num],
			"List",num,
			_,{1}];
			(*Selects the number of wavefunctions to be plotted*)

		If[TrueQ[Head[coFunc]==String],
			coFunc=ColorData[coFunc]];
		volElFactor[r_,\[Phi]_,\[Theta]_]:=r^2 Sin[\[Theta]];
		\[Psi][n_]:=MapThread[Append[#1,If[OptionValue[SquareWavefunction],#2^2,#2]]&,{X,\[CapitalPsi][[n]]}];
		If[TrueQ[maxProb==Automatic],
			With[{m=Max[Abs/@ \[Psi][num[[1]]][[All,4]]]},
			maxProb=Ceiling[  m,10^-2 ]
			]
			];
		(*Gets the max probability used in coloring the gridpoints and calculates the values to be plotted*)
		(*-----------------------------------------------------------------------------*)
		(*Deals with the potential field points (if any are to be used)*)
		Module[{p=OptionValue[PotentialMode],m},
		(*Print@p;*)
		p=With[{baseReplacements={
					"Network"->Line,
					"Traces"->{Line,{1}},
					"Circles"->{Line,{2}},
					"Arcs"->{Line,{3}},
					"Fan"->{Line,{1,3}},
					"Cone"->{Line,{1,2}},
					"Curves"->{Line,{2,3}},
					"Broad Network"->Blend,
					"Broad Traces"->{Blend,{1}},
					"Broad Circles"->{Blend,{2}},
					"Broad Arcs"->{Blend,{3}},
					"Broad Fan"->{Blend,{1,3}},
					"Broad Cone"->{Blend,{1,2}},
					"Broad Curves"->{Blend,{2,3}}}},
					Replace[p,{
								{s:(_String|_Rule),v__}:>(Replace[s,baseReplacements]->If[Length@{v}>1,{v},v]),
								s:(_String|_Rule):>Replace[s,baseReplacements]
								}]	
				];
		(*Print@p;*)
		{p,m}=Replace[p,{
						{x_,y_}:>{x,y},
						{x_,y__}:>{x,{y}},
						((l:(_List(*|_Rule*)):None|v_)->c_):>With[{cho=Replace[c,{
											(_Rule|_RuleDelayed)->{c},
											{(_Rule|_RuleDelayed)..}->c,
											{(_List|_Rule|_RuleDelayed)..}:>MapIndexed[If[MatchQ[#1,(_Rule|_RuleDelayed)],#1,#2[[1]]->#1]&,c],
											o_:>({1->{o,2->o,3->o},2->{o,1->o,3->o},3->{o,1->o,2->o}})
											}]},
										If[l===None,{v,cho},{l[[1]],Cases[Flatten@cho,(*Print@*)(Alternatives@@Thread[(l[[2]])->_])]~Join~Cases[l[[2]],Except@(Alternatives@@First/@cho)]}]
										],
						(a:(_->_)->b_):>{a,b},
						_:>{p,{1,2,3}}
						}];
		FP=Replace[p,{
			(False|None|"None")->False,
			(True|Point)->Point,
			(Joined|Line)->Line,
			("Field"|Blend)->Blend,
			("Dispersed"|"Spread"|Blobby)->Blobby,
			_String->Point,
			
			(a_->b_):>{a,b}
			(*f:(_Function|_Symbol):>{Point,f}*)
			}];
		(*Print@FP;*)
		potentialMode=m
		];
		ps=PointSize[
				With[{ws=OptionValue[WavefunctionSize],D=(Max@rX-Min@rX)/(Length@rX*boxsize)},
					Switch[ws,
						Sparse,D/2,
						Small,.85*D,
						Normal,D,
						Dense,1.25*D,
						Blobby,2*D,
						_,ws]
					]
				];

		If[TrueQ@OptionValue[Tooltip],
			pointFunc[\[CurlyPhi]_,\[Lambda]_]:=Tooltip[
					Point[sph2orth@@\[CurlyPhi][[1;;3]]],
						Column[
							Function[{r,j,q,y},{
										Row[{"R: ",r}],Row[{"\[Phi]: ",j}],Row[{"\[Theta]: ",q}],Row[{"\[Psi](R,\[Phi],\[Theta]): ",y}]
									}]@@(NumberForm[#,OptionValue[EnergyDigits]]&@@@({#1,#2 180/\[Pi],#3 180/\[Pi],#4-\[Lambda]}&@@\[CurlyPhi]))]
					],
			pointFunc[\[CurlyPhi]_,\[Lambda]_]:=Point[sph2orth@@\[CurlyPhi][[1;;3]]]
			];
		(*This function formats a gridpoint as a tooltip, allowing precise information to be gotten about it*)

		If[TrueQ[Head[potCo]==String],potCo=ColorData[potCo]];
	potentialPlotObjectFunction=PotentialObjectFunction[X,V,rX,\[Phi]X,\[Theta]X,FP,potCo,OptionValue[PotentialSize],potentialMode,OptionValue[PotentialCutOff]];
	Options[potentialPlotObjectWrapper]={
					PotentialMode->FP,
					PotentialColoring->potCo,
					PotentialSize->OptionValue[PotentialSize],
					PotentialCutOff->OptionValue[PotentialCutOff],
					Mode->potentialMode};
	potentialPlotObjectWrapper[pOps:OptionsPattern[]]:=
				Module[{
						mode=OptionValue@Mode,
						potentialColoring=OptionValue@PotentialColoring,
						drawMode=Replace[OptionValue@PotentialMode,{
									(False|None|"None")->False,
									(True|Point)->Point,
									(Joined|Line)->Line,
									("Field"|Blend)->Blend,
									("Dispersed"|"Spread"|Blobby)->Blobby,
									_String->Point
									(*f:(_Function|_Symbol):>{Point,f}*)
									}],
						cutOff=OptionValue@PotentialCutOff
						},
					If[Head@potentialColoring==String,potentialColoring=ColorData[potentialColoring]];
					potentialPlotObjectFunction[X,V,drawMode,potentialColoring,rX,\[Phi]X,\[Theta]X,mode,cutOff]
					];
	colorPoints=potentialPlotObjectFunction[];
	(*This produces the fieldpoints. 
		Should be rewritten to format them as lines between adjacent field points, 
			giving a spider-web form of the potential.
		Find same \[CurlyPhi],\[Theta] closest r, same r,\[CurlyPhi] closest \[Theta] and same r,\[Theta] closest \[CurlyPhi].
		Draw as average color (as average values colored by the coloring function)*)
	(*-----------------------------------------------------------------------------*)
	(*Does the actual dirty work of plotting*)
			(*takes the points and makes a graph out of them*) 
	With[{eFunc=Function[{n},\[CapitalLambda][[n]]]},

	wavefunctionPlotFunction=WavefunctionPlotFunction[X,\[Psi],eFunc,
			coFunc,pointFunc,colorPoints,
			rX,\[Phi]X,\[Theta]X,
			OptionValue[RadialPointSelection],OptionValue[AzimuthalPointSelection],OptionValue[PolarPointSelection],
			OptionValue[EnergyDigits],boxsize,axesThing,ps,
			FilterRules[
					With[{pops=Alternatives@@(#[[1]]&/@ops)//Except},
					Flatten[
						{
							PlotLabel->plotLab,
							FilterRules[Options[SphericalDVRPlot],pops],
							ops
							}
						]
						],
						Options[WavefunctionPlotFunction]
					]
				];
	Options[wavefunctionPlotWrapper]=With[{base={
						PotentialMode->colorPoints,
						Number->num[[1]],
						Axes->axesThing,
						Min->1.Ec,
						Max->maxProb,
						PointSize->ps,
						AddTo->OptionValue@AddTo,
						PotentialColoring->potCo,
						ColorFunction->coFunc,
						BarLegend->colorPoints[[1]],
						PlotLegends->{True,True},
						RadialPointSelection->OptionValue@RadialPointSelection,
						AzimuthalPointSelection->OptionValue@AzimuthalPointSelection,
						PolarPointSelection->OptionValue@PolarPointSelection}},
					Flatten[{base,FilterRules[{ops},Except[Alternatives@@(#[[1]]&/@base)]]}]
				];
	wavefunctionPlotWrapper[wPlotOps:OptionsPattern[]]:=
		With[{
			potentialColoring=With[{p=OptionValue@PotentialColoring},If[TrueQ@(Head[p]==String),ColorData[p],p]],
			colFunc=With[{v=OptionValue@ColorFunction},If[TrueQ@(Head[v]==String),
							ColorData[v],v]],
			pSize=OptionValue@PointSize,
			legends=OptionValue@PlotLegends,
			n=OptionValue@Number,min=OptionValue@Min,max=OptionValue@Max,
			rSel=OptionValue@RadialPointSelection,
			\[Phi]Sel=OptionValue@AzimuthalPointSelection,
			\[Theta]Sel=OptionValue@PolarPointSelection,
			epi=OptionValue@AddTo,
			ax=OptionValue@Axes,
			dops=FilterRules[{wPlotOps},Options[Graphics3D]],
			pm=OptionValue@PotentialMode},
			With[{s=Sequence[n,min,max,
					OptionValue@BarLegend,BarLegend[{Function[x,colFunc[x/If[max===0,max,1]]],{0,max}}],
					TrueQ@legends[[1]],TrueQ@legends[[2]],
					rSel,\[Phi]Sel,\[Theta]Sel,
					pSize,ax,pm,epi]},
				If[(Length@dops)>0,wavefunctionPlotFunction[s,dops],wavefunctionPlotFunction[s]]
				]
			]
		];
	Switch[OptionValue[Return],
		"Inputs",
			{X,V,\[Psi]},
		"Functions",
			Do[With[{sym=sym},Attributes[sym]={}],{sym,{wavefunctionPlotFunction,wavefunctionPlotWrapper,potentialPlotObjectFunction,potentialPlotObjectWrapper}}];
			<|"Wavefunction Plotter"->wavefunctionPlotFunction,
				"Wavefunction"->wavefunctionPlotWrapper,
				"Potential Plotter"->potentialPlotObjectFunction,
				"Potential"->potentialPlotObjectWrapper|>,
		"Potential",
			With[{g=colorPoints(*potentialPlotObjectWrapper[]*)},Graphics3D[g[[2]],Boxed->OptionValue@Boxed] ],
		"Wavefunction",
			wavefunctionPlotWrapper[],
		_,
			Module[{pstep=Switch[OptionValue[Manipulator\[Psi]ValueStep],
							Ultrafine,.00001,
							Fine,.0001,
							Small,.005,
							Medium,.01,
							Coarse,.1,
							_,1.OptionValue[Manipulator\[Psi]ValueStep]]
			(*dofunc*)},
			(*dofunc=wavefunctionPlotFunction*)
			Module[{manipulate=True,args,drawops},
			
				Switch[OptionValue[Interactivity],
					None,(*Provides just the graphic*)
						manipulate=False;
						args=HoldComplete@Sequence[num[[1]],1.Ec,maxProb,colorPoints[[1]],BarLegend[{Function[x,coFunc[x/If[maxProb===0,1,maxProb]]],{0,maxProb}}],True,True],
						(*drawops=Control@{{n,num[[1]],"\[Psi]"},ControlType->None},*)
					Less,(*Lets the \[Psi] number vary and the minimum probability*)
						args=HoldComplete@Sequence[n,1.Ec,maxProb,colorPoints[[1]],BarLegend[{Function[x,coFunc[x/If[maxProb===0,1,maxProb]]],{0,maxProb}}],fF,gF];
						drawops=Sequence[
							Control@{{n,num[[1]],"\[Psi]"},
								VariableSetBar[#,{num},
									SynchronousAdjustment->False,
									ImageSize->Small]&,
								ControlPlacement->Top},
							(*Column[{*)
							Row@{Control@{{fF,True,"Show Potential   "},{True,False}},Control@{{gF,True,"Show Wavefunction"},{True,False}}},
								(*Control@{{c,{1.Ec,maxProb},"\[Psi]-Bounds"},1.(10^-55),1,pstep,ControlType\[Rule]IntervalSlider}*)
								(*},Alignment->Left],*)
							ControlPlacement->Bottom,
							Paneled->False],
		
					Normal,(*Lets the \[Psi] number, minimum probability, and max probability vary*)
						args=HoldComplete@Sequence[n,c[[1]],c[[2]],colorPoints[[1]],BarLegend[{Function[x,coFunc[x/c[[2]]]],{0,c[[2]]}}],fF,gF];
						drawops=Sequence[
							Control@{{n,num[[1]],"\[Psi]"},
								VariableSetBar[#,num,
									SynchronousAdjustment->False,
									ImageSize->Small]&,
								ControlPlacement->Top
							},
							Column[{
								Control@{{c,{1.Ec,1.maxProb},"\[Psi]-Bounds"},LabeledInterval[#,{0,1,pstep},Bottom,FieldSize->10,Appearance->Frameless,Method->"Stop"]&},
								Row@{Control@{{fF,True,"Show Potential   "},{True,False}} ,Control@{{gF,True,"Show Wavefunction"},{True,False}} }
								},Alignment->Left],
							ControlPlacement->Bottom,
							Paneled->False],
					Global`More,(*Lets the \[Psi] number, min prob, max prob, and r, \[Phi], \[Theta] selections vary*)
						args=HoldComplete@Sequence[n,c[[1]],c[[2]],colorPoints[[1]],BarLegend[{Function[x,coFunc[x/c[[2]]]],{0,c[[2]]}}],fF,gF,r,f,q];
						drawops=Sequence[
							Control@{{n,num[[1]],"\[Psi]"},
								VariableSetBar[#,num,
									SynchronousAdjustment->False,
									ImageSize->Small]&,
								ControlPlacement->Top},
							Grid[{
								{Control@{{c,{1.Ec,1.maxProb},"\[Psi]-Bounds"},LabeledInterval[#,{0,1,pstep},Bottom,FieldSize->10,Appearance->Frameless,Method->"Stop"]&},Control@{{r,Flatten@{OptionValue[RadialPointSelection]},"R Selection"},Append[Range[1,Length[rX]],All],ControlType->TogglerBar} },
								{Control@{{fF,True,"Show Potential   "},{True,False}},Control@{{f,Flatten@{OptionValue[AzimuthalPointSelection]},"\[Phi] Selection"},Append[Range[1,Length[\[Phi]X]],All],ControlType->TogglerBar} },
								{Control@{{gF,True,"Show Wavefunction"},{True,False}},Control@{{q,Flatten@{OptionValue[PolarPointSelection]},"\[Theta] Selection"},Append[Range[1,Length[\[Theta]X]],All],ControlType->TogglerBar} }
								},Alignment->Left],
							ControlPlacement->Bottom,
							Paneled->False],
					Most,
						args=HoldComplete@With[{cps=potentialPlotObjectWrapper[PotentialMode->mo,Mode->ms]},
										With[{s=List[n,c[[1]],c[[2]],cps[[1]],BarLegend[{Function[x,coFunc[x/c[[2]]]],{0,c[[2]]}}],fF,gF,r,f,q,pos,ax,cps]},Sequence@@s]];
						drawops=Sequence[
							Control@{{n,num[[1]],"\[Psi]"},
								VariableSetBar[#,num,
									SynchronousAdjustment->False,
									ImageSize->Small]&,
								ControlPlacement->Top},
							Grid[{
								{Control@{{c,{1.Ec,1.maxProb},"\[Psi]-Bounds"},LabeledInterval[#,{0,1,pstep},Bottom,FieldSize->10,Appearance->Frameless,Method->"Stop"]&},Control@{{r,Flatten@{OptionValue[RadialPointSelection]},"R Selection"},Prepend[Range[1,Length[rX]],All],ControlType->TogglerBar} },
								{Control@{{pos,ps,"Point Size"},0,.2},Control@{{f,Flatten@{OptionValue[AzimuthalPointSelection]},"\[Phi] Selection"},Prepend[Range[1,Length[\[Phi]X]],All],ControlType->TogglerBar} },
								{Row@{Control@{{fF,True,"Show Potential"},{True,False}},Control@{{gF,True,"Show Wavefunction"},{True,False}}},Control@{{q,Flatten@{OptionValue[PolarPointSelection]},"\[Theta] Selection"},Prepend[Range[1,Length[\[Theta]X]],All],ControlType->TogglerBar} },
								{Control@{{mo,OptionValue@PotentialMode,"Potential Mode"},{Point->"Points",Blobby->"Dispersed",Line->"Network",Blend->"Broad Network"}},
											Control@{{ms,Flatten@{OptionValue@PotentialSelection},"Potential Selection"},{"Points","Radial","Azimuthal","Polar"},ControlType->TogglerBar}},
								{Control@{{ax,axesThing,"Show Axes"},{{},axesThing},ControlType->Checkbox}}
								},Alignment->Left],
							ControlPlacement->Bottom,
							Paneled->False],
					_,
						args=HoldComplete@Sequence[n,1.Ec,maxProb,colorPoints[[1]],BarLegend[{Function[x,coFunc[x/maxProb]],{0,maxProb}}],True,True];
						drawops=Sequence[
							Control@{{n,num[[1]],"\[Psi]"},
								VariableSetBar[#,num,
									SynchronousAdjustment->False,
									ImageSize->Small]&},
							Paneled->False
							]
					];
				If[TrueQ@OptionValue[Paneled],
					Panel,
					Identity]
				(*Module[{m,a,o},*)
					@With[{d=wavefunctionPlotFunction,a=args,o=drawops},
						If[manipulate==True,
							Manipulate[Column[{
								With[{sub=Subscript["\[Psi]",n]},
									If[OptionValue[SquareWavefunction],Row[{sub,"\[Conjugate]",sub}],sub]],
										d[ReleaseHold@a]
									}],
								o],
							Column[{
								With[{sub=Subscript["\[Psi]",num[[1]]]},
									If[OptionValue[SquareWavefunction],Row[{sub,"\[Conjugate]",sub}],sub]],
										d[ReleaseHold@a]
								}]
							]
						]
				]
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Trace Viewing Function*)


(* ::Text:: *)
(*Should take the array of wavefunctions and the grid points to make a trace of any set of r, \[Phi], or \[Theta] points*)


(*VariableTrace[waveGrid_,OptionsPattern[{R->Variable,\[CapitalPhi]->1,\[CapitalTheta]->1}]]:=
Module[{G=waveGrid,
rChoice,\[Phi]Choice,\[Theta]Choice,
condition,set,sortFunction,
o\[CapitalPhi]={},o\[CapitalTheta]={},oR={},results={}},
While[Length[G[[1]]]!=4,G=Flatten[G,1]];
rChoice=With[{r=OptionValue[R]},
Which[TrueQ[r==Variable],True,
TrueQ[Head[r]==List],oR=Sort[DeleteDuplicates[waveGrid[[All,1]]]];r,
TrueQ[Head[r]==Integer],oR=Sort[DeleteDuplicates[waveGrid[[All,1]]]];{r},
True,True
]
];
\[Phi]Choice=With[{\[Phi]=OptionValue[\[CapitalPhi]]},
Which[TrueQ[\[Phi]==Variable],True,
TrueQ[Head[\[Phi]]==List],o\[CapitalPhi]=Sort[DeleteDuplicates[waveGrid[[All,2]]]];\[Phi],
TrueQ[Head[\[Phi]]==Integer],o\[CapitalPhi]=Sort[DeleteDuplicates[waveGrid[[All,2]]]];{\[Phi]},
True,True
]
];
\[Theta]Choice=With[{\[Theta]=OptionValue[\[CapitalTheta]]},
Which[TrueQ[\[Theta]==Variable],True,
TrueQ[Head[\[Theta]]==List],o\[CapitalTheta]=Sort[DeleteDuplicates[waveGrid[[All,3]]]];\[Theta],
TrueQ[Head[\[Theta]]==Integer],o\[CapitalTheta]=Sort[DeleteDuplicates[waveGrid[[All,3]]]];{\[Theta]},
True,True
]
];
rChoice=If[TrueQ[rChoice],rChoice,oR[[rChoice]]];
\[Phi]Choice=If[TrueQ[\[Phi]Choice],\[Phi]Choice,o\[CapitalPhi][[\[Phi]Choice]]];
\[Theta]Choice=If[TrueQ[\[Theta]Choice],\[Theta]Choice,o\[CapitalTheta][[\[Theta]Choice]]];
condition[list_]:=
(TrueQ[rChoice]||MemberQ[rChoice,list[[1]]])&&
(TrueQ[\[Phi]Choice]||MemberQ[\[Phi]Choice,list[[2]]])&&
(TrueQ[\[Theta]Choice]||MemberQ[\[Theta]Choice,list[[3]]]);
Select[waveGrid,condition]
]*)


End[]


(* ::Subsection:: *)
(*DVR Set Up*)


End[]


`SphericalDVR`$DVRDimension=3;
(*`$DVRUnits={{}};*)
`SphericalDVR`$FormatGrid=`SphericalDVR`SphericalDVRFormatGrid;
`SphericalDVR`$PointLabels=Flatten@{`RadialDVR`$PointLabels,`MeyerDVR`$PointLabels,`LegendreDVR`$PointLabels};
`SphericalDVR`$GridPointsFunction=`SphericalDVR`SphericalDVRPoints;
`SphericalDVR`$KineticMatrixFunction=`Spherical`SphericalDVRK;
`SphericalDVR`$PotentialMatrixFunction=`SphericalDVR`SphericalDVRV;
`SphericalDVR`$WavefunctionsFunction=`SphericalDVR`SphericalDVRWavefunctions;
`SphericalDVR`$PlotFunction=`SphericalDVR`SphericalDVRPlot;


EndDVRExtension[];
