(* ::Package:: *)

(* ::Section:: *)
(*Prolate Top DVR*)


BeginDVRExtension@"ProlateTopDVR";


(* ::Subsection::Closed:: *)
(*Exposed Code*)


ProlateTopDVRK::usage="The adjusted kinetic operator form of SphericalDVRK"


(* ::Subsection:: *)
(*Internal Code*)


LoadDVR/@{"SphericalDVR","LegendreDVR","RadialDVR","MeyerDVR"}


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Kinetic Energy Matrix*)


Clear["ProlateTopDVRK"]
	Options[ProlateTopDVRK]=Flatten[{
			m->1,
			DVRUnits->{1,1,1},
			RotationalConstantA->10,
			RotationalConstantB->1,
			RotationalConstantC->1,
			Options[LegendreDVRK],
			Options[RadialDVRK],
			Options[MeyerDVRK]}];
	ProlateTopDVRK[grid_,ops:OptionsPattern[]]:=
		Module[
			{T,X,
				rX,\[Phi]X,\[Theta]X,
				rK,\[Phi]K,\[Theta]K,
				rN,\[Phi]N,\[Theta]N,
				rind,\[Phi]ind,\[Theta]ind,
				A=OptionValue@RotationalConstantA,
				B=OptionValue@RotationalConstantB,
				C=OptionValue@RotationalConstantC,
				\[Delta]r,\[Delta]\[Phi],\[Delta]\[Theta],
				Kel,K,units=Replace[OptionValue@DVRUnits,{Automatic|None->{1,1,1}}]
			},
			(*Global`scheeeeemps=10;*)
			{X,T}=grid;
			X=Cases[X,{_?NumericQ,_?NumericQ,_?NumericQ},\[Infinity]];
			{rX,\[Phi]X,\[Theta]X}=DeleteDuplicates/@Array[X[[All,#]]&,3];
			rK=units[[1]]*RadialDVRK[rX];\[Phi]K=units[[2]]*MeyerDVRK[\[Phi]X];\[Theta]K=units[[3]]*LegendreDVRK[{\[Theta]X,T}];
			{rN,\[Phi]N,\[Theta]N}=Length/@{rK,\[Phi]K,\[Theta]K};
			rind[i_,j_]:=Function[k,1+Mod[Floor[(k-1)/\[Phi]N],rN]]/@{i,j};
			\[Phi]ind[i_,j_]:=Function[k,1+Mod[k-1,\[Phi]N]]/@{i,j};
			\[Theta]ind[i_,j_]:=Function[k,1+Floor[(k-1)/(rN*\[Phi]N)]]/@{i,j};
			\[Delta]r[i_,j_]:=KroneckerDelta@@rind[i,j];
			\[Delta]\[Phi][i_,j_]:=KroneckerDelta@@\[Phi]ind[i,j];
			\[Delta]\[Theta][i_,j_]:=KroneckerDelta@@\[Theta]ind[i,j];
			Kel[i_,j_]:=(
				((\[Delta]\[Phi][i,j]*\[Delta]\[Theta][i,j])rK[[ Sequence@@rind[i,j] ]])+
				((\[Delta]r[i,j]*\[Delta]\[Theta][i,j])(A-1/2(B+C))\[Phi]K[[ Sequence@@\[Phi]ind[i,j] ]])+
				((\[Delta]\[Phi][i,j]*\[Delta]r[i,j])(1/2(B+C))\[Theta]K[[ Sequence@@\[Theta]ind[i,j] ]])
				);
			K=Parallelize[Compile[{rN,\[Phi]N,\[Theta]N},Array[Kel,{rN*\[Phi]N*\[Theta]N,rN*\[Phi]N*\[Theta]N}]]]
		]


End[]


(* ::Subsection:: *)
(*DVR Set Up*)


End[]


`ProlateTopDVR`$DVRDimension=3;
`ProlateTopDVR`$FormatGrid=`SphericalDVR`$FormatGrid;
`ProlateTopDVR`$PointLabels=`SphericalDVR`$PointLabels;
`ProlateTopDVR`$GridPointsFunction=`SphericalDVR`SphericalDVRPoints;
`ProlateTopDVR`$KineticMatrixFunction=ProlateTopDVRK;
`ProlateTopDVR`$PotentialMatrixFunction=`SphericalDVR`SphericalDVRV;
`ProlateTopDVR`$WavefunctionsFunction=`SphericalDVR`SphericalDVRWavefunctions;
`ProlateTopDVR`$PlotFunction=`SphericalDVR`SphericalDVRPlot;


EndDVRExtension[]
