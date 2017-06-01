(* ::Package:: *)

(* ::Section:: *)
(*Plane Point DVR (extends Spherical DVR)*)


(* ::Subsection:: *)
(*Exposed Code*)


(* ::Subsubsection:: *)
(*Initializing Package*)


BeginDVRExtension@"PlanePointDVR";


(* ::Subsubsection::Closed:: *)
(*Exposed Functions*)


PlanePointDVRK::usage="The adjusted kinetic operator form of ProlateTopDVRK"


(* ::Subsection:: *)
(*Internal Code*)


LoadDVR/@{"SphericalDVR","LegendreDVR","RadialDVR","MeyerDVR"}


Begin["`Private`"]


(* ::Subsubsection::Closed:: *)
(*Defining Matrix Representations*)


cot\[Theta]d\[Theta]2Matrix[n_]:=SparseArray[Band[{1,1}]->Array[(-2 #^2*(1./(2#+1)))+(-((2.#)/(2#+1)))&,n],{n,n}]
d\[Theta]2Matrix[n_,\[Theta]grid_]:=Module[{el,kd=KroneckerDelta},
	el[i_,j_]:=With[{qi=\[Theta]grid[[i]],qj=\[Theta]grid[[j]]},
		If[i==j,((n-1.)(n+2.)+(n^2+n+6)qi)/(3(1.-qi^2)^2),
		((2.qi)/((qi-qj)(1-qj^2))-2./(qi-qj)^2)Sqrt[((1-qj^2)/(1-qi^2))]
		]
		];
	Array[el,{n,n}]
	]
d\[CurlyPhi]Matrix[N_]:=Array[With[{i=#1,j=#2},If[i==j,0.,(-1)^(i-j)/(2.*Tan[(\[Pi](i-j))/N])]]&,{N,N}]
d\[CurlyPhi]2Matrix[N_]:=Array[With[{i=#1,j=#2},
If[i==j,
-(1.) (1/2 N^2+1)/6,
(-1)^(i-j) 1./(2Sin[(\[Pi](i-j))/N]^2)]
]&,{N,N}]


(* ::Subsubsection:: *)
(*Kinetic Energy Matrix*)


kineticUnitSpec={
	"RadialCoordinate","RadialMatrix",
	"AzimuthalCoordinate","AzimuthalMatrix",
	"PolarCoordinate","PolarMatrix",
	"JBJCMatrix","EffectiveAdjustment"};
Options[PlanePointDVRK]=Flatten[{
Options[SphericalDVRK],
DVRUnits->Thread[kineticUnitSpec->1.],
RotationalConstantA->10,
RotationalConstantB->3,
RotationalConstantC->1,
ReducedMass->1,
DVR\[HBar]->1
}];
PlanePointDVRK[grid_,ops:OptionsPattern[]]:=
	Module[
		{T,X,
		rX,\[Phi]X,\[Theta]X,
		rK,\[Phi]K,\[Theta]K,
		rN,\[Phi]N,\[Theta]N,
		rind,\[Phi]ind,\[Theta]ind,
		cotd\[Theta]mat,d\[Theta]2mat,
		runits,\[Phi]units,\[Theta]units,
		d2cotdmat,hb=OptionValue@DVR\[HBar],
		djmat,dj2mat,\[HBar]=OptionValue@DVR\[HBar],
		jbU,rCoord,aCoord,pCoord,efU,
		Am=OptionValue[RotationalConstantA],
		Bm=OptionValue[RotationalConstantB],
		Cm=OptionValue[RotationalConstantC],
		\[Mu]=OptionValue[ReducedMass],
		units=Replace[OptionValue@DVRUnits,{
			None:>Thread[kineticUnitSpec->1.],
			l_List:>Thread[kineticUnitSpec->((kineticUnitSpec/.l)/.
						Thread[kineticUnitSpec->1.])],
			(o_?(MatchQ[#,Except[_Rule|_RuleDelayed]]&)):>Thread[kineticUnitSpec->o]
			r_:>(Thread[kineticUnitSpec->1.]/.((r[[1]]->_):>r))
		}],
		Kel,K},
		{X,T}=grid;X=Flatten[X,Depth@X-3];{rX,\[Phi]X,\[Theta]X}=DeleteDuplicates/@Array[X[[All,#]]&,3];
		rK=("RadialMatrix"/.units)*RadialDVRK[rX];
		\[Phi]K=("AzimuthalMatrix"/.units)*MeyerDVRK[\[Phi]X];
		\[Theta]K=("PolarMatrix"/.units)*LegendreDVRK[{\[Theta]X,T}];
		{rN,\[Phi]N,\[Theta]N}=Length/@{rK,\[Phi]K,\[Theta]K};
		jbU=("JBJCMatrix"/.units);
		efU=("EffectiveAdjustment"/.units);
		With[
			{rf=Function[k,1+Mod[Floor[(k-1)/\[Phi]N],rN]],
			ff=Function[k,1+Mod[k-1,\[Phi]N]],
			qf=Function[k,1+Floor[(k-1)/(rN*\[Phi]N)]]},
			rind[i_,j_]:={rf[i],rf[j]};
			\[Phi]ind[i_,j_]:={ff[i],ff[j]};
			\[Theta]ind[i_,j_]:={qf[i],qf[j]};
			];
		d2cotdmat=T.cot\[Theta]d\[Theta]2Matrix[\[Theta]N].(Transpose[T]);
		d\[Theta]2mat=d\[Theta]2Matrix[\[Theta]N,\[Theta]X];
		djmat=d\[CurlyPhi]Matrix[\[Phi]N];
		dj2mat=d\[CurlyPhi]2Matrix[\[Phi]N];
		(*Global`amTable={};*)
		Kel[i_,j_]:=Block[
			{r,\[Phi],\[Theta],
			ri,rj,\[Phi]i,\[Phi]j,\[Theta]i,\[Theta]j,
			\[Delta]r,\[Delta]\[Phi],\[Delta]\[Theta],
			cos2\[CurlyPhi],d2\[Theta],d2\[Theta]cot\[Theta]d\[Theta],cot\[Theta],d2\[CurlyPhi],
			sin2\[CurlyPhi],csc\[Theta]2,d\[CurlyPhi],
			Aeff,Beff,Ceff,
			Jam2,Jm2,Jbm2Jcm2,
			\[Rho]r2},

		{ri,rj}=rind[i,j];r=rX[[ri]];\[Delta]r=KroneckerDelta[ri,rj];
		{\[Phi]i,\[Phi]j}=\[Phi]ind[i,j];\[Phi]=\[Phi]X[[\[Phi]i]];\[Delta]\[Phi]=KroneckerDelta[\[Phi]i,\[Phi]j];
		{\[Theta]i,\[Theta]j}=\[Theta]ind[i,j];\[Theta]=\[Theta]X[[\[Theta]i]];\[Delta]\[Theta]=KroneckerDelta[\[Theta]i,\[Theta]j];
		(*If[\[Delta]r==0,*)
		(*0(*The operator is block-diagonal at r*),*)
		cos2\[CurlyPhi]=\[Delta]\[Phi]*Cos[2\[Phi]]; d2\[Theta]=d\[Theta]2mat[[\[Theta]i,\[Theta]j]]; d2\[Theta]cot\[Theta]d\[Theta]=d2cotdmat[[\[Theta]i,\[Theta]j]];
		cot\[Theta]=\[Delta]\[Theta]*Cot[\[Theta]]     ; d2\[CurlyPhi]=dj2mat[[\[Phi]i,\[Phi]j]];
		sin2\[CurlyPhi]=\[Delta]\[Phi]*Sin[2\[Phi]]; csc\[Theta]2=\[Delta]\[Theta]*Csc[\[Theta]]^2   ; d\[CurlyPhi]=djmat[[\[Phi]i,\[Phi]j]];
		Aeff=Am+\[Delta]r*efU/(2*\[Mu]*r^2);Beff=Bm+\[Delta]r*efU/(2*\[Mu]*r^2);Ceff=Cm+\[Delta]r*efU/(2*\[Mu]*r^2);
		(*Global`amTable~AppendTo~{Aeff,Beff,Ceff};*)
		Jam2=\[Phi]K[[\[Phi]i,\[Phi]j]];Jm2=\[Theta]K[[\[Theta]i,\[Theta]j]];\[Rho]r2=rK[[ri,rj]]/(2*\[Mu]);
		Jbm2Jcm2=jbU*( 
			cos2\[CurlyPhi](2d2\[Theta]-d2\[Theta]cot\[Theta]d\[Theta]-(cot\[Theta]^2)d2\[CurlyPhi])
			-sin2\[CurlyPhi](2(d2\[Theta]cot\[Theta]d\[Theta]-d2\[Theta])+(1-2csc\[Theta]2)d\[CurlyPhi]) 
		);
		(*Kinetic Operator*)
		
			( 
			(\[Delta]r*\[Delta]\[Theta])(Aeff-1/2(Beff+Ceff))Jam2
			+(\[Delta]\[Phi]*\[Delta]r)1/2(Beff+Ceff)Jm2
			+(\[Delta]r*\[Delta]\[Phi]*\[Delta]\[Theta])1/2(Beff-Ceff)Jbm2Jcm2
			+(\[Delta]\[Phi]*\[Delta]\[Theta])\[Rho]r2
			)
		(*]*)
		];
	K=With[{N=rN*\[Phi]N*\[Theta]N},ParallelTable[1.Kel[i,j],{i,1,N},{j,1,N}](*,DistributedContexts\[Rule]cont]*)];
	\[HBar]*K
]


End[]


(* ::Subsection:: *)
(*DVR Set Up*)


End[]


`PlanePointDVR`$DVRDimension=3;
`PlanePointDVR`$FormatGrid=`SphericalDVR`$FormatGrid;
`PlanePointDVR`$PointLabels=`SphericalDVR`$PointLabels;
`PlanePointDVR`$GridPointsFunction=`SphericalDVR`SphericalDVRPoints;
`PlanePointDVR`$KineticMatrixFunction=`PlanePointDVR`PlanePointDVRK;
`PlanePointDVR`$PotentialMatrixFunction=`SphericalDVR`SphericalDVRV;
`PlanePointDVR`$WavefunctionsFunction=`SphericalDVR`SphericalDVRWavefunctions;
`PlanePointDVR`$PlotFunction=`SphericalDVR`SphericalDVRPlot;


EndDVRExtension[];
