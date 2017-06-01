(* ::Package:: *)

BeginDVRExtension[]


Cartesian1DDVR::usage="A subclass of DVRBaseClass for 1-D Cartesian DVRs"


Begin["`Private`"]


(* ::Subsection:: *)
(*Class Cartesian1DDVR*)


Cartesian1DDVR:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Extends DVRBaseClass*)


ParentClasses={DVRBaseClass}


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


InitializationFunction[self_,points_,Range->{{-10,10}},OptionsPattern[]]:=
	Module[{kw=$KeyWordArguments},
		With[{p=If[NumberQ@points,{points},points],
				r=Replace[kw[Range],{a_?NumberQ,b_?NumberQ}:>{{a,b}}]},
		DVRBaseClass::$Init[self,p,r,"Cartesian1DDVR",None,
			Sequence@Normal[kw]];
		]
		];


(* ::Subsubsection:: *)
(*End Class*)


EndClass[]


End[]


EndDVRExtension[]
