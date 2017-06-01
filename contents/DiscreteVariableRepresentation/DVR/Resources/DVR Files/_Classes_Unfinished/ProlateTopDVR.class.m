(* ::Package:: *)

ProlateTopDVR::usage="A subclass of DVRBaseClass for prolate top DVRs"


Begin["`Private`"]


(* ::Subsection:: *)
(*Class ProlateTopDVR*)


ProlateTopDVR:=BeginClass[ProlateTopDVR];


(* ::Subsubsection::Closed:: *)
(*Extends DVRBaseClass: *)


ParentClasses={DVRBaseClass};


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


InitializationFunction[self_,points_,
		Range->{{0,1},{0,\[Pi]},{0,2\[Pi]}},
		file:_String:"ProlateTopDVR",
		OptionsPattern[]]:=
		Module[{kw=$KeyWordArguments},
			DVRBaseClass::$Init[self,points,kw[Range],"ProlateTopDVR",None,Sequence@Normal[kw]];
		];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


End[]
