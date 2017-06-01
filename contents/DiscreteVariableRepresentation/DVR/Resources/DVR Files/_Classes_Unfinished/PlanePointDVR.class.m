(* ::Package:: *)

PlanePointDVR::usage="A subclass of DVRBaseClass for plane-point DVRs with a given complex"


Begin["`Private`"]


(* ::Subsection:: *)
(*Class PlanePointDVR*)


PlanePointDVR:=BeginClass[PlanePointDVR];


(* ::Subsubsection::Closed:: *)
(*Extends DVRBaseClass*)


ParentClasses={DVRBaseClass};


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


	InitializationFunction[self_,complex_,points_,
		Range->{{0,1},{0,\[Pi]},{0,2\[Pi]}},
		SphericalDVR`RotationalConstantA->None,
		SphericalDVR`RotationalConstantB->None,
		SphericalDVR`RotationalConstantC->None,
		SphericalDVR`ReducedMass->None,
		OptionsPattern[]]:=
		Module[{kw=$KeyWordArguments},
			With[{vals=Diagonal[complex::ABCVectors[[1]] ],rm=complex::ReducedMass},
				If[kw[SphericalDVR`RotationalConstantA]===None,kw[SphericalDVR`RotationalConstantA]=DVRUnits@Quantity[vals[[1]],"AtomicMassUnit" ("Angstroms")^2],"Kilograms" ("Meters")^2 ];
				If[kw[SphericalDVR`RotationalConstantB]===None,kw[SphericalDVR`RotationalConstantB]=DVRUnits@Quantity[vals[[2]],"AtomicMassUnit" ("Angstroms")^2],"Kilograms" ("Meters")^2 ];
				If[kw[SphericalDVR`RotationalConstantC]===None,kw[SphericalDVR`RotationalConstantC]=DVRUnits@Quantity[vals[[3]],"AtomicMassUnit" ("Angstroms")^2],"Kilograms" ("Meters")^2 ];
				If[kw[SphericalDVR`ReducedMass]===None,kw[SphericalDVR`ReducedMass]=DVRUnits@Quantity[rm,"AtomicMassUnit"]];
			];
			DVRBaseClass::$Init[self,points,kw[Range],"PlanePointDVR",complex,Sequence@Normal[kw]];
		];


(* ::Subsubsection:: *)
(*End Class*)


EndClass[]


End[]
