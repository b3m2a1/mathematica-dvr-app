(* ::Package:: *)

(* ::Section:: *)
(*Exposed Code*)


(* ::Subsection:: *)
(*Exposed Classes*)


ClearAll/@{"MatrixTableClass"};


MatrixTableClass::usage="Represents a matrix as an SQL table"


(* ::Section:: *)
(*Internal Code*)


Begin["`Private`"];


(* ::Subsection:: *)
(*MatrixDatabaseClass*)


(* ::Subsection:: *)
(*MatrixTable Class*)


MatrixTableClass:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Extends DataTableClass*)


ParentClasses={DataTableClass};


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


InitializationFunction[self_,database:_?((IsInstance[#,DatabaseClass])&),
	name_String,
	extraColumns:({(_String->_)..}|Automatic):Automatic,
	ops___]:=(
		Superclass[]::$Init[database,name,Replace[extraColumns,Automatic->{}]~Join~{"Row"->"Integer","Column"->"Integer","Value"->"Real"},ops]
	);


(* ::Subsubsection::Closed:: *)
(*Bound method: Save*)


BoundMethod[Save,self_,matrix:{{__?NumberQ}..},extras:((_String|_Column)->_List)...]:=
With[{rows=Length@matrix,cols=Length@matrix[[1]],
		extra=Replace[{extras},{{}:>{{},{}},else_:>(self::FormatInsertData[else])}]},
	With[{addRow=Replace[extra[[2]],
			l:{__}:>PadRight[l,rows*cols,List@ConstantArray[Null,Length@l[[1]]] ]
			]},
	With[{m=If[addRow=={},
			Table[{i,j,matrix[[i,j]]},{i,rows},{j,cols}],
			Table[{i,j,matrix[[i,j]]}~Join~addRow[[(i-1)*cols+j]],{i,rows},{j,cols}]]},
		AbsoluteTiming[
			self::RowInsert[{"Row","Column","Value"}~Join~extra[[1]],Flatten[m,1]]
			][[1]]
		]
	]
];


(* ::Subsubsection:: *)
(*Bound method : Load*)


BoundMethod[Load,self_]:=With[{basedata=self::Select[{"Row","Value"}]},
If[basedata=!={},
	With[{data=Transpose@basedata},
		With[{cols=Length@Cases[data[[1]],1]},
			Partition[data[[2]],cols]
		]
	],
	None
]];


(* ::Subsubsection:: *)
(*Bound method: View*)


BoundMethod[View,self_,viewFunction_:Automatic]:=
Superclass[]::View[Replace[viewFunction,{
			Automatic:>(If[Head@#===List,ArrayPlot[#],"NO DATA"]&),
			Except[_String|Automatic]:>(With[{data=self::Load[]},viewFunction@data]&)
			}]];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Section:: *)
(*End Package*)


End[]
