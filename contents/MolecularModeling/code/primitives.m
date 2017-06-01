(* ::Package:: *)

(* ::Chapter:: *)
(*Chemistry Basics Package*)


(* ::Section:: *)
(*Exposed Code*)


(*Begin@"MolecularModeling`";*)


(* ::Subsection::Closed:: *)
(*Classes Exposed*)


ChemDataHolder::usage="Wrapper up data lookup functions, etc."
ChemData::usage="Instance of ChemDataHolder"
Atom::usage="Atom class"
Molecule::usage="Molecule class"
ChemicalComplex::usage="A complex of an atom/molecule with atom/molecule"
ChemWrapperClass::usage="Wrapper for things like making molecules, rotating molecules, etc."
ChemWrapper::usage="Instance of ChemWrapperClass"


(* ::Subsection::Closed:: *)
(*Display Options Exposed*)


WithUnits::usage="(boolean) Option name for ChemDataHolder.Property to specify whether to return units or not"
Origin::usage="(vector) Option name for <class>.DrawOb which sets the center of the graphic. Defaults to center of mass or {0,0,0}."
ConstantBondWidth::usage="(boolean) Options for Molecule.BondOb specifying whether to use a constant bond width or variable"
BondWidth::usage="(0<x<1) Option name for Molecule.BondOb specifying the multiplier to use for bond widths (assuming not using ConstantBondWidth)"
DoubleBondSeparation::usage="(0<x<1) Option name for Molecule.BondOb: specifies how much to separate double bond segments"
TripleBondSeparation::usage="(0<x<1) Option name for Molecule.BondOb: specifies how much to separate triple bond segments"
ShowCenterOfMass::usage="(boolean) Option name for Molecule.DrawOb: specifies whether to display the center of mass or not"
InertialAxes::usage="(boolean) Option name for Molecule.DrawOb: specifies whether to display inertial axes or not"
MobileAtoms::usage="(atom list) Option name for Molecule.DrawOb: specifies which atoms to have be dynamically mobile"
ReturnAssociation::usage="(boolean) Option name for Molecule.DrawOb: specifies whether to return a map of objects or just the list of objects"
WithMagnitude::usage="(boolean) Option name for Molecule.ABCAxes: specifies whether to use the relative magnitudes of the intertial A, B, and C axes"


(* ::Section:: *)
(*Internal Code*)


Begin["`Private`"]


BeginClass=ObjectOrientedProgramming`BeginClass;
EndClass=ObjectOrientedProgramming`EndClass;


(* ::Subsection::Closed:: *)
(*Chemistry Data Class*)


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Class: ChemDataHolder*)
(*A class for tracking various useful chemical data*)


ChemDataHolder=BeginClass[];


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Class field: ColorMap*)
(*Coloring Dictionary*)


	ClassField[ColorMap]=<|"H"->White,"X"->Purple,"Invisible"->Opacity[0],
							"C"->Gray,"O"->Red,"N"->Blue,
							"F"->RGBColor[.4,1.,1.],"Cl"->Green,"Br"->RGBColor[.3,.8,1],
							"He"->GrayLevel[.8],"Ne"->Hue[0,0.81,1],"Ar"->\!\(\*
TagBox[
StyleBox[
RowBox[{"RGBColor", "[", 
RowBox[{"0.6", ",", "0.15", ",", "1."}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\),
							"Li"->\!\(\*
TagBox[
StyleBox[
RowBox[{"RGBColor", "[", 
RowBox[{"0.095`", ",", "0.4`", ",", "0.44`"}], "]"}],
ShowSpecialCharacters->False,
ShowStringCharacters->True,
NumberMarks->True],
FullForm]\)
							|>;


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Class field: NoAttribute*)
(*Key for missing attributes*)


	ClassField[NoAttribute]=MissingProperty;


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Class field: BondDistanceMap*)
(*Dictionary of default bond distances*)


	ClassField[BondDistanceMap]=<|
			{"H","H",1}->1.,{"H","O",1}->.98,{"H","N",1}->1.02,{"H","C",1}->1.11,
			{"H","F",1}->.92,{"H","Cl",1}->1.37,
			{"C","C",1}->1.56,{"C","C",2}->1.34,{"C","C",3}->1.20,
			{"C","N",1}->1.47,{"C","N",2}->1.25,{"C","N",3}->1.16,
			{"C","O",1}->1.43,{"C","O",2}->1.21,
			{"C","F",1}->1.37,{"C","Cl",1}->1.76,{"C","Br",1}->1.94,{"C","I"}->2.14,
			{"C","S",1}->1.82,{"C","P",1}->1.84|>;


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Bound method: BondDistance*)
(*If the atoms and bond types provided as arguments are in BondDistanceMap, then it returns the default distance provided*)


	BoundMethod[BondDistance,self_,atom1_,atom2_,mode_:1]:=
		With[{M=self::BondDistanceMap,a1=ObjectString[atom1],a2=ObjectString[atom2],
			type=Switch[mode,"single",1,"double",2,"triple",3,_String,ToExpression[mode],_,mode]},
			Replace[M[{a1,a2,type}],
				_Missing:>Replace[M[{a2,a1,type}],
						_Missing:>With[{bd=UnitConvert[self::Property[a1,"Radius",WithUnits->True]+self::Property[a2,"Radius",WithUnits->True],"Angstroms"]//QuantityMagnitude},
								self::BondDistanceMap[{a1,a2,type}]=bd;
								bd
								]
							]			
						]
			];


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Class field: PropertyMap*)
(*Dictionary of property keys for looking up things like atomic masses and radii*)


	ClassField[PropertyMap]=<|
		{"Atom","Mass"}->"AtomicMass",
		{"Atom","Radius"}->"VanDerWaalsRadius",
		{"Atom","Valence"}->"Valence",
		{"Moleucle","Mass"}->{"MolecularMass"}|>;


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Bound method: Color*)
(*If an object is in ColorMap, it returns that color. Otherwise it looks up colors in ColorData[obj].*)


	BoundMethod[Color,self_,obj_]:=
		With[{s=ObjectString@obj},
			With[{r=self::ColorMap[s]},
				If[TrueQ[Head[r]==Missing],
					With[{m=ColorData["Atoms"][StringTake[s,1]]},
						If[ColorQ[m]//Not,
							With[{c=RandomColor[]},
								self::ColorMap[s]=c;
							c],
						self::ColorMap[s]=m;
						m]
						],
					r]
			]
		];


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Bound method: DataProperty*)
(*Runs the arguments through PropertyMap and then uses the object string of the object as a look up to the appropriate Mathematica database*)


	BoundMethod[DataProperty,self_,obj_,property_,mode_:"Atom"]:=
		With[{lookup=Switch[mode,
				"Atom",ElementData,
				"Isotope",IsotopeData,
				_,ChemicalData],
				str=ObjectString[obj],
				prop=Replace[self::PropertyMap[{mode,property}],_Missing->property]
			},
			If[MemberQ[lookup[str,"Properties"],prop],
				lookup[str,prop],
				self::NoAttribute[prop]
				]
		];


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Bound method: IsAtom*)
(*Tests if an object can be interpreted as an atom*)


	StaticMethod[IsAtom,obj_]:=
		Module[{o=IsInstance[obj,AtomClass],s,l,tests},
			If[Not[o],
				s=ObjectString[obj];
				l=StringLength[s];
				With[{c1=l>0,c2=l<3,c3=LetterQ[s]},
						o=(c1&&c2&&c3)
					]
			];
			o
		];


(* ::Subsubsection::RGBColor[0.5, 0, 0.5]::Closed:: *)
(*Bound method: Property*)
(*A general property getting method. This is a good default method to use, unless a better one is known.*)


	BoundMethod[Property,self_,obj_,property:_String|_Symbol,WithUnits->False]:=
		Module[{ob=obj,r,ret,mode,prop=ToString@Unevaluated[property]},
			If[self::IsAtom[obj],
				mode="Atom",
				mode="Molecule"
			];
			If[HasAttribute[self,prop],
				r=GetAttribute[self,ToString@prop][ob];,
				r=With[{P=self::DataProperty},
					P[obj,prop,mode]
					];
				];
			ret=If[OptionValue[WithUnits],
				r,
				QuantityMagnitude[r]
			];
			If[TrueQ[Head[ret]==QuantityMagnitude],
				r,
				ret]
			]


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];
ChemData=ChemDataHolder[];


(* ::Subsection:: *)
(*Atom Class*)


(* ::Subsubsection::Closed:: *)
(*Class Atom:*)


Atom=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


(* ::Text:: *)
(*Initializes the position, element type, bonds, name, and mass of the atom*)


InitializationFunction[self_,element_,position_:{0,0,0},Label->""]:=
	Module[{E=element,P=position},
		self::Position=Replace[P,{n_?NumberQ:>{n,n,n},{x_?NumberQ}:>{x,0,0},
								{x_?NumberQ,y_?NumberQ}:>{x,y,0},
								{x_?NumberQ,y_?NumberQ,z_?NumberQ}:>{x,y,z},
								_:>{RandomReal[{0,10}],RandomReal[{0,10}],RandomReal[{0,10}]}
								}];
			self::Element=ObjectString@E;
			self::Bonds=<||>;
			self::Name=OptionValue@Label;
			With[{s="atom$"<>ToString@$ModuleNumber},
				Evaluate@ToExpression[s,StandardForm,HoldPattern]=None;
				self::UpdateSymbol=ToExpression[s,StandardForm,HoldPattern];
				self::UpdateArgs=None;
				];
			Switch[E,"X"|"Invisible",self::Mass=0;self::Radius=1;self::Valence=\[Infinity],
				_,With[{M=(ChemData::Property[E,"Mass"]),
						R=ChemData::Property[E,"Radius",WithUnits->True],
						V=ChemData::Property[E,"Valence"]},
					self::Radius=UnitConvert[R,"Angstroms"]//QuantityMagnitude;
					self::Mass=M;
					self::Valence=V;
					]
				];
			];


(* ::Subsubsection::Closed:: *)
(*Bound property: InitList*)


BoundProperty[InitList,self_]:=With[{l={self::Element,self::Position,Label->self::Name}},l];


(* ::Subsubsection:: *)
(*Bound method: DrawOb*)


(* ::Text:: *)
(*Gives the display object for the atom.*)
(**)
(*Options: Origin, the origin for the display graphic*)
(*DynamicUpdates: Whether to dynamically update the graphics object's position or not*)
(*Label: What label to give the object*)
(**)
(*Should Implement: Custom atom display shapes and other options*)


	BoundMethod[DrawOb,self_,Origin->{0,0,0},Scale->1,Update->True,
			Label->None,LabelingFunction->Automatic,Style->Automatic]:=
		Module[{
			df=OptionValue[Update],styleFunction=Replace[OptionValue@Style,Automatic->(Sphere[#1,#2]&)],
			color=self::Color,position,
			radius=(self::Radius)*OptionValue@Scale,
			ls=Replace[OptionValue@LabelingFunction,{
						Automatic:>({If[ColorDistance[Black,#6,DistanceFunction->"DeltaL"]>.6,Black,White],Inset[#1,#2+(#4+.1)*#5,#3]}&),
						Text:>({If[ColorDistance[Black,#6,DistanceFunction->"DeltaL"]>.6,Black,White],Text[#1,#2]}&),
						c_?ColorQ:>{c,Inset[#1,#2+(#4+.1)*#5,#3]&},
						{ops___,Text}:>({ops,Text[#1,#2]}&),
						l_List:>(Append[l,Inset[#1,#2+(#4+.1)*#5,#3]]&)
						}],
			label=Replace[OptionValue@Label,{None:>"",k:Style[_]:>k,Automatic:>self::Name,k_:>ToString@k}]},
				With[{comAdjust=OptionValue@Origin},position=self::Position-comAdjust;];
				With[{ob=
					{color,styleFunction[position,radius,self],
							If[label=!="",
									With[{d={0,0,1}},
										(*Move to using a small rectangle instead*)
										ls[label,position,{1,1},radius,Normalize@d,color]
										]
								]
						,Null}},
					If[df,With[{p=self::UpdateSymbol},
						self::UpdateArgs=$KeyWordArguments;
						p=ob;
						ReplacePart[p,0->Dynamic]],
						ob
						]
					]
				];


(* ::Subsubsection::Closed:: *)
(*Bound method: Draw*)


(* ::Text:: *)
(*Draws the atom, using atom::DrawOb to generate the object to draw*)


	BoundMethod[Draw,self_,
		Boxed->False,Style->Automatic,Lighting->"Neutral",Label->None,LabelingFunction->Automatic,OptionsPattern[]]:=
		With[{kw=$KeyWordArguments,s=self::DrawOb},
			With[{o=s[FilterRules[Normal@kw,Options@s]]},
			Graphics3D[o,FilterRules[Normal@kw,Options[Graphics3D]]]
			]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: Update*)


(* ::Text:: *)
(*Updates the symbol returned from DrawOb with DynamicUpdates->True*)


BoundMethod[Update,self_]:=With[{u=self::UpdateArgs},If[u=!=None,self::DrawOb[Normal@u]];];


(* ::Subsubsection::Closed:: *)
(*Bound method: Distance*)


BoundMethod[Distance,self_,other_]:=(self::Position-other::Position//Norm);


(* ::Subsubsection::Closed:: *)
(*Bound method: CouldBond*)


BoundMethod[CouldBond,self_,other_,tolerance_:0,bType_:1]:=(
	!TrueQ[bType>self::Valence]
	&&
	!TrueQ[bType>other::Valence]
	&&
	self::Distance[other]<tolerance+ChemData::BondDistance[self::Element,other::Element]
	);


(* ::Subsubsection::Closed:: *)
(*Bound method: Bond*)


(* ::Text:: *)
(*Tries to bond to an object (usually an atom)*)
(*If this fails, it returns false, else, it returns true*)


BoundMethod[Bond,self_,other_,type:_?(MatchQ[#,_Integer|_Real|_String]&):1,overWrite_:False]:=
	Module[{c1,c2,make,testType=Replace[type,{_?NumberQ->type,_->0}]},
		make[a1_,a2_]:=(a1::Bonds[a2]=type;a1::Valence+=-testType;True);
		c1=self::Valence>=testType;
		c2=other::Valence>=testType;
		With[{b=self::Bonds[other]},
		If[MatchQ[b,_Missing],
			If[(overWrite//TrueQ)||c1&&c2,make[self,other];make[other,self],False],
			If[type<=b//TrueQ,
				Replace[overWrite||c1&&c2,True:>(
				self::Bonds[other]+=type;self::Valence-=type;
				other::Valence-=type;other::Bonds[self]+=type;
				True)],
				With[{v1=self::Valence+b,v2=other::Valence+b},
					c1=v1>=testType;c2=v2>=testType;
					If[(overWrite//TrueQ)||c1&&c2,
						self::Valence=v1-testType;other::Valence=v2-testType;
						self::Bonds[other]=type;other::Bonds[self]=type;
						(self::Update[];other::Update[]);True,
						False
						]	
					]
				]
		]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: BondType*)


(* ::Text:: *)
(*Returns the type of bond the atom has with an object*)


BoundMethod[BondType,self_,other_]:=Replace[self::Bonds[other],_Missing:>None];


(* ::Subsubsection::Closed:: *)
(*Bound property: BondVector*)


(* ::Text:: *)
(*Returns the average of all bond vectors.*)
(*A function can be specified to return the vector. Should be a two-argument function that returns a number.*)


BoundProperty[BondVector,self_,Function->None]:=
	With[{bonds=self::Bonds,f=Replace[OptionValue@Function,_?(Not@IsCallable[#]&):>(Normalize[#1::Position]&)]},
		With[{L=Length@(bonds//Keys)},
			If[L===0,
				{0,0,0},
				Total@MapThread[f,{bonds//Keys,bonds//Values}]/L
				]
			]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: Move*)


(* ::Text:: *)
(*Moves the atom*)


	BoundMethod[Move,self_,vectorList:_?(MatchQ[#,{_,_,_}]&):None,"X"->0,"Y"->0,"Z"->0,Set->False]:=
		With[{vector=Replace[vectorList,None->OptionValue/@{"X","Y","Z"}]},
			If[OptionValue@Set//TrueQ,
				self::Position=vector,
				self::Position+=vector];
				self::Update[];
				self::Position
			];


(* ::Subsubsection::Closed:: *)
(*Bound method: Rotate*)


(* ::Text:: *)
(*Rotates an atom about a vector and a point.*)


	BoundMethod[Rotate,self_,\[Theta]_,Axis->{0,0,1},Center->{0,0,0}]:=
		With[{c=OptionValue@Center,a=OptionValue@Axis},
			With[{R=RotationMatrix[\[Theta],a],P=self::Position-c},
				With[{pos=(1.(R.P))+c},
					self::Position=pos;
					self::Update[];
					pos
					]
				]
			];


(* ::Subsubsection::Closed:: *)
(*Bound property: CenterOfMass*)


(* ::Text:: *)
(*Just here for convenience. Routes to position.*)


BoundProperty[CenterOfMass,self_]:=self::Position


(* ::Subsubsection::Closed:: *)
(*Bound method: GetAtoms*)


(* ::Text:: *)
(*Just here for convenience. Routes to self.*)


BoundMethod[GetAtoms,self_,args___]:=self


(* ::Subsubsection::Closed:: *)
(*Bound method: $ToString*)


(* ::Text:: *)
(*Turns an atom into a string*)


StringFunction[self_]:=
	With[{s=self::Element},s];


(* ::Subsubsection::Closed:: *)
(*Bound method: $ToRepresentation*)


RepresentationFunction[self_]:=
With[{c=self::Color},
	Graphics[{Opacity[0],Disk[],Opacity[1],EdgeForm[Thick],c,Disk[{0,0},self::Radius/2],
			Inset@Style[self::Element,If[ColorDistance[Black,c,DistanceFunction->"DeltaL"]>.6,Black,White]]
			},ImageSize->{50,50}]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: $GetAttr*)


(* ::Text:: *)
(*The function to apply if an attribute cannot be found*)


	NoAttributeFunction[self_,attr_]:=
		With[{v=ChemData::Property},
			(*Print[Row@{self,Spacer[10],attr}];*)
			With[{value=v[self,attr]},
				If[TrueQ[Head[value]==ChemData::NoAttribute],
					Message[MathematicaClass::noattr,self,attr],
					If[ObjectType@self=!=MathematicaClass,
						SetAttribute[self,ToString@attr,value],value](*;value*)
					]
				]
			];


(* ::Subsubsection::Closed:: *)
(*End Class Atom*)


EndClass[];


(* ::Subsection::Closed:: *)
(*Molecule Class*)


(* ::Subsubsection::Closed:: *)
(*Class Molecule*)


Molecule:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


(* ::Text:: *)
(*Initializes the position, element type, bonds, name, and mass of the atom*)


InitializationFunction[self_,atomList:{{_String,_List,___}..}|{__?(IsInstance[#,Atom]&)}:{},bondList:{{_?NumberQ..}...}:{}]:=
	Module[{atoms=atomList,atomF=Atom::$New,c,b,bonds=bondList,i},
		For[i=1,i<=Length[atoms],i++,
			c=atoms[[i]];
			If[MatchQ[c,_List],atoms[[i]]=atomF@@c];
			];
		self::Atoms=atoms;
		For[i=1,i<=Length[bonds],i++,
			b=bonds[[i]];
			If[Length[b]==3,
				With[{at=atoms[[b[[1]]]],ot=atoms[[b[[2]]]],t=b[[3]]},
						at::Bond[ot,t](*;AppendTo[Global`testShit,{at,ot}];*)],
					
				With[{at=atoms[[b[[1]]]],ot=atoms[[b[[2]]]]},
						at::Bond[ot]]
				]
			];
		];


(* ::Subsubsection::Closed:: *)
(*Class method: Import*)


ClassMethod[Import,cls_,file_,
				fileType:"ZMatrix"|"ChemicalTable"|Automatic:Automatic,
				atomList:{_String..}|{{_String..}}|_String:"X",
				bondList:{{__Integer,_?NumberQ}...}|{{{__Integer,_?NumberQ}...}}|Automatic:Automatic]:=
	Switch[Replace[fileType,{"ZMatrix"->"gjf","ChemicalTable"->"mol",_->FileExtension@file}],
		"m"|"nb",cls@@Get@file,
		"sdf"|"sd"|"mol",
		With[{data=Import[file,"Table"]},
			With[{blocks=
				SequenceCases[data,{{__,"V2000"},block:Except[{"M","END"}|{"M_END"}]..,{"M","END"}}:>{block},Overlaps->False]
				},
				Replace[Table[cls::FromList[b,"ChemicalTable"],{b,blocks}],{{m_}:>m,{}->$Failed}]
				]
			],
		"stf",
		With[{dataBlocks=SequenceCases[Import[file,"Table"],
			{{_Integer},b:{_Integer,_Integer,_Integer,_Integer,_Real,_Real,_Real,_Real}..,{_String,__}}:>{b}]},
			With[{blocks=MapThread[With[{data=Sort[#,First],atomTypes=#2,bondBlock=#3},
				(Transpose@{atomTypes[[;;Length@data]],data[[All,2]],data[[All,5]],data[[All,3]],data[[All,6]]\[Degree],data[[All,4]],data[[All,7]]\[Degree]}/.{a__,__?(#==0&)}:>{a})
				~Join~bondBlock
				]&,{dataBlocks,
						Replace[atomList,{
									s:{__String}:>Table[s,{l,dataBlocks}],
									s_String:>Table[ConstantArray[s,Length@l],{l,dataBlocks}]
									}],
						Replace[Replace[bondList,Automatic->{}],{
									b:{{__Integer}...}:>Table[b,{l,dataBlocks}]
									}]}]},
				Replace[Table[With[{m=cls::FromList[b,"ZMatrix"]},If[bondList===Automatic,m::ResolveBonds[]];m],{b,blocks}],{{m_}:>m,{}->$Failed}]
				]
			],
		"gjf",
		With[{data=Import[file,"Table"]},
			With[{blocks=SequenceCases[data,{a:{_String,_Integer|_Real...}..,{}...,b:({_Integer,_Integer,_Integer|_Real}|{_Integer,_Integer})...}:>{a,b}]},
				Replace[Table[cls::FromList[b,"ZMatrix"],{b,blocks}],{{m_}:>m,{}->$Failed}]
				]
			]
		]


(* ::Subsubsection::Closed:: *)
(*Class method: ListToAtoms*)


StaticMethod[ListToAtoms,data_,mode_:Automatic]:=
	Switch[Replace[mode,Automatic:>If[MatchQ[data[[1]],{_String,Except[_List]...}],"ZMatrix","ChemicalTable"]],
		"ChemicalTable",
			Replace[
				Reap[
					Replace[data,{
						{crd1_Real,crd2_Real,crd3_Real,elm_String,massDifference_,charge_,hCount_,stereoFlag_,hCount2_,__,aMap_,invRet_,exactChg_}:>Sow@{elm,{crd1,crd2,crd3}},
						{atom1_Integer,atom2_Integer,bondType_Integer,otherShit___}:>Sow@{atom1,atom2,bondType}
						},1]
					][[2]],{
						Except[{{{_String,___}..,{_Integer..}...}}]:>$Failed
						}
					],
		"ZMatrix",
			With[{setPos=(With[{args={##}},With[{getArg=(Replace[Quiet[args[[#]]],Except[#2]->#3])&},
				With[{previous=getArg[1,_List,None],dref=getArg[2,_Integer,None],dist=getArg[3,_?NumericQ,None],
						aref=getArg[4,_Integer,None],ang=getArg[5,_?NumericQ,None]Degree,
						href=getArg[6,_Integer,None],dihed=getArg[7,_?NumericQ,None]Degree},
					With[{pos=(previous[[#,2]]&)},
						Switch[{dref,dist,aref,ang,href,dihed},
							{(None|_Times)..},{0,0,0},
							{_Integer,_?NumericQ,(None|_Times)..},
								With[{vec={1,0,0}},pos@dref+dist*Normalize@(vec-pos@dref)],
							{_Integer,_?NumericQ,_Integer,_?NumericQ,(None|_Times)..},
								With[{vec=With[{cent=pos@dref,ref=pos@aref},
													cent+RotationMatrix[ang,{0,0,1}].(ref-cent)
														]},
										pos@dref+dist*Normalize@(vec-pos@dref)],
							{_Integer,_?NumericQ,_Integer,_?NumericQ,_Integer,_?NumericQ},
								With[{vec=With[{cent=pos@dref,avec=pos@aref,hvec=pos@href},
														With[{norm=(((avec-cent))\[Cross]((hvec-cent)))},
															cent+RotationMatrix[dihed,((avec-cent))].RotationMatrix[ang,norm].((avec-cent))
															]
														]},
												pos@dref+dist*Normalize@(vec-pos@dref)
												],
							_,{-1,-1,-1}
							]
						]
					]
				]
				]
			)&,
			atomBlock=Cases[data,{_String,___}]},
			Block[{previousAtoms={}},
				Do[(*Print@previousAtoms;*)AppendTo[previousAtoms,{L[[1]],setPos[previousAtoms,Sequence@@L[[2;;]] ]}],{L,atomBlock}];
				{previousAtoms,Cases[data,{__Integer,_?NumberQ}]}
				]
			],
		_,$Failed
		]


(* ::Subsubsection::Closed:: *)
(*Class method: FromString*)


ClassMethod[FromString,cls_,string_]:=(Message[MathematicaClass::noimp,cls,"FromString"];$Failed)


(* ::Subsubsection::Closed:: *)
(*Class method: FromList*)


ClassMethod[FromList,cls_,data_,mode_:Automatic]:=Replace[cls::ListToAtoms[data,mode],{atoms:{{s_String,_List,___}..},bonds:{{__Integer}...}}:>cls[atoms,bonds]]


(* ::Subsubsection::Closed:: *)
(*Class method: BuildInterface*)


ClassMethod[BuildInterface,cls_]:=With[{makeSym=With[{ext="$"<>ToString@$ModuleNumber},(Symbol@(#<>ext)&)]},
	With[{draw=makeSym@"draw",mode=makeSym@"mode",astring=makeSym@"astring",bstring=makeSym@"bstring",instance=makeSym@"instance"},
		{mode=TraditionalForm,instance=cls[],astring="",bstring=""};
		DialogInput[Grid[{
			{Framed@ExpressionCell[Dynamic[Graphics3D[{},ImageSize->{350,350},Boxed->False,Lighting->"Neutral"]
							~Show~
					Replace[draw,{
						Except[_Real]:>Graphics3D[{},Epilog->{Line[{{1,0},{0,1}}],Line[{{0,0},{1,1}}]},Boxed->False],
						_:>instance::Draw[PlotStyle->mode]}],TrackedSymbols:>{draw}],Selectable->True,Deployed->False],
				{InputField[Dynamic@astring,String,FieldSize->{{25,\[Infinity]},{5,\[Infinity]}},FieldHint->"Input atoms either as atom table or z-matrix.\nUpdate molecule via Command-Click.\n(may require clicking below)"],
							InputField[Dynamic@bstring,String,FieldSize->{{25,\[Infinity]},{5,\[Infinity]}},FieldHint->"Input bonds as atom1 atom2 bond type.\nUpdate molecule via Command-Click.\n(may require clicking above)"]
							}//Column//EventHandler[#,{
										"ReturnKeyDown":>NotebookWrite[EvaluationNotebook[],"\n"],
										{"MouseDown",1}:>If[MemberQ[CurrentValue["ModifierKeys"],"Command"|"Alt"],(
															instance::Atoms={};
															instance::$Init@@cls::ListToAtoms[ImportString[astring<>"\n"<>bstring,"Table"]];
															draw=RandomReal[];)],
										PassEventsDown->True}]&},
			{Button["Finish",DialogReturn[instance],ImageSize->{Full,Automatic}]}
			},Alignment->Top]]
	]
]


(* ::Subsubsection::Closed:: *)
(*Bound property: InitList*)


BoundProperty[InitList,self_]:=With[{l={
	(#::InitList)&/@self::Atoms,
	DeleteCases[Join@@With[{p=self::Atoms},
	Table[
		With[{p1=Position[p,#][[1,1]],p2=Position[p,b[[1]]][[1,1]]},
			If[p1<p2,
				{p1,p2,b[[2]]},
				None
				]],{b,Normal@#::Bonds}]&/@p
		],None]
	}},l];


(* ::Subsubsection::Closed:: *)
(*Bound method: Add*)


(* ::Text:: *)
(*Adds obejcts to class, either by adding the object's atoms or adding the atom itself*)


	BoundMethod[Add,self_,objects__]:=
		Do[
			If[IsInstance[o,Atom],
				AppendTo[self::Atoms,o],
				With[{a=GetAttribute[o,"Atoms"]},
					If[IsInstance[a,List],self::Atoms=Join[self::Atoms,a]]
						]
				],
			{o,Flatten[{objects}/.{s_String,crds:{__?NumericQ},prp___}:>Atom[s,crds,prp]]}];


(* ::Subsubsection::Closed:: *)
(*Bound method: Remove*)


(* ::Text:: *)
(*Removes an object or objects from the atom list*)


	BoundMethod[Remove,self_,objects__]:=
		Module[{A=self::Atoms,R},
			R=Table[If[MemberQ[A,o],o],{o,Flatten@{objects}}];
			self::Atoms=Complement[A,R];
			];


(* ::Subsubsection::Closed:: *)
(*Bound method: GetAtoms*)


(* ::Text:: *)
(*Gets atoms in a molecule, selecting for some property*)


BoundMethod[GetAtoms,self_,key_,Property->ObjectString]:=
	Switch[key,
		All,self::Atoms,
		None,{},
		_,Flatten@Table[With[{Pfunction=Replace[OptionValue@Property,{s_String:>(GetAttribute[#,s]&),s_:>s}]},
				With[{R=Reap[
					Do[If[Pfunction[A]==k,Sow@A],
					{A,self::Atoms}]
					][[2]]},
				If[Length[R]>0,R[[1]],R]
				]
			],{k,If[MatchQ[key,_List],key,{key}]}]
		];



(* ::Subsubsection::Closed:: *)
(*Bound method: ResolveBonds*)


BoundMethod[ResolveBonds,self_,MaxSteps->5,Tolerance->.1]:=
Module[{atoms=self::Atoms,bondStructures=<||>,initial,bondMatrix,testValence,
	tol=OptionValue@Tolerance,step},
	testValence:=Total@(#::Valence&/@atoms);
	initial=#->#::Bonds&/@atoms;
	step:=(
		
		bondStructures[testValence]=#->#::Bonds&/@atoms;
		atoms=Replace[Reap[With[{v=#::Valence},If[v>0,Sow@{v,#}]]&/@atoms][[2]],{a:{__}}:>a];
		atoms=SortBy[atoms,First][[All,2]];
		bondMatrix=Array[
				With[{a=atoms[[#]],b=atoms[[#2]]},
					If[#2>#,{a::Distance[b],a::CouldBond[b,tol],b},a]
					]&,{Length@atoms,Length@atoms}];
		Do[With[{atom=First@l,others=Cases[l,{_,True,_}]},
			Replace[SortBy[others,First],{f:_List,___}:>atom::Bond[Last@f]]
			],{l,bondMatrix}]
		);
	Do[If[testValence>0,step,Return[]],OptionValue@MaxSteps];
	testValence
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: Split*)


(* ::Text:: *)
(*Splits into connected chunks*)


BoundMethod[Split,self_]:=
With[{atoms=self::Atoms},
	Block[{bondingMatrix=Table[With[{a=a,b=b},a===b||a::BondType[b]=!=None],{a,atoms},{b,atoms}]},
		Do[bondingMatrix[[i,j]]=bondingMatrix[[i,j]]||(bondingMatrix[[i,k]]&&bondingMatrix[[k,j]]),
			{k,Length@atoms},{j,Length@atoms},{i,Length@atoms}];
		With[{mols=bondingMatrix//DeleteDuplicates},
			MathematicaClass[ObjectType[self]]/@(Pick[atoms,#]&/@mols)
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Static method: BondOb*)


(* ::Text:: *)
(*Takes two atoms and a degree (implicitly 1) and returns a graphics object*)


	StaticMethod[BondOb,atom1_,atom2_,
		degree_:1,Origin->{0,0,0},Style->Automatic,ConstantBondWidth->False,Update->False,BondWidth->.5,
					TripleBondSeparation->Automatic,DoubleBondSeparation->Automatic,Scale->1]:=
		Module[{rad1=atom1::Radius,rad2=atom2::Radius,scale=OptionValue@Scale,
				at1=atom1,at2=atom2,comP=OptionValue[Origin],
				styleFunction=With[{
					repList={
						(Style->op_):>{Style->op,Automatic},
						({Style->op_,others___,Automatic}|{others___,Automatic}|Automatic):>{others,
							1->{1,({op,Cylinder[{#1,#1+#2},#3]}&)},
							2->{2,({op,With[{p=#1[[1]],v=#1[[2]]},
								Cylinder[{{p-v,p-v+#2},{p+v,p+v+#2}},#3]
								]}&)},
							3->{3,({op,With[{p=#1[[1]],a=#1[[2,1]],b=#1[[2,2]],c=#1[[2,3]]},
								Cylinder[{{p+a,p+a+#2},{p+b,p+b+#2},{p+c,p+c+#2}},#3]
								]}&)},
							_->{1,({op,With[{p1=#1,p2=#1+#2,t=Min@{#3,.3}},
								With[{v=Normalize@(p2-p1),N=(Round@(Norm@(p2-p1)/t))},
									Table[
										With[{s=p1+v*(t*(i-1/3)),e=p1+v*(t*(i+1/3))},
											Cylinder[{s,e},t]
											],
										{i,N}]]]}&)}},
						{Style->op_,others___?(!MatchQ[#,Automatic]&)}:>{Style->op,others,Automatic},
												a_?(!MatchQ[#,_List]&):>{a,_->{1,(With[{p1=#1[[1]],p2=#1[[2]],t=Min@{#2,.3}},
								With[{v=Normalize@(p2-p1),N=(Round@(Norm@(p2-p1)/t))},
									Table[
										With[{s=p1+v*(t*(i-1/3)),e=p1+v*(t*(i+1/3))},
											Cylinder[{s,e},t]
											],
										{i,N}]]]&)}}
									}},
							Block[{r=OptionValue@Style},
								Do[r=Replace[r,repList],Length@repList];
								(*Print@r;*)
								r]],
				uFlag=OptionValue@Update},
			If[rad2<rad1,
				at1=atom2;at2=atom1;
				{rad1,rad2}={rad2,rad1};
				];
			SetAttributes[BondingObjectHoldComplete,HoldAllComplete];
			With[{a1=at1,a2=at2,comAdjust=comP},
				With[{pReps={2:>(X=Cross[vector,{0,0,1}];
								X=Normalize[X];
								If[NumberQ[c],w=If[c>.25,c/2,c],If[w<.35,w=w*r1,w=w*r1/2]];
								dbs=Replace[OptionValue[DoubleBondSeparation],{None->0,o_:>Replace[o,Automatic->.05]+w(**r1*)}];
								X=dbs*X;
								{EdgeForm[None],
									{a1::Color,f[{p1,X},vector*If[d>0,d+r1,0],w,a1]},
									{a2::Color,f[{p2,X},-vector*If[d>0,d+r2,0],w,a2]}
								}),
							3:>(X=Normalize@Cross[vector,{0,0,1}];
								{X,Y,Z}={X,RotationMatrix[120 Degree,vector].X,RotationMatrix[240 Degree,vector].X};
									If[NumberQ[c],w=If[c>.25,c/2,c],If[w<.35,w=w*r1,w=w*r1/2]];
									tbs=Replace[OptionValue[TripleBondSeparation],{None->0,o_:>Replace[o,Automatic->.05]+w(**r1*)}];
									{X,Y,Z}={tbs*X,tbs*Y,tbs*Z};
										{EdgeForm[None],
											{a1::Color,f[{p1,{X,Y,Z}},vector*If[d>0,d+r1,0],w,a1]},
											{a2::Color,f[{p2,{X,Y,Z}},-vector*If[d>0,d+r2,0],w,a2]}
										}),
							_:>(If[NumberQ[c],w=c,w=w*r1];
								{EdgeForm[None],
									{a1::Color,f[p1,vector*If[d>0,d+r1,0],w,a1]},
									{a2::Color,f[p2,-vector*If[d>0,d+r2,0],w,a2]}
								})
							}
						},
				rad1=scale*rad1/100;rad2=scale*rad2/100;
				
				If[uFlag,
					With[{ch1=GetAttribute[a1,"UpdateSymbol"]~ReplacePart~{0->Hold},
							ch2=GetAttribute[a2,"UpdateSymbol"]~ReplacePart~{0->Hold}},
							(HoldComplete[{p1,p2,vector,d,X,Y,Z,tbs,r1=rad1,r2=rad2,reps,
										w=Replace[OptionValue[BondWidth],Automatic->.5],dbs,sf=styleFunction,
										c=Replace[OptionValue[ConstantBondWidth],Automatic->False]},
									reps=repzzz;
									Dynamic[ch1;ch2;
											p1=a1::Position-comAdjust;
											p2=a2::Position-comAdjust;
											vector=p2-p1;
											d=Norm@vector;
											vector=vector/d;
											d=Max@{(d-(r1+r2))/2,0};
											With[{rep=Replace[degree,sf]},
											With[{h=Replace[rep[[1]],pReps]},
												Block[{f=rep[[2]]},
													h//ReleaseHold]
												]],TrackedSymbols:>{ch1,ch2}]]/.repzzz->pReps)~ReplacePart~(0->DynamicModule)
										],
					
							(Hold[{p1,p2,vector,d,X,Y,Z,r1=rad1,r2=rad2,reps,
										w=Replace[OptionValue[BondWidth],Automatic->.5],
										dbs=Replace[OptionValue[DoubleBondSeparation],Automatic->.025],
										tbs=Replace[OptionValue[TripleBondSeparation],Automatic->.025],
										c=Replace[OptionValue[ConstantBondWidth],Automatic->False]},
								p1=With[{a=a1},a::Position-comAdjust];
								p2=With[{a=a2},a::Position-comAdjust];
								vector=p2-p1;
								d=Norm@vector;
								vector=vector/d;
								d=Max@{(d-(r1+r2))/2,0};
								reps=repzzz;
								With[{rep=Replace[degree,styleFunction]},
											With[{h=Replace[rep[[1]],reps]},
													(h/.f->rep[[2]])//ReleaseHold
												]]
								]/.repzzz->pReps)~ReplacePart~(0->Module)
				]
				]
			]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: DrawOb*)


(* ::Text:: *)
(*Generates a list of graphics an directives that display as the molecule. Dynamic updating defaults to enabled.*)


BoundMethod[DrawOb,self_,ConstantBondWidth->.1,Scale->1,ReturnAssociation->False,Origin->Automatic,
		Label->False,LabelingFunction->Automatic,PlotStyle->Automatic,Style->Automatic,
		DoubleBondSeparation->Automatic,TripleBondSeparation->Automatic,
		ShowCenterOfMass->Automatic,Update->All,InertialAxes->Automatic,Rotate->True]:=
	Module[
		{spheres={},tubes={},atoms=self::Atoms,bondOb=self::BondOb,
			bonds,keys,vector,com,dob,bob,n=0,
			styleFunctions=Replace[OptionValue@Style,{Automatic->{Automatic,Automatic},{a_,b_}:>{a,b},a_:>{a,Automatic}}],
			df=False(*OptionValue[DynamicUpdates]*),
			movable=self::GetAtoms[OptionValue@Update],
			uflag=False,
			doneBonds={},
			origin=OptionValue@Origin,
			cf=OptionValue[ShowCenterOfMass],
			lf=OptionValue[InertialAxes],
			rf=OptionValue[ReturnAssociation],
			label=Replace[OptionValue@Label,{False->None,True->Automatic,k_:>ToString@k}],
			lflag,
		ret=<||>},
		
		df=Length@movable>0;
		If[df,
			If[cf===Automatic,cf=False];
			If[lf===Automatic,lf=False]
			];
		If[cf===Automatic,cf=True];
		com=If[MatchQ[origin,{_?NumberQ,_?NumberQ,_?NumberQ}],
					origin,
					Replace[origin,
						{Automatic:>self::CenterOfMass,
						_:>With[{a=self::GetAtoms[origin]},
							With[{p=(#::Position&/@a),m=(#::Mass&/@a)},
								Total@MapThread[#1*#2&,{p,m}]/(Total@m)
							]]
						}]
				];
		lflag=IsInstance[label,String];
		Do[With[{cur=atom1},
			uflag=MemberQ[movable,cur];
			n=Max[{n,Norm@(cur::Position)}];
			dob=cur::DrawOb[Origin->com,Label->If[lflag,None,label],LabelingFunction->OptionValue@LabelingFunction,
					Style->styleFunctions[[1]],Scale->OptionValue@Scale,Update->MemberQ[movable,cur]];
			If[rf,
				ret[cur]=dob,
				AppendTo[spheres,dob]
			];
			bonds=cur::Bonds;
			keys=Keys[bonds];
			Do[With[{other=atom2},
				If[Not[MemberQ[doneBonds,{other,cur}]],
					bob=bondOb[cur,other,bonds[other],Origin->com,
						ConstantBondWidth->OptionValue[ConstantBondWidth],
						DoubleBondSeparation->OptionValue@DoubleBondSeparation,
						TripleBondSeparation->OptionValue@TripleBondSeparation,
						Scale->OptionValue@Scale,
						Update->MemberQ[movable,cur|other],Style->styleFunctions[[2]]];
					If[rf,
						ret[{cur,other}]=bob,
						AppendTo[tubes,bob]
					];
					AppendTo[doneBonds,{cur,other}];
				]],
			{atom2,keys}]],
		{atom1,atoms}];
		
		If[Not[rf],ret={spheres,tubes}];
		If[cf||lf,
			If[rf,
				ret["COM"]=Point[origin],
				AppendTo[spheres,Point[origin]]
			];
			If[lf,
				If[rf,
					ret["axes"]=self::ABCAxes[{n,n,n}],
					AppendTo[ret,self::ABCAxes[{n,n,n}] ]
					]
				]
			];
		ret=Replace[OptionValue@Rotate,{
			True:>self::RotateGraphics[ret],
			l:{_String|_Times,_String|_Times,_String|_Times}:>self::RotateGraphics[ret,Map[Replace[#,{HoldPattern[Times[value_,s_String]]:>value*ToUpperCase[s],
																										s_String:>ToUpperCase[s]}]&,l]],
			_->ret
		}];
		ret
	];


(* ::Subsubsection::Closed:: *)
(*Class Field: DrawStyles*)


ClassField[DrawStyles]=<|"Primitives"->{Automatic,Line,Point,TraditionalForm,Filling,Tube},
						"Combinations"->{"BallAndLine","BallAndTube","PointAndLine"}|>;


(* ::Subsubsection::Closed:: *)
(*Bound method: Draw*)


(* ::Text:: *)
(*Takes uses DrawOb to construct a graphic representing the Molecule.*)


BoundMethod[Draw,
	self_,ConstantBondWidth->Automatic,ShowCenterOfMass->Automatic,
	Origin->Automatic,Scale->.3,InertialAxes->Automatic,
	PlotStyle->Automatic,Label->False,LabelingFunction->Automatic,
	Update->None,Boxed->False,Lighting->Default,OptionsPattern]:=
	Module[{kw=$KeyWordArguments,objects,d=self::DrawOb},
		With[{reps={Line:>With[{L=kw[Lighting],dbs=kw[DoubleBondSeparation],tps=kw[TripleBondSeparation]},
								If[L===Default,kw[Lighting]="Neutral"];
									If[MatchQ[dbs,Automatic|_Missing],kw[DoubleBondSeparation]=.05];
									If[MatchQ[tps,Automatic|_Missing],kw[TripleBondSeparation]=.05];
									If[MatchQ[kw[ConstantBondWidth],Automatic],kw[ConstantBondWidth]=0];
										{(Deploy@Inset[Text[#3::Element],#1]&),
												{1->{1,Line[{#1+.1(#2//Normalize),#1+#2}]&},
												2->{2,{Line[#1[[1]]+#1[[2]]+.1(#2//Normalize),#1[[1]]+#1[[2]]+#2],
														Line[#1[[1]]-#1[[2]]+.1(#2//Normalize),#1[[1]]-#1[[2]]+#2]}&},
												3->{3,With[{p=#1[[1]],a=#1[[2,1]],b=#1[[2,2]],c=#1[[2,3]],v=#2,vn=#2//Normalize},
														{Line@{p+a+.1vn,p+a+v},Line@{p+b+.1vn,p+b+v},Line@{p+c+.1vn,p+c+v}}
														]&},
													_->({1,{Dashed,Line[{#1,#1+.8*#2}]}&})
													}}],
									TraditionalForm:>With[{L=kw[Lighting]},If[L===Default,kw[Lighting]="Neutral"];{{Specularity[Gray, 50],Sphere[#1,#2]}&,Style->Specularity[Gray, 50]}],
									Filling:>With[{L=kw[Lighting]},If[L===Default,kw[Lighting]="Neutral"];kw[Scale]=1;{Sphere[#1,2*#2]&,{_->{1,({}&)}}}],
									Point:>{{PointSize[#2/5],Point[#1]}&,
											{2->{2,{PointSize[#3/2],
														Point[#1[[1]]+#1[[2]]+.8#2],
														Point[#1[[1]]-#1[[2]]+.8#2]}&},
												3->{3,{PointSize[#3/2],
														Point[#1[[1]]+#1[[2,1]]+.8#2],
														Point[#1[[1]]+#1[[2,2]]+.8#2],
														Point[#1[[1]]+#1[[2,3]]+.8#2]}&},
												_->{1,{PointSize[#3/2],Point[#1+.8#2]}&}
												}},
									Tube:>With[{L=kw[Lighting]},
												If[L===Default,kw[Lighting]="Neutral"];
												kw[ConstantBondWidth]=.2;
												kw[DoubleBondSeparation]=None;kw[TripleBondSeparation]=None;
												{{Specularity[Gray, 50],Sphere[#1,.2]}&,
													{Style->Specularity[Gray, 50],
														(_?(!MatchQ[#,1|2|3]&)->{1,{Dashed,Line[With[{ps=#1},With[{v=Normalize@(ps[[2]]-ps[[1]])},{ps[[1]]+v*.2,ps[[2]]}]]]}&} )}}]
							},secondaryReps={
												(Style->{a_,b_}):>{{a,Sphere[#1,#2]}&,(Style->b)},
												(Style->s_):>{{s,Sphere[#1,#2]}&,(Style->s)},
												"BallAndLine"->{Automatic,Line},
												"BallAndTube"->{{Specularity[Gray, 50],Sphere[#1,Max@{.5#2,.2}]}&,Tube},
												"PointAndLine"->{Point,Line}}},
			kw[PlotStyle]=Replace[(kw[PlotStyle]/.{Directive[x_]:>(Style->x),None->({}&)}),{{Style->a_,b_}:>{{a,Sphere[#1,#2]}&,b},(Style->x_):>Replace[Style->x,secondaryReps]}];
			kw[Style]=Replace[kw[PlotStyle],{{a_,b_}:>With[{S=(self::DrawStyles)},
														{With[{r1=If[MemberQ[S["Combinations"],a],Replace[a,secondaryReps][[1]],a]},
															If[MemberQ[S["Primitives"],r1],With[{r=Replace[r1,reps]},If[Length@r>1,r[[1]],r]],r1]
															],
														With[{r1=If[MemberQ[S["Combinations"],b],Replace[b,secondaryReps][[2]],b]},
															If[MemberQ[S["Primitives"],r1],With[{r=Replace[r1,reps]},If[Length@r>1,r[[2]],r]],r1]
															]}],
													c_:>With[{r1=If[MemberQ[self::DrawStyles["Combinations"],c],Replace[c,secondaryReps],c]},
																		Replace[r1,reps]
																		]}];
		];
		If[MatchQ[kw[ConstantBondWidth],Automatic],kw[ConstantBondWidth]=.1];
		If[MatchQ[kw[Lighting],Default],kw[Lighting]="Neutral"];
		objects=d[ FilterRules[Normal@kw,Options@d] ];
		Graphics3D[objects,FilterRules[Normal@kw,Options[Graphics3D]]]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: RotateGraphics*)


BoundMethod[RotateGraphics,self_,graphic_,rotationAxes_:{"A","B","C"}]:=
	With[{abcPat=With[{alts={"A","B","C"}},Alternatives@@Join[alts,HoldPattern[Times[_,#]]&/@alts]],
					abcAx={"A"->{1,1},Times[x_,"A"]:>{x,1},"B"->{1,2},Times[y_,"B"]:>{y,2},Times[z_,"C"]:>{z,3},"C"->{1,3}},
					standardBasis=IdentityMatrix@3},
	(*Print@{rotationAxes,abcPat};*)
	Replace[rotationAxes,{
		{xaxis:abcPat,yaxis:abcPat,zaxis:abcPat}:>
			With[{X=Replace[xaxis,abcAx],
					Y=Replace[yaxis,abcAx],
					Z=Replace[zaxis,abcAx]},
				
				If[self::IsLinear,
					With[{a=self::Atoms},
						If[Length@a>1,
							With[{v=GetAttribute[a[[1]],"Position"]-GetAttribute[a[[-1]],"Position"],
									axis=X[[1]]*standardBasis[[ X[[2]] ]]},
								With[{azimuthal=VectorAngle[v,axis],
									about=v\[Cross]axis},
									If[azimuthal>0&&about!={0,0,0},Rotate[graphic,azimuthal,about],graphic]
									]
								],
							graphic
							]
						],
					With[{abcaxes=self::ABCVectors[[2]],O=graphic},
						With[{toX=X[[1]]*abcaxes[[ X[[2]] ]],toY=Y[[1]]*abcaxes[[ Y[[2]] ]],toZ=Z[[1]]*abcaxes[[ Z[[2]] ]]},
						With[{aboutX=(toX\[Cross]standardBasis[[1]]),
								angleX=VectorAngle[toX,standardBasis[[1]]]},
							With[{angleY=-VectorAngle[If[(angleX!=0&&aboutX!={0,0,0})//TrueQ,RotationMatrix[angleX,aboutX].toY,toY],standardBasis[[3]]]},
								O//If[(angleX!=0&&aboutX!={0,0,0})//TrueQ,Rotate[#,angleX,aboutX],#]&
									//If[(angleY!=0//TrueQ),Rotate[#,angleY,standardBasis[[1]]],#]&//Normal
								]
							]
							]
						]
				]
			]
		}]
		];


(* ::Subsubsection::Closed:: *)
(*Bound property: Mass*)


(* ::Text:: *)
(*Adds all atom masses to get total mass*)


	BoundProperty[Mass,self_]:=
		Plus@@(GetAttribute[#,"Mass"]&/@self::Atoms);


(* ::Subsubsection::Closed:: *)
(*Bound method: Move*)


(* ::Text:: *)
(*Moves some set of atoms (defaults to everything). Can specify motion along intertial axes or any other axis system.*)


BoundMethod[Move,self_,move:_?(MatchQ[#,{_?NumberQ,_?NumberQ,_?NumberQ}]&):None,
		atomList:_?((MatchQ[#,(_Rule|_RuleDelayed)]//Not)&):All,
		"X"->0,"Y"->0,"Z"->0,Axes->IdentityMatrix[3],Set->False]:=
	With[{vector=Replace[move,None->(OptionValue/@{"X","Y","Z"}*Replace[OptionValue@Axes,Automatic->self::ABCVectors[[1]]])],
			atoms=self::GetAtoms[atomList]},
			Block[{$DynamicEvaluation=True},
				With[{a=#},a::Move[vector]]&/@atoms
			]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: Rotate*)


(* ::Text:: *)
(*Rotates some set of atoms (defaults to everything)*)


BoundMethod[Rotate,self_,\[Theta]_,atomList:_?((MatchQ[#,(_Rule|_RuleDelayed)]//Not)&):All,
	Axis->{0,1,0},Center->Automatic]:=
		With[{atoms=self::GetAtoms[atomList],
				axis=Replace[OptionValue@Axis,{Automatic,i_(1|2|3)}:>self::ABCVectors[[i]]],
				center=Replace[OptionValue@Center,{Automatic:>self::CenterOfMass,
													{x_?NumberQ,y_?NumberQ,z_?NumberQ}:>{x,y,z},
													id_:>Mean@(#::Position&/@self::GetAtoms[id])}]},
			Block[{$DynamicEvaluation=True},
				With[{a=#},a::Rotate[\[Theta],Axis->axis,Center->center]]&/@atoms
				]
			];


(* ::Subsubsection::Closed:: *)
(*Bound method: Breathe*)


(* ::Text:: *)
(*Extends atoms along bond angles, if defined, and along the center of mass vector if not. Breathes by either a fixed amount or a fraction of the bond length (second is default).*)


BoundMethod[Breathe,self_,quantity:_?(NumberQ):.1,atomList:_?(MatchQ[#,_List|None|All]&):All,
				Mode->Scaled]:=
		With[{atoms=self::GetAtoms[atomList]},
			With[{a=#},
				With[{v=a::BondVector},a::Move[Replace[OptionValue@Mode,{Scaled->v*quantity,_->quantity}]]]
				]&/@atoms
			];			


(* ::Subsubsection::Closed:: *)
(*Bound property: CenterOfMass*)


	BoundProperty[CenterOfMass,self_]:=
		With[
			{atoms=self::Atoms,T=Replace[self::Mass,0->1]},
			Total@Table[(GetAttribute[A,"Mass"]/T*GetAttribute[A,"Position"]),{A,atoms}]
		];


(* ::Subsubsection::Closed:: *)
(*Bound property: EMCMatrix*)


(* ::Text:: *)
(*Returns {element, mass, position} triplets for every atom*)


	BoundProperty[EMCMatrix,self_]:=
		{#::Element,#::Mass,#::Position}&/@self::Atoms;


(* ::Subsubsection::Closed:: *)
(*Bound property: IsLinear*)


(* ::Text:: *)
(*Returns whether the molecule is linear (or a single atom) or not*)


BoundProperty[IsLinear,self_]:=
	With[{a=self::Atoms},
		If[Length@a<3,
		True,
		With[{p1=GetAttribute[a[[1]],"Position"]},
			With[{v1=(p1-GetAttribute[a[[2]],"Position"])//Normalize},
			AllTrue[a[[3;;]],
				With[{v=(p1-#::Position)//Normalize},v===v1||(-v)===v1]&]
			]
		]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound property: DegreesOfFreedom*)


BoundProperty[DegreesOfFreedom,self_]:=
	With[{N=3*Length@self::Atoms},
		If[self::IsLinear,{3,2,N-2},{3,3,N-3}]
		]


(* ::Subsubsection::Closed:: *)
(*Bound property: InteriaTensor*)


	BoundProperty[InertiaTensor,self_]:=
		(*Units are mass*distance^2, which is most likely amu\[VeryThinSpace]\[Angstrom]^2*)
		With[{coords=self::EMCMatrix(*,diagI,offDiag,fill,tensI*)},
		With[{masses=coords[[All,2]],pos=coords[[All,3]]},
			Total@MapThread[
				With[{m=#1,x=#2[[1]],y=#2[[2]],z=#2[[3]]},
					m*{ 
						{y^2+z^2,-x*y,-x*z},
						{-x*y,x^2+z^2,-y*z},
						{-x*z,-y*z,x^2+y^2}
					}]&,{masses,pos}]
			(*diagI[J_]:=Sum[
					masses[[i]]*(
						(pos[[i, Mod[J+1,3,1] ]])^2+(pos[[i, Mod[J+2,3,1] ]])^2
						),
				{i,Length[coords]}];
			offDiag[i_,j_]:=-Sum[
				masses[[k]]*(pos[[k,i]]*pos[[k,j]]),
				{k,Length[coords]}];
			fill[i_,j_]:=If[i==j,diagI[i],offDiag[i,j]];
				tensI=Array[fill,{3,3}]
			]*)
		]];


(* ::Subsubsection::Closed:: *)
(*Bound property: ABCVectors*)


(* ::Text:: *)
(*Extracts the inertial axis vectors and inertial constants*)


	BoundProperty[ABCVectors,self_]:=
		Module[{tensI,o,\[CapitalLambda],T},
			tensI=self::InertiaTensor;
			{\[CapitalLambda],T}=Eigensystem[tensI];
			\[CapitalLambda]=(1/#)&/@\[CapitalLambda];
			o=Reverse@Ordering[\[CapitalLambda]];
			\[CapitalLambda]=\[CapitalLambda][[o]];T=T[[o]];
			tensI=DiagonalMatrix[\[CapitalLambda]];
			{tensI,T}
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: ABCAxes*)


(* ::Text:: *)
(*Turns the ABCVectors into graphics axes*)


	BoundMethod[ABCAxes,self_,magnitudes_:{1,1,1},WithMagnitude->False]:=
		Module[{abc,kw=$KeyWordArguments,vectors,
			mFlag,letters={"A","B","C"},
			df},
			{abc,vectors}=self::ABCVectors ;
			mFlag=kw[WithMagnitude];
			kw=KeyDrop[kw,{WithMagnitude}];
			If[Not[mFlag],abc=magnitudes];
				MapThread[{
				Line[{{0,0,0},#1*#2}],
				{Dashed,Line[{{0,0,0},-(#1*#2)}]},
				Inset[Text[#3],(#1+.25)*#2]
				}&,{abc,vectors,letters}]
			];


(* ::Subsubsection:: *)
(*Bound method: EmpiricalFormula*)


BoundMethod[EmpiricalFormula,self_]:=
	With[{enums=SortBy[ObjectString/@self::Atoms//Counts//Normal,(First[#]/.{"C"->0,"H"->1,_->2}&)]},
		Replace[enums,(el_->num_):>If[num>1,Subscript[el,num],el],1]//Row
	];


(* ::Subsubsection:: *)
(*Bound method: ChemicalFormula*)


BoundMethod[ChemicalFormula,self_]:=(Message[MathematicaClass::noimp,self,"ChemicalFormula"];$Failed)


(* ::Subsubsection:: *)
(*Bound method: $ToString*)


BoundMethod[$ToString,self_]:=(self::EmpiricalFormula[]);


(* ::Subsubsection:: *)
(*Representation Function*)


RepresentationFunction[self_]:=Framed[DynamicModule[{icon="SquarePlusIconMedium",mode="Elided"},
	With[{b=Button[Dynamic[RawBoxes@FEPrivate`FrontEndResource["FEBitmaps",icon]],
			mode=Replace[mode,{"Elided":>(icon="SquareMinusIconMedium";None),
								None->(icon="SquarePlusIconMedium";"Elided")}],
		Appearance->"Frameless"]},
		Framed[
			Row[{b,Spacer[5],Dynamic[(Replace[mode,{None:>ExpressionCell[self::Draw[],Deployed->False],"Elided":>self::$ToString[]}]),TrackedSymbols:>{mode}]}],
			RoundingRadius->5,Background->White
			]
		]
	],RoundingRadius->5,Background->GrayLevel[.95]]


(* ::Subsubsection::Closed:: *)
(*End Molecule Class*)


EndClass[];


(* ::Subsection::Closed:: *)
(*Complex Class*)


(* ::Subsubsection::Closed:: *)
(*Class ChemicalComplex*)


ChemicalComplex=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


InitializationFunction[self_,compA_,compB_]:=
	With[{componentA=ChemWrapperClass::GetPrimitive[compA,ObjectType@self],
		componentB=ChemWrapperClass::GetPrimitive[compB,ObjectType@self]},
	With[{kw=$KeyWordArguments,strA=ToString@ObjectType[componentA],strB=ToString@ObjectType[componentB]},
		self::Component1=componentA;
		self::Component2=componentB;
		self::Components={componentA,componentB};
		]
		];


(* ::Subsubsection::Closed:: *)
(*Bound property: InitList*)


BoundProperty[InitList,self_]:=With[{l={self::Component1::InitList,self::Component2::InitList}},l];


(* ::Subsubsection::Closed:: *)
(*Extends Molecule*)


	ParentClasses={Molecule};


(* ::Subsubsection::Closed:: *)
(*Bound property: Atoms*)


	BoundProperty[Atoms,self_]:=With[{s=self},Flatten@Table[
									If[HasAttribute[obj,"Atoms"],
										GetAttribute[obj,"Atoms"],
										obj],
									{obj,{s::Component1,s::Component2}}
											]
									];


(* ::Subsubsection::Closed:: *)
(*Bound method: GetAtoms*)


(* ::Text:: *)
(*Extends Molecule::GetAtoms by taking specifications in either component*)


BoundMethod[GetAtoms,self_,key_,Property->ObjectString]:=
	Replace[key,{
	{"Component",i_?(MatchQ[#,(1|2)]&),k_:All}:>With[{comp=self::Components[[i]]},
					comp::GetAtoms[k,Property->OptionValue@Property]],
	_:>Superclass[]::GetAtoms[key,Property->OptionValue@Property]}
	]


(* ::Subsubsection::Closed:: *)
(*Bound property: ReducedMass*)


	BoundProperty[ReducedMass,self_]:=
		With[{M1=self::Component1::Mass,M2=self::Component2::Mass},
			(M1*M2)/(M1+M2)
			];


(* ::Subsubsection::Closed:: *)
(*Bound method: Rotate*)


(* ::Text:: *)
(*Extends Molecule::Rotate by allowing for rotation about the component of the complex*)


BoundMethod[Rotate,self_,\[Theta]_,atomList:_?((MatchQ[#,(_Rule|_RuleDelayed)]//Not)&):All,
	Axis->{0,1,0},Center->Automatic]:=
	Superclass[]::Rotate[\[Theta],atomList,
		Axis->Replace[OptionValue@Axis,
			{i_?(MatchQ[#,(1|2)]&),Automatic,j_?(MatchQ[#,(1|2|3)]&)}:>With[{comp=self::Components[[i]]},comp::IntertialAxes[[j]]]
			],
		Center->Replace[OptionValue@Center,(i_?(MatchQ[#,(1|2)]&)):>With[{comp=self::Components[[i]]},comp::CenterOfMass]]
		];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Subsection:: *)
(*Convenience Functions Class*)


ChemWrapperClass:=BeginClass[ChemWrapper];


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


	InitializationFunction[self_,molecule:_?(Not@MatchQ[#,_(Rule|RuleDelayed)]&):None,Update->{}]:=
		With[{kw=$KeyWordArguments},
			self::Object=molecule;
			self::ActiveAtoms=kw[Update];
			self::kwargs=kw;
			];


(* ::Subsubsection::Closed:: *)
(*Class field: CompoundMap*)


	ClassField[CompoundMap]=<|
		"HOH":>With[{O={0,0,0},BD=ChemData::BondDistance},
				Molecule[{{"O",O},
								{"H",O+BD["O","H"]*RotationMatrix[104.5/2 Degree,{0,0,1}].{1,0,0}},
								{"H",O+BD["O","H"]*RotationMatrix[-104.5/2 Degree,{0,0,1}].{1,0,0}}},
							{{1,2,1},{1,3,1}}]
				],
		"HCCH":>Molecule@@
				With[{O={0,0,0},BD=ChemData::BondDistance},
				{
				{
					{"C",O},{"C",O+{1,0,0}*BD@@{"C","C",3}},
					{"H",O+BD["C","H"]*{-1,0,0}},{"H",O+{1,0,0}*BD@@{"C","C",3}+BD["C","H"]*{1,0,0}}
				},
				{
					{1,2,3},
					{1,3,1},
					{2,4,1}
				}
				}],
		"HF":>Molecule[{{"F",{0,0,0}},{"H",{1,0,0}*ChemData::BondDistance["F","H"]}},{{1,2,1}}],
		"HCL":>Molecule[{{"Cl",{0,0,0}},{"H",{1,0,0}*ChemData::BondDistance["Cl","H"]}},{{1,2,1}}],
		"ARVCL":>Module[
					{BM=ChemData::BondDistance,CH,CCl,CC,CAr=3,
					origin={0,0,0},CHvec=Normalize[{-.5,-.5,0}],CCvec={1,0,0},Arvec={0,1,0},
					VCl,Ar},
					Arvec=CAr*RotationMatrix[30*Degree,CCvec].Arvec;
					CC=BM["C","C",2];CH=BM["C","H"];CCl=BM["C","Cl"];
					CCvec=CC*CCvec;
					VCl=Molecule[
							{
								{"H",origin+CH*CHvec},{"H",origin+CH*CHvec*{1,-1,1}},
								{"C",origin},{"C",origin+CCvec},
								{"H",origin+CCvec+CH*CHvec*{-1,1,1}},{"Cl",origin+CCvec+(-1*CCl)*CHvec}
							},
							{
								{1,3,1},{2,3,1},
								{3,4,2},
								{4,5,1},{4,6,1}
							}
						];
					Ar=Atom["Ar",origin+(.5*CCvec)+Arvec];
					ChemicalComplex[Ar,VCl]
				]
		|>;


(* ::Subsubsection::Closed:: *)
(*Class method: GetPrimitive*)


ClassMethod[GetPrimitive,cls_,l_,complexType_:ChemicalComplex]:=Replace[l,{
		a:{_String,_List,___}:>Atom@@a,
		m:{{{_String,_List,___}..},{{_?NumberQ..}...}}|{{{_String,_List,___}..}}:>Molecule@@m,
		o_List:>ChemicalComplex@@o}]


(* ::Subsubsection::Closed:: *)
(*Bound method: GetCompound*)


BoundMethod[GetCompound,self_,compKey_]:=
	With[{c=self::CompoundMap,s=ToUpperCase@ObjectString[compKey]},
		c[Replace[s,{
					"H2O"|"HHO"|"WATER"->"HOH",
					"HCCH"|"ETHYNE"|"ACETYLENE"->"HCCH",
					"HYDROCHLORIC ACID"->"HCL",
					"ARGON-VINYL CHLORIDE"->"ARVCL",
					"HYDROFLUORIC ACID"->"HF"
					}]]
		];


(* ::Subsubsection::Closed:: *)
(*Static method: BondDistances*)


(* ::Text:: *)
(*Returns both the average bond distances and the bond distances for each atom*)


StaticMethod[BondDistances,chemical_/;(Quiet@Check[ChemicalData[chemical];True,False])]:=
With[{bondMap=
	With[{positions=(QuantityMagnitude/@#)&/@ChemicalData[chemical,"AtomPositions"],
		atoms=MapIndexed[{#1,#2[[1]]}&,ChemicalData[chemical,"VertexTypes"]],
		bonds=With[{b=ChemicalData[chemical,"EdgeRules"]},
				Block[{Rule=List},b]
				]},
		Table[Table[
			If[MemberQ[bonds,{i,j}],
				{atoms[[i]],atoms[[j]]}->((positions[[i]]-positions[[j]])//Norm)/100,
					None
				],
				{j,Range[i+1,Length@positions]}],
				{i,Length@positions-1}]
			]~Flatten~1//DeleteCases[#,None]&
		},
	With[{droppedMap=bondMap/.{{s1_String,_Integer},{s2_String,_Integer}}:>{s1,s2}},
			{With[{keys=DeleteDuplicates@(#[[1]]&/@droppedMap)},
				Table[k->Mean@Cases[droppedMap,(k->v_):>v],
					{k,keys}
					]
				],
		bondMap}]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: Rotate*)


BoundMethod[Rotate,self_,object:_?(MatchQ[#,_MathematicaClass]&):Automatic,
				{degrees_,args__}|degrees_?(!MatchQ[#,_List]&),
				Repeated->1,Timing->.2,Delayed->0,Cycles->1]:=
	With[{ob=Replace[object,Automatic:>self::Object],c=OptionValue@Cycles,deg=Replace[degrees,Times[n_,Degree]:>n]Degree,r=OptionValue@Repeated,t=OptionValue@Timing,d=OptionValue@Delayed},
		With[{R=ob::Rotate},
			(*Print@{R,deg,args};*)
			If[r>1,
				Do[RunScheduledTask[R[deg,args],{t,r},AbsoluteTime[]+d],c],
				R[deg,args]
				]
			]
		];


(* ::Subsubsection::Closed:: *)
(*End Class ChemWrapper*)


EndClass[];
ChemWrapper=ChemWrapperClass[];


(* ::Section:: *)
(*End Package*)


(*End[];*)


End[];
