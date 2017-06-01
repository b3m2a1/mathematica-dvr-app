(* ::Package:: *)

(* ::Chapter:: *)
(*Python OOP Package*)


(* ::Section:: *)
(*Exposed Code*)


(* ::Subsection::Closed:: *)
(*Class Construction Aids*)


MathematicaClassConstructor::usage="Creates a mathematica class"
MathematicaClass::usage="Head for mathematica classes"
BeginClass::usage="Begins package like class construction. Returns class name."
EndClass::usage="Ends package like class construction. Returns a class."
ConstructClass::usage="Simplified class constructor"
InitializationFunction::usage="Makes an __init__ function"
ParentClasses::usage="Set classes to inherit from"
ClassField::usage="Makes a class field"
MethodPrepper::usage="Creates a method"
BoundMethod::usage="Makes a bound method"
ClassMethod::usage="Makes a class method"
StaticMethod::usage="Makes a static method"
BoundProperty::usage="Makes a property object"
NoAttributeFunction::usage="The function used if no attribute is provided"
RepresentationFunction::usage="The function to call when displaying the instance"
CallInstanceFunction::usage="The function to use when calling the instance"
StringFunction::usage="The function to use when generating a string of the instance"
PostLoadFunction::usage="A function to be applied after loading the object via LoadObject"


(* ::Subsection::Closed:: *)
(*Instance Construction Options*)


InstanceSymbolName::"The symbol name to assign to the new instance. Defaults to the class name and extension."
InstanceNameExtension::"The extension to add after the class name when making an instance. Allows for pseudo-typing."


(* ::Subsection::Closed:: *)
(*Method Keywords*)


$PassedArguments::usage="object which provides the arguments passed to a method"
$DefaultArguments::usage="object which provides the default arguments of a method"
$KeyWordArguments::usage="function to provide all arguments"
MethodAttributes::usage="Keyword passed to methods to set the attributes of the method"


(* ::Subsection::Closed:: *)
(*Class Usage Aids*)


$AutoRepresentation::usage="(boolean) Toggles whether $ToRepresentation is called automatically"
ClassQ::usage="Symbol->is class"
InstanceQ::usage="MathematicaClass->is an instance of some class"
Superclass::usage="Returns a superclass with parent attributes, with collisions resolved by parent order"
ObjectType::usage="Returns obj[MathematicaObjectClass] if provided, else Head[obj]"
ObjectString::usage="(Calls obj[$ToString], if one it's provide. Otherwise returns ToString[obj]"
ObjectRepresentation::usage="Calls defined representation if possible, else just returns the first argument of object"
IsInstance::usage="(boolean) Returns obj[IsInstance][cls] or TrueQ[Head[obj]==cls]"
HasAttribute::usage="(boolean) Checks an object's attribute dictionary for the attribute"
GetAttribute::usage="(arbitary) Uses obj[GetAttribute] if possible. Else tries to get by index"
SetAttribute::usage="(arbitrary) Uses obj[SetAttribute] if possible. Else sets by index"
DeleteAttribute::usage="(null) Unsets object attribute"
GetItem::usage="Gets objects by indices"
SetItem::usage="Sets object indices"
GetKey::usage="Returns field, bound method, class method, static method, property key"
ObjectValues::usage="Returns fields, bound methods, class methods, static methods, properties"
SaveObject::usage="Saves a class object to a file for later reloading"
LoadObject::usage="Reloads an object from file"


(* ::Subsection::Closed:: *)
(*Helper Functions*)


ClassCount::usage="(integer) Returns number of classes defined"
InstanceCount::usage="(integer) Returns number of instances defined"
ClearClasses::usage="(null) Deletes all classes (also deletes all instances)"
ClearInstances::usage="(null) Deletes all class instances"
IsCallable::usage="Returns whether or not one can call the symbol"
SymbolOverload::usage="Overloads the Dot operator in the desired fashion"


(* ::Section:: *)
(*Internal Code*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*Initialization Code*)


If[ValueQ@$AutoRepresentation//Not,$AutoRepresentation:=($KernelID===0)];
If[ValueQ@$ClassBuffer//Not,$ClassBuffer={}];
If[ValueQ@$ClassCounter//Not,$ClassCounter=100];
If[ValueQ@$InstanceCounter//Not,$InstanceCounter=100];
If[ValueQ@$ClassSymbols//Not,$ClassSymbols={}];
If[ValueQ@$InstanceSymbols//Not,$InstanceSymbols={}];
If[ValueQ@$LoadedObjects//Not,$LoadedObjects=<||>];


(* ::Subsection::Closed:: *)
(*MathematicaClass Head Definition*)


(* ::Subsubsection::Closed:: *)
(*Messages*)


MathematicaClass::noattr="`` has no attribute ``"
MathematicaClass::newargx="incorrect number of arguments passed to ``::$New; `` required, `` optional, options pattern: ``. Arguments passed: `` arguments, `` options"
MathematicaClass::invinit="``::$New has no instance argument"
MathematicaClass::argerr="``::$New failed with passed arguments"
MathematicaClass::nocll="`` is not callable"
MathematicaClass::noimp="``: method `` not implemented"


(* ::Subsubsection::Closed:: *)
(*Operator Overloading*)


(*MathematicaClass/:HoldPattern[(op_/;MatchQ[op,Except[(Head|List)]])[args1___,MathematicaClass[sym_],args2___]]:=op[args1,sym,args2]*)
MathematicaClass[s_String]:=LoadObject@s;
With[{messageExceptions=(MatchQ[#,Except@Alternatives["usage"]]&)},
MathematicaClass[m:MathematicaClass[_]]:=m;
Unprotect@MessageName;
SyntaxInformation@MessageName={"ArgumentsPattern"->{_,_,___}};
HoldPattern@MessageName[MessageName[cls_,attrs__][args___],k__]:=
	With[{sym=MessageName[cls,attrs][args]},
		MessageName[sym,k]
		];
Protect@MessageName;
(*Overloads the necessary operators to get MathematicaClass[_Symbol] to operate with :: like most languages do with .*)ClassOverload:=(
(*Instantiation*)
MathematicaClass/:HoldPattern[MathematicaClass[v_Symbol/;MemberQ[$ClassSymbols,v]][args___]]:=
	With[{o=v[args]},
		If[o===Unevaluated[v[args]],
			GetAttribute[MathematicaClass[v],"$New"][args],
			o
			]
		];
(*Call*)
MathematicaClass/:HoldPattern[MathematicaClass[v_Symbol/;MemberQ[$InstanceSymbols,v]][args___]]:=
	With[{o=v[args]},
		If[o===Unevaluated[v[args]],
			Replace[Quiet[GetAttribute[MathematicaClass[v],"$Call"],MathematicaClass::noattr],
						{Null:>Message[MathematicaClass::nocll,v],f_:>f[args]}],
			o
			]
		];
MathematicaClass/:HoldPattern[Symbol[MathematicaClass[v_Symbol]]]:=v;
MathematicaClass/:HoldPattern[(op:(ToString|Context|DownValues|UpValues|Clear|ClearAll|Remove|SymbolName))
											[MathematicaClass[v_Symbol]]]:=op@v;
MathematicaClass/:HoldPattern[MessageName[MathematicaClass[v_Symbol],"usage"]]:=
	Quiet[
		Block[{m=GetAttribute[v,"usage"],upv=UpValues@MessageName},
		If[Length@$MessageList>0,
			Unprotect@MessageName;
			UpValues[MessageName]={};
			m=MessageName[v,"usage"];
			UpValues[MessageName]=upv;
			Protect@MessageName];
		m]
	];
MathematicaClass/:HoldPattern@MessageName[v:MathematicaClass[_Symbol],attrs__?messageExceptions]:=
	Block[{sym=v,lisp=attrs},
		Do[sym=GetAttribute[sym,o];If[sym===Null,Return[]],{o,{lisp}}];
		sym];
Unprotect[MessageName];
	MessageName/:HoldPattern[Set[MessageName[v:MathematicaClass[_Symbol],attrs__?messageExceptions],k_]]:=
		Block[{sym=v,lisp={attrs}},
			Do[sym=GetAttribute[sym,o];If[sym===Null,Return[]],{o,lisp[[;;-2]]}];
			If[sym=!=Null,SetAttribute[sym,lisp[[-1]],k]];
			k];
	MessageName/:HoldPattern[SetDelayed[MessageName[v:MathematicaClass[_Symbol],attrs__?messageExceptions],k_]]:=
		Block[{sym=v,lisp={attrs}},
			Do[sym=GetAttribute[sym,o];If[sym===Null,Return[]],{o,lisp[[;;-2]]}];
			If[sym=!=Null,SetAttribute[sym,lisp[[-1]],k,"delayed"]];
			];
	MessageName/:HoldPattern[Set[MessageName[v:MathematicaClass[_Symbol],attrs__?messageExceptions][i___],k_]]:=
		Block[{sym=v,value=MessageName[v,attrs],lisp={attrs}},
			value[i]=k;
			Do[sym=GetAttribute[sym,o],{o,lisp[[;;-2]]}];
				SetAttribute[sym,lisp[[-1]],value];
			value];
	MessageName/:HoldPattern[SetDelayed[MessageName[v:MathematicaClass[_Symbol],attrs__?messageExceptions][i___],k_]]:=
		Block[{sym=v,value=MessageName[v,attrs],lisp={attrs}},
			value[i]:=k;
			Do[sym=GetAttribute[sym,o],{o,lisp[[;;-2]]}];
				SetAttribute[sym,lisp[[-1]],value];
			value];
	Unprotect[Part];
		Part/:HoldPattern[Set[Part[MessageName[MathematicaClass[v_Symbol],attrs__?messageExceptions],i__],k_]]:=
			Block[{sym=v,value=MessageName[v,attrs],lisp={attrs}},
				value[[i]]=k;
				Do[sym=sym[o],{o,lisp[[;;-2]]}];
					SetAttribute[sym,lisp[[-1]],value];
				value];
	Protect[Part];
	(*MessageName/:HoldPattern[AddTo[MessageName[v:MathematicaClass[_Symbol],attr_],k_]]:=SetAttribute[v,attr,k,"+"];*)
	(*MessageName/:HoldPattern[Set[MessageName[v:MathematicaClass[_Symbol],attrs__][k_],a_]]:=
		Block[{sym=v,value=MessageName[v,attrs],lisp={attrs}},
			value[i]=k;
			Do[sym=sym["GetAttribute",o],{o,lisp[[;;-2]]}];
				sym["SetAttribute",lisp[[-1]],value];
			value];(SetAttribute[v,attr,k->a,"+"];a);*)
	MessageName/:Unset[ MessageName[MathematicaClass[v_Symbol],attrs__] ]:=
		Block[{sym=v,lisp={attrs}},
			(*Print[lisp];*)
			Do[sym=sym[o],{o,lisp[[;;-2]]}];sym["$DeleteAttribute",Evaluate@lisp[[-1]]]
		];
Protect[MessageName];);
ClassOverload;
(*Overloads the setting process so that any variable assigned MathematicaClass[sym] as a value routes appropriately when used with ::)SymbolRouting[c_,a_/;Head[Unevaluated@a]===Symbol]:=*)
SymbolRouting[c_,a_/;Head[Unevaluated@a]===Symbol]:=
	With[{u=UpValues[MathematicaClass](*,v=Evaluate@ToExpression[a]*)},
		a/:HoldPattern[MessageName[a,args___?messageExceptions]]:=MessageName[Evaluate@a,args];
		Unprotect@MessageName;
		MessageName/:HoldPattern[Set[MessageName[a,attr___?messageExceptions],v_]]:=With[{s=Evaluate@a},Set[MessageName[s,attr],v]];
		MessageName/:HoldPattern[Set[MessageName[a,attr___?messageExceptions][k_],v_]]:=With[{s=Evaluate@a},Set[MessageName[s,attr][k],v]];
		MessageName/:HoldPattern[Unset[MessageName[a,attr___?messageExceptions]]]:=With[{s=Evaluate@a},Unset[MessageName[s,attr]]];
		Protect@MessageName;
		UpValues[MathematicaClass]={};
		Set[Unevaluated@a,c];
		UpValues[MathematicaClass]=u;
		c
		];
SetAttributes[SymbolRouting,HoldAll];
SetOverload[cla_]:=With[{c=cla},
	MathematicaClass/:HoldPattern[ Set[v_/;MatchQ[Hold[v],Hold[_Symbol]],MathematicaClass[c]] ]:=(SymbolRouting[MathematicaClass[c],Unevaluated@v]);
	];
SetAttributes[SetOverload,HoldAll];
SymbolOverload[sym_]:=With[{c=Evaluate[sym]},SetOverload[sym];];

];


(* ::Subsubsection::Closed:: *)
(*Formatting*)


Entity["Country","China"];
repr[m:MathematicaClass[_Symbol],format_]:=
Block[{EntityFramework`Private`GetEntityName=(#[[2]])&,
		EntityFramework`Private`OKEntityNameQ=(True&)
		},
	With[{sym=ToString@Symbol[m],nameQ=ObjectType[m]=!=MathematicaClass},
		With[{boxes=If[nameQ,
			ReplacePart[
				EntityFramework`MakeEntityFrameworkBoxes[Entity["Instance",sym],format],{
					{1,2}->RowBox[{ToString@MathematicaClass,"[",sym,"]"}],
					{1,3}->ObjectString[m::Type]
				}],
			ReplacePart[
				EntityFramework`MakeEntityFrameworkBoxes[EntityClass["Class",sym],format],{
					{1,2}->RowBox[{ToString@MathematicaClass,"[",sym,"]"}],
					{1,3}->ObjectString[m::Type]
				}]
			]},
		boxes
		]
		]
	];
MakeBoxes[MathematicaClassBoxForm[m:MathematicaClass[_Symbol]],StandardForm]:=repr[m,StandardForm];
MakeBoxes[MathematicaClassBoxForm[m:MathematicaClass[_Symbol]],TraditionalForm]:=repr[m,StandardForm];
Format[m:MathematicaClass[sym_Symbol]/;($AutoRepresentation&&MemberQ[$InstanceSymbols~Join~$ClassSymbols,sym])]:=
		Replace[ObjectRepresentation[m],{
			None:>MathematicaClassBoxForm[m],
			o_:>Interpretation[o//Deploy,m]
			}];


(* ::Subsection::Closed:: *)
(*Function Object Definition*)


StringRulePattern[attrs__]:=(Rule|RuleDelayed)[Alternatives[attrs],_];


(* ::Subsubsection::Closed:: *)
(*Getting Function Object Attributes*)


FunctionGetAttribute[function_,attr_String]:=
	Cases[function,StringRulePattern[attr]];
FunctionGetAttribute[funct_,attr__String]:=
	FunctionGetAttribute[funct,#]&/@{attr};
FunctionGetAttribute[f:FunctionHold[_],___]:=FunctionGetAttribute[f];


(* ::Subsubsection::Closed:: *)
(*Setting Function Object Attributes*)


SetAttributes[FunctionReplaceAttribute,HoldRest];
FunctionReplaceAttribute[function_,attr_String,obj_:None,head_:Rule]:=
	Replace[FunctionGetAttribute[function,attr],
		{
		{}:>Append[function,head[attr,obj]],
		{a_}:>Replace[function,a->head[attr,obj],1],
		{a_,b__}:>Replace[DeleteCases[function,Alternatives[b]],a->head[attr,obj],1]
		}];
FunctionReplaceAttribute[function_,attr:(_Rule|_RuleDelayed)]:=
	With[{attName=attr[[1]],obj=Extract[attr,2,Unevaluated],type=Head@attr},
		FunctionReplaceAttribute[function,attName,obj,type]
		];
FunctionReplaceAttribute[function_,attrs__?(MatchQ[#,_Rule|_RuleDelayed]&)]:=
	Block[{FunctionReplaceAttributereplacementThing=function},
		Do[With[{floop=a},FunctionReplaceAttributereplacementThing=FunctionReplaceAttribute[FunctionReplaceAttributereplacementThing,floop]],
			{a,{attrs}}];
		FunctionReplaceAttributereplacementThing
		];
FunctionReplaceAttribute[f:FunctionHold[_],___]:=f;


(* ::Subsubsection::Closed:: *)
(*Calling Function Objects*)


CallFunction[FunctionHold[f_],args___]:=CallFunction[f,args];
FunctionHold[f_][args___]:=CallFunction[f,args];


ClassMethod::nocls="ClassMethod missing parameter \"PassCls\" in calling ``"
With[{metPat=StringRulePattern["PassCls","SuperPassCls","BoundTo"],
clsPat=StringRulePattern@"PassCls",
supclsPat=StringRulePattern@"SuperPassCls"},

CallFunction[ClassMethod[function_,ops___?(MatchQ[#,metPat]&)],args___]:=
	With[{clsCases=Cases[{ops},clsPat]},
		If[clsCases=={}&&{args}==={},
			Message[ClassMethod::nocls,function];
			$Failed,
			With[{nargs=Sequence@@If[clsCases==={},{args}[[2;;]],{args}]},
			With[{cls=If[clsCases==={},{args}[[1]],clsCases[[1,2]]]},
				With[{supercls=Replace[Cases[{ops},supclsPat],{{}:>cls,{a_,b___}:>a[[2]]}]},
					Block[{$SuperclassConstructionPair={supercls,supercls}},function[cls,nargs]]
					]
				]
				]
			]
		]

];


BoundMethod::noobj="BoundMethod missing parameter \"PassObj\" in calling ``"
With[{
metPat=StringRulePattern["PassCls","PassObj","SuperPassCls","SuperPassObj","BoundTo"],
objPat=StringRulePattern@"PassObj",
clsPat=StringRulePattern["PassCls"],
supobjPat=StringRulePattern@"SuperPassObj",
supclsPat=StringRulePattern@"SuperPassCls"},

CallFunction[BoundMethod[function_,
	ops___?(MatchQ[#,metPat]&)],args___]:=
	With[{objCases=Cases[{ops},objPat]},
		If[objCases==={}&&{args}==={},
			Message[BoundMethod::noobj,function];
			$Failed,
			With[{nargs=Sequence@@If[objCases==={},{args}[[2;;]],{args}]},
			With[{obj=If[objCases==={},{args}[[1]],objCases[[1,2]]]},
				With[{cls=Replace[Cases[{ops},clsPat],{{}:>ObjectType@obj,{a_,b___}:>a[[2]]}]},
					With[{superobj=Replace[Cases[{ops},supobjPat],{{}:>obj,{(Rule|RuleDelayed)["SuperPassObj",a_],b___}:>(a)}],
							supercls=Replace[Cases[{ops},supclsPat],{{}:>cls,{a_,b___}:>a[[2]]}]},
					(*Print@{cls,obj};*)
						(*If[!MatchQ[superobj,_MathematicaClass],Print@{function,superobj}];*)
						Block[{$SuperclassConstructionPair={supercls,superobj}},function[obj,nargs]]
						]
					]
				]
				]
			]
		];

]


BoundProperty::noobj="BoundProperty missing parameter \"PassObj\""
With[{metPat=StringRulePattern["PassCls","PassObj","SuperPassCls","SuperPassObj","BoundTo"]},
CallFunction[p:BoundProperty[function_,ops__?(MatchQ[#,metPat]&)]]:=
	CallFunction@ReplacePart[p,0->BoundMethod]
];


CallFunction[StaticMethod[function_],args___]:=Replace[function[args],CodeChunk[f_]:>f];


(* ::Subsubsection::Closed:: *)
(*Function Object Operator Overloading*)


With[{funcs={StaticMethod,ClassMethod,BoundMethod,BoundProperty},
		overload={Options,DownValues,UpValues}},
Unprotect@@overload;
Do[With[{func=wrapper,alts=Alternatives@@overload},
	func/:HoldPattern[(f:func[___])[a___]]:=CallFunction[f,a];
	func/:HoldPattern[(call:alts)[func[f_,___]]]:=call@f;
	func/:HoldPattern[Function[func[f_,___]]]:=f;
	Do[With[{o=over},
		o/:HoldPattern[(s:(Set|SetDelayed))[o[func[f_,___]],v_]]:=s[o[f],v]
		]
		,{over,overload}];
	Do[With[{otherfunc=otherwrapper},
		func/:HoldPattern[otherfunc[func[f_,b___],o___]]:=otherfunc[f,o]
		],{otherwrapper,funcs}]
	],
	{wrapper,funcs}]
Protect@@overload;
];


(* ::Subsubsection::Closed:: *)
(*Formatting*)


Format[(m:BoundMethod)[f_,attrs___]]:=Interpretation[
Framed[f,
	Background->RGBColor[0.933,1.,0.997],
	FrameStyle->RGBColor[0,.4,1],
	RoundingRadius->5],m[f,attrs]];
Format[(m:ClassMethod)[f_,attrs___]]:=Interpretation[
Framed[f,
	Background->RGBColor[1.,0.933,0.95],
	FrameStyle->RGBColor[1.,0.,0.508],
	RoundingRadius->5],m[f,attrs]];
Format[(m:StaticMethod)[f_,attrs___]]:=Interpretation[
Framed[f,
	Background->RGBColor[0.995,1.,0.933],
	FrameStyle->RGBColor[0.721,0.733,0.],
	RoundingRadius->5],m[f,attrs]];
Format[(m:BoundProperty)[f_,attrs___]]:=Interpretation[
Framed[f,
	Background->RGBColor[0.933,1.,0.997],
	FrameStyle->RGBColor[0.015`,0.`,0.867`],
	RoundingRadius->5],m[f,attrs]];


(* ::Subsection::Closed:: *)
(*Class Constructor Definition*)


(* ::Subsubsection::Closed:: *)
(*keys*)


attributesKey=MathematicaClass$Attributes;
searchPathKey=MathematicaClass$SearchPath;
parentClassesKey=MathematicaClass$ParentClasses;
noattributeHead=MathematicaClass$NoAttribute;
getattributeKey=MathematicaClass$GetAttribute;


(* ::Subsubsection::Closed:: *)
(*getattr*)


findattr[self_,attr_]:=
	Replace[self[attributesKey][attr],{
	_Missing:>Replace[
			Do[Replace[src[attributesKey][attr],s:Except[_Missing]:>Return[{src,s}]],{src,self[searchPathKey]}],{
			Except[{_,_}]:>noattributeHead[self,attr],
			{src_,val_}:>val
			}]
	}];


With[{noattr=noattributeHead},
getattr[self_,attr_]:=
	Replace[Replace[findattr[self,attr]
			(*Catches a no attribute error from the instance call*),{
			_noattr:>Replace[findattr[self,(StringSplit[attr,"`"][[-1]])]
			(*If again the head is a no attribute error, it tries to get it from $GetAttr,
				if all fails, returns a "No Attribute" message*),
						_noattr:>Replace[If[attr=!="$GetAttr",Quiet[Check[getattr[self,"$GetAttr"],None],MathematicaClass::noattr],None],{
										None:>Message[MathematicaClass::noattr,self,attr],
										b_BoundMethod:>Replace[getboundmethod[MathematicaClass@self,b],{{True,v_}:>v,{False,v_}:>(setattr[self,"$GetAttr",v];v)}][attr],
										c_ClassMethod:>Replace[getclassmethod[Replace[ObjectType@MathematicaClass@self,MathematicaClass:>self],c],{{True,v_}:>v,{False,v_}:>(setattr[self,"$GetAttr",v];v)}][attr]
										}]
						]
				}],
		c_ClassMethod:>Replace[getclassmethod[Replace[ObjectType@MathematicaClass@self,MathematicaClass:>self],c],{{True,v_}:>v,{False,v_}:>(setattr[self,attr,v];v)}]
		]
];


(* ::Subsubsection::Closed:: *)
(*setattr*)


addFunction[ob_,plus_]:=
Which[
	NumberQ[ob],ob+plus,
	AssociationQ[ob],Switch[Head[plus],
						Rule,Append[ob,plus],
						_,Merge[{ob,plus},#[[-1]]&]
						],
	StringQ[ob],StringJoin[ob,plus],
	True,Quiet[With[{obb=ob+plus},If[Length@$MessageList>0,plus,obb]]]
	];


SetAttributes[setattr,HoldRest];	
setattr[symbol_,attr_,ClassField[val_]|val_,mode_:"normal"]:=
With[{self=Replace[symbol,{s_?ClassQ:>Symbol@s,_String:>Print@{symbol,attr}}]},
	With[{dict=self[attributesKey]},
		With[{value=Replace[Unevaluated[val],{
				f:Except[_StaticMethod|_BoundMethod|_ClassMethod|_BoundProperty]:>Replace[Unevaluated[f],_?(mode==="+"):>addFunction[dict[attr],f]]
				}]},
			self[attributesKey]=Append[dict,
					Switch[mode,
						"delayed",attr:>value,
						_,attr->value]
						]
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*delattr*)


delattr[self_,attr_]:=
	Replace[self[attributesKey][attr],{
		_Missing:>(Message[MathematicaClass::noattr,self,attr];$Failed),
		_:>(With[{sym=Replace[self,_?ClassQ:>Symbol[self]]},sym[attributesKey]=KeyDrop[sym[attributesKey],attr]];)
		}];


(* ::Subsubsection::Closed:: *)
(*clear*)


clear[self_]:=With[{s=ToString@self},Remove[s<>"`*"];UpValues@self={};DownValues@self={}];


(* ::Subsubsection::Closed:: *)
(*isinst*)


isinst[self_,cls_]:=(self["$GetAttribute","Class"]==cls);


(* ::Subsubsection::Closed:: *)
(*copyFunc*)


copyFunc[self_]:=Throw["Not Implemented"];(*If[IsInstance[self,MathematicaClass],
		With[{f=MathematicaClassConstructor[self::Type,
				methods->self[methodKey],
				classmethods->self[classmethodKey],
				staticmethods->self[staticmethodKey],
				propertyfunctions->self[propertyKey],
				parentclasses->self[parentClassesKey],
				noattrFunction->self::NoAttr]},
				Do[SetAttribute[f,attr,GetAttribute[self,attr]],{attr,Complement[self[attributesKey]//Keys,f[attributesKey]//Keys]}];
				f],
		With[{f=self::$New[Initialization->False],s=Symbol[self]},
			Do[SetAttribute[f,attr,GetAttribute[self,attr]],{attr,Complement[self[attributesKey]//Keys,f[attributesKey]//Keys]}];
			f
			]
		
		];*)


(* ::Subsubsection::Closed:: *)
(*getvars*)


getvars[self_]:= self[attributesKey];


(* ::Subsubsection::Closed:: *)
(*getboundmethod/getclassmethod*)


SetAttributes[subValueHold,HoldAllComplete];
subSymbolValues[sym1_,sym2_,value_]:=With[{d=value@sym1/.HoldPattern->subValueHold},
	(d/.(subValueHold[sym1[a___]]:>f_):>(subValueHold[sym2[a]]:>f))/.subValueHold->HoldPattern
	];


subDownValues[sym1_,sym2_]:=subSymbolValues[sym1,sym2,DownValues];


copyValues[sym1_,sym2_]:=
(Do[With[{d=valType},
	d[sym2]=subSymbolValues[sym1,sym2,d]],{valType,{DownValues,UpValues}}
	];
	Options[sym2]=Options[sym1]
);


getboundmethod[sym_Symbol|MathematicaClass[sym_Symbol],function:(_BoundMethod|_BoundProperty),passObj_:Automatic,passCls_:Automatic]:=
With[{obj=Replace[passObj,Automatic:>MathematicaClass@sym],
		cls=Replace[passCls,Automatic:>(MathematicaClass@ObjectType@MathematicaClass@sym)]},
		With[{fbase=FunctionReplaceAttribute[function,"PassObj"->obj,"PassCls"->cls],
				ffunc=Replace[Function@function,f:Except[_Symbol]:>Module[{method},method[a___]:=f[a];method]]},
			With[{symbol=Symbol[Context@ffunc<>ToString@sym<>"$boundMethod$"<>SymbolName@ffunc]},
			Replace[FunctionGetAttribute[function,"BoundTo"],
				{{}:>{False,(copyValues[ffunc,symbol];
					FunctionReplaceAttribute[ReplacePart[fbase,1->symbol],"BoundTo"->sym])},
				{"BoundTo"->s_,___}:>
					If[s=!=sym,
					{False,(copyValues[ffunc,symbol];
					FunctionReplaceAttribute[ReplacePart[fbase,1->symbol],"BoundTo"->sym])},
					{True,fbase}
					],
				o_:>{True,o}
				}]
				]
			]
		];


getclassmethod[sym_Symbol|MathematicaClass[sym_Symbol],function_ClassMethod,passCls_:Automatic]:=
	With[{cls=Replace[passCls,Automatic:>(MathematicaClass@sym)]},
		With[{fbase=FunctionReplaceAttribute[function,"PassCls"->cls],
				ffunc=Replace[Function@function,f:Except[_Symbol]:>Module[{method},method[a___]:=f[a];method]]},
			With[{symbol=Symbol[Context@ffunc<>ToString@sym<>"$classMethod$"<>SymbolName@ffunc]},
			Replace[FunctionGetAttribute[function,"BoundTo"],
				{{}:>(copyValues[ffunc,symbol];
					{False,FunctionReplaceAttribute[ReplacePart[fbase,1->symbol],"BoundTo"->sym]}),
				{"BoundTo"->s_,___}:>
					If[s=!=sym,
					(DownValues[symbol]=subDownValues[ffunc,symbol];
					{False,FunctionReplaceAttribute[ReplacePart[fbase,1->symbol],"BoundTo"->sym]}),
					{True,fbase}
					],
				o_:>{True,o}
				}]
				]
			]
		];


(* ::Subsubsection::Closed:: *)
(*instantiate*)


makeinstance[class_Symbol,
			initializationArgs___?(MatchQ[#,Except[_Rule|_RuleDelayed]]&),
			initializationOps___?(MatchQ[#,_Rule|_RuleDelayed]&),
			sym_,
			ext_,
			init_]:=
	With[{self=Replace[sym,Automatic:>Symbol[SymbolName@class<>StringTrim[ext,"`"]<>"$"<>ToString[$InstanceCounter++]]]},
		(*This builds an instance of the class*)
		With[{argconfig=
			Block[{initializationArgsList=initializationArgs,initializatioOpsList=initializationOps,initializationArgNum=-1,initializationOpsNum=0,initializationOpsPatEnabled=False},
			If[init=!=False,
				With[{initFunc=Replace[init,(BoundMethod|StaticMethod|ClassMethod)[f_,blah___]:>f]},
					If[MatchQ[initFunc, _Function],
						initializationArgNum=Length@Extract[initFunc,1,Hold]-1,
						With[{held=((Evaluate@initFunc//DownValues)[[1,1]]/.initFunc->HoldComplete)[[1]]},
	
							held/.{
								o_Optional:>With[{optionalResult=Replace[o[[2]],{_OptionsPattern:>(initializationOpsPatEnabled=True),_:>(initializationOpsNum+=1)}]},optionalResult/;True],
								p_Pattern:>With[{patternResult=Replace[p[[2]],{_OptionsPattern:>(initializationOpsPatEnabled=True),Except[_BlankNullSequence]:>(initializationArgNum+=1)}]},patternResult/;True]
								}];
					];
					If[initializationArgNum<0,Message[MathematicaClass::invinit,class]];
					If[Not@initializationOpsPatEnabled,initializationArgsList=Sequence[initializationArgsList,initializatioOpsList];initializatioOpsList=Sequence[]];
					With[{ia=initializationArgsList,io=initializatioOpsList},
					(*(Print@{Hold[ia],Hold[io]});*)
						If[Length@Hold[ia]<initializationArgNum||(Length@Hold[io]>1&&!initializationOpsPatEnabled),Message[MathematicaClass::newargx,class,initializationArgNum,initializationOpsNum,initializationOpsPatEnabled,Length@Hold[ia],Length@Hold[io]];Return[]];
						];
					]
				];
				{{initializationArgsList},{initializatioOpsList}}
			]},
	
	With[{initargs=Sequence@@argconfig[[1]],initops=Sequence@@argconfig[[2]],initFunction=BoundMethod[init,"PassObj"->MathematicaClass@self,"PassCls"->MathematicaClass@class]},
		AppendTo[$InstanceSymbols,self];
		(*With[{d=self},DownValues@d={};UpValues@d={}];*)
		(*self=class[subid];(*Try to move to context-based classing*)*)
		self[searchPathKey]=Prepend[class[searchPathKey],self];
		self[parentClassesKey]=ParentClasses@MathematicaClass@class;
		(*-------------------------------------------------------------------------------*)
		With[{bindMethod=Function[{m},BoundMethod[m,"PassObj"->MathematicaClass@self,"PassCls"->MathematicaClass@class]],
			dict=class[attributesKey]},
		self[attributesKey]=Merge[{dict,
				Association@@Reap[Do[Replace[dict[key],f:Except[_Missing]:>Sow[key->bindMethod@f]],
						{key,{"$GetAttribute","$SetAttribute","$DeleteAttribute",
								"$New","$Del","$Copy","$GetAttr","$PostLoad"}}]][[2,1]],
				<|"$Init"->initFunction,
					"MathematicaObjectClass"->class|>},Last]
		];
		(*-------------------------------------------------------------------------------*)
		self["$GetAttribute",attr_]:=With[{getattrib=Replace[getboundmethod[self,getattr[self,"$GetAttribute"]],{{True,v_}:>v,{False,v_}:>(setattr[self,"$GetAttribute",v];v)}]},
				Replace[getattrib[attr],{
					b_BoundMethod:>Replace[getboundmethod[self,b],{{True,v_}:>v,{False,v_}:>(setattr[self,attr,v];v)}],
					p_BoundProperty:>(CallFunction@Replace[getboundmethod[self,p],{{True,v_}:>v,{False,v_}:>(setattr[self,attr,v];v)}])
					}]
				];
		self["$SetAttribute",attr_,value_,mode_]:=			
			With[{s=StringSplit[ToString[Unevaluated[attr]],{"`"}][[-1]]},
				self["$GetAttribute","$SetAttribute"][s,value,mode]
				];
		self["$DeleteAttribute",attr_]:=
			With[{s=StringSplit[ToString[Unevaluated[attr]],{"`"}][[-1]]},
				self["$GetAttribute","$DeleteAttribute"][s]
				];
		SetAttributes[self,HoldRest];
		
		(*-------------------------------------------------------------------------------*)
		If[init===False||With[{initFunc=Function@initFunction},MatchQ[initFunction[initargs,initops],Except[_initFunc]]],
			SetOverload[self];
			MathematicaClass[self],
			Message[MathematicaClass::argerr,class];
			$Failed
			]
		]
		]
	];


instantiate[class_Symbol|MathematicaClass[class_Symbol],initializationArgs___?(MatchQ[#,Except[_Rule|_RuleDelayed]]&),
			initializationOps___?((MatchQ[#,_Rule|_RuleDelayed]
										&&
							MatchQ[#,Except[(InstanceSymbolName->_)|(InitializationFunction->_)|(InstanceNameExtension->_)]])&),
			instanceOptions___?(MatchQ[#,((InstanceSymbolName->_)|(InitializationFunction->_)|(InstanceNameExtension->_))]&)
			]:=
		With[{instOps={instanceOptions}},
			With[{initFunction=Replace[Replace[InitializationFunction/.instOps,InitializationFunction->True],True->Replace[Quiet[getattr[class,"$Init"],MathematicaClass::noattr],Null:>Function[{s},Null]]],
				symbolName=Replace[InstanceSymbolName/.instOps,InstanceSymbolName->Automatic],
				extensionString=Replace[InstanceNameExtension/.instOps,InstanceNameExtension->"Instance"]},
				makeinstance[class,initializationArgs,initializationOps,symbolName,extensionString,initFunction]
			]
		];


(* ::Subsubsection::Closed:: *)
(*MathematicaClassConstructor*)


Clear[MathematicaClassConstructor]
Options[MathematicaClassConstructor]={
	ParentClasses->{},
	ClassFields-><||>,
	BoundMethods-><||>,
	StaticMethods-><||>,
	ClassMethods-><||>,
	BoundProperties-><||>
};
MathematicaClassConstructor[classname:_String|_Symbol,ops:OptionsPattern[]]:=
With[{class=Replace[classname,_String:>Symbol[classname<>"$"<>ToString@$ClassCounter]],
		AttributeTable=attributesKey,
		SearchPath=searchPathKey,
		InheritFrom=parentClassesKey,
		NoAttribute=noattributeHead,
		GetAttribute=getattributeKey
		},
	ClearAll@class;
	(*Gives reasonably unique keys to the values Fields,BoundMethods,ClassMethods,StaticMethods,
	and PropertyFunctions, so that these will be hard to accidentally overwrite, but still possible to 
	query*)
	(*---------------------------------------------------------------------------------*)
	(*Begin["MathematicaClass`"];*)
	With[{d=class},DownValues@d={};UpValues@d={}];
	(*---------------------------------------------------------------------------------*)
	(*Sets initial attributes. Wraps all methods in appropriate functions.*)
	class[AttributeTable]=Merge[{
		OptionValue[ClassFields],
		Association@@Table[
				Replace[r,{
					(name_->b_BoundMethod):>(name->FunctionReplaceAttribute[b,"PassCls"->MathematicaClass@class]),
					(name_->b_):>(name->BoundMethod[b,"PassCls"->MathematicaClass@class])
					}],
				{r,OptionValue@BoundMethods//Normal}],
		Association@@Table[
			Replace[r,{
				(name_->c_ClassMethod):>(name->FunctionReplaceAttribute[c,"PassCls"->MathematicaClass@class]),
				(name_->c_):>(name->ClassMethod[c,"PassCls"->MathematicaClass@class])
				}],
				{r,OptionValue@ClassMethods//Normal}],
		Association@@Table[
				Replace[r,{
					(name_->s:Except[_StaticMethod]):>(name->StaticMethod[s])
					}],
				{r,OptionValue@StaticMethods//Normal}],
		Association@@Table[
				Replace[r,{
					(name_->p_BoundProperty):>(name->FunctionReplaceAttribute[p,"PassCls"->MathematicaClass@class]),
					(name_->p_):>(name->BoundProperty[p,"PassCls"->MathematicaClass@class])
					}],
				{r,OptionValue@BoundProperties//Normal}],
			<|
			"Type"->classname,
			"MathematicaObjectClass"->MathematicaClass,
			"$GetAttribute"->ClassMethod[getattr,"PassCls"->class],
			"$SetAttribute"->ClassMethod[setattr,"PassCls"->class],
			"$DeleteAttribute"->ClassMethod[delattr,"PassCls"->class],
			"$Del"->ClassMethod[clear,"PassCls"->class],
			"$Copy"->ClassMethod[copyFunc,"PassCls"->class],
			"$Dict"->BoundProperty[getvars,"PassCls"->class],
			"$New"->ClassMethod[instantiate,"PassCls"->class]
			|>},First];
	class[InheritFrom]=With[{parent=OptionValue[ParentClasses]},Flatten@{parent,(ParentClasses/@parent)}];
	class[SearchPath]=Prepend[class[InheritFrom],class];
	(*-------------------------------------------------------------------------------*)
	class["$GetAttribute",attr_]:=
		With[{s=StringSplit[ToString[Unevaluated[attr]],{"`"}][[-1]]},
				getattr[class,"$GetAttribute"][s]
			];
	class["$SetAttribute",attr_,value_,mode_:"normal"]:=
		With[{s=StringSplit[ToString[Unevaluated[attr]],{"`"}][[-1]]},
			class["$GetAttribute","$SetAttribute"][s,value,mode]
			];
	class["$DeleteAttribute",attr_]:=
		With[{s=StringSplit[ToString[Unevaluated[attr]],{"`"}][[-1]]},
			class["$GetAttribute","$DeleteAttribute"][s]
			];
	(*-------------------------------------------------------------------------------*)
	SetAttributes[class,HoldRest];
	$ClassCounter++;
	AppendTo[$ClassSymbols,class];
	MathematicaClass[class]
	];


(* ::Subsection:: *)
(*Simplified Class Constructor*)


$eConstructingClass=False;


(* ::Subsubsection::Closed:: *)
(*Begin/End Class*)


BeginClass/:HoldPattern[Set[n_,BeginClass[]]]:=(BeginClass[n,n]);
BeginClass/:HoldPattern[SetDelayed[n_,BeginClass[]]]:=(BeginClass[n,n]);
BeginClass/:HoldPattern[Set[n_,BeginClass[s:_String|_Symbol]]]:=(BeginClass[s,n]);
BeginClass/:HoldPattern[SetDelayed[n_,BeginClass[s:_String|_Symbol]]]:=(BeginClass[s,n]);
BeginClass/:HoldPattern[Set[n_,BeginClass[Symbol->s_]]]:=(BeginClass[Symbol->s,n]);
BeginClass/:HoldPattern[SetDelayed[n_,BeginClass[Symbol->s_]]]:=(BeginClass[Symbol->s,n]);

BeginClass[name:_String|_Symbol,sym:_Symbol|None]:=
With[{n=ToString[Unevaluated[name]]},
	$eConstructingClass=ToExpression[n<>"$"<>ToString@$ClassCounter,StandardForm,MathematicaClass];
	AppendTo[$ClassBuffer,{"__Class__",n,Unevaluated[sym]}];
	];
BeginClass[Symbol->name_,sym:_Symbol|None]:=
With[{n=name},
	$eConstructingClass=name;
	AppendTo[$ClassBuffer,{"__Class__",n,Unevaluated[sym]}];
	];
SetAttributes[BeginClass,HoldAll];


EndClass[variable_:None]:=
With[{p1=Position[$ClassBuffer,{"__Class__",_String|_Symbol,___}]},
	If[p1=!={},
		With[{p=p1[[-1,1]]},
			With[{C=$ClassBuffer[[p;;]]},
				With[{c=ConstructClass@@Prepend[Drop[C,1],C[[1,2]]],
					var=(If[Length@C[[1]]>2,
						If[Extract[C,{1,3},Unevaluated]===None,variable,Extract[C,{1,3}]],
						variable])},
					$ClassBuffer=Drop[$ClassBuffer,p;;];
					$eConstructingClass=False;
					If[var=!=None,var=c,c]
					]
				]
			]
		];
	];


(* ::Subsubsection::Closed:: *)
(*Useful Functions*)


NoRuleQ[x_]:=MatchQ[Unevaluated[x],Except[_Rule|_RuleDelayed]];
SetAttributes[NoRuleQ,HoldFirst];
NoPatternQ[x_]:=MatchQ[Unevaluated[x],Except[_Pattern|_Optional]];
SetAttributes[NoPatternQ,HoldFirst];


(* ::Subsubsection::Closed:: *)
(*Method Prepping*)


CollectArguments[passed___]:=
	With[{args=Thread[Hold[{passed}]]},
		With[{arglist=Cases[args,r:Hold[_?NoRuleQ]:>r]},
			With[{opsANDattrs=Complement[args,arglist]},
				With[{ops=Replace[opsANDattrs,Hold[((MethodAttributes->_)|MethodAttributes:>_)]:>Sequence[],1]},
					With[{attrs=Replace[Complement[opsANDattrs,ops],{{Hold[((MethodAttributes->l_)|MethodAttributes:>l_)]}:>l,_->{}}]},
						(Thread[#,Hold]&/@{arglist,ops,attrs})//ReleaseHold
						]
					]
				]
			]
		];
SetAttributes[CollectArguments,HoldAll]


CodeChunk~SetAttributes~HoldAllComplete;
CodeChunk[func_]/;$evalFunc:=(func);


With[{context=$Context},
MethodPrepper[name_,arglist___,opsFlag:OptionsPattern:None,funcCode_,m:(MethodAttributes->_List):(MethodAttributes->{})]:=
	With[{method=Symbol[context<>StringReplace[ToString@Unevaluated@name<>"$","_"->"\[LetterSpace]"]<>ToString@$ModuleNumber++],
		functionthing=CodeChunk[funcCode],
		argTriple=CollectArguments[arglist]},
		
		With[{args=argTriple[[1]],options=argTriple[[2]],attrs=Replace[argTriple[[3]],{}:>m[[2]]],opsF=(opsFlag=!=None)},
		
			SetAttributes[method,attrs];
			If[opsF||Length@options>0,
				(*If[opsF,Append[options,{}],_->Default];*)
				Options[method]=options;
				(HoldComplete[
					method[Sequence@@args,KeyWordArgumentsPattern:OptionsPattern[]]:=Block[
						{$KeyWordArguments=Association@@Join[Options[method],{KeyWordArgumentsPattern}],
							$DefaultArguments=Association@@Options[method],
							$PassedArguments=Association[KeyWordArgumentsPattern],
							$evalFunc=True},
						fSlot]]/.{
					fSlot/;opsF:>Quiet[functionthing,OptionValue::nodef],
					fSlot:>functionthing})//ReleaseHold,					
				method[Sequence@@args]:=Block[{$evalFunc=True},functionthing]
				]
			];
		method
		];
];
SetAttributes[MethodPrepper,HoldAll]


(* ::Subsubsection::Closed:: *)
(*Attributes*)


ClassField[name_,value_]:=
	With[{m={ClassField,With[{n=Unevaluated[name]},If[Head@n===Symbol,SymbolName@n,n]],value}},
		If[$eConstructingClass=!=False,AppendTo[$ClassBuffer,m]];
		m];
	SetAttributes[ClassField,HoldAll];


With[{metPat=StringRulePattern["PassCls","PassObj","SuperPassObj","SuperPassCls","BoundTo"]},
BoundMethod[name_,arglist__,functionthing:Except[metPat]]:=
	With[{m=With[{n=If[Head@Unevaluated[name]===Symbol,SymbolName@Unevaluated[name],Unevaluated[name]]},
			{BoundMethod,n,MethodPrepper[n,arglist,Unevaluated@functionthing]}
			]},
		If[$eConstructingClass=!=False,AppendTo[$ClassBuffer,m]];
		m];
	SetAttributes[BoundMethod,{HoldAll,Listable}];
];


With[{metPat=StringRulePattern["PassCls","SuperPassCls","BoundTo"]},
ClassMethod[name_,arglist__,functionthing:Except[metPat]]:=
	With[{m=With[{n=If[Head@Unevaluated[name]===Symbol,SymbolName@Unevaluated[name],Unevaluated[name]]},
			{ClassMethod,n,MethodPrepper[n,arglist,Unevaluated@functionthing]}
			]},
		If[$eConstructingClass=!=False,AppendTo[$ClassBuffer,m]];
		m];
	SetAttributes[ClassMethod,{HoldAll,Listable}];
];


StaticMethod[name_,arglist___,functionthing_]:=
	With[{m=With[{n=If[Head@Unevaluated[name]===Symbol,SymbolName@Unevaluated[name],Unevaluated[name]]},
			{StaticMethod,n,MethodPrepper[n,arglist,Unevaluated@functionthing]}
			]},
		If[$eConstructingClass=!=False,AppendTo[$ClassBuffer,m]];
		m];
	SetAttributes[StaticMethod,{HoldAll,Listable}];


With[{metPat=StringRulePattern["PassCls","PassObj","SuperPassObj","SuperPassCls","BoundTo"]},
BoundProperty[name_,arglist__,functionthing:Except[metPat]]:=
	With[{m=With[{n=If[Head@Unevaluated[name]===Symbol,SymbolName@Unevaluated[name],Unevaluated[name]]},
			{BoundProperty,n,MethodPrepper[n,arglist,Unevaluated@functionthing]}
			]},
		If[$eConstructingClass=!=False,AppendTo[$ClassBuffer,m]];
		m];
	SetAttributes[BoundProperty,{HoldAll,Listable}];
];


(* ::Subsubsection::Closed:: *)
(*Special Attributes*)


ParentClasses[values___]:=
	With[{m={"Extends",Flatten@{values}}},
		If[$eConstructingClass=!=False,AppendTo[$ClassBuffer,m]];
		m];
InitializationFunction[arglist__,functionthing_]:=
	BoundMethod["$Init",arglist,functionthing]
	SetAttributes[InitializationFunction,{HoldAll,Listable}];
RepresentationFunction[arglist__,functionthing_]:=
	BoundMethod["$ToRepresentation",arglist,functionthing];
	SetAttributes[RepresentationFunction,{HoldAll,Listable}];
CallInstanceFunction[arglist__,functionthing_]:=
	BoundMethod["$Call",arglist,functionthing];
	SetAttributes[CallInstanceFunction,{HoldAll,Listable}];
StringFunction[arglist__,functionthing_]:=
	BoundMethod["$ToString",arglist,functionthing];
	SetAttributes[StringFunction,{HoldAll,Listable}];
NoAttributeFunction[arglist__,functionthing_]:=
	ClassMethod["$GetAttr",arglist,functionthing];
	SetAttributes[NoAttributeFunction,{HoldAll,Listable}];
PostLoadFunction[arglist__,functionthing_]:=
	ClassMethod["$PostLoad",arglist,functionthing];
	SetAttributes[PostLoadFunction,{HoldAll,Listable}];


(* ::Subsubsection::Closed:: *)
(*UpValues*)


Do[With[{c=ToExpression@p},
	c/:HoldPattern[Set[c[args__],v_]]:=c[args,Unevaluated@v];
	c/:HoldPattern[SetDelayed[c[args__],v_]]:=c[args,Unevaluated@v]
	(*c/:HoldPattern[ConstructClass[a___,c[args__],b___]]:=(
		ConstructClass[a,
		(c[args,Unevaluated@v];With[{r=$ClassBuffer[[-1]]},$ClassBuffer=Drop[$ClassBuffer,-1]]),
		b]);*)
	],{p,{ClassField,BoundMethod,ClassMethod,StaticMethod,BoundProperty,
		InitializationFunction,NoAttributeFunction,
		RepresentationFunction,CallInstanceFunction,StringFunction,
		PostLoadFunction}}]
ParentClasses/:HoldPattern[Set[ParentClasses,v_]]:=ParentClasses@v;
ParentClasses/:HoldPattern[SetDelayed[ParentClasses,v_]]:=ParentClasses@v;


(* ::Subsubsection::Closed:: *)
(*Construct Class*)


ConstructClass[name_,properties___]:=Block[{
	$eConstructingClass=Replace[name,
		_String:>ToExpression[name<>"$"<>ToString@$ClassCounter,StandardForm,MathematicaClass]
		],
	$ClassBuffer={},initializationFunction=Automatic,parentClassList={},
	objectFields=<||>,objectBoundMethods=<||>,objectStaticMethods=<||>,
	objectClassMethods=<||>,objectBoundProperties=<||>,
	classProperties={properties},builtClass},
	
	Do[With[{head=prop[[1]],val=prop[[2]]},
		Switch[head,
			"Extends",      parentClassList                    =val,
			ClassField,       objectFields         [ToString@val]=prop[[3]],
			BoundMethod, objectBoundMethods   [ToString@val]=prop[[3]],
			ClassMethod, objectClassMethods   [ToString@val]=prop[[3]],
			StaticMethod,objectStaticMethods  [ToString@val]=prop[[3]],
			BoundProperty,     objectBoundProperties[ToString@val]=prop[[3]]
			];
		],
		{prop,classProperties}];
	builtClass=MathematicaClassConstructor[name,
		ParentClasses->parentClassList,
		ClassFields->objectFields,
		BoundMethods->objectBoundMethods,
		StaticMethods->objectStaticMethods,
		ClassMethods->objectClassMethods,
		BoundProperties->objectBoundProperties];
	
	SetOverload[builtClass[[1]]];
	builtClass
];
SetAttributes[ConstructClass,HoldAll]


(* ::Subsection:: *)
(*Usage Aids*)


(* ::Subsubsection::Closed:: *)
(*ClassCount, InstanceCount, ClearClasses, ClearInstances*)


(*
This section is convenience functions for working with these classes. 
Any convenience function should deal with both the case that an object is a MathematicaClass or that it's a 
real Mathematica primitive
*)
ClassCount:=Length@$ClassSymbols;
InstanceCount:=Length@$InstanceSymbols;
ClearClasses:=Do[GetAttribute[x,"__Del"][x],{x,$ClassSymbols}];
ClearInstances:=Do[GetAttribute[x,"__Del"],{x,$InstanceSymbols}];


(* ::Subsubsection::Closed:: *)
(*ClassQ, InstanceQ*)


ClassQ[obj_]:=Replace[Length@obj>0,True:>(Head@obj===MathematicaClass&&MemberQ[$ClassSymbols~Join~$InstanceSymbols,obj[[1]]])];
InstanceQ[obj_]:=ClassQ[obj]&&(ObjectType[obj]=!=MathematicaClass);


(* ::Subsubsection::Closed:: *)
(*ParentClasses, ObjectType*)


ParentClasses[m:MathematicaClass[_Symbol]]:=m[parentClassesKey];
(*With[{o=ObjectType[m]},
	If[o===MathematicaClass,
		m[parentClassesKey],
		o[parentClassesKey]
		]
	];*)
ObjectType[obj_]:=With[{c=ClassQ[obj],h=Head[obj]},
		If[c,
			getattr[obj,"MathematicaObjectClass"],
			h
		]
	];


(* ::Subsubsection::Closed:: *)
(*IsCallable, IsInstance*)


IsCallable[obj_]:=Switch[ObjectType@obj,
	Function,True,
	Symbol,Length@DownValues[obj]>0,
	_,False];
IsInstance[obj_,classes__]:=
With[{classSet={classes}/.MathematicaClass[s_]:>s},
	Replace[TrueQ@MemberQ[classSet,ObjectType[obj]],
		False:>With[{tests=If[ClassQ@obj,ParentClasses@obj/.MathematicaClass[s_]:>s,{}]},
			AnyTrue[tests,MemberQ[classSet,#]&]
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*ObjectString, ObjectRepresentation*)


ObjectString[obj_]:=If[ClassQ@obj,
						If[HasAttribute[obj,"$ToString"],obj::$ToString[],ToString@obj],
						ToString@obj];
ObjectRepresentation[obj_]:=If[ClassQ@obj&&MemberQ[$ClassSymbols~Join~$InstanceSymbols,obj//Symbol],
							If[HasAttribute[obj,"$ToRepresentation"]&&!IsInstance[obj,SuperclassObject],
								If[InstanceQ[obj],obj::$ToRepresentation[],None],
								None],
							obj
							];


(* ::Subsubsection::Closed:: *)
(*HasAttribute, GetAttribute, SetAttribute, DeleteAttribute*)


HasAttribute[obj_,attr_]:=
	If[ClassQ@obj,
		Replace[Do[If[KeyMemberQ[c[attributesKey],attr],Return[True]],{c,Prepend[ParentClasses[obj],obj]}],
			Except[True]:>False
			],
		Block[{hasAttribute=False},
		Do[
			If[arg===HoldPattern[obj[attr]],
				Return[hasAttribute=True]],
			{arg,DownValues[obj][[All,1]]}
			];
		hasAttribute]
	];
SetAttributes[HasAttribute,HoldAll];


GetAttribute[obj_,attr_]:=If[ClassQ@obj,
	obj["$GetAttribute",attr],
	obj[attr]
	];
SetAttributes[GetAttribute,HoldAll];


SetAttribute[obj_,attr_,value_,mode_:"static"]:=
	(If[ClassQ@obj,
		obj["$SetAttribute",attr,value,mode],
		obj[attr]=value;
		];value);
SetAttributes[SetAttribute,HoldAll];


DeleteAttribute[obj_,attr_]:=If[ClassQ@obj,
	obj["$DeleteAttribute",attr],
	obj[attr]=.
	]
SetAttributes[DeleteAttribute,HoldAll];


(* ::Subsubsection::Closed:: *)
(*GetItem, SetItem*)


GetItem[obj_,indices__]:=If[ClassQ@obj,
		If[HasAttribute[obj,"__GetItem"],obj::GetItem[indices],obj[indices]],
		With[{o=obj[indices]},If[Head@o===obj,obj[[indices]],o]]
		];

SetItem[obj_,indices__,value_]:=(If[ClassQ@obj,
		If[HasAttribute[obj,"__SetItem"],obj::SetItem[indices,value],obj[indices]=value],
		Quiet[obj[[indices]]=value;If[Length@$MessageList>0,obj[indices]=value]]
		];value);


(* ::Subsubsection::Closed:: *)
(*GetKey, ObjectValues*)


keyDict=<|
	"Fields"->fieldKey,
	"Bound Methods"->methodKey,
	"Static Methods"->staticmethodKey,
	"Class Methods"->classmethodKey,
	"Properties"->propertyKey|>;
GetKey[key:_String:None]:=Replace[key,{None:>keyDict,_:>keyDict[key]}];
(*MathematicaClass[c_?ClassQ]:=If[c::MathematicaObjectClass===MathematicaClass,c,c::MathematicaObjectClass];*)
ObjectValues[obj_,key_]:=With[{K=GetKey[obj,key]},obj[K]]


(* ::Subsubsection::Closed:: *)
(*Superclass Definition*)


(* ::Item:: *)
(*Superclass Object*)


SuperclassObject:=BeginClass[];
InitializationFunction[self_,class_,passobj_]:=
With[{parentclasses=ParentClasses[MathematicaClass[class]]},
	self::obj=passobj;
	self::cls=class;
	self::parents=parentclasses;
	self::$GetAttribute=self::FindAttribute
	(*delattr[self,"$SetAttribute"];*)
	];
BoundMethod[FindAttribute,self_,attr_]:=
With[{attrib=Do[If[HasAttribute[c,attr],Return[c]],{c,getattr[self,"parents"]}]},
	If[attrib===Null,
		Message[MathematicaClass::noattr,self,attr];$Failed,
		Replace[GetAttribute[attrib,attr],{
			b:_BoundMethod:>With[{supCls=Sequence@@Replace[Cases[b,(Rule|RuleDelayed)["PassCls",_]],{a_,___}:>{(ReplacePart[a,1->"SuperPassCls"])}]},
							FunctionHold@FunctionReplaceAttribute[b,"PassObj"->Evaluate[getattr[self,"obj"]],"PassCls"->Evaluate[getattr[self,"cls"]],supCls]
							],
			c:_ClassMethod:>With[{supCls=Sequence@@Replace[Cases[c,(Rule|RuleDelayed)["SuperPassCls",_]],{a_,___}:>{ReplacePart[a,1->"SuperPassCls"]}]},
							FunctionHold@FunctionReplaceAttribute[c,"PassCls"->getattr[self,"cls"],supCls]
							]
			}]
		]
	];
EndClass[];


(* ::Item:: *)
(*Superclass Constructor*)


Superclass::noarg="No argument passed to Superclass"
$SuperclassConstructionPair=None;
Superclass[]:=If[Length@$SuperclassConstructionPair==0,Message[Superclass::noarg],Superclass@@$SuperclassConstructionPair];
Superclass[obj:_Symbol|_MathematicaClass,passobj_:Automatic]:=(
SuperclassObject[
	Replace[MathematicaClass[obj]//ObjectType,{
		MathematicaClass:>(MathematicaClass[obj])[[1]],
		e_:>MathematicaClass[e]
		}],
	Replace[passobj,{Automatic:>MathematicaClass@obj,inst_?(IsInstance[#,SuperclassObject]&):>(inst::MathematicaObjectBoundmethodPassObject)}],
	InstanceNameExtension->SymbolName@obj
	]
);
Superclass/:HoldPattern[MessageName[s:Superclass[___],messages__]]:=MessageName[Evaluate[s],messages];



(* ::Subsubsection::Closed:: *)
(*SaveObject, LoadObject*)


(* ::Text:: *)
(*NEED TO INCLUDE OPTIONS SAVING*)
(*ALLOW FOR SAVING WITH OOP FILES*)


saveMethod[name:_String|Automatic:Automatic,function_]:=
With[{f=function[[1]],h=Head@function},
	If[MatchQ[Unevaluated[f],_Symbol],
	With[{dv=f//DownValues//First,n=Replace[name,Automatic:>SymbolName@f],ops=Sequence@@Options@f},
		
		HoldForm[dv]/.{HoldPattern:>HoldComplete,f:>h}/.{
			HoldPattern[HoldComplete[h[args___]]:>(CodeChunk[fu_]|Block[{_},CodeChunk[fu_]])]:>(h[n,args,ops]:=fu),
			HoldPattern[HoldComplete[h[args___,opsPat_]]:>Block[{__},CodeChunk[fu_]]]:>(h[n,args,ops]:=fu),
			HoldPattern[HoldComplete[h[args___,opsPat_]]:>Block[{__},Quiet[CodeChunk[fu_],_]]]:>(h[n,args,ops,OptionsPattern]:=fu),
			HoldPattern[HoldComplete[h[args___]]:>fu_]:>(h[n,args,ops]:=fu)
			}
		],
		HoldForm[h[name,vars___]:=f[vars]]
		]
	];


saveField[name_,obj_]:=Sequence@@Replace[
	{HoldForm[ObjectOrientedProgramming`ClassField[name]:=obj]},
		l_?(MatchQ[obj,_Symbol]):>Prepend[l,With[{f=FullDefinition@obj},HoldForm[f]]]
	];


setAttributes[saveHold,HoldAllComplete];
With[{names=Names@"ObjectOrientedProgramming`*"},
saveAttribute[(Rule|RuleDelayed)[name_,ob_]]:=Replace[
	Head@ob,{
		(BoundMethod|ClassMethod|StaticMethod|BoundProperty):>saveMethod[name,ob],
		_:>saveField[name,ob]
	}]/.s_Symbol/;!(MatchQ[Unevaluated[s],HoldPattern[Symbol[_Symbol]]]||StringQ@MakeBoxes@s):>
		With[{e=StringTake[ToString@Unevaluated[s],{9,-2}]},
			With[{r=If[MemberQ[names,e],ToExpression[("ObjectOrientedProgramming`"<>e),StandardForm,saveHold],s]},
				r/;True]
			]/.saveHold[s_]:>s
	];


pathComplement[file1_,file2_]:=
	With[{p1=FileNameSplit@file1,p2=FileNameSplit@file2},
		With[{root=SequenceAlignment[p1,p2][[1]]},
			{root,p1[[Length@root+1;;]],p2[[Length@root+1;;]]}
		]
	]


relativePath[file1_,file2_]:=With[{pc=pathComplement[file1,file2]},
	FileNameJoin@Join[ConstantArray["..",Length@pc[[2]] - If[FileExtension@pc[[2,-1]]==="",0,1] ],pc[[3]]]
	]


$SaveStack=<||>;
SetAttributes[loadHold,HoldAll];
attrString[attr_,dir_]:=
			attr/.(
				m:MathematicaClass[sym_]:>With[{eval=
				Replace[Replace[$SaveStack[sym],_Missing:>($SaveStack[sym]=SaveObject[m,Directory->dir,CreateArchive->False])],{
					s_String:>loadHold[LoadObject[s]],
					_SaveObject:>m}]},eval/;True]
				)/.(
					loadHold[s_]:>s
				)/.(
					HoldForm[s_]:>HoldForm[InputForm[s]]
				)//ToString;


Options[SaveObject]={
		Path->"Relative",
		Directory->Automatic,
		CreateArchive->True,
		Contexts->{}
		};


(* ::Text:: *)
(*An implementation for saving classes*)


(SaveObject[MathematicaClass[sym_Symbol],fileName:_String|Automatic:Automatic,ops:OptionsPattern[]]/;MemberQ[$ClassSymbols,sym]):=
Block[{$SaveStack=Replace[$SaveStack,_Symbol:><||>]},
With[{dir=Replace[OptionValue@Directory,Except[_String]:>$ObjectDirectory]//If[OptionValue@CreateArchive//TrueQ,FileNameJoin@{#,ToString@sym},#]&},
	If[!DirectoryQ@dir,CreateDirectory@dir];
With[{f=Replace[fileName,Automatic:>FileNameJoin@{dir,ToString@sym<>".m"}]},
	$SaveStack[sym]=f;
	With[{
		fullPaths=OptionValue@Path===Full,
		attrs=attrString[#,dir]&/@saveAttribute/@Normal@sym[attributesKey],
		parents=MathematicaClass/@sym[parentClassesKey]
		},
		With[{file=OpenWrite[f],
			parentFiles=SaveObject[#,Directory->dir,CreateArchive->False]&/@parents,
			contexts=Prepend[Table[Replace[c,{con_String,fil_String}:>{con,If[fullPaths,fil,relativePath[f,fil]]}],{c,OptionValue@Contexts}],
						{"ObjectOrientedProgramming`",If[fullPaths,$ContextFile,relativePath[f,$ContextFile]]}]},
			WriteLine[file,#]&/@(
			{
			"SetDirectory@DirectoryName@$InputFileName;"
			}
			~Join~
			(TemplateApply["Needs[\"``\",\"``\"];",#]&/@contexts)
			~Join~
			(StringTemplate["LoadObject[\"``\"];"][If[fullPaths,#,relativePath[f,#]]]&/@parentFiles)
			~Join~
				{
				StringTemplate["`` := BeginClass[Symbol \[Rule] ``];"][Replace[MathematicaClass[sym]::Type//ToString,s:ToString@sym:>(s<>"$")],sym],
				StringTemplate["ParentClasses = ``"][parents]
				}
			~Join~
			attrs
			~Join~
			{"EndClass[];",
			"ResetDirectory[];",
			StringTemplate["``"]@MathematicaClass[sym]::Type}
			);
			Close[file];
			];
		If[OptionValue@CreateArchive//TrueQ,
			Quiet@DeleteFile@FileNameJoin@{ParentDirectory@dir,FileBaseName@dir<>".zip"};
			With[{arc=CreateArchive[dir,ParentDirectory@dir]},
				DeleteDirectory[dir,DeleteContents->True];arc],
			f]
		]
		]
	]
	];


(* ::Text:: *)
(*An implementation for saving instances*)


(SaveObject[MathematicaClass[sym_Symbol],fileName:_String|Automatic:Automatic,ops:OptionsPattern[]]/;MemberQ[$InstanceSymbols,sym]):=
Block[{$SaveStack=Replace[$SaveStack,_Symbol:><||>]},
With[{dir=Replace[OptionValue@Directory,Except[_String]:>$ObjectDirectory]//If[OptionValue@CreateArchive//TrueQ,FileNameJoin@{#,ToString@sym},#]&,
		fullPaths=OptionValue@Path===Full},
	If[!DirectoryQ@dir,CreateDirectory@dir];
	With[{f=Replace[fileName,Automatic:>FileNameJoin@{dir,ToString@sym<>".m"}]},
		$SaveStack[sym]=f;
	With[{
		cls=MathematicaClass[ObjectType@MathematicaClass[sym]],
		attrs=StringTemplate["setAttr@(``);"][attrString[#,dir]]&/@saveAttribute/@Normal@sym[attributesKey]},
	With[{file=OpenWrite[f],
		clsFile=SaveObject[cls,Directory->dir,CreateArchive->False],
		contexts=Prepend[Table[Replace[c,{con_String,fil_String}:>{con,If[fullPaths,fil,relativePath[f,fil]]}],{c,OptionValue@Contexts}],
					{"ObjectOrientedProgramming`",If[fullPaths,$ContextFile,relativePath[f,$ContextFile]]}]
		},
		WriteLine[file,#]&/@(
			{
			"SetDirectory@DirectoryName@$InputFileName;"
			}
			~Join~
			(TemplateApply["Needs[\"``\",\"``\"];",#]&/@contexts)
			~Join~
			{
			StringTemplate["LoadObject@\"``\";"][If[fullPaths,clsFile,relativePath[f,clsFile]]],
			StringTemplate["``[InitializationFunction -> False,InstanceSymbolName -> ``]"][cls,sym],
			StringTemplate["With[{setAttr=Replace[#,{type_,name_,ob_}:>SetAttribute[ MathematicaClass[``],name,type@ob] ]&},"][sym]
			}
			~Join~
			attrs
			~Join~{
			"];",
			"ResetDirectory[];",
			StringTemplate["MathematicaClass[``]"][sym]});
		Close[file];
		];
		If[OptionValue@CreateArchive//TrueQ,
			Quiet@DeleteFile@FileNameJoin@{ParentDirectory@dir,FileBaseName@dir<>".zip"};
			With[{arc=CreateArchive[dir,ParentDirectory@dir]},
				DeleteDirectory[dir,DeleteContents->True];arc],
			f]
		]
		]
		]
	]


MathematicaClass::noof="Couldn't find object file ``"


LoadObject/:HoldPattern[MessageName[l:LoadObject[__],a__]]:=With[{ob=l},MessageName[ob,a]];
(LoadObject[file_String,reload_:False]/;(FileExtension[file]=="zip")):=
With[{dir=FileNameJoin@{DirectoryName@file,file//FileBaseName}},
	Quiet[ExtractArchive[file,DirectoryName@file],ExtractArchive::filex];
	With[{obj=LoadObject@FileNameJoin@{dir,(file//FileBaseName)<>".m"}},
		DeleteDirectory[dir,DeleteContents->True];
		obj
		]
	];

$LoadStack;
(LoadObject[file_String,reload_:False]/;(FileExtension[file]=="")):=LoadObject[file<>".m",reload];
LoadObject[file_String,reload_:False]:=
Block[{$LoadStack=Replace[$LoadStack,_Symbol:>{}]},
With[{loadCommand=(
	If[MemberQ[$LoadStack,#],
		Module[{loadingObject},loadingObject/;!MatchQ[$LoadedObjects[#],_Missing]:=(loadingObject=$LoadedObjects[#]);loadingObject],
		AppendTo[$LoadStack,#];With[{o=Get@#},If[HasAttribute[o,"$PostLoad"],o::$PostLoad[]];$LoadedObjects[#//FileBaseName]=o]]&)},
Replace[If[reload//TrueQ,Missing["Reload"],$LoadedObjects[file//FileBaseName]],
	_Missing:>
		Replace[file,{
				f_?(FileExistsQ@#&):>loadCommand@f,
				_:>
		Replace[FileNameJoin@{NotebookDirectory[],file},{
				f_?(FileExistsQ@#&):>loadCommand@f,
				_:>
		Replace[FileNameJoin@{$ObjectDirectory,file//FileBaseName,file},{
				f_?(FileExistsQ@#&):>loadCommand,
				_:>(Message[MathematicaClass::noof,file];$Failed)
				}]
				}]
				}]
		]
	]
	];



(* ::Subsection::Closed:: *)
(*End Internal*)


End[]
