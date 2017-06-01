(* ::Package:: *)

(* ::Section::Closed:: *)
(*Exposed Code*)


(* ::Subsection::Closed:: *)
(*Exposed Classes*)


DatabaseRootClass::usage="Stores properties and controls database creation and closing";
DatabaseClass::usage="A database"
DataTableClass::usage="A table in a database"


(* ::Subsection:: *)
(*Exposed Functions*)


GeneralData::usage="Maps to SQLExpr when formatting data, but is a clearer name, generally"
SetAttributes[GeneralData,HoldAllComplete];


(* ::Subsection::Closed:: *)
(*Exposed Properties*)


$DatabaseDefaultRoot::usage="Root made at load time. If a root is not provided, this becomes the default"


(* ::Section::Closed:: *)
(*Internal Code*)


Begin["`Private`"]


$CodeDirectory=DirectoryName@$InputFileName;


(* ::Subsection::Closed:: *)
(*DatabaseRoot Class*)


DatabaseRootClass=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Class field: Drivers*)


ClassField[Drivers]=<||>;


(* ::Subsubsection::Closed:: *)
(*Class field: ConnectedDatabases*)


ClassField[ConnectedDatabases]={};


(* ::Subsubsection::Closed:: *)
(*Class field: AllowedConnections*)


ClassField[AllowedConnections]=1;


(* ::Subsubsection::Closed:: *)
(*Bound method: KillConnections*)


BoundMethod[KillConnections,self_,n_:All]:=
With[{conn=self::ConnectedDatabases},
(#::Close)&/@Take[conn,n];
self::ConnectedDatabases=Delete[conn,
Replace[n,Except[_Integer|_List]:>Key[n]]]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: KillDrivers*)


(*BoundMethod[KillDrivers,self_,drivers:_String|{_String..}]:=
With[{ds=Flatten@{drivers},dl=self::Drivers},
Replace[dl[#],d:Except[_Missing]\[RuleDelayed]]&/@ds
];*)


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];
$DatabaseDefaultRoot=DatabaseRootClass[InstanceNameExtension->"Default"];


(* ::Subsection::Closed:: *)
(*Database Class*)


DatabaseClass=BeginClass[];


(* ::Subsubsection:: *)
(*Bound method: $Init*)


(* ::Text:: *)
(*Check if specified driver has not already been initialized. If not, initialize it.*)
(*Save driver to root field drivers.*)


InitializationFunction[self_,root:_?(IsInstance[#,DatabaseRootClass]&):$DatabaseDefaultRoot,driver:Alternatives@@JDBCDriverNames[]:"SQLite",
	database:(_String|None):None]:=(
		self::root=root;
		self::Database=database;
		self::Connection=None;

		self::Driver=(Replace[root::Drivers[driver],
			m_Missing:>With[{d=JDBCDrivers[driver]},root::Drivers[driver]=d;d]];driver);
			If[database=!=None,self::Open[]];
		)


(* ::Subsubsection:: *)
(*Bound Property: IsOpen*)


BoundProperty[IsOpen,self_]:=
	With[{conn=self::Connection[[2]]},
		JLink`JavaObjectQ[conn]
			&&
		conn=!=Null
			&&
		!conn[DatabaseLink`SQL`Private`isClosed[]]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: Open*)


BoundMethod[Open,self_,dbfile:(_String|None):None,ops:OptionsPattern[Flatten@{Options@OpenSQLConnection,OverwriteTarget->False}]]:=
With[{r=self::root,driverANDfile=
Replace[dbfile,{
	None:>Replace[self::Database,{
		None:>{"HSQL(Standalone)","temp"},
		o_:>{self::Driver,self::Database}
	}],
	d_:>{self::Driver,dbfile}
}]},
If[OptionValue@OverwriteTarget,Replace[FileExistsQ@driverANDfile[[2]],f_?FileExistsQ:>DeleteFile[f]]];
With[{allowed=r::AllowedConnections,dbs=r::ConnectedDatabases,connection=OpenSQLConnection[JDBC@@driverANDfile,FilterRules[{ops},Options@OpenSQLConnection]]},
r::ConnectedDatabases=
Prepend[If[Length[dbs]>=allowed,
r::KillConnections[allowed-(Length@dbs-1)],
dbs],self];
self::Database=driverANDfile[[2]];
self::Connection=connection
]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: Close*)


BoundMethod[Close,self_]:=CloseSQLConnection[self::Connection];


(* ::Subsubsection:: *)
(*Bound method: Apply*)


BoundMethod[Apply,self_,function_,args___]:=
With[{conn=self::Connection},
	Replace[function[conn,args],h_?(Head@#===function&):>(If[MatchQ[{args},{_List}],function@@@Thread[{conn,args}],h])]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: Execute*)


BoundMethod[Execute,self_,commands_,args___]:=
	Switch[commands,
		_List,SQLExecute[self::Connection,#,args]&/@commands,
		_String,SQLExecute[self::Connection,commands,args]
		];


(* ::Subsubsection::Closed:: *)
(*Class field: LoadedTables*)


ClassField[LoadedTables]=<||>;


(* ::Subsubsection::Closed:: *)
(*Bound method: LoadTable*)


BoundMethod[LoadTable,self_,
name_,
columns:((_String->_)|{(_String->_)..}|Automatic):Automatic,
ops___,
Table->Automatic,
"Reload"->False,
"Overwrite"->False]:=
	Replace[If[(OptionValue@"Reload"//TrueQ)||(OptionValue@"Overwrite"//TrueQ),Missing["RELOAD"],self::LoadedTables[name]],
		_Missing:>
			With[{tbl=With[{cls=Replace[OptionValue[Table],Automatic->DataTableClass]},
				If[OptionValue@"Overwrite",self::DropTables[name]];
				cls[self,name,columns,ops,InstanceNameExtension->name]
				]},
			If[IsInstance[tbl,DataTableClass],self::LoadedTables[name]=tbl];
			tbl]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: TableNames*)


BoundMethod[TableNames,self_,sel:(_String|{_String..}|{_Integer..}|All):All,pattern_:None]:=
	Replace[Flatten[{sel},1],{
		Except[{_String..}]:>With[{tableNames=self::Apply[SQLTableNames,Replace[pattern,None->Sequence[]]]},
			tableNames[[sel]]
			],
		s_:>With[{tableNames=self::Apply[SQLTableNames,Replace[pattern,None->Sequence[]]]},
			Intersection[tableNames,s]
			]
		}];


(* ::Subsubsection::Closed:: *)
(*Bound method: DropTables*)


BoundMethod[DropTables,self_,tabs:(_String|{_String..}|{_Integer..}|All):All]:=
With[{tableNames=self::TableNames[tabs]},
	self::Apply[SQLDropTable,tableNames];
	self::LoadedTables=KeyDrop[self::LoadedTables,tableNames];
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: View*)


BoundMethod[View,self_,viewFunction_:Automatic]:=
With[{loaded=self::LoadedTables},
With[{tables=Replace[self::TableNames[],{{None}->None,tn_:>(Replace[loaded[#],_Missing:>self::LoadTable[#,Automatic]]&/@tn)}]},
If[tables=!=None,
Replace[viewFunction,{
	"Elided":>(DynamicModule[{l=tables,i=1},Panel@
		Column@{Row@{
				Button["\[FirstPage]",i=1,Appearance->"Palette"],
				Button["\[LeftPointer]",i=Max@{i-1,1},Appearance->"Palette"],
				Button["\[RightPointer]",i=Min@{i+1,Length@l},Appearance->"Palette"],
				Button["\[LastPage]",i=Length@l,Appearance->"Palette"]},
			Panel[Dynamic[
				With[{tbl=l[[i]]},
					tbl::View["Elided"]
					],
				TrackedSymbols:>{i}],Background->White]
			}]),
	Automatic:>(DynamicModule[{l=tables,i=1},Panel@
		Column@{Row@{
				Button["\[FirstPage]",i=1,Appearance->"Palette"],
				Button["\[LeftPointer]",i=Max@{i-1,1},Appearance->"Palette"],
				Button["\[RightPointer]",i=Min@{i+1,Length@l},Appearance->"Palette"],
				Button["\[LastPage]",i=Length@l,Appearance->"Palette"]},
			Panel[Dynamic[Column@{GetAttribute[l[[i]],"Name"],GetAttribute[l[[i]],"View"][]},TrackedSymbols:>{i}],
				Background->White]
				}
			]),
	_:>viewFunction@tables
	}],
"EMPTY DATABASE"
]
]];


(* ::Subsubsection:: *)
(*Bound method: $ToRepresentation*)


RepresentationFunction[self_]:=With[{tn=self::TableNames[]},
	Framed[
			Grid[{
				{self::Database//TextCell},
				If[Length@tn>0,
				{Framed[Grid[
					Partition[(Framed[TextCell[#],RoundingRadius->2,Background->RGBColor[1,1,.8]]&/@tn),3]
					]//Pane[#,ImageSize->{Automatic,100}]&,
					RoundingRadius->5]},
				{"EMPTY"//TextCell}]
				},
				Alignment->Center,
				Background->GrayLevel[.95],Dividers->True],
			Background->GrayLevel[.9],
			RoundingRadius->5]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: InterpretType*)


BoundMethod[InterpretType,self_,typeName_]:=
With[{typeChoices=self::Apply[SQLDataTypeNames],type=ToUpperCase@ToString[typeName]},
	If[Not@MemberQ[typeChoices,type],
		With[{reps={
				("INT"|"INTEGER"|"TINYINT")->"INT",
				("NULL")->"NULL",
				("REAL")->"REAL",
				("STR"|"STRING"|"VARCHAR"|"TEXT")->"STR",
				("BLOB"|"BIN")->"BIN"}},
			Replace[Position[typeChoices/.reps,type/.reps],{{}->"NULL",{i_}:>typeChoices[[i]]}]
			],
		type
		]
	];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Subsection::Closed:: *)
(*DataTable Class*)


DataTableClass=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Bound method: $Init*)


InitializationFunction[self_,
	database:_?(IsInstance[#,DatabaseClass]&),
	name_String,
	columns:((_String->(_String|_List|_Alternatives))|{(_String->(_String|_List|_Alternatives))..}|Automatic):Automatic,
	ops___]:=
	With[{cols=Replace[columns,{Automatic->{},c_:>(self::FormatColumn/@Flatten[{c},1])}]},
		self::Database=database;
		self::Name=name;
		With[{tables=database::TableNames[name]},
			If[MatchQ[tables,{}],
				database::Apply[SQLCreateTable,name,cols,ops];
				self::Columns=(cols/.SQLColumn[con_,___]:>{name,con}),
				self::Columns=Automatic;
				];
			]
		];


(* ::Subsubsection::Closed:: *)
(*Static method: FormatColumn*)


StaticMethod[FormatColumn,colSpec_]:=
	With[{colName=colSpec[[1]],colDef=colSpec[[2]]},
		SQLColumn[colName,
			Sequence@@With[{sp=Flatten[{colDef},1]},
				ReplacePart[sp,
						1->(sp[[1]]/.{a:_Alternatives:>
							Sequence@@With[{l=ReplacePart[a,0->List]},
											ReplacePart[l,{
												1->(l[[1]]/.(s_String:>("DataTypeName"->Replace[s,"General":>"VARCHAR"]))),
												i_:>(l[[i]]/.{Null->("Nullable"->True),Key->("PrimaryKey"->True),o:Except[_Rule]:>("Default"->o)})
												
												}]],
									o:Except[_Rule]:>("DataTypeName"->o)}
								)
						]
					]]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: Apply*)


BoundMethod[Apply,self_,function_,args___]:=self::Database::Apply[function,self::Name,args];


(* ::Subsubsection::Closed:: *)
(*Bound method: TableCommandExecute*)


BoundMethod[TableCommandExecute,self_,command_String,subcommand_String,argstrings:_List:{" "},
	"SpanLines"->True,
	"Parentheses"->False]:=With[{cmd=StringJoin@{
StringTrim[command]," ",self::Name,If[OptionValue["SpanLines"]//TrueQ,"\n\t"," "],
	subcommand
	},args=argstrings~Riffle~If[OptionValue["SpanLines"]//TrueQ,",\n\t",","]},
self::Database::Execute[StringJoin@{cmd," ",If[OptionValue["Parentheses"]//TrueQ,{"(",args,")"},args],";"}
]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: TemplateExecute*)


BoundMethod[TemplateExecute,self_,argTemplate_,args___,
		Replace->Automatic,StringReplace->None,Repeated->False,Verbose->False,StringJoin->True]:=
With[{template=StringTemplate[
	StringReplace[StringTrim[StringTrim@argTemplate,";"]<>If[OptionValue@StringJoin//TrueQ,";\n",";"],
		Replace[OptionValue@StringReplace,{
			Automatic:>{"<<TABLE>>"->self::Name},
			None->{},
			l_List:>(l/.{
				Table:>("<<TABLE>>"->self::Name),
				Column[a:{__}]:>("<<COLUMNS>>"->StringJoin@Riffle[self::ColumnNames[a],","])
					})
				}]
			]
	]@@(
		{args}/.Replace[OptionValue@Replace,{
					Automatic:>{Table->self::Name,Column[{key_}]:>First@self::ColumnNames[key]},
					None->{}
					}])
		},
	With[{command=
		Replace[(OptionValue@Repeated)/.
				Column[a:{__}]:>Replace[self::ColumnNames[a],{s_String}:>s],
				{
			n_Integer:>If[OptionValue@StringJoin,StringJoin,Identity]@Table[template,n],
			Rule[key_String,data_List]:>If[OptionValue@StringJoin,StringJoin,Identity]@Table[StringReplace[template,key->ToString[d]],{d,data}],
			Rule[key_String,data_]:>StringReplace[template,key->ToString[data]],
			r:{Rule[_String,_List]..}:>If[OptionValue@StringJoin,StringJoin,Identity]@With[{replacements=First/@r,lists=Last/@r},
										MapThread[StringReplace[template,Thread[replacements->(ToString/@{##})]]&,lists]
										],
			r:{Rule[_String,_]..}:>If[OptionValue@StringJoin,StringJoin,Identity]@StringReplace[template,((#[[1]]->ToString[#[[2]]])&)/@r],
			_->template
			}]},
		If[OptionValue@Verbose,Print@command];
		self::Database::Execute[If[OptionValue@StringJoin,StringReplace[command,{"\n"->" ","\t"->""}],command]]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: RunString*)


BoundMethod[RunString,self_,routineString:(_String|File[_String]),args___]:=
	With[{stringCommand=Replace[routineString,File[s_]:>Import[s,"Plaintext"]],argList={args}},
		With[{strReps=Cases[argList,s:((_String)->_):>s]},
		self::TemplateExecute[stringCommand,Sequence@@Complement[argList,strReps],Replace->None,StringReplace->strReps]
			]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: RunRoutine*)


BoundMethod[RunRoutine,self_,fileSpec_,args___]:=
	With[{f=FileNameJoin@{$CodeDirectory,"SQLRoutines",Replace[fileSpec,f_?(FileExtension[#]===""&):>f<>".sql"]}},
		If[FileExistsQ@f,self::RunString[File[f],args],Message[RunRoutine::nffil,RunRoutine];$Failed]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: ColumnLengths*)


BoundMethod[ColumnLengths,self_,colSpec_:All]:=
With[{cols=self::ColumnNames[colSpec],template=("SELECT COUNT(``) FROM "<>self::Name)},
	If[MatchQ[cols,_List],
	(#->(self::TemplateExecute[template,#])[[1,1]])&/@cols,
	$Failed]
	]


(* ::Subsubsection::Closed:: *)
(*Bound method: ColumnNames*)


BoundMethod[ColumnNames,self_,cols:(_String|_Column|{(_String|_Column)..}|{_Integer..}|All):All]:=
With[{allCols=Cases[self::Apply[SQLColumnNames],{tab_,name_}:>name]},
Replace[
Replace[
	Replace[Flatten[{cols},1],Except[{(_String|_Column)..}]:>allCols[[cols]]],
	{}:>Cases[self::Columns[[cols]],{tab_,name_}:>name]
	],{Column[n_]:>n,Except[n_?(MemberQ[allCols,#]&)]:>Sequence[]},1
	]];


(* ::Subsubsection::Closed:: *)
(*Bound method: ColumnApply*)


BoundMethod[ColumnApply,self_,function_,colTOvals:((_String->_)|{(_String->_)..}|(_Integer->_)|{(_Integer->_)..}|{{__}}),args___]:=
	self::Apply[function,Sequence@@self::FormatInsertData[colTOvals],args];


(* ::Subsubsection::Closed:: *)
(*Bound method: FormatInsertData*)


BoundMethod[FormatInsertData,self_,colTOvals:((_String->_)|{(_String->_)..}|(_Integer->_)|{(_Integer->_)..}|{{__}})]:=
With[{colND=Replace[Thread[Flatten[{colTOvals},1],Rule],{(a_->b_):>{a,b},d_:>{All,d}}]},
	With[{insertData=With[{ml=Max@(Length/@colND[[2]])},Transpose@(PadRight[#,ml,Null]&/@colND[[2]])],
				genData=If[MemberQ[{"MySQL"},self::Database::Driver],
							GeneralData[s_]:>SQLExpr[s],
							GeneralData[s_]:>TemplateApply["DatabaseLink<*\"`\"*>SQLExpr[``]",ToString[s,InputForm]]]},
		{self::ColumnNames[colND[[1]]],insertData/.genData}
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: ColumnInsert*)


BoundMethod[ColumnInsert,self_,colTOvals:((_String->_)|{(_String->_)..}|(_Integer->_)|{(_Integer->_)..}|{{__}})]:=
	self::ColumnApply[SQLInsert,colTOvals];


(* ::Subsubsection::Closed:: *)
(*Bound method: RowInsert*)


BoundMethod[RowInsert,self_,colids:({(_String)..}|{(_Integer)..}|All),values_List]:=
	self::Apply[SQLInsert,self::ColumnNames[colids],values];


(* ::Subsubsection::Closed:: *)
(*Bound method: Insert*)


BoundMethod[Insert,self_,args__]:=
Replace[{args},{
		a:{{_Rule..}}:>self::ColumnInsert[args],
		a:{_List}:>self::ColumnInsert[a],
		{a_,b_List}:>self::RowInsert[args],
		_:>self::ColumnInsert[args]
		}];


(* ::Subsubsection::Closed:: *)
(*Bound update: Update*)


BoundMethod[Update,self_,cols_,data:Except[_Rule]:None,Row->Interval[{1,Automatic}]]:=
	Switch[self::Database::Driver,
		_?(StringMatchQ[#,"SQLite"~~___]&),
			With[{colANDdata=Replace[data,{None:>self::FormatInsertData[cols],_->{cols,data}}]},
				With[{valuePairs=Table[StringJoin@Riffle[MapThread[StringTemplate["`1` = `2`"][##]&,{colANDdata[[1]],d}],","],{d,colANDdata[[2]]}],
						rowIndices=Flatten@self::Select[Column["ROWID"]]},
				
				self::RunRoutine["updateRow",
					"<<TABLE>>"->self::Name,
					"<<INDEXINGCOLUMN>>"->"ROWID",
					Repeated->With[{rowData=(*Part[rowIndices,*)Replace[OptionValue@Row,{
								Interval[{i_Integer,Automatic}]:>(Range[i,i+Length@valuePairs-1]),
								Interval[{start_Integer,end_Integer}]:>Range[start,end],
								o_:>{o}}] (*]*)
							},{
							"<<COLUMNVALUEPAIRS>>"->valuePairs[[;;Length@rowData]],
							"<<ROWINDEX>>"->rowData
							}],
					StringJoin->False
				]]
			]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: Select*)


BoundMethod[Select,self_,cols:(_String|_Column|_List|All):All,cond_:None]:=
With[{columns=self::ColumnNames[cols]},
	self::Apply[SQLSelect,columns,Replace[cond,(None|Null):>Sequence[]]]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: AddColumns*)


BoundMethod[AddColumns,self_,cols:(_String->_)|{(_String->_)..},mode_:"Batch"]:=
With[{colSpecs=(self::FormatColumn/@Flatten[{cols},1])},
	With[{columnList=colSpecs/.{
				SQLColumn[name_,args___]:>Riffle[{
					name,
					Replace[Cases[{args},("DataTypeName"->s_):>s],{{}:>"NULL",{s_}:>(self::Database::InterpretType[s])}],
					If[FreeQ[{args},"Nullable"->True],"NOT NULL",Sequence[]],
					Replace[Cases[{args},("Default"->v_):>v],{{}->Sequence[],{v_}:>("DEFAULT "<>ToString[v])}]
					}~DeleteCases~Null," "]}},
		If[mode==="Batch",
			self::TableCommandExecute["ALTER TABLE","ADD",columnList,"Parentheses"->(Length@colSpecs>1)],
			self::TableCommandExecute["ALTER TABLE","ADD",#,"Parentheses"->False]&/@columnList
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: DropColumns*)


BoundMethod[DropColumns,self_,cols:(_String)|{(_String)..}]:=
self::TableCommandExecute["ALTER TABLE",
"DROP COLUMN",Flatten@{cols},
"Parentheses"->((Length@Flatten@{cols})>1)
];


(* ::Subsubsection::Closed:: *)
(*Bound method: Drop*)


BoundMethod[Drop,self_]:=self::Database::DropTables[self::Name];


(* ::Subsubsection::Closed:: *)
(*Bound method: View*)


BoundMethod[View,self_,viewFunction_:Automatic]:=
Replace[viewFunction,{
	Automatic:>(TableForm[self::Select[],TableHeadings->{None,TextCell/@self::ColumnNames[]}]),
	"Elided":>With[{cnames=Replace[self::ColumnNames[],Except[_List]:>{}]},
				Framed[Column[{TextCell[Replace[Evaluate[self::Database::Database//FileBaseName],Except[_String]:>ToString@self<>": Database"]],
					Grid[{
						{TextCell@Replace[Evaluate[self::Name],Except[_String]:>ToString@self],SpanFromLeft},
							TextCell/@cnames},
						Dividers->{{1->Black,-1->Black}
									~Join~
									Replace[Length@cnames,{Except[_?(MatchQ[#,_Integer]&&#>0&)]:>{},_:>Array[#->GrayLevel[.8]&,Length@cnames-1,2]}],
								{1->Black,-1->Black,2->GrayLevel[.8]}},
						Background->GrayLevel[.95]
						]},Alignment->Center],
					Background->GrayLevel[.9],RoundingRadius->5
					]
				],
	_:>viewFunction[self::Select[],self::ColumnNames[]]
	}];


(* ::Subsubsection::Closed:: *)
(*Bound method: $ToRepresentation*)


RepresentationFunction[self_,viewFunction_:Automatic]:=
DynamicModule[{icon="SquarePlusIconMedium",mode="Elided"},
		With[{b=Button[Dynamic[RawBoxes@FEPrivate`FrontEndResource["FEBitmaps",icon]],
				mode=Replace[mode,{"Elided":>(icon="SquareMinusIconMedium";None),
									None->(icon="SquarePlusIconMedium";"Elided")}],
			Appearance->"Frameless"]},
			Row[{b,Spacer[5],Dynamic[(self::View[Replace[mode,None->viewFunction]]),TrackedSymbols:>{mode}]}]
			]
		];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Section::Closed:: *)
(*End Package*)


End[]
