(* ::Package:: *)

(* ::Chapter:: *)
(*DVR Wrapper Class*)


(* ::Section:: *)
(*Exposed Code*)


(* ::Subsection::Closed:: *)
(*Exposed Options*)


(* ::Subsubsection::Closed:: *)
(*DVR Initialization Options*)


DVRName::usage="Specifies the name of the DVR"
DVRDirectory::usage="Specifies the directory for DVR files"
KineticMatrixDirectory::usage="Specifies the directory for kinetic matrices"
PotentialDirectory::usage="Specifies the directory for potential matrices"
WavefunctionDirectory::usage="Specifies the directory for wavefunctions"
ReloadPackage::usage="Specifies that the DVR package should be reloaded"
SaveKineticMatrices::usage="Specifies whether kinetic matrices should be saved by default"
StorageMode::usage="Specifies whether to save in a database or a file. Defaults to database"


(* ::Subsubsection::Closed:: *)
(*DVRRun Options*)


RunPoints::usage="Specifies the point divisions for the run"
ReturnPoint::usage="Specifies where the DVR should stop running"
KineticMatrix::usage="Specifies the kinetic energy matrix of the DVR"
PotentialMatrix::usage="Specifies the potential energy matrix of the DVR"
Wavefunctions::usage="Specifies the wavefunctions for the DVR"
StoreResults::usage="Specifies whether to store intermediate results during the run"
RecalculateKineticMatrix::usage="Specifies whether to try to reload an old kinetic matrix"
RecalculateWavefunctions::usage="Specifies whether to try to reload old wavefunctions"
ShowCompound::usage="Specifies whether to display the object the DVR calculations are performed on (if provided)"


(* ::Subsubsection::Closed:: *)
(*Reading Potentials*)


PotentialPointConversion::usage="The conversion to apply to each potential point"
PotentialSheetIndex::usage="The sheet index to get the potential from"
PotentialPointExtension::usage="Makes new potential points from old ones found. Run after reading in potential."
PotentialSortingFunction::usage="The sorting function to use in sorting the final grid. A value of Automatic applies canonical sorting. A value of None does not sort the potential."


(* ::Subsubsection::Closed:: *)
(*DVR Passed Options*)


PotentialFunction::usage="The function used to generate the potential"
PotentialColoring::usage="(string or function) Option name for coloring the potential"
PotentialCutOff::usage="(function) A cutoff function for displaying the potential"
SquareWavefunction::usage="(boolean) Specifies whether to square the wavefunction or not"
DVRUnits::usage="Specifies units for the DVR. Also used for proper application to a DVR."
DVR\[HBar]::usage="Specifies the \[HBar] to apply to the expression"


(* ::Subsection::Closed:: *)
(*Exposed Classes*)


DVRBaseClass::usage="A general wrapper for a DVR. Should be subclassed for specific instances"
DVRInterfaceClass::usage="A class that provides an interface to use a DVR and make new ones"
KineticMatrixTableClass::usage="A database for saving kinetic matrix data"
WavefunctionTableClass::usage="A datatable for saving wavefunction data"


(* ::Subsection::Closed:: *)
(*Exposed Functions*)


DVRNotebook::usage="Opens a notebook version of a DVR instance"


(* ::Section:: *)
(*Internal Code*)


Begin["`Private`"]


(* ::Subsection::Closed:: *)
(*Helper Code*)


QVU[Q_,U_]:=UnitConvert[Quantity@Q,U]//QuantityMagnitude
SetAttributes[QVU,HoldAll];


(* ::Subsubsection::Closed:: *)
(*Defining DVRUnits*)


SetAttributes[DVRUnits,Listable];
DVRUnits[a_->b_]:=a->DVRUnits[b];
DVRUnits[a_:>b_]:=a:>DVRUnits[b];
DVRUnits`allowed=(_?NumericQ|_Quantity|None|Automatic);
With[{DVRUnits`allowed=DVRUnits`allowed},
DVRUnits/:HoldPattern[Plus[other_,DVRUnits[n:DVRUnits`allowed]]]:=
	other+Replace[Global`flee=n;n,{Quantity[v_,u_]:>Replace[other,{
						_Quantity:>n,
						_:>QuantityMagnitude@n}],
				_?NumericQ:>n,
				_:>0
				}];
(*DVRUnits/:HoldPattern[Plus[other_,DVRUnits[n*)
(*DVRUnits/:HoldPattern[Plus[DVRUnits[n:DVRUnits`allowed],other_]]:=other+DVRUnits[n];*)
DVRUnits/:HoldPattern[Times[DVRUnits[n:DVRUnits`allowed],other_]]:=
	Replace[n,{
			_Quantity:>Replace[other,{_Quantity:>n,_?NumericQ:>QuantityMagnitude@n,_:>n}],
			_?NumericQ:>n,
			_:>1}]*other;
DVRUnits/:HoldPattern[Times[other_,d_DVRUnits]]:=
	d*other;
DVRUnits/:HoldPattern[NumericQ[DVRUnits[_]]]:=True
DVRUnits/:HoldPattern[Times[DVRUnits[f:Except[DVRUnits`allowed]],other_]]:=f[other];
DVRUnits/:HoldPattern[DVRUnits[n:DVRUnits`allowed][x_]]:=DVRUnits[n]*x;
DVRUnits/:HoldPattern[DVRUnits[f:Except[DVRUnits`allowed]][x_]]:=f[x];
DVRUnits/:HoldPattern[DisplayForm[DVRUnits[Quantity[v_,u_]]]]:={v,u};
]


(* ::Subsection:: *)
(*DVR Base Class*)


DVRBaseClass:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Messages*)


DVRBaseClass::fnf="DVR configuration file `` not found";
DVRBaseClass::grdim="DVR grid dimensions (``) and range dimensions (``) unequal";
DVRBaseClass::fgdim="DVR grid dimensions (``) and file specified dimensions (``) unequal";
DVRBaseClass::nopot="Potential file not found: ``";
DVRBaseClass::bckfail="Couldn't submit calculation to kernel"
DVRBaseClass::toolng="DVR calculation timed out after `` seconds"


(* ::Subsubsection:: *)
(*Bound method: $Init*)


InitializationFunction[self_,points:({(__?NumericQ)..}|None):None,
		domain:{{_?NumericQ,_?NumericQ}..}:{{0,\[Infinity]},{0,\[Pi]},{0,2\[Pi]}},
		file:_String:"SphericalDVR",compound:(_?(IsInstance[#,MolecularModeling`Molecule]&))|None:None,
		DVRName->"",
		DVRUnits->None,
		KineticMatrixDirectory->$KineticMatrixDirectory,
		DVRDirectory->$ConfigFileDirectory,
		PotentialDirectory->$PotentialFileDirectory,
		WavefunctionDirectory->$WavefunctionFileDirectory,
		SaveKineticMatrices->True,
		FileExtension->{".xlsx"},
		ReloadPackage->False,
		StorageMode->"Database",
		OptionsPattern]:=
		Module[{
			args=$KeyWordArguments,
			reload,
			DVRResults,MonitoringGrid,
			baseops={SaveKineticMatrices,StorageMode},
			stringops={DVRName,DVRDirectory,WavefunctionDirectory,PotentialDirectory,KineticMatrixDirectory}
			},
			reload=args[ReloadPackage];
			KeyDropFrom[args,ReloadPackage];
			self::Points=points;
			self::Range=domain;
			self::BackgroundKernel=None;
			self::DVRFile=FileBaseName[file];
			self::Compound=compound;
			self::ResultString=ToString@DVRResults;
			self::MonitoringString=ToString@MonitoringGrid;
			self::MonitoringSetter=HoldPattern[MonitoringGrid];
			self::MonitoringGrid=MonitoringGrid;
			self::DVROptions=None;
			self::Results=DVRResults;
			args[DVRUnits]=Replace[args[DVRUnits],{
					l_List:>Table[Replace[r,{p:(Rule[k_,v_]|RuleDelayed[k_,v_]):>ReplacePart[p,{2->DVRUnits[v]}],
											o_:>DVRUnits[o]}],{r,l}],
					a_Association:>Table[Replace[r,
											{p:(Rule[k_,v_]|RuleDelayed[k_,v_]):>ReplacePart[p,{2->DVRUnits[v]}],
											o_:>DVRUnits[o]}],{r,Normal@a}]
					n:(_?NumericQ|_Quantity):>{Wavefunctions->DVRUnits[n]}
					}];
			Do[SetAttribute[self,x,args[x]],
				{x,baseops}
			];
			Do[SetAttribute[self,x,ToString[args[x]]],
				{x,stringops}
			];
			If[args[StorageMode]=="Database",
				self::GetDatabase[]
				];
			self::DVRUnits=args[DVRUnits];
			self::Arguments=KeyDrop[args,Flatten@{baseops,stringops,DVRUnits}];
			self::Initialize[reload];
		]


ClassField[Database]=None;


BoundMethod[GetDatabase,self_]:=With[{base=ObjectType@self},
		With[{type=MathematicaClass[base]},
		self::Database=Replace[type::Database,
			None:>With[{dbNew=DatabaseClass[FileNameJoin@{$DVRRoot,"Resources","Data.sql"}]},
					type::Database=dbNew;dbNew
						]
					]
				]
			];


(* ::Subsubsection::Closed:: *)
(*Bound method: CopyFunction*)


BoundMethod[CopyFunction,self_,function_,symbol_]:=
With[{func=Replace[Evaluate@function,{
			f_Function:>Module[{method},method[a___]:=f[a];method],
			e:Except[_Symbol]:>(Print@e;Module[{exception},exception[]:>e])}]},
	Print@func;
	With[{selfSym=SymbolName@self,funcSym=SymbolName@func},
		With[{newFunc=Symbol[Evaluate@TemplateApply["``$``$``$``",{Context@selfSym,funcSym,symbol,ToString@$ModuleNumber}]]},
			ObjectOrientedProgramming`Private`copyValues[func,newFunc];
			newFunc
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: Initialize*)


(* ::Text:: *)
(*Called by $Init to do the dirty work of initialization*)
(**)
(*Rewrite so that there's only ever one copy of the DVR parameters floating about. Too confusing if there are more. This will require minor reconfiguration, but will make the lives of future people easier and improve debugging.*)
(**)


BoundMethod[Initialize,self_,reload_:False]:=
		Module[{r=self::Range,p=self::Points,gridDim,
			L,F,dvrfile,i,filebasename,loadFlag=True,context,package},
			If[TrueQ[Head[r]==List],If[Head[r[[1]]]==Integer,r={r}],r={}];
			gridDim=Length[p];
			If[Not[Length[r]==gridDim],Message[DVRBaseClass::grdim,gridDim,Length[r]];
			loadFlag=False;
			];
			filebasename=self::DVRFile;
			context=$BaseContext<>filebasename<>"`";
			package=filebasename<>".m";
			F=FileNameJoin[{self::DVRDirectory,package}];
			dvrfile=FindFile[F];
			If[dvrfile==$Failed,
				SetDirectory[self::DVRDirectory];
				Do[
					If[StringContainsQ[f,package],
					dvrfile=f
					]
				,{f,FileNames[]}];
				ResetDirectory[];
			];
			(*Takes parameters from the loaded DVR config file,
			Dimension from $DVRDimension,
			GridPoints from $GridPointsFunction,
			KineticMatrix from $KineticMatrixFunction,
			PotentialMatrix from $PotentialMatrixFunction,
			WaveFunctions from $WavefunctionsFunction,
			Plot from $PlotFunction*)
			self::File=dvrfile;
			If[Not@TrueQ[dvrfile==$Failed],
				If[reload,Clear@Evaluate@(context<>"*")];
				LoadDVR[filebasename,$ConfigFileDirectory,reload];
				With[{copy=self::CopyFunction,mkSym=Function[s,ToExpression@(context<>s)]},
					self::Dimension=mkSym@"$DVRDimension";
					self::PointsPattern=mkSym@"$PointLabels";
					self::FormatGrid=mkSym@"$FormatGrid";
					self::GridPoints=mkSym@"$GridPointsFunction";
					self::KineticMatrix=mkSym@"$KineticMatrixFunction";
					self::PotentialMatrix=mkSym@"$PotentialMatrixFunction";
					self::Wavefunctions=mkSym@"$WavefunctionsFunction";
					self::Plot:=mkSym@"$PlotFunction";
					];

				If[self::Dimension===gridDim,
					With[{LP=self::LoadPotentialMatrix,VF=self::PotentialMatrix,
						LK=self::LoadKineticMatrix,KF=self::KineticMatrix,
						LW=self::LoadWavefunctions,WF=self::Wavefunctions},
						(Options[#1]=Flatten@{Options[#1],Options[#2]})&@@@{{LP,VF},{LK,KF},{LW,WF}}
						];,
					Message[DVRBaseClass::fgdim,gridDim,self::Dimension];
					self::PointsPattern=.;
					self::GridPoints=.;
					self::KineticMatrix=.;
					self::PotentialMatrix=.;
					self::Wavefunctions=.;
					self::Plot=.;
					],
					Message[DVRBaseClass::fnf,F];
			];
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: FindDirectory*)


BoundMethod[FindDirectory,self_,directorySpec_]:=
	Replace[directorySpec//ToLowerCase,{
				("wavefunction"|"Wavefunction"|"wavefunctions"|"Wavefunctions"):>self::WavefunctionDirectory,
				("potential"|"potentials"|"Potentials"|"Potential"):>self::PotentialDirectory,
				("kinetic"|"Kinetic"):>self::KineticMatrixDirectory,
				("dvrs"|"config"):>self::DVRDirectory,
				_:>FileNameJoin@{self::DVRDirectory//ParentDirectory,directorySpec}}]


(* ::Subsubsection::Closed:: *)
(*Bound method: FindFile*)


(* ::Text:: *)
(*Finds files relative to the base directories the DVR holds*)


	BoundMethod[FindFile,self_,filename:(_String|_Integer),folder_:"Wavefunctions",filePattern_:"*"]:=
		With[{fol=self::FindDirectory@folder},
			Replace[filename,
				{s_String:>FileNameJoin[{fol,self::DVRFile,s}],
				i_Integer:>Block[{findingfile},SetDirectory@fol;findingfile=FileNames[filePattern][[i]];ResetDirectory[];findingfile]
				}]
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: SaveAttributes*)


BoundMethod[SaveAttributes,self_,context_:None]:=
		
		(
		(#->GetAttribute[self,#]&/@{"Points","DVRName","DVRFile","Range"})
				~Join~
		(FilterRules[(self::Arguments//Normal),
				Replace[Replace[context,Except[_String]->"all"]//ToLowerCase,
							{
								"all":>_,
								"potential":>Options@(Evaluate@self::PotentialMatrix),
								"kinetic":>Options@(Evaluate@self::KineticMatrix),
								"wavefunctions":>Except[FileExtension],
								o_:>Except[FileExtension]
							}]
							]//SortBy[First])
			)


(* ::Subsubsection::Closed:: *)
(*Bound method: LookupKey*)


(* ::Text:: *)
(*Specifies a relatively unique file base based on the various DVR components so that kinetic matrices and things can be imported rather than recalculated. Takes a context parameter*)


	BoundMethod[LookupKey,self_,context_:"kinetic",hash_:False]:=
		(*Extend this so as to be applicable to a grid, too*)
		With[{ext=".csv",
			name=self::DVRName},
			With[{rules=self::SaveAttributes[context]/.{
				(PotentialFunction->v_):>(PotentialFunction->Hash@v),
				(d:DVRUnits[_]):>Hash@d
				}},
			With[{pairs={ToString@#[[1]],
							Replace[
								Replace[#[[2]],{f:(_Function|_Symbol):>Hash@f,a_?AtomQ:>a,o_:>Hash@o}],
								{n_?NumberQ:>ToString[n,CForm],o_:>ToString@o}]
								}&/@rules
							},
					With[{args=StringJoin@(MapThread[#1<>"="<>#2&,Thread@pairs]~Riffle~"_")
			},
			Replace[StringJoin@(ToString/@Flatten[{name,"[",args,"]",
							StringJoin@Riffle[ToString/@self::Points,"_"]}]),
							s_:>If[hash//TrueQ,ToString@Hash@s,s]
							]
			]]]]


(* ::Subsubsection::Closed:: *)
(*Bound properties: KineticMatrixFile, PotentialFile, WavefunctionsFile*)


(* ::Text:: *)
(*These use the previous two methods to find files to import and save to.*)


	BoundProperty[KineticMatrixFile,self_,ext_:".csv"]:=
		self::FindFile[self::LookupKey["Kinetic",True]<>ext,"Kinetic"];
	BoundProperty[PotentialFile,self_,ext_:".xlsx"]:=
		self::FindFile[ToString@Hash@self::LookupKey["Potential",True]<>ext,"Potential"];
	BoundProperty[WavefunctionsFile,self_,ext_:".csv"]:=
		self::FindFile[ToString@Hash@self::LookupKey["Wavefunctions",True]<>ext,"Wavefunctions"];


(* ::Subsubsection::Closed:: *)
(*Bound methods: WavefunctionsDataTable, KineticMatrixDataTable*)


BoundMethod[WavefunctionsDataTable,self_,overwrite_:False]:=
		self::Database::LoadTable["Wavefunctions"<>self::LookupKey["Wavefunctions",True],self,
			Table->WavefunctionTableClass,"Overwrite"->overwrite];
BoundMethod[KineticMatrixDataTable,self_,overwrite_:False]:=
		self::Database::LoadTable["Kinetic"<>self::LookupKey["Kinetic",True],self,
			Table->KineticMatrixTableClass,"Overwrite"->(overwrite)];


(* ::Subsubsection::Closed:: *)
(*Bound methods: LoadWavefunctions, SaveWavefunctions, *)
(*LoadKineticMatrix, LoadPotentialMatrix*)


(* ::Text:: *)
(*Pretty self explanatory*)


BoundMethod[LoadWavefunctions,self_,gridORfile:(MatchQ[#,_String]&):None,Mode->Automatic,Verbose->False]:=
	With[{m=Replace[OptionValue@Mode,Automatic->self::StorageMode]},
		Switch[m,
			"File",
			Module[{f=If[gridORfile===None,self::WavefunctionsFile,gridORfile]},
				With[{d=DirectoryName@f},If[Not@FileExistsQ[d],CreateDirectory[d]]];
					If[FileExistsQ@f,
						If[OptionValue@Verbose,PrintTemporary@"Loading Wavefunctions..."];
						With[{data=Import@f},
							{data[[All,1]],data[[All,2;;-1]]}
						],
						$Failed
					]
				],
			"Database",
			With[{table=self::WavefunctionsDataTable[]},
				If[("Value"/.Replace[Evaluate[table::ColumnLengths["Value"]],Except[_List]:>{}])>0,
					If[OptionValue@Verbose,PrintTemporary@"Loading Wavefunctions..."];
					table::Load[],
					$Failed]
				]
			]
		];


BoundMethod[SaveWavefunctions,self_,eigensystem_,Mode->Automatic,Verbose->False]:=
With[{m=Replace[OptionValue@Mode,Automatic->self::StorageMode]},
	Switch[m,
		"File",
		With[{fil=self::WavefunctionsFile},
		With[{d=DirectoryName@fil},If[Not@FileExistsQ[d],CreateDirectory[d]]];
			Export[fil,Table[Flatten[f],{f,Transpose@eigensystem}]];
		],
		"Database",
		With[{table=self::WavefunctionsDataTable[True]},
			If[OptionValue@Verbose,PrintTemporary["Saving Wavefunctions..."]];
			table::Save[eigensystem]
			]
		];
	];


BoundMethod[LoadKineticMatrix,self_,grid_,Mode->Automatic,RecalculateKineticMatrix->False,Verbose->False,Save->None,OptionsPattern]:=
	(*First tries to find the appropriate file to load based on the KineticMatrixFile parameter and if that fails calculates one. Then based on the flag SaveKineticMatrices, it either saves and returns that or simply saves that*)
	With[{m=Replace[OptionValue@Mode,Automatic->self::StorageMode]},
		Module[
			{f=self::KineticMatrixFile,kf=self::KineticMatrix,
			kw=$KeyWordArguments,table,
			K,overWrite=OptionValue[RecalculateKineticMatrix]//TrueQ,saveFlag=OptionValue[Save]},

			If[m==="Database",table=self::KineticMatrixDataTable[overWrite]];

			If[saveFlag===None,saveFlag=self::SaveKineticMatrices];

			If[overWrite||Not@TrueQ@Switch[m,
						"File",FileExistsQ[f],"Database",
						("Value"/.Replace[table::ColumnLengths["Value"],Except[_List]:>{}])>0],
					If[OptionValue@Verbose,PrintTemporary@"Calculating Kinetic Energy Matrix..."];
					K=kf[grid,FilterRules[Normal@kw,Options[kf]]];
					If[saveFlag&&MatrixQ[K,NumberQ],
						Switch[m,
							"File",With[{d=DirectoryName@f},If[Not@FileExistsQ[d],CreateDirectory[d]]];Export[f,K],
							"Database",table::Save[K]
							]
					];,
					If[OptionValue@Verbose,PrintTemporary@"Loading Kinetic Energy Matrix..."];
						Switch[m,
							"File",K=Import[f],
							"Database",K=table::Load[]
								]
						];
				K]
		];


	BoundMethod[LoadPotentialMatrix,self_,gridORfile_:None,OptionsPattern[]]:=
		(*Here to later be overwritten by a child class, potentially*)
		Switch[Head@gridORfile,
				List,
					With[{V=self::PotentialMatrix,kw=$KeyWordArguments},V[gridORfile,FilterRules[Normal@kw,Options[V]]]],
				String,
					With[{V=self::FilePotential[gridORfile],kw=$KeyWordArguments},V[gridORfile,FilterRules[Normal@kw,Options[V]]]],
				Symbol,
					With[{file=If[gridORfile===None,self::PotentialFile,SymbolName@gridORfile]},
						With[{V=self::FilePotential[file],kw=$KeyWordArguments},V[gridORfile,FilterRules[Normal@kw,Options[V]]]]
					]
			];


(* ::Subsubsection::Closed:: *)
(*Bound properties: GetFiles, PotentialFiles, KineticMatrixFiles, WavefunctionFiles*)


(* ::Text:: *)
(*Returns the potential files, kinetic matrix files, and wavefunction files this DVR could use*)


BoundMethod[GetFiles,self_,directorySpec_:"Kinetic",extensions:_String|_List|All:All]:=
	With[{d=self::FindDirectory[directorySpec]},
		With[{base=FileNameJoin@{d,self::DVRFile,"*"}},
			FileNames@(Replace[extensions,{All->base,
												s_String:>base~~"."<>StringTrim[s,"."],
												l:{_String..}:>base~~Alternatives@@("."<>StringTrim[#,"."]&/@l)
									}])
		]
	]


BoundMethod[PotentialFiles,self_,extensions:_String|_List|All:"xlsx"]:=
	self::GetFiles["Potentials",extensions]


BoundMethod[KineticMatrixFiles,self_,extensions:_String|_List|All:"csv"]:=
	self::GetFiles["Kinetic",extensions]


BoundMethod[WavefunctionFiles,self_,extensions:_String|_List|All:"csv"]:=
	self::GetFiles["Wavefunctions",extensions]


(* ::Subsubsection::Closed:: *)
(*Bound method: FilePotential*)


(* ::Text:: *)
(*Takes a .xlsx file and turns it into a potential energy matrix. File should be formatted like Jimmy Yu and Gillian Lupinski did in their thesis projects.*)


BoundMethod[FilePotential,self_,gridfile_?((MatchQ[#,_Integer]||StringMatchQ[#,___~~".xlsx"])&),
				PotentialSheetIndex->1,
				PotentialPointConversion->Identity,
				PotentialPointExtension->None,
				PotentialSortingFunction->Automatic,
				Verbose->False]:=
		(*Collect the grid points and potential values and correlate them
			\[CenterDot]Figure out which column is which based on some built-in map
			\[CenterDot]Collect the points
			\[CenterDot]Order points with based on some prioritization map*)
		Module[{pointLabels=self::PointsPattern,file=Replace[gridfile,n_Integer:>self::PotentialFiles[][[n]]]},
			If[!FileExistsQ[file],file=self::FindFile[file,"Potential"]];
			If[FileExistsQ@file,
			If[OptionValue@Verbose,PrintTemporary@"Loading Potential Energy Matrix..."];
			Module[{
				N=self::Dimension,grid,conversion=OptionValue@PotentialPointConversion,
				data=Import[file][[OptionValue[PotentialSheetIndex]]],
				newFunction=Replace[OptionValue@PotentialPointExtension,
								Symmetric[s:({(_Integer|_String)..}|_Integer|_String)]:>
									With[{symList=With[{symPat=Replace[self::PointsPattern,l_:>Thread[l->Array[#&,Length@l]]]},Table[Replace[pos,symPat],{pos,Flatten[{s}]}]],
											rng=((#2(*-#1*))&@@@self::Range)},
									With[{add=Append[Array[Replace[#,{Alternatives@@symList:>rng[[#]](*/2*),_->\[Infinity]}]&,Length@rng],\[Infinity]]},
										({##}+((add-2{##})/.DirectedInfinity[_]:>0)&)
									]
									]],
				sorting=OptionValue@PotentialSortingFunction,
				(*sheets=Import[file,"Sheets"],*)
				keys=Append[self::PointsPattern,("Energy"|"E"|"energy")~~__],
				columns,
				values,
				sowF,
				pointF,
				points,
				gpf=self::FormatGrid,
				head
				},
				head=data[[1]];
				columns=With[{p=Position[head,#]},If[Length[p]>0,p[[1,1]],-1]]&/@keys;
				Module[{i,L=Length[columns]},For[i=1,i<=L,i++,
					Module[{j=i},
						If[keys[[i]]==-1,
							While[MemberQ[columns,j],j=Mod[j+1,L,1]];
							columns[[i]]=j
							]
						]
					]
				];
				data=Reap[Do[If[AllTrue[c,NumericQ],Sow[c[[#]]&/@columns]],{c,data}]][[2,1]];
				
				data=Replace[conversion,{
					l:({(_Rule|_RuleDelayed)..}|_Association):>With[{symPat=Replace[self::PointsPattern,lis_:>Thread[lis->Range[Length@lis]]]},
							With[{coordinateTransformations=Range[N+1]/.Append[Normal[l]/.symPat,_Integer->Identity]},
								Table[MapThread[#1@#2&,{coordinateTransformations,pos}],{pos,data}]
								]
							],
					Except[None|Identity]:>conversion@@@data,
					_->data
						}];
				(*Print@data;*)
				(*Print@newFunction;*)
				If[newFunction=!=None,data=DeleteDuplicatesBy[data~Join~With[{d=Apply[newFunction,data,{1}]}, Flatten[d,(Depth@d)-3] ],(* Identity*)#[[;;-2]]& ]];
				(*Print@data;*)
				pointF[plist_List]:=Reap[Module[{last="string"},Do[If[!TrueQ@(x==last),Sow@x];last=x,{x,plist}]]][[2,1]];
				points=Array[Sort@DeleteDuplicates@data[[All,#]]&,N];
				data=Replace[sorting,{None:>data,Automatic:>Sort[data],f_:>SortBy[data,f@@#&]}];
				grid=data[[All,;;3]];data=data[[All,4]];
				Do[grid=Array[grid[[(#-1)*n+1;;#*n]]&,Floor[Length@grid/n]],{n,(Length/@points//Reverse)[[;;-2]]}];
				{gpf[grid,Length/@points],data,Length/@points}
				],
			Message[DVRBaseClass::nopot,file];
			$Failed
			]
			];


(* ::Subsubsection::Closed:: *)
(*Bound method: RunDVR*)


BoundMethod[RunDVR,self_,
	Verbose->False,
	RunPoints->Automatic,
	ReturnPoint->Plot,
	ProcessObject->False,
	Grid->Automatic,
	KineticMatrix->Automatic,
	PotentialMatrix->Automatic,
	DVRUnits->Automatic,
	Wavefunctions->Automatic,
	StoreResults->None,
	StorageMode->Automatic,
	ShowCompound->False,
	RecalculateWavefunctions->False,
	RecalculateKineticMatrix->False,
	OptionsPattern]:=

	Module[{GRID,KINETICMATRIX,POTENTIALMATRIX,WAVEFUNCTIONS,
		kw=$KeyWordArguments,
		verboseFlag,unitValueMap,
		args=self::Arguments,
		pf=self::Plot,repKeys,repMap,
		pnts,end,continue=True,ret,
		plot,replacementPattern},
		Replace[kw[StorageMode],{Automatic:>(kw[StorageMode]=self::StorageMode),e_:>(self::StorageMode=e)}];
		If[kw[ProcessObject]=!=False,kw[Verbose]=False];
(*Create the map for checking whether a replacement has been done for a given key*)
		kw=Merge[{kw,args},First];
		repKeys={
			Grid|"Grid"|"grid",
			PotentialMatrix|"PotentialMatrix",
			KineticMatrix|"KineticMatrix",
			Wavefunctions|"Wavefunctions"};
		repMap=Thread[repKeys->repKeys];
		unitValueMap=(Replace[Replace[Replace[kw[DVRUnits],Automatic->self::DVRUnits],l_List:>Association@@l],
				{Ac_Association:>
					Block[{unitMapping=KeyMap[(#/.repMap&),Ac]},
						Do[unitMapping[key]=Replace[unitMapping[key],_Missing->Replace[unitMapping[All],_Missing:>None]],{key,repKeys}];
						unitMapping],
				o_:>(Association@@Thread[repKeys->None])}]);
		KeyDropFrom[kw,DVRUnits];
		replacementPattern[key_]:=((Normal@kw)~Append~Replace[unitValueMap[key/.repMap],{(Automatic|Null|None|_Missing):>(None->None),v_:>(DVRUnits->v)}]);
		end=kw[ReturnPoint];
(*Set the grid point divisions for the run. In verbose mode prints a header.*)
		pnts=Replace[kw[RunPoints],{Automatic:>self::Points,e_:>(self::Points=e;e)}];
		verboseFlag=kw[Verbose];
		If[verboseFlag&&end===Plot,
			If[kw[ProcessObject]===False,
			Print[StringForm["DVR: `` Points: ``",self::DVRName,
				StringJoin[
					Flatten@{"< ",
							(MapThread[ToString@StringForm["``:``",#1,#2]&,
							{self::PointsPattern[[All,1]],pnts}]~Riffle~", "),
							" >"}]]
				]]
			];
		self::DVROptions=kw;
(*In verbose mode, prints a dynamically updating progress grid*)
		Clear@Evaluate[self::MonitoringString];
		If[verboseFlag=!=False,
			With[{sym=self::MonitoringSetter,g=self::MonitoringGrid},
				If[verboseFlag//TrueQ,PrintTemporary[Dynamic[Grid[g,Alignment->Left]]]];
				sym={
					{"Grid",If[MatchQ[kw[Grid],_List],"\[CheckmarkedBox]","\[Square]"]},
					{"Kinetic Matrix",If[MatchQ[kw[KineticMatrix],_List],"\[CheckmarkedBox]","\[Square]"]},
					{"Potential Matrix",If[MatchQ[kw[PotentialMatrix],_List],"\[CheckmarkedBox]","\[Square]"]},
					{"Wavefunctions",If[MatchQ[kw[Wavefunctions],_List],"\[CheckmarkedBox]","\[Square]"]},
					{"Elapsed Time",DynamicModule[{base=TimeObject[][[1]],h,m,s},
						
						Dynamic[{h,m,s}=TimeObject[][[1]]-base;
								If[s<0,s=60-s;m-=1];
								If[m<0,m=60-m;h-=1];
								"``:``:``"~TemplateApply~{h,IntegerString[m,10,2],NumberForm[s,4]},
									UpdateInterval->.01],TrackedSymbols:>{}]
						}
					}
				]
			];


(*----------------------------Grid------------------------------*)

(*Calculates the grid*)
		GRID=Replace[kw[Grid],
			Automatic:>
				With[{gf=self::GridPoints},
					With[{ops=FilterRules[replacementPattern[Grid],Options[gf]]},
						If[Length[ops]==0,
							gf@@Join[pnts,self::Range],(*pnts is the number of grid point divisions for each dimension*)
							If[(DVRUnits/.ops)=!=DVRUnits,unitValueMap[Grid/.repMap]=None];
							gf[Sequence@@pnts,ops]]
						]
					]
				];
(*If a unit is specified for the grid in DVRUnits 
		AND 
the grid generating function does not support DVRUnits as an option*)
		If[unitValueMap=!=None,With[{g=unitValueMap[Grid/.repMap]},If[g=!=None,GRID=g*GRID]]];

(*Stores grid if StoreResults is true*)
		If[kw[StoreResults],With[{sym=self::ResultsSetter},sym=GRID]];
(*If the specified return point is Grid, break and return the grid*)
		If[end==Grid,ret=GRID;Return@ret];

(*Tells the dynamic monitor that the grid has been calculated*)
		If[verboseFlag=!=False,
			With[{sym=self::MonitoringSetter,mg=self::MonitoringGrid},
				sym=ReplacePart[mg,{1,2}->"\[CheckmarkedBox]"]
				]
			];

(*---------------------WAVEFUNCTIONS--------------------*)

(*Loads old wavefunctions, if possible, and if RecalculateWavefunctions is False*)
		If[Not@kw[RecalculateWavefunctions],
			If[kw[Wavefunctions]===Automatic,
				kw[Wavefunctions]=With[{w=self::LoadWavefunctions},
							w[FilterRules[replacementPattern[Wavefunctions],Options@w]]
							]
				]
			];

(*----------------------------KINETIC-----------------------------*)

		KINETICMATRIX=Replace[kw[Wavefunctions],
			Automatic|$Failed:>
			Replace[kw[KineticMatrix],{
				km:Automatic|"Overwrite":>
					With[{kf=self::LoadKineticMatrix},
						If[km==="Overwrite",kw[RecalculateKineticMatrix]=True];
							With[{ops=FilterRules[replacementPattern[KineticMatrix],Options[kf]]},
								If[Length[ops]==0,
									kf[GRID],
									If[(DVRUnits/.ops)=!=DVRUnits,unitValueMap[KineticMatrix/.repMap]=None];
									kf[GRID,ops]]
								]
							],
						o_:>o}]
				];

		If[unitValueMap=!=None,With[{u=unitValueMap[KineticMatrix/.repMap]},If[u=!=None,KINETICMATRIX=u*KINETICMATRIX]]];
		
		If[end==KineticMatrix,ret=KINETICMATRIX;Return@ret];

		If[kw[StoreResults],With[{sym=self::ResultsSetter},sym=KINETICMATRIX]];

		If[verboseFlag=!=False,
			With[{sym=self::MonitoringSetter,mg=self::MonitoringGrid},
				sym=ReplacePart[mg,{2,2}->"\[CheckmarkedBox]"]
				]
			];

(*-------------------------POTENTIAL-----------------------------*)

		POTENTIALMATRIX=Replace[kw[PotentialMatrix],
					Automatic:>
					With[{vf=self::LoadPotentialMatrix},
						With[{ops=FilterRules[replacementPattern[PotentialMatrix],Options[vf]]},
						If[Length[ops]==0,
							vf[GRID],
							If[(DVRUnits/.ops)=!=DVRUnits,unitValueMap[PotentialMatrix/.repMap]=None];
							vf[GRID,ops]]
							]
					]]//Replace[Normal@#,{m_?MatrixQ:>DiagonalMatrix@Diagonal[m],o_:>DiagonalMatrix[o]}]&;

		If[unitValueMap=!=None,With[{unitscaling=unitValueMap[PotentialMatrix/.repMap]},If[unitscaling=!=None,POTENTIALMATRIX=unitscaling*POTENTIALMATRIX]]];

		If[end==PotentialMatrix,ret=POTENTIALMATRIX;Return@ret];

		If[kw[StoreResults],With[{sym=self::ResultsSetter},sym=POTENTIALMATRIX]];

		If[verboseFlag=!=False,
			With[{sym=self::MonitoringSetter,mg=self::MonitoringGrid},
				sym=ReplacePart[mg,{3,2}->"\[CheckmarkedBox]"]
				]
			];

(*------------------------WAVEFUNCTIONS-------------------------*)

		WAVEFUNCTIONS=Replace[kw[Wavefunctions],
			Automatic|$Failed:>
				With[{w=
					With[{wf=self::Wavefunctions},
							With[{ops=FilterRules[replacementPattern[Wavefunctions],Options[wf]]},
								If[kw@Verbose,PrintTemporary@"Calculating Wavefunctions..."];
							If[Length[ops]==0,
								wf[KINETICMATRIX,POTENTIALMATRIX],
								If[(DVRUnits/.ops)=!=DVRUnits,unitValueMap[Wavefunctions/.repMap]=None];
								wf[KINETICMATRIX,POTENTIALMATRIX,ops]]]
							]},
						self::SaveWavefunctions[w];
						w
						]
					];
		If[unitValueMap=!=None,With[{g=unitValueMap[Wavefunctions/.repMap]},If[g=!=None,WAVEFUNCTIONS=g*WAVEFUNCTIONS]]];
		
		If[end==Wavefunctions,ret=WAVEFUNCTIONS;Return@ret];
		
		If[kw[StoreResults],With[{sym=self::ResultsSetter},sym=WAVEFUNCTIONS]];
		
		If[verboseFlag=!=False,
			With[{sym=self::MonitoringSetter,mg=self::MonitoringGrid},
				sym=ReplacePart[mg,{4,2}->"\[CheckmarkedBox]"]
				]
			];

(*--------------------PLOT-----------------------*)
		If[kw[ShowCompound],
			With[{o=self::Compound::DrawOb},
				kw[AddTo]=With[{drob=o[FilterRules[Normal@kw,Options@o]]},
					Replace[kw[AddTo],
						_Missing:>drob
						m:Except[_Missing]:>{m,drob}
						]]
				]
			];
		plot=Replace[pf[WAVEFUNCTIONS,GRID,POTENTIALMATRIX,FilterRules[Normal@kw,Options@pf]],
				o_?(kw[ProcessObject]=!=False):>{o}
				];
		If[end==Plot,ret=plot;Return@ret];

(*---------------RETURN RET--------------------*)
		ret
		];


(* ::Subsubsection::Closed:: *)
(*Bound method: RunPotentialFile*)


(* ::Text:: *)
(*Uses RunDVR to run, but taking a potential file as potential energy matrix*)


	BoundMethod[RunPotentialFile,
		self_,pFile_?((MatchQ[#,_Integer]||MemberQ[{"xlsx"},FileExtension@#])&),OptionsPattern
		]:=
		With[{kwargs=$KeyWordArguments},
			With[{G=
				With[{P=self::FilePotential},
					P[pFile,FilterRules[Normal@kwargs,Options@P]]
					],
				R=self::RunDVR},
				If[MatchQ[G,_List],
					self::Arguments[PotentialFunction]=Replace[pFile,n_Integer:>self::PotentialFiles[][[n]]];
					R[Grid->G[[1]],
						PotentialMatrix->With[{v=G[[2]]},DiagonalMatrix@v],
						RunPoints->G[[3]],
						PotentialFunction->pFile,
						FilterRules[Normal@kwargs,Except[Grid|PotentialMatrix|RunPoints]]
						],
					Message[DVRBaseClass::nopot,pFile];
					G
					]
				]
			];


(* ::Subsubsection::Closed:: *)
(*Bound method: RunInBackground*)


BoundMethod[RunInBackground,self_,
				fileName:_?(StringQ[#]&&StringMatchQ[#,___~~".xlsx"]&)|None:None,
				ReturnPoint->Plot,
				"Script"->False,
				"Server"->False,
				Verbose->False,
				Quiet->None,
				OptionsPattern]:=
	Switch[{OptionValue@"Script",OptionValue@"Server"},
		{_,True},
			With[{f=self::MakeRunScript["backgroundDVRRunScript"<>self::LookupKey["Kinetic",True],Function->Replace[fileName,{None->"RunDVR",_String:>"RunPotentialFile"}],"FunctionArguments"->{Replace[fileName,None->Sequence[]]}],
					runScript=FileNameJoin@{$DVRRoot,"code","DVRServerRun.sh"},
					server=If[HasAttribute[self,"Server"],
								self::Server,
								Replace[
									DialogInput[{serverShare="<USERNAME>@romulus.amherst.edu"},
												Grid[{{"Input your server share:",InputField[Dynamic@serverShare,String]},
														{Button["OK",DialogReturn@serverShare]}}
														Alignment->Left]],{
										r:Except[""|"<USERNAME>@romulus.amherst.edu"]:>(self::Server=r)
										_->$Failed}]
									]
									},
				If[server=!=$Failed,
					self::ServerProcess=StartProcess[runScript,server,
						FileNameJoin@{Replace[
							FileNameSplit@$ContextFile,
							{___,"Mathematica",e___}:>{"Mathematica",e}],
							"backgroundDVRRunScript"<>self::LookupKey["Kinetic",True]}],
					$Failed]
				]
		{True,_},(*Background on current machine, via script*)
		ParallelProcessClass::RunScript[
			self::MakeRunScript["backgroundDVRRunScript"<>self::LookupKey["Kinetic",True],Function->Replace[fileName,{None->"RunDVR",_String:>"RunPotentialFile"}],"FunctionArguments"->{Replace[fileName,None->Sequence[]]}]
				],
		{_,_},(*Background on current machine, via Mathematica's built in parallelization stuff*)
		With[{kwargs=$KeyWordArguments,sym=Symbol@self,
			r=self::RunDVR,func=If[fileName===None,self::RunDVR,self::RunPotentialFile],
			n=EvaluationNotebook[],evc=EvaluationCell[]},
		With[{kwargSyms=Sequence@@Cases[Normal@kwargs,_Symbol,\[Infinity]],fsym=Function@func,
			context=self::DVRFile<>"`",
			fcontexts=Sequence@@(Context/@(GetAttribute[self,#]&/@{"GridPoints","PotentialMatrix","KineticMatrix","Wavefunctions","Plot"})//DeleteDuplicates)},
			With[{p=ParallelProcess[
							func[
									Replace[fileName,None:>Sequence[]],
									Sequence@@Flatten@{Normal@kwargs,ProcessObject->True}],
							SharedPackages[
									{"ObjectOrientedProgramming`",ObjectOrientedProgramming`Private`$ContextFile},
									{"DiscreteVariableRepresentation`",$ContextFile}],
							SharedDefinitions["ObjectOrientedProgramming`",(*context,*)fcontexts,sym,fsym,kwargSyms]
							(*SuccessResult\[Rule]Function[{s,r},
										SelectionMove[evc,After,Cell];
										NotebookWrite[n,Cell[BoxData@ToBoxes@r,"Output"]]
										]*)
							]},
				self::RunProcess=p;
				p::Run[Verbose->OptionValue@Verbose,Quiet->OptionValue@Quiet]
				]
			]
		]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: MakeRunScript*)


BoundMethod[MakeRunScript,self_,fileBaseName:_String|None:None,arch:"Windows"|"Unix"|Automatic:Automatic,
		Directory->Automatic,Function->"Run","FunctionArguments"->{}]:=
With[{saveFile=SaveObject[self,Directory->FileNameJoin@{$DVRRoot,"Resources","Run Scripts"}]},
With[{string=
With[{cmd=
		TemplateApply[StringTemplate["
RunScript::needs=\"Requires package <*\"`\"*><*\"`\"*> in some enclosing folder\"
Block[{fileHierarchy=FileNameSplit@$InputFileName},
	Replace[Do[Replace[FileNameJoin@Append[fileHierarchy,\"ObjectOrientedProgramming\"],{
						d_?DirectoryQ:>Return[d],
						_:>(fileHierarchy=Delete[fileHierarchy,-1])
						}],{i,fileHierarchy}],{
				s_String:>Needs[\"ObjectOrientedProgramming<*\"`\"*>\",FileNameJoin@{s,\"init.m\"}],
				Null\[RuleDelayed]Message[RunScript::needs,\"ObjectOrientedProgramming\"]
				}]
			];
Block[{fileHierarchy=FileNameSplit@$InputFileName},
	Replace[Do[Replace[FileNameJoin@Append[fileHierarchy,\"DiscreteVariableRepresentation\"],{
						d_?DirectoryQ:>Return[d],
						_:>(fileHierarchy=Delete[fileHierarchy,-1])
						}],{i,fileHierarchy}],{
				s_String:>Needs[\"DiscreteVariableRepresentation<*\"`\"*>\",FileNameJoin@{s,\"init.m\"}],
				Null\[RuleDelayed]Message[RunScript::needs,\"DiscreteVariableRepresentation\"]
				}]
			];

With[{dvrInstance=LoadObject@`saveFile`},
	dvrInstance::`function`@@`args`
]",InsertionFunction->(ToString[#,InputForm]&)],
<|
	"saveFile"->saveFile,
	"function"->Replace[OptionValue@Function,Except["RunDVR"|"RunPotentialFile"]:>"RunDVR"],
	"args"->Replace[OptionValue@"FunctionArguments",{a_Association:>Normal@a,o:Except[_List]:>ReplacePart[o,0->List]}]
|>]},


Switch[Replace[Replace[arch,Automatic:>"Unix"],Except["Windows"|"Unix"]:>"Unix"],
"Unix",
	"#! usr/local/bin/WolframScript -script\n\n"<>cmd,
"Windows",
	Throw["NotImplemented"]
]]},
If[fileBaseName=!=None,
With[{dir=Replace[OptionValue@Directory,Automatic:>FileNameJoin@{$DVRRoot,"Resources","Run Scripts"}]},
		If[Not@DirectoryQ@dir,CreateDirectory@dir];
		With[{fileObj=OpenWrite[FileNameJoin@{dir,fileBaseName<>".m"}]},
			WriteLine[fileObj,string];
			Close@fileObj
			]
		],
	string]
]
];


(* ::Subsubsection::Closed:: *)
(*Bound property: Interface*)


BoundProperty[Interface,self_]:=(self::Interface=DVRInterfaceClass[self]);


(* ::Subsubsection:: *)
(*Load function*)


PostLoadFunction[self_]:=Quiet[
	With[{sym=Symbol@self},AppendTo[sym[ObjectOrientedProgramming`Private`parentClassesKey],DVRBaseClass]];
	With[{t=ObjectType@self},MathematicaClass[t]::Database=None];
	self::GetDatabase[];
	self::Initialize[True]]


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Subsection::Closed:: *)
(*KineticMatrixTableClass*)


LoadDatabaseType["Matrix"]


KineticMatrixTableClass:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Extends MatrixTable*)


ParentClasses={MatrixTableClass};


(* ::Subsubsection::Closed:: *)
(*$Init*)


InitializationFunction[self_,database:Automatic|_?(IsInstance[#,DatabaseClass]&):Automatic,
				name_,cols:Automatic:Automatic,dvrInstance_?(IsInstance[#,DVRBaseClass]&),ops___]:=
(
	self::DVR=dvrInstance;
	Superclass[]::$Init[dvrInstance::Database,name,{"Property_Name"->"General"|Null,"Property_Value"->"General"|Null}];
	);


(* ::Subsubsection::Closed:: *)
(*Bound method: Save*)


BoundMethod[Save,self_,kineticMatrix_]:=With[{properties=Sort@self::DVR::SaveAttributes["kinetic"]},
	If[!self::Database::IsOpen[],self::Database::Open[]];
		Superclass[]::Save[kineticMatrix,
			"Property_Name"->(GeneralData[First@#]&/@properties),
			"Property_Value"->(GeneralData[Last@#]&/@properties)];
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: View*)


BoundMethod[View,self_,func_:Automatic]:=
	Replace[func,{
		Automatic:>With[{mx=self::Load[]},
				Replace[mx,{
					None:>"NO DATA",
					_:>Interpretation[ArrayPlot@mx,mx]}]
					],
		_:>Superclass[]::View[func]
		}]


(* ::Subsubsection::Closed:: *)
(*Bound property: Properties*)


BoundProperty[Properties,self_]:=With[{namesProps=self::Select[{"Property_Name","Property_Value"}]},
	Prepend[
		(Rule@@(ToExpression/@#)&)/@DeleteCases[namesProps,{Null,Null}]/.DatabaseLink`SQLExpr[x_]:>x,
		"Kinetic Matrix"->""
		]
	];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Subsection::Closed:: *)
(*WavefunctionTableClass*)


WavefunctionTableClass:=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*Extends MatrixTableClass*)


ParentClasses={MatrixTableClass};


(* ::Subsubsection::Closed:: *)
(*$Init*)


InitializationFunction[self_,database:Automatic|_?(IsInstance[#,DatabaseClass]&):Automatic,
			name_,cols:Automatic:Automatic,dvrInstance_?(IsInstance[#,DVRBaseClass]&),ops___]:=(
	self::DVR=dvrInstance;
	Superclass[]::$Init[dvrInstance::Database,name,{"Property_Name"->"General"|Null,"Property_Value"->"General"|Null,"Energies"->("Real"|Null)}]
	);


(* ::Subsubsection::Closed:: *)
(*Bound method: Save*)


BoundMethod[Save,self_,wavefunctionMatrix_]:=With[
	{properties=Sort@self::DVR::SaveAttributes["wavefunctions"],energies=wavefunctionMatrix[[1]]},
	If[!self::Database::IsOpen,self::Database::Open[]];
	Superclass[]::Save[
		wavefunctionMatrix[[2]],
		"Property_Name"->(GeneralData[First@#]&/@properties),
		"Property_Value"->(GeneralData[Last@#]&/@properties),
		"Energies"->energies ]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: Load*)


BoundMethod[Load,self_]:=With[{l={Flatten[self::Select["Energies"]]~DeleteCases~Null,Superclass[]::Load[]}},l];


(* ::Subsubsection::Closed:: *)
(*Bound method: View*)


BoundMethod[View,self_,func_:Automatic]:=
	Replace[func,{
		Automatic:>With[{mx=self::Load[]},
				Replace[mx,{
					None:>"NO DATA",
					_:>Interpretation[ArrayPlot/@mx//Row,mx]
					}]],
		_:>Superclass[]::View[func]
		}]


(* ::Subsubsection::Closed:: *)
(*Bound property: Properties*)


BoundProperty[Properties,self_]:=With[{namesProps=self::Select[{"Property_Name","Property_Value"}]},
	Prepend[
	(Rule@@(ToExpression/@#)&)/@DeleteCases[namesProps,{Null,Null}]/.DatabaseLink`SQLExpr[x_]:>x,
	"Wavefunctions"->""]
	];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[]


(* ::Subsection:: *)
(*Interface Class*)


DVRInterfaceClass=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*$Init*)


InitializationFunction[self_,
	dvrInstance:_?(IsInstance[#,DVRBaseClass]&):None,
	symbol:(Dynamic[_Symbol]|None):None]:=(
		self::DVR=dvrInstance;
		self::Symbol=Replace[symbol,Dynamic[s_]:>HoldPattern[s]]
		);


(* ::Subsubsection::Closed:: *)
(*Bound method: DictPanel*)


BoundMethod[DictPanel,self_,
	initDict:_Association:<||>,
	args:(_List|False):False,
	title:_String:"Attribute Notebook"]:=
DialogInput@
With[{inst=self::DVR},
	Module[{attrs=initDict},

		Notebook[
			{Cell[title,"Section"],
				Cell["DVR","Subsection"],
				Cell["MathematicaClass["<>ToString@self<>"]","InputOnly",Evaluatable->False]}
				~Join~
				Replace[args,{False:>{},l_List:>({Cell["Arguments","Subsection",DefaultNewCellStyle->"ListInput"],
												Cell["Enter list elements below, one element per cell","Text"]}
													~Join~
												(Cell[BoxData@#,"ListInput"]&/@(ToBoxes/@l)))}]
				~Join~
				{Cell["Attribute Rules","Subsection",DefaultNewCellStyle->"RuleInput"],
				Cell["Enter rules below, one rule per cell","Text"]}
				~Join~
			Table[
				Cell[BoxData@ToBoxes@k,"RuleInput"],{k,Normal@attrs}
				]
				~Join~
			{Cell[BoxData@ToBoxes@
				Button["Save Changes",
					(*SetOptions[EvaluationNotebook[],Visible\[Rule]False];*)
					With[{cells=NotebookRead/@Cells@EvaluationNotebook[]},
						With[{listCells=Cases[cells,Cell[BoxData[v_],"ListInput",___]:>v],
								ruleCells=Cases[cells,Cell[BoxData[v_],"RuleInput",___]:>v]},
							If[args=!=False,
								(ToExpression@BoxData@RowBox@#&)/@{
													Append[Prepend[Riffle[listCells,","],"{"],"}"],
													Prepend[Append[Riffle[ruleCells,","],"|>"],"<|"]}//DialogReturn,
								ToExpression@BoxData@RowBox@Prepend[Append[Riffle[ruleCells,","],"|>"],"<|"]//DialogReturn;
							]
						]
					],
					Method->"Preemptive"],"InputOnly"]},
				WindowTitle->"Attribute Notebook",
				WindowSize->{700,700},
				Saveable->False,
				WindowFloating->True,
				StyleDefinitions->Notebook[{
					Cell[StyleData[StyleDefinitions->"Default.nb"]],
					Cell[StyleData[All],DefaultNewCellStyle->"RuleInput"],
					Cell[StyleData["ListInput",StyleDefinitions->StyleData["Code"]],
							CellDingbat->"\[CenterDot]",
							CellMargins->{{86,0},{0,0}},
							Evaluatable->False],
					Cell[StyleData["RuleInput",StyleDefinitions->StyleData["Code"]],
							CellDingbat->"\[CenterDot]",
							CellMargins->{{86,0},{0,0}},
							Evaluatable->False]
					}]
				]
			]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: GridPointsSelector*)


BoundMethod[GridPointsSelector,self_]:=
With[{dvr=self::DVR},
	Module[{gridnum=dvr::Points,
			d=dvr::Dimension,range=dvr::Range,gf=dvr::GridPoints,patternToG,pSwitch=True,
			pl=dvr::PointsPattern[[All,1]],fInitializers,P,i=1,gP,cF=Hue,
			lin=("x"|"X"|"y"|"Y"|"z"|"Z"|"linear"|"Linear"|"r"|"R"|"radial"|"Radial"),
			ang="\[CapitalPhi]"|"\[CurlyPhi]"|"\[Phi]"|"azimuthal"|"Azimuthal",
			azim="\[Theta]"|"\[CapitalTheta]"|"angular"|"Angular"},
		fInitializers=Switch[pl,
			{lin},{{"x"},"x^2"},
			{ang},{{"\[CurlyPhi]"},"Sin[\[CurlyPhi]]"},
			{azim},{{"\[Theta]"},"Cos[\[Theta]]"},
			{lin,lin},{{"x","y"},"x+y"},
			{ang,azim},{{"\[CurlyPhi]","\[Theta]"},"Sin[\[CurlyPhi]]*Cos[\[Theta]]"},
			{lin,ang},{{"r","\[CurlyPhi]"},"r^2 Sin[\[CurlyPhi]]"},
			{lin,azim},{{"r","\[Theta]"},"r^2 Cos[\[Theta]]"},
			{lin,lin,lin},{{"x","y","z"},"x*y*z"},
			{lin,ang,azim},{{"r","\[CurlyPhi]","\[Theta]"},"r^2 Sin[\[CurlyPhi]]*Cos[\[Theta]]"},
			_,{{},None}
			];
		patternToG[grid_,pointFunction_,pot_:None]:=
			With[{gr=(With[{G=gf@@Join[grid,range]},i=1;If[Length@G==2,G[[1]],G]])//Flatten[#,Max@{Depth[#]-3,0}]&},
				With[{pList=If[cF=!=None&&pot=!=None,
							With[{pFunction=pot},Replace[pFunction,{Except[_String]:>pFunction@@@gr}]],
							None]},
					With[{minP=If[pList=!=None,Min@pList],maxP=If[pList=!=None,Max@pList]},
						Module[{pMode,D=Replace[maxP-minP,0->1]},
							(With[{p=pointFunction[Sequence@@#]},
								If[pList=!=None,{cF@((pList[[i++]]-minP)/D),p},p]
							]&/@gr)//If[d==3,Graphics3D[{#},Boxed->False],Graphics[{#}]]&
							]
						]
				]
			];
		gP[grid_,pot_:None]:=
			patternToG[grid,
			Switch[pl,
				{lin},Function[x,Point[{x,.5}]],
				{ang},Function[\[CurlyPhi],Point[{Cos[\[CurlyPhi]],Sin[\[CurlyPhi]]}]],
				{azim},Function[\[Theta],Point[{\[Theta],Sin[ArcCos[\[Theta]]]}]],
				{lin,lin},Function[{x,y},Point[{x,y}]],
				{ang,azim},Function[{\[CurlyPhi],\[Theta]},Point[{\[Theta] Sin[\[CurlyPhi]],Sqrt[1-\[Theta]^2]Sin[\[CurlyPhi]],Cos[\[CurlyPhi]]}]],
				{lin,ang},Function[{r,\[CurlyPhi]},Point[{r Cos[\[CurlyPhi]],r Sin[\[CurlyPhi]]}]],
				{lin,azim},Function[{r,\[Theta]},Point[{r \[Theta],r Sqrt[1-\[Theta]^2] }]],
				{lin,lin,lin},Function[{x,y,z},Point[{x,y,z}]],
				{lin,ang,azim},Function[{r,\[CurlyPhi],\[Theta]},Point[{r \[Theta] Sin[\[CurlyPhi]],r Sqrt[1-\[Theta]^2]Sin[\[CurlyPhi]],r Cos[\[CurlyPhi]]}]],
				_,(Point[Replace[#,p:Except[_List]:>{p,.5}]])&
				],pot];
		P=gP@gridnum;
		Grid@
			{
			{
			{
			pl,
			Array[InputField[
				Dynamic[range[[#,{1,2}]],Function[v,range[[#]]=v;P=gP[gridnum]]],FieldSize->6,Appearance->"Frameless"]&,d],
					Array[InputField[
						Dynamic[gridnum[[#]],Function[v,gridnum[[#]]=v;P=gP[gridnum]]],
						Number,FieldSize->6,Appearance->"Frameless"]&,d]
				}//Grid[#,Dividers->{All,{1->Black,2->Gray,3->Gray,-1->Black}}]&, 
			Column@{
				Button["Apply Potential",P=gP[gridnum,Replace[dvr::Arguments[PotentialFunction],_Missing:>(1&)]]],
				PopupMenu[Dynamic@cF,{Hue->"Hue",GrayLevel->"Gray Scale"}~Join~((ColorData[{#,{0,1}}]->#&)/@ColorData["Gradients"])]		
				}
			},
			{Dynamic[ExpressionCell[P,Deployed->False]],SpanFromLeft},
			{ Button["Save Changes",DialogReturn[dvr::Points=gridnum;dvr::Range=range]],SpanFromLeft}
			}
		]
	]//DialogInput[#,Selectable->True]&


(* ::Subsubsection::Closed:: *)
(*Static method: FunctionMaker*)


StaticMethod[FunctionMaker,Dynamic[symbol_Symbol],dimension_,Variables->{},Function->None,Initialization->1,Evaluate->False,Dynamic->False]:=
	DynamicModule[{podvars=With[{o=OptionValue@Variables},ToExpression[
				Array[If[#>Length@o,"x"<>If[#>1,ToString@#,""],o[[#]]]&,dimension],
				StandardForm,Hold]],pFunction,
			pod,rep,test=ConstantArray[OptionValue@Initialization,dimension]},
	
	{Row@{Array[
		InputField[With[{i=#},Dynamic[podvars[[i]]]],Hold[Expression],FieldSize->Infinity]&,dimension]//Row[#]&,
	pod=Replace[symbol,{f_Function:>
		With[{vars=Thread@Extract[f,1,Hold]},
			podvars[[1;;Length@vars]]=vars;
			Extract[f,2,Hold]
			],_->podvars[[1]]}];
	symbol=ReplacePart[With[{p=pod,var=podvars},HoldComplete[var,p]/.Hold[f_]:>f],0->Function];
	InputField[Dynamic[pod,
		Function[v,
			pod=v;
			symbol=ReplacePart[With[{p=pod,var=podvars},HoldComplete[var,p]/.Hold[f_]:>f],0->Function];
			],TrackedSymbols:>{pod,podvars}],Hold[Expression]]
			},
			If[OptionValue@Evaluate,Row@{SymbolName[Unevaluated@symbol],"[",
						Array[
					InputField[Dynamic[test[[#]],Function[v,test[[#]]=v],Hold[Expression]],
					FieldSize->Infinity,Appearance->"Frameless"]&,dimension]//Row[Riffle[#,","]]&,"]=",Dynamic[symbol@@test]},
				Null
				]
	}//If[OptionValue@Evaluate,Column,Row]//TextCell[#,"Input"]&
	
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: PotentialChooser*)


BoundMethod[PotentialChooser,self_]:=DialogInput@With[{dvr = self::DVR},
Module[{potential = Quiet[Check[dvr::Arguments[PotentialFunction],None],MathematicaClass::noattr]},
	Column@{
	TabView[{
	"Function"->
	Column@{
		self::FunctionMaker[Dynamic@potential,self::Dimension,Evaluate->True]
		},
	"File"->
	Column@{
		Dynamic@Replace[potential, Except[_String]:>"None chosen..."],
		FileNameSetter[Dynamic[potential]]
		}
	}],
	Button["Save Potential",DialogReturn[dvr::Arguments[PotentialFunction] = potential]]
	}
	]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: NewDVR*)


BoundMethod[NewDVR,self_,symbol:Dynamic[_Symbol]|None:None]:=
With[{mnum=$ModuleNumber//ToString},With[{mkSym=(Symbol[#<>mnum]&)},
With[{points=mkSym@"points",domain=mkSym@"domain",file=mkSym@"file",compound=mkSym@"compound",
		name=mkSym@"name",finalized=mkSym@"finalized",units=mkSym@"units",
		storagemode=mkSym@"storagemode",confdir=mkSym@"confidr",updateFiles=mkSym@"updateFiles",
		kindir=mkSym@"kindir",potdir=mkSym@"potdir",wfdir=mkSym@"wfdir",
		saveflg=mkSym@"kindir",fileexts=mkSym@"fileexts",relpkg=mkSym@"relpkg",
		ops=mkSym@"ops",useClass=mkSym@"useClass"},
	{points={},domain={{}},file,compound=None,name="",
		finalized=None,
		units=None,storagemode="Database",
			confdir=$ConfigFileDirectory,updateFiles,
			kindir=$KineticMatrixDirectory,
			potdir=$PotentialFileDirectory,
			wfdir=$WavefunctionFileDirectory,
		saveflg=True,fileexts={".xlsx"},relpkg=False,ops=<||>,
		useClass=DVRBaseClass};
	With[{g=
	Grid[{{Dynamic[Row[{Replace[file,_Symbol:>"No file chosen"],Replace[points,{Except[{__Integer}]:>"No point division",_->(points//MatrixForm)}],Replace[domain,{Except[{{_,_}..}]:>"No range",_:>(MatrixForm/@(List/@domain)//MatrixForm)}]}," : "//TextCell],
					TrackedSymbols:>{file,points,domain}],SpanFromLeft},
	{"DVR File:",Dynamic[PopupMenu[Dynamic[updateFiles;file,(file=#;&),TrackedSymbols:>{file}],
						Block[{fileNames},
							SetDirectory[$ConfigFileDirectory];
							fileNames=With[{ls=Last@FileNameSplit@#&/@FileNames[]},Cases[ls,s_?(StringMatchQ[FileExtension@#,"m"]&&!StringMatchQ[FileExtension@FileBaseName@#,"class"]&):>(s->FileBaseName@s)]];
							ResetDirectory[];
							fileNames]],
						TrackedSymbols:>{updateFiles}]//EventHandler[#,{"MouseClicked":>(updateFiles=RandomReal[])},PassEventsDown->True]&//Item[#,Alignment->Left]&,
				Button["New DVR",
					With[{nb=Get@FileNameJoin@{$DVRRoot,"Resources","Interfaces","NewDVRTemplate.nb"}},
						CreateDocument[nb,
							WindowTitle->"Untitled DVR",
							NotebookEventActions->{"WindowClose":>(updateFiles=RandomReal[])}]
						]
					]},
	{"DVR Class:",Dynamic[PopupMenu[Dynamic[updateFiles;useClass::Type,(With[{symStr=#},
															Replace[ToExpression@symStr,{
																ob_MathematicaClass:>(useClass=ob),
																s_:>(Quiet[Remove@s];Begin@$BaseContext;Get@FileNameJoin@{$ConfigFileDirectory,symStr<>".class.m"};End[];
																		useClass=ToExpression@symStr)
																}]]&),TrackedSymbols:>{useClass}],
						Block[{classNames},
							SetDirectory[$ConfigFileDirectory];
							classNames=With[{ls=Last@FileNameSplit@#&/@FileNames[]},Cases[ls,s_?(StringMatchQ[FileExtension@#,"m"]&&StringMatchQ[FileExtension@FileBaseName@#,"class"]&):>(FileBaseName@FileBaseName@s->FileBaseName@FileBaseName@s)]];
							ResetDirectory[];
							Prepend[classNames,"DVRBaseClass"->"Default"]]],
						TrackedSymbols:>{updateFiles}]//EventHandler[#,{"MouseClicked":>(updateFiles=RandomReal[])},PassEventsDown->True]&//Item[#,Alignment->Left]&},
	{"Grid Points:",InputField[Dynamic@points,Appearance->"Frameless"]~TextCell~"Input"},
	{"DVR Range:",InputField[Dynamic@domain,Appearance->"Frameless"]~TextCell~"Input"},
	{"Compound:",Dynamic[compound],Button["Change",Replace[Molecule::BuildInterface[],m:Except[$Failed]:>(compound=m)],ImageSize->Automatic,Method->"Queued"]},
	{Button["Finalize",
		With[{dvr=Quiet[useClass[points,domain,file,compound,
					DVRName->name,
					DVRUnits->units,
					StorageMode->storagemode,
					DVRDirectory->confdir,
					KineticMatrixDirectory->kindir,
					PotentialDirectory->potdir,
					WavefunctionDirectory->wfdir,
					SaveKineticMatrices->saveflg,
					FileExtension->fileexts,
					ReloadPackage->relpkg,
					Sequence@@Normal@ops],General::shdw]},
			self::DVR=dvr;
			With[{s=self::Symbol},finalized=True;If[s=!=None,s=dvr]]
		]
		]
		}},Alignment->Left,Dividers->{None,{2->Black,-2->Black}}]},
		Dynamic[If[finalized===None,g//Panel,self::DVRInterface[]],TrackedSymbols:>{finalized}]
		]]]
	];


(* ::Subsubsection::Closed:: *)
(*Bound method: DVRInterface*)


BoundMethod[DVRInterface,self_]:=With[{m=
	With[{mnum=$ModuleNumber//ToString},With[{mkSym=(Symbol[#<>mnum]&)},
	With[{title=mkSym@"title",points=mkSym@"points",potentialFunction=mkSym@"potentialFunction",
		output=mkSym@"output",statusFields=mkSym@"statusFields",DVRoptionsForm=mkSym@"DVRoptionsForm",
		runDVR=mkSym@"runDVR",modeSelect=mkSym@"modeSelect",runMode=mkSym@"runMode",RUNoptionsForm=mkSym@"RUNoptionsForm",
		runArgs=mkSym@"runArgs",runOps=mkSym@"runOps",displayPane=mkSym@"displayPane",setDVROptions=mkSym@"setDVROptions",
		setRUNOptions=mkSym@"setRUNOptions",leftButtonBar=mkSym@"leftButtonBar",potential=mkSym@"potential",
		verbFlag=mkSym@"verbFlag",verbCheck=mkSym@"verbCheck",procOb=mkSym@"procOb"},
		{
		title=TextCell[self::DVRFile,"Chapter"],
		points=self::DVR::Points,potentialFunction=Replace[self::Arguments[PotentialFunction],_Missing:>None],
		output=Graphics[Text@"",ImageSize->{250,250},Background->Lighter[Gray, 0.5]],
		runMode="RunDVR",runArgs={},runOps=Association@@Join@@(Options/@{self::RunDVR,self::RunPotentialFile,self::RunInBackground,
		procOb=Quiet[Check[self::ProcessObject,None]]})
		};
		displayPane=Framed@Pane[Dynamic@ExpressionCell[Interpretation[Replace[output,{{__?NumericQ}..}:>ArrayPlot@output],output],Selectable->True,Deployed->False,Editable->True]];

		DVRoptionsForm:=Replace[self::DictPanel[self::Arguments],e:Except[$Failed]:>(self::Arguments=e)];
		RUNoptionsForm:=Replace[self::DictPanel[runOps,runArgs],e:Except[$Failed]:>({runArgs,runOps}=e)];
		statusFields:={
			{"DVR Type:",self::DVRFile,SpanFromLeft},
			{"DVR:",self::DVRName,Item[Button["View System",DialogInput[Replace[self::Compound,c:Except[None]:>c::Draw[]],Selectable->True,Deployed->False];,Method->"Queued",Appearance->"Palette"]]},
			{"DVR Points:",Dynamic[StringJoin[
					Flatten@{"< ",
							(MapThread[TemplateApply["``:``",{#1,#2}]&,
							{self::PointsPattern[[All,1]],points}]~Riffle~", "),
							" >"}],TrackedSymbols:>{points}],
					Button["Set Points",self::GridPointsSelector[];points=self::DVR::Points,Method->"Queued",Appearance->"Palette"]},
			{"Potential:",Dynamic@potentialFunction,Button["Set Potential",self::PotentialChooser[];potentialFunction=Replace[self::Arguments[PotentialFunction],_Missing:>None],Method->"Queued",Appearance->"Palette"]},
			{"Data Storage Mode:",self::StorageMode,Button["Storage Info",CreateDialog@Grid@Join[{
						{"Root Directory:",self::DVRDirectory},
						{"Potential Directory:",self::PotentialDirectory}
						},
						Switch[self::StorageMode,
							"File",
							{
								{"Kinetic Matrix Directory:",self::KineticMatrix},
								{"Wavefunction Directory:",self::WavefunctionDirectory}
								},
							"Database",
							{
								{"Database",self::Database}
							}]],Appearance->"Palette"]},
			{"Process Object:",Dynamic[procOb],SpanFromLeft}
			};
		modeSelect=PopupMenu[Dynamic@runMode,
			{"RunDVR"->"Standard","RunInBackground"->"Background","RunPotentialFile"->"Potential"}];
		runDVR=Button["Run DVR",
							If[Quiet[self::DVR::Database::IsOpen]//Not,self::DVR::Database::Open[]];
							If[runMode=!="RunInBackground"&&verbFlag=!=False,output=(Dynamic@TableForm@self::DVR::MonitoringGrid)];
							output=GetAttribute[self::DVR,runMode][Sequence@@runArgs,Sequence@@Normal@Merge[{self::DVR::Arguments,runOps},First]];
							If[runMode=="RunInBackground",procOb=Quiet[Check[self::ProcessObject,None]]];,
					Method->"Queued",Appearance->"Palette"];
		setDVROptions=Button["DVR Options",DVRoptionsForm,Method->"Queued",Appearance->"Palette"];
		setRUNOptions=Button["Run Options",RUNoptionsForm,Method->"Queued",Appearance->"Palette"];
		verbCheck=Row@{"Verbose ",Checkbox[Dynamic[verbFlag,(runOps[Verbose]=#;verbFlag=#;&)],{False,Update}]};
		Panel@Grid[{
			{title,SpanFromLeft},
			{Grid[statusFields,Alignment->Left],SpanFromLeft},
			{Item[Column@{runDVR,modeSelect,Item[verbCheck,Alignment->Right]},Alignment->Top],Item[displayPane,Alignment->Top],SpanFromLeft},
			{Item[Column[{setDVROptions,setRUNOptions}],Alignment->{Right,Bottom}],SpanFromAbove,SpanFromBoth}
			},Alignment->Left,Dividers->{{},{2->Black,-3->Black}}]
		]]]},
	
	self::DVRInterface=StaticMethod[(m&)];
	m];


(* ::Subsubsection::Closed:: *)
(*Representation Function*)


RepresentationFunction[self_]:=If[self::DVR===None,Replace[Quiet[self::displayDVR],(Null|_None|None):>(self::displayDVR=self::NewDVR[])],self::DVRInterface[]];


(* ::Subsubsection:: *)
(*$GetAttr*)


NoAttributeFunction[self_,attr_]:=If[HasAttribute[self,"DVR"],GetAttribute[self::DVR,attr],Message[MathematicaClass::noattr,self,attr]];


(* ::Subsubsection:: *)
(*Post Load Function*)


PostLoadFunction[self_]:=(
		self::DVRInterface=With[{class=ObjectType@self},MathematicaClass[class]::DVRInterface];
		Replace[Quiet[self::DVR],o:Except[None|Null]:>Replace[Quiet[self::Symbol],s:Except[None]:>(s=o)]];
		)


(* ::Subsubsection::Closed:: *)
(*End*)


EndClass[]


(* ::Subsection:: *)
(*DVRNotebook*)


DVRInstanceHold~SetAttributes~HoldAll;
DVRNotebook:=
	With[{sessionKey=DateString[{"Year","Month","Day","Hour","Minute"}]},
	With[{dvrInstance=Symbol["FEDVR`dvrInstance$"<>sessionKey],
			loadFlag=Symbol["FEDVR`loadFlag$"<>sessionKey],
			interfaceSym=Symbol["FEDVRInterface$"<>sessionKey],
			saveFlag=Symbol["FEDVR`saveFlag$"<>sessionKey]},
		Format[DVRInstanceHold[dvrInstance]]:=Interpretation[
										Framed[Framed[TextCell["DVR","Input"],Background->RGBColor[0.704, 0.8, 0.904],FrameStyle->{Thick,LightBlue},RoundingRadius->5],FrameStyle->{Thick,RGBColor[0.4, 0., 0.6]},RoundingRadius->5,FrameMargins->None],
										dvrInstance];
		With[{interface=DVRInterfaceClass[Dynamic@dvrInstance,InstanceSymbolName->interfaceSym]},
			With[{doc=GenerateDocument[FileNameJoin@{$DVRRoot,"Resources","Interfaces","DVRInterfaceTemplate.nb"},
				<|"LoadFlag"->loadFlag,
					"DVRInterface"->With[{interSym=Symbol@interface},
									Style[Dynamic[
									Replace[loadFlag,
										_Symbol:>Replace[FileNameJoin@{Replace[Quiet[NotebookDirectory[]],$Failed:>"thisDirectoryProbablyDoesn'tExistIBet"],"Interface Instances",ToString@interfaceSym<>".zip"},
											f_?FileExistsQ:>(
													Needs["DiscreteVariableRepresentation`",
															FileNameJoin@{Nest[ParentDirectory,NotebookDirectory[],3],"init.m"}];
													Quiet[ObjectOrientedProgramming`LoadObject[f]]
													)]
											];
									If[ValueQ@interSym,loadFlag=True];
									interface,TrackedSymbols:>{loadFlag}],
									DynamicEvaluationTimeout->\[Infinity]]
									],
					"DVRInstance"->Interpretation[Dynamic[Replace[dvrInstance,_Symbol:>None],TrackedSymbols:>{dvrInstance}],MathematicaClass@dvrInstance],
					"DVRSymbol"->DVRInstanceHold[dvrInstance],
					"SaveBox"->Row@{TextCell["Save on Close: ","Text"],Checkbox[Dynamic@saveFlag]}
					|>
				]},
			SetOptions[doc,
				CellContext->"Global`",
				NotebookEventActions->{"WindowClose":>If[saveFlag,
							Quiet[If[NotebookFileName[]=!=$Failed,
							With[{dspD=Quiet[interface::displayDVR],dvrDB=Quiet[interface::DVR::Database],
								type=Quiet[MathematicaClass[ObjectType[interface::DVR]]]},
								Quiet[type::Database=None];
								Quiet[interface::DVR::Database=None];
								interface::displayDVR=None;
								SaveObject[interface,Directory:>FileNameJoin@{$DVRRoot,"Resources","Saved DVRs","Interface Instances"},
									Contexts->{
											{"DatabaseWrapper`",DatabaseWrapper`Private`$ContextFile},
											{"ParallelProcess`",ParallelProcess`Private`$ContextFile}
											}]
								Quiet[type::Database=dvrDB];
								Quiet[interface::DVR::Database=dvrDB];
								interface::displayDVR=dspD]]]
							]
					}
				]
			]
			]
		]
		];


(* ::Section:: *)
(*End Package Code*)


End[]
