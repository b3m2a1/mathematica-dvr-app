(* ::Package:: *)

(* ::Section:: *)
(*External*)


(* ::Subsection::Closed:: *)
(*Exposed Classes*)


ParallelProcessClass::usage="A class for performing parallel processes"


(* ::Subsection::Closed:: *)
(*Exposed Functions*)


ParallelProcess::usage="Constructs a ParallelProcessClass object"


UnsubmittedJob::usage="A head to wrap a command in for formatting"
CompletedJob::usage="A head to wrap a result in for formatting"


SuccessResult::usage="A head for a successful result"
DequeuedResult::usage="A head for a result of 'dequeued'"
InvalidResult::usage="A head for a result of 'invalid'"


(* ::Subsection::Closed:: *)
(*Exposed Parameters*)


SharedPackages::usage="Specifies the packages to be called with ParallelNeeds or via ParallelEvaluate[Needs@@<pkg>]"
SharedDefinitions::usage="Specified the definitions to distribute"
SharedFunctions::usage="Specifies functions to distribute"
SharedVariables::usage="Specifies variables to distribute"
VariableClosure::usage="Specifies the variable closure to construct when submitting"


(* ::Section:: *)
(*Internal*)


Begin["`Private`"];


SharedPackages;
SetAttributes[SharedDefinitions,HoldAllComplete];
SetAttributes[SharedFunctions,HoldAllComplete];
SetAttributes[SharedVariables,HoldAllComplete];
SetAttributes[VariableClosure,HoldAllComplete];


(* ::Subsubsection::Closed:: *)
(*Job Heads*)


SetAttributes[UnsubmittedJob,HoldAllComplete];
SetAttributes[CompletedJob,HoldAllComplete];


Format[j:UnsubmittedJob[r_],StandardForm]:=Interpretation[
Framed[
	Framed[(*Column[{*)
		HoldForm@r,
		(*TextCell["Unsubmitted","Text",FontSize\[Rule]10,FontColor\[Rule]Gray,Deployed\[Rule]True]},Alignment\[Rule]Center],*)
		Background->GrayLevel[.95],RoundingRadius->3
		],
	RoundingRadius->8,Background->RGBColor[.98,1.,.9]],
j];


Format[j:CompletedJob[r_],StandardForm]:=Interpretation[
Framed[
	Framed[(*Column[{*)
		HoldForm@r,
		(*TextCell["Unsubmitted","Text",FontSize\[Rule]10,FontColor\[Rule]Gray,Deployed\[Rule]True]},Alignment\[Rule]Center],*)
		Background->GrayLevel[.95],RoundingRadius->3
		],
	RoundingRadius->8,Background->RGBColor[.88,1.,.84]],
j];


(* ::Subsubsection::Closed:: *)
(*Result Heads*)


SetAttributes[SuccessResult,HoldAllComplete];
SetAttributes[DequeuedResult,HoldAllComplete];
SetAttributes[InvalidResult,HoldAllComplete];


Format[s:SuccessResult[r_],StandardForm]:=Interpretation[
Row[{
	Graphics[{EdgeForm[GrayLevel[0.5]], RGBColor[0.4, 0.68, 1.], Disk[{0, 0}]}, ImageSize -> {25., Automatic},Epilog->Inset[TextCell["\[Checkmark]","Text",FontColor->LightGray,FontSize->12,Deployed->True],{.1,-.05}]],
	HoldForm[r]//Short},Spacer[10]],
s]


Format[s:DequeuedResult[r_],StandardForm]:=Interpretation[
Row[{
	Graphics[{EdgeForm[GrayLevel[0.5]], RGBColor[1.,1., .5], Disk[{0, 0}]}, ImageSize -> {25., Automatic},Epilog->Inset[TextCell["\[CircleDot]","Text",FontColor->Gray,FontSize->12,Deployed->True]]],
	HoldForm[r]//Short},Spacer[10]
	],
s]


Format[s:InvalidResult[r_],StandardForm]:=Interpretation[
Row[{
	Graphics[{EdgeForm[GrayLevel[0.5]], RGBColor[0.8,0, .08], Disk[{0, 0}]}, ImageSize -> {25., Automatic},Epilog->Inset[TextCell["\[Times]","Text",FontColor->LightGray,FontSize->12,Deployed->True]]],
	HoldForm[r]//Short},Spacer[10]
	],
s]


(* ::Subsection::Closed:: *)
(*Parallel Process Class*)


ParallelProcessClass=BeginClass[];


(* ::Subsubsection::Closed:: *)
(*External Constructor*)


ParallelProcess[cmd_,specs___]:=(
ParallelProcessClass[HoldComplete[cmd],specs]
);
SetAttributes[ParallelProcess,HoldAllComplete];


(* ::Subsubsection::Closed:: *)
(*Initialization Function*)


InitializationFunction[self_,jobcmd_HoldComplete,
	p:_SharedPackages:SharedPackages[],
	d:_SharedDefinitions:SharedDefinitions[],
	f:_SharedFunctions:SharedFunctions[],
	v:_SharedVariables:SharedVariables[],
	c:_VariableClosure:VariableClosure[],
	checkPeriod:_?NumericQ:1,
SuccessResult-> Function[{proc,suc},Null],
DequeuedResult-> Function[{proc,fal},Null],
InvalidResult-> Function[{proc,inv},Null],
Quiet->All]:=(
self::Job=jobcmd;
self::State="Unstarted";
self::Result=None;
self::CheckPeriod=checkPeriod;
self::SharedPackages=p;
self::SharedDefinitions=d;
self::SharedFunctions=f;
self::SharedVariables=v;
self::VariableClosure=c;
self::Quieted=OptionValue@Quiet;
self::OnSuccess=BoundMethod[OptionValue@SuccessResult];
self::OnDequeue=BoundMethod[OptionValue@DequeuedResult];
self::OnInvalid=BoundMethod[OptionValue@InvalidResult];
);


(* ::Subsubsection::Closed:: *)
(*Bound method: Queue*)


BoundMethod[Queue,self_,
	packs:_SharedPackages|Automatic:Automatic,
	defs:_SharedDefinitions|Automatic:Automatic,
	funcs:_SharedFunctions|Automatic:Automatic,
	svars:_SharedVariables|Automatic:Automatic,
	var:_VariableClosure|Automatic:Automatic,
	Verbose->False,
	Quiet->Automatic]:=
With[{jobcmd=self::Job,vflag=OptionValue@Verbose,
	q=Replace[OptionValue@Quiet,Automatic:>self::Quieted],
	v=Replace[var,Automatic:>self::VariableClosure],
	d=Replace[defs,Automatic:>self::SharedDefinitions],
	p=Replace[packs,Automatic:>self::SharedPackages],
	f=Replace[funcs,Automatic:>self::SharedFunctions],
	s=Replace[svars,Automatic:>self::SharedVariables]},
	If[$KernelCount==0,LaunchKernels[1]];
	If[Length@p>0,
		If[vflag,"Requiring packages: "<>Riffle[ReplacePart[(ToString[Unevaluated[#]]&/@p),0->List],", "]//PrintTemporary];
		Replace[#,{
		cont_String:>ParallelNeeds[cont],
			{cont_String,file_String}:>ParallelEvaluate[Needs[cont,file]]
			}]&/@p];
	If[Length@d>0,
		If[vflag,"Distributing definitions: "<>Riffle[ReplacePart[(ToString[Unevaluated[#]]&/@d),0->List],", "]//PrintTemporary];
		DistributeDefinitions@@Cases[d,_Symbol];DistributeDefinitions/@Cases[d,_String]];
	If[Length@f>0,
		If[vflag,"Sharing functions: "<>Riffle[ReplacePart[(ToString[Unevaluated[#]]&/@f),0->List],", "]//PrintTemporary];
		SetSharedFunction@@Replace[f,Automatic:>self::SharedFunctions]];
	If[Length@s>0,
		If[vflag,"Sharing variables: "<>Riffle[ReplacePart[(ToString[Unevaluated[#]]&/@s),0->List],", "]//PrintTemporary];
		SetSharedVariable@@Replace[s,Automatic:>self::SharedVariables]];
	self::Job=ReleaseHold[
		HoldComplete[ParallelSubmit[v,Quiet[{jobcmd,$MessageList},q]]]/.(ParallelSubmit[c_,Quiet[{HoldComplete[cmd_],m_},a_]]:>ParallelSubmit[c,Quiet[{cmd,m},a]])/.ParallelSubmit[VariableClosure[closure___],cmd_]:>ParallelSubmit[{closure},cmd]
	]
];


(* ::Subsubsection::Closed:: *)
(*Bound method: Start*)


BoundMethod[Start,self_,
	p:_SharedPackages|Automatic:Automatic,
	d:_SharedDefinitions|Automatic:Automatic,
	f:_SharedFunctions|Automatic:Automatic,
	s:_SharedVariables|Automatic:Automatic,
	v:_VariableClosure|Automatic:Automatic,
	Verbose->False,
	Quiet->Automatic
]:=
Replace[self::Job,{
	_HoldComplete:>(self::Queue[p,d,f,s,v,Verbose->OptionValue@Verbose,Quiet->OptionValue@Quiet];self::Start[]),
	e_EvaluationObject:>(Parallel`Concurrency`Private`queueRun[{e}];e)
}];


(* ::Subsubsection::Closed:: *)
(*Bound method: Query*)


BoundMethod[Query,self_]:=Replace[self::Job,{
_HoldComplete:>$Failed,
EvaluationObject[num_,cmd_,bigQ_,state_]:>Replace[state,{
	Parallel`Developer`finished[{res_,messages_}]:>(self::Messages=messages;SuccessResult[res]),
	Parallel`Developer`dequeued[{res_,messages_}]:>(self::Messages=messages;DequeuedResult[res]),
	Parallel`Developer`dequeued[{res_,messages_}]:>(self::Messages=messages;InvalidResult[res])
(*Parallel`Developer`queued\[RuleDelayed]"Unstarted",
Parallel`Developer`running[ker_]\[RuleDelayed]"Running on Kernel "<>ToString@ker,
Parallel`Developer`created\[RuleDelayed]"Created??"*)
	}]
}];


(* ::Subsubsection::Closed:: *)
(*Bound method: Retry*)


BoundMethod[Retry,self_]:=Parallel`Concurrency`Private`queueRun[{self::Job}];


(* ::Subsubsection::Closed:: *)
(*Bound method: CheckResult*)


BoundMethod[CheckResult,self_]:=Replace[self::Query[],{
	s_SuccessResult:>(self::OnSuccess[s];self::OnSuccess=Function[{a},None];s),
	d_DequeuedResult:>(self::OnDequeue[d];
	self::OnDequeue=Function[{a},None];d),
	i_InvalidResult:>(self::OnInvalid[i];
	self::OnInvalid=Function[{a},None];i)
}];


(* ::Subsubsection::Closed:: *)
(*Bound method: WaitResult*)


BoundMethod[WaitResult,self_]:=Replace[self::CheckResult[],{
$Failed:>(self::Start[];self::WaitResult[]),
Parallel`Developer`running[ker_]:>(WaitAll[self::Job];self::CheckResult[])
}
];


(* ::Subsubsection::Closed:: *)
(*Bound method: CheckRetry*)


BoundMethod[CheckRetry,self_]:=Replace[self::CheckResult[],
Except[_SuccessResult|_DequeuedResult|_InvalidResult]:>self::Retry[]
];


(* ::Subsubsection::Closed:: *)
(*Representation Function*)


RepresentationFunction[self_]:=Dynamic[Refresh[
Replace[self::CheckResult[],{
	$Failed:>Evaluate@(self::Job)/.HoldComplete->UnsubmittedJob,
	r:_SuccessResult|_DequeuedResult|_InvalidResult:>CompletedJob[r],
	_:>self::Job
	}],
	UpdateInterval->self::CheckPeriod],
TrackedSymbols:>{}];


(* ::Subsubsection::Closed:: *)
(*Bound method: Run*)


BoundMethod[Run,self_,
	p:_SharedPackages|Automatic:Automatic,
	d:_SharedDefinitions|Automatic:Automatic,
	sf:_SharedFunctions|Automatic:Automatic,
	sv:_SharedVariables|Automatic:Automatic,
	v:_VariableClosure|Automatic:Automatic,
	checkEvery:_?NumericQ|Automatic:Automatic,
	checkTimes:_Integer|\[Infinity]:\[Infinity],
	delay:_?NumericQ:0,
	Verbose->False,
	Quiet->Automatic]:=With[
	{(*e=EvaluationCell[],*)check=self::CheckRetry,
		checkPeriod=Replace[checkEvery,Automatic:>self::CheckPeriod]},
	(self::Start[p,d,sf,sv,v,Verbose->OptionValue@Verbose,Quiet->OptionValue@Quiet];
	With[{endF=Function[{s,r},self::Result=r;s::Loop//RemoveScheduledTask;s::Loop=.]},
		self::OnSuccess=With[{bm=self::OnSuccess},BoundMethod@Function[{s,r},bm[r];endF[s,r]]];
		self::OnDequeue=With[{bm=self::OnDequeue},BoundMethod@Function[{s,r},bm[r];endF[s,r]]];
		self::OnInvalid=With[{bm=self::OnInvalid},BoundMethod@Function[{s,r},bm[r];endF[s,r]]]
		];
	self::Loop=RunScheduledTask[check[],{checkPeriod,checkTimes},delay];
	self
	)];


(* ::Subsubsection::Closed:: *)
(*Class method: RunScript*)


ClassMethod[RunScript,cls_,scriptFile_?(MatchQ[#,_String]&&FileExistsQ[#]&),ops___]:=
	Evaluate[cls[HoldComplete[Get@scriptFile]]]::Run[ops];


(* ::Subsubsection::Closed:: *)
(*End Class*)


EndClass[];


(* ::Section::Closed:: *)
(*End*)


End[];
