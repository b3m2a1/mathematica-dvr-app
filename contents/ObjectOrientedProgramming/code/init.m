(* ::Package:: *)

ClearAll@"ObjectOrientedProgramming`*"
BeginPackage["ObjectOrientedProgramming`"];
$BaseLanguage="Python";
User`$MMADir=DirectoryName[$InputFileName]//ParentDirectory//ParentDirectory;
`Private`$ObjectDirectory =  FileNameJoin@{DirectoryName[$InputFileName]//ParentDirectory,"Saved Objects"};
`Private`$ContextFile=$InputFileName;
If[Context@Association=!="System`",
	Needs["BackwardsCompatibility`",FileNameJoin@{User`$MMADir,"BackwardsCompatibility","init.m"}];
	LoadBackwardsCompatible["Association"]
];

ObjectOrientedProgramming::nolang="Couldn't load class of language ``";
LoadLanguage[language_:$BaseLanguage]:=(
	ClearAll/@Cases[Names["ObjectOrientedProgramming`*"],Except["$BaseLanguage"|"$MMADir"|"LoadLanguage"]];
	If[Get@FileNameJoin@{DirectoryName@$InputFileName,language<>".m"}=!=$Failed,
		$BaseLanguage=language,
		Message[ObjectOrientedProgramming::nolang,language];
		$Failed
		]
	);
LoadLanguage[];
EndPackage[];
