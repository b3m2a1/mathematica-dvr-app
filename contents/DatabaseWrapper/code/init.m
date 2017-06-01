(* ::Package:: *)

ClearAll@"DatabaseWrapper`*"
BeginPackage["DatabaseWrapper`"];
User`$MMADir=DirectoryName[$InputFileName]//ParentDirectory//ParentDirectory;
`Private`$ContextFile=$InputFileName;
Needs["ObjectOrientedProgramming`",FileNameJoin@{User`$MMADir,"ObjectOrientedProgramming","init.m"}];
Needs["DatabaseLink`"];
With[{d=DirectoryName@$InputFileName},
	(*Get@FileNameJoin@{d,"init.m"};*)
	Get@FileNameJoin@{d,"database.m"};
	
	LoadDatabaseType[extension_]:=(
		BeginPackage["DatabaseWrapper`",{"ObjectOrientedProgramming`"}];
		If[extension===All,
			SetDirectory@d;
			(Get@FileNameJoin@#&)/@FileNames[Except["init.m"|"database.m"]];
			ResetDirectory[];,
			Get@FileNameJoin@{d,extension<>".m"}
			];
		EndPackage[];
		)
		];
EndPackage[];
