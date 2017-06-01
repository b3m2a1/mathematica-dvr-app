(* ::Package:: *)

BeginPackage["MolecularModeling`"];
(*Clear@"MolecularModeling`*";*)
User`$MMADir=DirectoryName[$InputFileName]//ParentDirectory//ParentDirectory;
`Private`$ContextFile=$InputFileName;
Needs["ObjectOrientedProgramming`",FileNameJoin[{User`$MMADir,"ObjectOrientedProgramming","init.m"}]];
Get@FileNameJoin@{DirectoryName@$InputFileName//ParentDirectory,"code","primitives.m"};
EndPackage[];
