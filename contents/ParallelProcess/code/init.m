(* ::Package:: *)

BeginPackage["ParallelProcess`"];

Needs["ObjectOrientedProgramming`",FileNameJoin[{User`$MMADir,"ObjectOrientedProgramming","init.m"}]];
Get@FileNameJoin@{DirectoryName@$InputFileName,"ParallelProcess.m"};
`Private`$ContextFile=$InputFileName;

EndPackage[];
