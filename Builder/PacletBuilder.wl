(* ::Package:: *)


GetCommit[owner_,repo_,branch_:"master"]:=Module[
	{url=StringTemplate["https://api.github.com/repos/`owner`/`repo`/commits/`branch`"]},
	URLExecute[url[<|"owner"->owner,"repo"->repo,"branch"->branch|>],"RawJson"]
]


GithubUpdateCheck[dir_,repo_List]:=Block[
	{log,logs,update,last},
	log=FileNameJoin[{dir,"paclet.log.json"}];
	If[
		FileExistsQ@log,
		logs=Import[log,"RawJSON"],
		CreateFile[log];logs={<|"Last"->DateString[DateObject[0],"ISODateTime"]|>}
	];
	update=DateObject@DateString[Apply[GetCommit,repo]["commit","committer","date"]];
	last=DateObject[First[logs]["Last"]];
	If[Negative@Subtract[UnixTime@last,UnixTime@update],Return[True]];
	Export[log,Prepend[logs,<|
		"Now"->DateString[Now,"ISODateTime"],
		"Last"->DateString[last,"ISODateTime"],
		"Update"->DateString[update,"ISODateTime"],
		"Build"->False
	|>]];
	Return[False]
]
