(* ::Package:: *)

(* ::Chapter:: *)
(*Environment*)


$here=DirectoryName[$InputFileName/."":>NotebookFileName[]];
$tasks=FileNames["paclet.config.m",$here,Infinity];


(* ::Chapter::Closed:: *)
(*Assist Functions*)


DamnDeleteFiles[path_,rules_]:=Quiet@Block[
	{all=Union[Join[
		FileNames[(#<>"\\*"&)/@rules,path,Infinity],
		FileNames[rules,path,Infinity]]
	]},
	DeleteFile[Select[all,FileExistsQ]];
	DeleteDirectory[Select[all,DirectoryQ,DeleteContents->True]];
];


(* ::Chapter:: *)
(*Build Functions*)


PacletsAutoBuilder[file_]:=Switch[
	Import[file][Method],
	"Manualdownload",file//Manualdownload,
	"GithubRelease",file//GithubRelease,
	"GithubReleaseHook",file//GithubReleaseHook,
	"GithubRepo",file//GithubRepo,
	"GithubRepoHook",file//GithubRepoHook,
	"SomethingElse",file//SomeMethod,
	_,"Custom"
];


(* ::Section:: *)
(*Manual download*)


Manualdownload[___]:=Return[];


(* ::Section:: *)
(*From Github Release*)


GetRelease[owner_,repo_]:=Module[
	{url=StringTemplate["https://api.github.com/repos/`owner`/`repo`/releases/latest"]},
	URLExecute[url[<|"owner"->owner,"repo"->repo|>],"RawJson"]
];
ReleaseAssetsDownload[url_]:=Block[
	{path=FileNameJoin@{$here,"Paclets",Last@StringSplit[url,"/"]}},
	If[FileExistsQ[path],DeleteFile[path]];
	URLDownloadSubmit[url,CreateFile[path]];
	Return[path]
];
GithubReleastLogInit[path_]:=If[
	!FileExistsQ@path,
	Export[path,{<|
		"CheckTime"->DateObject[0],
		"DownloadInfo"->"Initialize release log"
	|>}]
];


(* ::Section:: *)
(*From Github Repo*)


GetCommit[owner_,repo_,branch_:"master"]:=Module[
	{url=StringTemplate["https://api.github.com/repos/`owner`/`repo`/commits/`branch`"]},
	URLExecute[url[<|"owner"->owner,"repo"->repo,"branch"->branch|>],"RawJson"]
];
GithubRepoLogInit[path_]:=If[
	!FileExistsQ@path,
	Export[path,{<|
		"CheckTime"->DateObject[0],
		"BuildInfo"->"Initialize build log",
		"Version"->"0.0.0"
	|>}]
];
GithubRepoCheck[dir_,repo_List]:=Block[
	{log,logs,update,last},
	GithubRepoLogInit[log=FileNameJoin[{dir,"paclet.log.m"}]];
	logs=Import[log];
	update=DateObject[Apply[GetCommit,repo]["commit","committer","date"]];
	last=DateObject[First[logs]["CheckTime"]];
	If[Negative@Subtract[UnixTime@last,UnixTime@update],Return[last]];
	Export[log,Prepend[logs,<|
		"CheckTime"->Now,
		"LastCommit"->update,
		"BuildInfo"->"No need to update",
		"Version"->"0.0.0"
	|>]];
	Return[False]
];
VersionPlus[dir_]:=Block[
	{vlast,vnow,config},
	vlast=First[Import@FileNameJoin[{dir,"paclet.log.m"}]]["Version"];
	config=Apply[Association,Import@FileNameJoin[{dir,"temp","PacletInfo.m"}]];
	If[
		Or@@Greater@@@Transpose[ToExpression@Most@StringSplit[#,"."]&/@{vnow=config[Version],vlast}],
		Return["No need to auto add version number."]
	];
	vnow=StringRiffle[#+UnitVector[Length[#],Length[#]]&[ToExpression@StringSplit[vlast,"."]],"."];
	Export[
		FileNameJoin[{dir,"temp","PacletInfo.m"}],
		AssociateTo[config, Version->vnow]/.Association->Paclet
	];
	Return[vnow]
];
GithubRepo[file_] := Block[
	{
		$now = Now;
		dir = DirectoryName[file];
		ass = Import[file],
		temp, pack, update, vnow, copy,logs
	},
	(*Check if need update*)
	update = GithubRepoCheck[dir, ass[Path]];
	If[
		update === False,
		Echo["No need to update", "Skip: "]; Return[]
	];
	temp = CopyDirectory[FileNameJoin[{dir, "source"}],
		FileNameJoin[{dir, "temp"}]];
	(*Check if use auto version*)
	If[ass["AutoVersion"] === True, vnow = VersionPlus[dir]];
	copy = CopyFile[
		pack = PackPaclet[temp],
		FileNameJoin[{$here, "Paclets", FileNameTake[pack]}],
		OverwriteTarget->True
	];
	DeleteFile[pack];
	DeleteDirectory[temp, DeleteContents -> True];
	logs = Import[FileNameJoin[{dir, "paclet.log.m"}]];
	Export[FileNameJoin[{dir, "paclet.log.m"}], Prepend[logs, <|
		"CheckTime" -> $now,
		"LastCommit" -> update,
		"BuildInfo" -> <|
			"Paclet" -> FileNameTake[pack],
			"TimeUsed" -> Now - $now
		|>,
		"Version" -> vnow
	|>]];
]


(* ::Chapter:: *)
(*Build All*)
