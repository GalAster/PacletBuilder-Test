(* ::Package:: *)

(* ::Chapter:: *)
(*Environment*)


$here=DirectoryName[$InputFileName/."":>NotebookFileName[]];
$tasks=FileNames["paclet.config.m",$here,Infinity];


(* ::Chapter:: *)
(*Assist Functions*)


DamnDeleteFiles[path_,rules_]:=Quiet@Block[
	{all=Union[Join[
		FileNames[(#1<>"\\*"&)/@ass[Delete],temp,Infinity],
		FileNames[ass[Delete],temp,Infinity]]
	]},
	DeleteFile[Select[all,FileExistsQ]];
	DeleteDirectory[Select[all,DirectoryQ,DeleteContents->True]];
]


(* ::Chapter:: *)
(*Build Functions*)


PacletsAutoBuilder[ass_Association]:=Switch[
	ass[Method],
	"Manualdownload",ass//Manualdownload,
	"GithubRelease",ass//GithubRelease,
	"GithubReleaseHook",ass//GithubReleaseHook,
	"GithubRepo",ass//GithubRepo,
	"GithubRepoHook",ass//GithubRepoHook,
	"SomethingElse",ass//SomeMethod,
	_,"Custom"
];


(* ::Section:: *)
(*Manual download*)


Manualdownload[___]:=Return[];


(* ::Section:: *)
(*From Github Release*)


ReleaseAssetsDownload[url_]:=Block[
	{path=FileNameJoin@{$here,"Paclets",Last@StringSplit[url,"/"]}},
	If[FileExistsQ[path],DeleteFile[path]];
	URLDownloadSubmit[url,CreateFile[path]]
]



(* ::Section:: *)
(*From Github Repo*)


GetCommit[owner_,repo_,branch_:"master"]:=Module[
	{url=StringTemplate["https://api.github.com/repos/`owner`/`repo`/commits/`branch`"]},
	URLExecute[url[<|"owner"->owner,"repo"->repo,"branch"->branch|>],"RawJson"]
]


GithubRepoUpdateCheck[dir_,repo_List]:=Block[
	{log,logs,update,last},
	log=FileNameJoin[{dir,"paclet.log.m"}];
	If[
		FileExistsQ@log,
		logs=Import[log,"RawJSON"],
		CreateFile[log];logs={<|"Last"->DateObject[0]|>}
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





(* ::Chapter:: *)
(*Build All*)
