(* ::Package:: *)

(* ::Chapter:: *)
(*Environment*)


$here = DirectoryName[$InputFileName /. "" :> NotebookFileName[]];
$tasks = FileNames["paclet.config.m", $here, Infinity];
$pause = 0;
$site = "http://math.owo.site";


(* ::Chapter::Closed:: *)
(*Assist Functions*)


DamnDeleteFiles[path_, rules_] := Quiet@Block[
    {all = Union[Join[
        FileNames[(# <> "\\*"&) /@ rules, path, Infinity],
        FileNames[rules, path, Infinity]]
    ]},
    DeleteFile[Select[all, FileExistsQ]];
    DeleteDirectory[Select[all, DirectoryQ, DeleteContents -> True]];
];

PacletSiteBuild[] := Block[
    {pkgs, info},
    pkgs = FileNames["*.paclet", FileNameJoin[{$here, "Paclets"}]];
    info = (PacletInformation /@ pkgs) /. {
        ("Location" -> l_) :> ("Location" -> URLBuild[{$site, "paclets", FileNameTake[l]}]),
        ("Publisher" -> "") :> ("Publisher" -> $site)
    };
    Export[
        FileNameJoin[{$here, "PacletSite.mz"}],
        <|"PacletSite.m" -> PacletSite @@ Paclet @@@ info|>,
        {"ZIP", "Rules"}
    ]
];



(* ::Chapter:: *)
(*Build Functions*)


PacletsAutoBuilder[file_] := Switch[
    Import[file][Method],
    "Manualdownload", file // Manualdownload,
    "GithubRelease", file // GithubRelease,
    "GithubReleaseHook", file // GithubReleaseHook,
    "GithubRepo", file // GithubRepo,
    "GithubRepoHook", file // GithubRepoHook,
    "SomethingElse", file // SomeMethod,
    _, "Custom"
];


(* ::Section::Closed:: *)
(*Manual download*)


Manualdownload[___] := Return[];


(* ::Section::Closed:: *)
(*From Github Release*)


GetRelease[owner_, repo_] := Module[
    {url = StringTemplate["https://api.github.com/repos/`owner`/`repo`/releases/latest"]},
    URLExecute[url[<|"owner" -> owner, "repo" -> repo|>], "RawJson"]
];
ReleaseAssetsDownload[url_] := Block[
    {path = FileNameJoin@{$here, "Paclets", Last@StringSplit[url, "/"]}},
    If[FileExistsQ[path], DeleteFile[path]];
    URLDownloadSubmit[url, CreateFile[path]];
    Return[path]
];
GithubReleaseLogInit[path_] := If[
    !FileExistsQ@path,
    Export[path, {<|
        "CheckTime" -> DateObject[0],
        "ReleaseInfo" -> "Initialize release log"
    |>}]
];
GithubRelease[file_] := Block[
    {
        $now = Now,
        dir = DirectoryName[file],
        config = Import[file],
        api, logs, update, last
    },
    api = Apply[GetRelease, config[Path]];
    GithubReleaseLogInit[FileNameJoin[{dir, "paclet.log.m"}]];
    logs = Import[FileNameJoin[{dir, "paclet.log.m"}]];
    update = DateObject[api["published_at"]];
    last = DateObject[First[logs]["CheckTime"]];
    If[Positive@Subtract[UnixTime@last, UnixTime@update],
        Echo[Text@Style[FileBaseName@DirectoryName[file]], "Skip: "];
        Export[FileNameJoin[{dir, "paclet.log.m"}], Prepend[logs, <|
            "CheckTime" -> Now,
            "LastCommit" -> update,
            "ReleaseInfo" -> "No need to update"
        |>]];
        Return[file]
    ];
    ReleaseAssetsDownload /@ ("browser_download_url" /. api["assets"]);
    Export[FileNameJoin[{dir, "paclet.log.m"}], Prepend[logs, <|
        "CheckTime" -> $now,
        "LastCommit" -> update,
        "BuildInfo" -> <|
            "url" -> api["html_url"],
            "TimeUsed" -> Now - $now
        |>
    |>]]
];



(* ::Section::Closed:: *)
(*From Github Repo*)


GetCommit[owner_, repo_, branch_ : "master"] := Module[
    {url = StringTemplate["https://api.github.com/repos/`owner`/`repo`/commits/`branch`"]},
    URLExecute[url[<|"owner" -> owner, "repo" -> repo, "branch" -> branch|>], "RawJson"]
];
GithubRepoLogInit[path_] := If[
    !FileExistsQ@path,
    Export[path, {<|
        "CheckTime" -> DateObject[0],
        "BuildInfo" -> "Initialize build log",
        "Version" -> "0.0.0"
    |>}]
];
GithubRepoCheck[dir_, repo_List] := Block[
    {log, logs, update, last},
    GithubRepoLogInit[log = FileNameJoin[{dir, "paclet.log.m"}]];
    logs = Import[log];
    update = DateObject[Apply[GetCommit, repo]["commit", "committer", "date"]];
    last = DateObject[First[logs]["CheckTime"]];
    If[Negative@Subtract[UnixTime@last, UnixTime@update], Return[last]];
    Export[log, Prepend[logs, <|
        "CheckTime" -> Now,
        "LastCommit" -> update,
        "BuildInfo" -> "No need to update",
        "Version" -> "0.0.0"
    |>]];
    Return[False]
];
VersionPlus[dir_] := Block[
    {vlast, vnow, config},
    vlast = First[Import@FileNameJoin[{dir, "paclet.log.m"}]]["Version"];
    config = Apply[Association, Import@FileNameJoin[{dir, "temp", "PacletInfo.m"}]];
    If[
        Or @@ Greater @@@ Transpose[ToExpression@Most@StringSplit[#, "."]& /@ {vnow = config[Version], vlast}],
        Return["No need to auto add version number."]
    ];
    vnow = StringRiffle[# + UnitVector[Length[#], Length[#]]&[ToExpression@StringSplit[vlast, "."]], "."];
    Export[
        FileNameJoin[{dir, "temp", "PacletInfo.m"}],
        AssociateTo[config, Version -> vnow] /. Association -> Paclet
    ];
    Return[vnow]
];
GithubRepo[file_] := Block[
    {
        $now = Now,
        dir = DirectoryName[file],
        config = Import[file],
        temp, pack, update, vnow, copy, logs
    },
(*Check if need update*)
    update = GithubRepoCheck[dir, config[Path]];
    If[update === False,
        Echo[Text@Style[FileBaseName@DirectoryName[file]], "Skip: "];
        Return[file]
    ];
    temp = CopyDirectory[FileNameJoin[{dir, "source"}],
        FileNameJoin[{dir, "temp"}]];
    (*Check if use auto version*)
    If[config["AutoVersion"] === True, vnow = VersionPlus[dir]];
    copy = CopyFile[
        pack = PackPaclet[temp],
        FileNameJoin[{$here, "Paclets", FileNameTake[pack]}],
        OverwriteTarget -> True
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
    |>]]
];


(* ::Chapter:: *)
(*Build All*)


(* ::CodeText:: *)
(* $do//Activate *)


$do = Append[
    Riffle[Inactive[PacletsAutoBuilder] /@ $tasks, Inactive[Pause][$pause]],
    Inactive[PacletSiteBuild][]
]
