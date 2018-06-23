<|
    Method->"GithubRepo",
    Path->{"GalAster","BilibiliLink"},
    Check -> 86400,
    Delete->{
        "Resources",
        "*.git*"
    },
    Extract->False,
    "AutoVersion"->True,
    "Readme"->False
|>