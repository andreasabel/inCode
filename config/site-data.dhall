{ authorInfo =
    { bitcoin =
        "3D7rmAYgbDnp4gp4rf22THsGt74fNucPDU"
    , coinbase =
        "mstksg"
    , email =
        "justin@jle.im"
    , gPlus =
        "+JustinLe"
    , github =
        "mstksg"
    , keybase =
        "mstksg"
    , linkedIn =
        "lejustin"
    , name =
        "Justin Le"
    , patreon =
        "justinle"
    , rel =
        "https://plus.google.com/107705320197444500140"
    , twitch =
        "justin_l"
    , twitter =
        "mstk"
    }
, blobs =
    [ { renderBranch =
          [ "gh-pages" ] : Optional Text
      , sourceBranch =
          [ "master" ] : Optional Text
      , tree =
          "https://github.com/mstksg/inCode/tree"
      }
    ] : Optional
        { renderBranch :
            Optional Text
        , sourceBranch :
            Optional Text
        , tree :
            Text
        }
, blogPrefs =
    { feedEntries =
        10
    , homeEntries =
        8
    , ledeMax =
        6
    , sidebarEntries =
        5
    , slugLength =
        8
    }
, codeSamples =
    [ "code-samples" ] : Optional Text
, copyright =
    "2018 Justin Le"
, desc =
    "Weblog of Justin Le, covering various adventures in programming and explorations in the vast worlds of computation physics, and knowledge."
, developerAPIs =
    { addThis =
        "ra-5234d67a6b68dcd4"
    , analytics =
        { _1 = "UA-443711-8", _2 = "jle.im" }
    , disqus =
        "incode"
    , facebook =
        "641852699171929"
    , feedburner =
        "incodeblog"
    , flattr =
        "3p9jqr"
    }
, envType =
    < Production = {=} | Development : {} >
, feed =
    "http://feeds.feedburner.com/incodeblog"
, hostInfo =
    { base =
        "blog.jle.im"
    , port =
        [] : Optional Natural
    , root =
        [] : Optional Text
    , secure =
        True
    }
, interactive =
    [ "https://www.fpcomplete.com/user/jle/" ] : Optional Text
, license =
    "CC-BY-NC-ND 3.0"
, licenseLink =
    "https://creativecommons.org/licenses/by-nc-nd/3.0/"
, title =
    "in Code"
}
