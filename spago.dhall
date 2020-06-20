{ name =
    "trout"
, dependencies =
    [ "argonaut", "media-types", "prelude", "smolder", "spec", "spec-discovery", "uri" ]
, sources =
     [ "src/**/*.purs", "test/**/*.purs" ]
, packages =
    ./packages.dhall
}
