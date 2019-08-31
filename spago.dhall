{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "halogen-contenteditable"
, dependencies =
    [ "console"
    , "effect"
    , "prelude"
    , "psci-support"
    , "halogen"
    , "halogen-svg"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
