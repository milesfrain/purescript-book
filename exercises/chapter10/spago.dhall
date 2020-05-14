{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut"
  , "argonaut-generic"
  , "console"
  , "debug"
  , "effect"
  , "foreign-generic"
  , "pairs"
  , "psci-support"
  , "random"
  , "react-basic-hooks"
  , "simple-json"
  , "test-unit"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
