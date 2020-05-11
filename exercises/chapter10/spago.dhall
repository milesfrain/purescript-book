{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut"
  , "console"
  , "effect"
  , "foreign-generic"
  , "pairs"
  , "psci-support"
  , "random"
  , "simple-json"
  , "validation"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
