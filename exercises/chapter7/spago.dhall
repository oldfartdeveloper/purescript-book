{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
<<<<<<< HEAD
  [ "console", "effect", "psci-support", "strings", "test-unit", "validation" ]
=======
  [ "console"
  , "effect"
  , "psci-support"
  , "strings"
  , "test-unit"
  , "validation"
  ]
>>>>>>> master
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
