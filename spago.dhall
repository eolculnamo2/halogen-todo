{ name = "halogen-project"
, dependencies = [ "arrays", "console", "effect", "halogen", "prelude" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
