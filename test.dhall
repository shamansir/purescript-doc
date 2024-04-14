let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs", "example/Hydra/Hydra/**/*.purs" ],
  dependencies =
    conf.dependencies #
    [ "spec"
    , "either"
    , "foldable-traversable"
    , "newtype"
    , "aff"
    , "lists"
    ]
}