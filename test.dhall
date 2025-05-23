let conf = ./spago.dhall

in conf // {
  sources = conf.sources # [ "test/**/*.purs" ],
  dependencies =
    conf.dependencies #
    [ "spec"
    , "either"
    , "foldable-traversable"
    , "newtype"
    , "ordered-collections"
    , "aff"
    , "lists"
    , "console"
    ]
}