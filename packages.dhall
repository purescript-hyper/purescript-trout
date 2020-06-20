let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20200615/packages.dhall sha256:5d0cfad9408c84db0a3fdcea2d708f9ed8f64297e164dc57a7cf6328706df93a

let overrides = 
      { argonaut = upstream.argonaut // { version = "v7.0.0" }
      , argonaut-codecs = upstream.argonaut-codecs // { version = "v7.0.0" }
      , argonaut-traversals = upstream.argonaut-traversals // { version = "v8.0.0" }
      }

let additions = {=}

in  upstream // overrides // additions
