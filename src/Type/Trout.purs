-- Experimental implementation of servant-server style routing API.
module Type.Trout
       ( Lit
       , Capture
       , CaptureAll
       , Resource
       , Method
       , Raw
       , Sub
       , LitSub
       , Alt(..)
       , Named
       , QueryParam
       , QueryParams
       , type (:>)
       , type (:/)
       , type (:<|>)
       , type (:=)
       ) where

-- | A literal path segment, matching paths where the next segment is equal
-- | to the value of the `Symbol`.
-- |
-- | For example, the type `Lit "settings" :> Lit "general" :> "reset"`  would
-- | match the path `/settings/general/reset`.
data Lit (v :: Symbol)

-- | Captures one segment of a path as type `t`. The `v` is a
-- | `Symbol` that describes the captured value.
data Capture (v :: Symbol) t

-- | Captures all remaining segments of a path, all as type `t`. The `v`
-- | is a `Symbol` that describes the captured value.
data CaptureAll (v :: Symbol) t

-- | A type-level description of an HTTP resource, terminating a chain of
-- | path literals, captures, and other endpoint type constructs. The `ms` are
-- | the methods handled by this resource.
data Resource ms

-- | The `m` symbol is the HTTP method that is handled, `r` is the
-- | response representation (usually some type specific to the application
-- | domain,) and `cts` are the content types supported.
data Method (m :: Symbol) r cts

-- | A type-level description of a raw middleware, terminating a chain of path
-- | literals, captures, and other endpoint type constructs. The `m` symbol is
-- | the HTTP method that is handled.
data Raw (m :: Symbol)

-- | The `Sub` is used to create the chain of `Lit`, `Capture`, `Resource`,
-- | and other such type constructs that build up an endpoint type. `Sub`
-- | is more often used infix with the `:>` operator.
data Sub e t

-- | A handy type alias for `Sub (Lit v)`, meant to be used infix with the `:/`
-- | operator. Instead of writing `Lit "a" :> Lit "b" :> ...`, you can write
-- | `"a" :/ "b" :/ ...`.
type LitSub (v :: Symbol) t = Sub (Lit v) t

-- | `Alt` represents choice, i.e. that endpoint `a` is tried first, and if
-- | it fails, `b` is tried next. `Alt` is written infix using `:<|>` and is
-- | used to compose multiple endpoint types into a larger API or site. Using
-- | the infix operator, `Alt a (Alt b c)` can be written `a :<|> b :<|> c`.
data Alt a b

-- | `Named` associates some routing type `t` with a `name` symbol. This is
-- | used to give names to combined routes in a routing type.
data Named (name :: Symbol) t

-- | Captures the first value of the query string parameter, or `Nothing`. `t`
-- | is the type of the value. `k` is the name of the key as a `Symbol`.
data QueryParam (k :: Symbol) t

-- | Captures all values of the query string parameter, or `[]`. `t` is the
-- | type of the value. `k` is the name of the key as a `Symbol`.
data QueryParams (k :: Symbol) t

infixr 9 type Sub as :>
infixr 9 type LitSub as :/
infixr 8 type Named as :=
infixr 7 type Alt as :<|>
