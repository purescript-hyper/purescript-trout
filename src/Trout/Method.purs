module Trout.Method
       ( Options
       , Get
       , Head
       , Post
       , Put
       , Patch
       , Delete
       , Trace
       , Connect
       ) where

import Trout (Method)

type Options = Method "OPTIONS"

type Get = Method "GET"

type Head = Method "HEAD"

type Post = Method "POST"

type Put = Method "PUT"

type Patch = Method "PATCH"

type Delete = Method "DELETE"

type Trace = Method "TRACE"

type Connect = Method "CONNECT"
