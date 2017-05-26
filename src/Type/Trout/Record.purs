module Type.Trout.Record (get, set, insert, delete) where

import Data.Symbol (class IsSymbol, SProxy, reflectSymbol)

foreign import unsafeGet
  :: forall r a
   . String
  -> Record r
  -> a

get
  :: forall r r' l a
   . IsSymbol l
  => RowCons l a r' r
  => SProxy l
  -> Record r
  -> a
get l = unsafeGet (reflectSymbol l)

foreign import unsafeSet
  :: forall r1 r2 a
   . String
  -> a
  -> Record r1
  -> Record r2

set
  :: forall r1 r2 r l a b
   . IsSymbol l
  => RowCons l a r r1
  => RowCons l b r r2
  => SProxy l
  -> b
  -> Record r1
  -> Record r2
set l = unsafeSet (reflectSymbol l)

insert
  :: forall r1 r2 l a
   . IsSymbol l
  => RowCons l a r1 r2
  => SProxy l
  -> a
  -> Record r1
  -> Record r2
insert l = unsafeSet (reflectSymbol l)

foreign import unsafeDelete
  :: forall r1 r2
   . String
  -> Record r1
  -> Record r2

delete
  :: forall r1 r2 l a
   . IsSymbol l
  => RowCons l a r2 r1
  => SProxy l
  -> Record r1
  -> Record r2
delete l = unsafeDelete (reflectSymbol l)
