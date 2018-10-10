module Instances where

import           Types

--------------------------------------------------------------------------------

instance Semigroup Version where
  v1 <> v2 = if v1 > v2 
             then v1
             else v2

instance Monoid Version where
  mempty = Version { major = 0, minor = 0, patch = 0}

--------------------------------------------------------------------------------

instance Semigroup VersionBump where
  NoBump      <> y           = y
  x           <> NoBump      = x

  (BumpPatch x) <> (BumpPatch y) = BumpPatch $ x + y
  (BumpPatch _) <> (BumpMinor y) = BumpMinor y
  (BumpPatch _) <> (BumpMajor y) = BumpMajor y

  (BumpMinor x) <> (BumpPatch _) = BumpMinor x
  (BumpMinor x) <> (BumpMinor y) = BumpMinor $ x + y
  (BumpMinor _) <> (BumpMajor y) = BumpMajor y

  (BumpMajor x) <> (BumpPatch _) = BumpMajor x
  (BumpMajor x) <> (BumpMinor _) = BumpMajor x
  (BumpMajor x) <> (BumpMajor y) = BumpMajor $ x + y

  (BumpTo x)    <> (BumpTo y)    = BumpTo $ x <> y
  (BumpTo x)    <> _             = BumpTo x 
  _             <> (BumpTo y)    = BumpTo y 

instance Monoid VersionBump where
  mempty = NoBump
  



