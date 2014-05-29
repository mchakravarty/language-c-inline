{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- Marshalling a record structure

module Particle (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Foundation/Foundation.h>", "HsFFI.h"]


-- Haskell code used from Objective-C.

type Point = (Float, Float)

origin = (0, 0)

newtype Vector = Vector (Float, Float)

zero = Vector (0, 0)

data Particle = Particle 
                { mass :: Float
                , loc  :: Point
                , vel  :: Vector
                , acc  :: Vector
                }

newParticle :: Float -> Particle
newParticle mass = Particle mass origin zero zero

objc_record "Particle" ''Particle [Typed 'newParticle]
  [ [objcprop| @property (readonly) float mass; |] --> 'mass
  , [objcprop| @property (readonly) float locX; |] ==> ([t| Float |], 
                                                        [|  fst . loc |], 
                                                        [|  \p locX -> p { loc = (locX, snd . loc $ p) } |])
  , [objcprop| @property (readonly) float locY; |] ==> ([t| Float |], 
                                                        [|  snd . loc |], 
                                                        [|  \p locY -> p { loc = (fst . loc $ p, locY) } |])
  ]
  [objcifdecls|
    + (instancetype)particleWithMass:(float)mass;
  |] 
  [objcimdecls|
    + (instancetype)particleWithMass:(float)mass
    {
      return [[Particle alloc] initWithParticleHsPtr:newParticle(mass)];
    }
  |]

objc_emit
