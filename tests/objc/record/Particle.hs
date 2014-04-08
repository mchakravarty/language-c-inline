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

particleMass :: Particle -> Float
particleMass = mass


objc_interface [cunit|

@interface Particle : NSObject

@property (readonly) float mass;

+ (typename instancetype)particleWithMass:(float)mass;

- (typename instancetype)init;

- (typename instancetype)initWithMass:(float)mass;


@end
|]


objc_implementation ['newParticle, 'particleMass] [cunit|

@interface Particle ()

@property (readonly, assign, nonatomic) typename HsStablePtr particle;

@end

@implementation Particle

// Initialisation

+ (typename instancetype)particleWithMass:(float)mass
{
  return [[Particle alloc] initWithMass:mass];
}

- (typename instancetype)init
{
  return [self initWithMass:0.0];
}

- (typename instancetype)initWithMass:(float)mass
{
  self = [super init];
  if (self)
    _particle = newParticle(mass);
  return self;
}

// Getters and setters

- (float)mass
{
  return particleMass(self.particle);
}

@end
|]


objc_emit
