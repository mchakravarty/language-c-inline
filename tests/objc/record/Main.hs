{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Foundation/Foundation.h>", "Particle_objc.h"]

go :: IO ()
go = $(objc [] ''() [cexp| ({ 
    typename Particle *particle = [Particle particleWithMass:1.0]; 
    NSLog(@"The mass is %f", particle.mass); 
  }) |])

objc_emit


main = objc_initialise >> go
