{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module AppDelegate (objc_initialise) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


objc_interface [cunit|

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (assign) typename NSWindow *window;

@end
|]


objc_implementation [] [cunit|

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  NSLog(@"HSApp did finish launching!");
}

@end
|]


objc_emit
