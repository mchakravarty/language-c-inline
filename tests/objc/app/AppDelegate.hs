{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module AppDelegate (objc_initialise) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


-- We are going to use this message in the Objective-C code of the app delegate.
--
launchMsg :: String
launchMsg = "HSApp did finish launching!"


objc_interface [cunit|

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (assign) typename NSWindow *window;

@end
|]


objc_implementation ['launchMsg] [cunit|

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  NSLog(@"%@", launchMsg());
}

@end
|]


objc_emit
