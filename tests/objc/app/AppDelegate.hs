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

// IBOutlets
@property (assign) typename NSWindow     *window;
@property (assign) typename NSScrollView *scrollView;

@end
|]


objc_implementation ['launchMsg] [cunit|

@interface AppDelegate ()

@property typename NSTextView *textView;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.textView = self.scrollView.documentView;
  [self.textView insertText:launchMsg() ];
  NSLog(@"%@", launchMsg());
}

@end
|]


objc_emit
