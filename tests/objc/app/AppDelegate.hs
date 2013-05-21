{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module AppDelegate (objc_initialise) where

import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

objc_import ["<Cocoa/Cocoa.h>"]


-- Haskell code used from Objective-C.

launchMsg :: String
launchMsg = "HSApp did finish launching!"

mkLine :: String -> String
mkLine input = "> " ++ input ++ "\n"


objc_interface [cunit|

@interface AppDelegate : NSObject <NSApplicationDelegate>

// IBOutlets
@property (weak) typename NSWindow     *window;
@property (weak) typename NSScrollView *scrollView;
@property (weak) typename NSTextField  *textField;
// @property (weak, nonatomic) typename NSWindow     *window;
// @property (weak, nonatomic) typename NSScrollView *scrollView;
// @property (weak, nonatomic) typename NSTextField  *textField;

@end
|]


objc_implementation ['launchMsg, 'mkLine] [cunit|

@interface AppDelegate ()

@property typename NSTextView *textView;

- (void)appendOutput:(typename NSString *)text;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.textView = self.scrollView.documentView;
  NSLog(@"%@", launchMsg());
}

// IBAction
- (void)textFieldDidSend:(typename NSTextField *)sender
{
  [self appendOutput:mkLine([sender stringValue])];
  [sender setStringValue:@""];
}

- (void)appendOutput:(typename NSString *)text
{
  typename NSAttributedString *attrText = [[NSAttributedString alloc] initWithString:text];
  [self.textView.textStorage appendAttributedString:attrText];
}


@end
|]


objc_emit
