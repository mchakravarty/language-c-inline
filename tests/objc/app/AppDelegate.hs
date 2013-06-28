{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

-- HSApp: a simple Cocoa app in Haskell
--
-- Application delegate object, abused as a view controller

module AppDelegate (objc_initialise) where

  -- language-c-inline
import Language.C.Quote.ObjC
import Language.C.Inline.ObjC

  -- friends
import Interpreter

objc_import ["<Cocoa/Cocoa.h>"]


-- Haskell code used from Objective-C.

launchMsg :: String
launchMsg = "HSApp did finish launching!"

evalExpr :: String -> IO String
evalExpr expr 
  = do 
    { result <- eval expr
    ; return $ "Prelude> " ++ expr ++ "\n" ++ result ++ "\n"
    }


objc_interface [cunit|

@interface AppDelegate : NSObject <NSApplicationDelegate>

// IBOutlets
@property (weak) typename NSWindow     *window;
@property (weak) typename NSScrollView *scrollView;
@property (weak) typename NSTextField  *textField;
// FIXME: urgh: bug in the ObjC parser...
// @property (weak, nonatomic) typename NSWindow     *window;
// @property (weak, nonatomic) typename NSScrollView *scrollView;
// @property (weak, nonatomic) typename NSTextField  *textField;

@end
|]


objc_implementation ['launchMsg, 'evalExpr] [cunit|

@interface AppDelegate ()

@property typename NSTextView *textView;

- (void)appendOutput:(typename NSString *)text;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  self.textView = self.scrollView.documentView;
  [self.textView becomeFirstResponder];
  NSLog(@"%@", launchMsg());
}

// IBAction
- (void)textFieldDidSend:(typename NSTextField *)sender
{
  [self appendOutput:evalExpr([sender stringValue])];
  [sender setStringValue:@""];
}

- (void)appendOutput:(typename NSString *)text
{
  typename NSFont             *menlo13  = [NSFont fontWithName:@"Menlo-Regular" size:13];
  typename NSAttributedString *attrText = [[NSAttributedString alloc] initWithString:text 
                                                                          attributes:@{ NSFontAttributeName : menlo13 }];
  [self.textView.textStorage appendAttributedString:attrText];
}


@end
|]


objc_emit
