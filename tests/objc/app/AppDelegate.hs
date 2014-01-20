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

objc_import ["<Cocoa/Cocoa.h>", "HsFFI.h"]


-- Haskell code used from Objective-C.

launchMsg :: String
launchMsg = "HSApp did finish launching!"

evalExpr :: Session -> String -> IO String
evalExpr _session ""
  = return ""
evalExpr session input@(':' : withCommand)
  = case break (== ' ') withCommand of
      ("type", expr) -> do
                        { result <- typeOf session expr
                        ; return $ formatResult input result
                        }
      (command, _)   -> return $ "Prelude> " ++ input ++ "\nUnknown command" ++ command ++ "\n"
evalExpr session expr
  = do 
    { result <- eval session expr
    ; return $ formatResult expr result
    }
  where

formatResult :: String -> Result -> String
formatResult input result = "Prelude> " ++ input ++ "\n" ++ showResult result ++ "\n"
  where
    showResult (Result res) = res
    showResult (Error  err) = "ERROR: " ++ err


objc_interface [cunit|

@interface AppDelegate : NSObject <NSApplicationDelegate>

// IBOutlets
@property (weak, nonatomic) typename NSWindow     *window;
@property (weak, nonatomic) typename NSScrollView *scrollView;
@property (weak, nonatomic) typename NSTextField  *textField;

@end
|]


objc_implementation ['launchMsg, 'start, 'evalExpr] [cunit|

@interface AppDelegate ()

// The NSTextView in the UI.
@property (nonatomic) typename NSTextView *textView;

// Reference to the interpreter session in Haskell land.
@property (assign) typename HsStablePtr interpreterSession;

- (void)appendOutput:(typename NSString *)text;

@end

@implementation AppDelegate

- (void)applicationDidFinishLaunching:(typename NSNotification *)aNotification
{
  [[self.textField cell] setPlaceholderString:@"Enter an expression, or use the :type or :load command"];
  self.textView           = self.scrollView.documentView;
  self.interpreterSession = start();
  NSLog(@"%@", launchMsg());
}

// IBAction
- (void)textFieldDidSend:(typename NSTextField *)sender
{
  [self appendOutput:evalExpr(self.interpreterSession, [sender stringValue])];
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
