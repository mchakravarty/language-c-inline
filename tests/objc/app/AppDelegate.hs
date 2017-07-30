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

evalExpr :: Session -> String -> IO String
evalExpr _session ""
  = return ""
evalExpr session input@(':' : withCommand)
  = case break (== ' ') withCommand of
      ("type", expr)  -> do
                         { result <- typeOf session expr
                         ; return $ formatResult input result
                         }
      (command, _)    -> return $ "Haskell> " ++ input ++ "\nUnknown command '" ++ command ++ "'\n"
evalExpr session expr
  = do 
    { result <- eval session expr
    ; return $ formatResult expr result
    }
  where

loadModule :: Session -> String -> IO String
loadModule session mname
  = do
    { result <- load session mname
    ; return $ formatResult "" result      
    }

formatResult :: String -> Result -> String
formatResult input result = (if null input then "" else "Haskell> " ++ input ++ "\n") ++ showResult result ++ "\n"
  where
    showResult (Result res) = res
    showResult (Error  err) = "ERROR: " ++ err


objc_interface [cunit|

@interface AppDelegate : NSResponder <NSApplicationDelegate>

// IBOutlets
@property (weak, nonatomic) typename NSWindow     *window;
@property (weak, nonatomic) typename NSScrollView *scrollView;
@property (weak, nonatomic) typename NSTextField  *textField;

@end
|]


objc_implementation [Typed 'launchMsg, Typed 'start, Typed 'evalExpr, Typed 'loadModule] [cunit|

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
  [[self.textField cell] setPlaceholderString:@"Enter an expression, or use the :type command"];
  self.textView           = self.scrollView.documentView;
  self.interpreterSession = start();
  NSLog(@"%@", launchMsg());
}

// IBAction
- (void)textFieldDidSend:(typename NSTextField *)sender
{
  typename NSString *result;
  NSLog(@"expression: %@", [sender stringValue]);
  result = evalExpr(self.interpreterSession, [sender stringValue]);
  NSLog(@"result: %@", result);
  [self appendOutput:result];
//  [self appendOutput:evalExpr(self.interpreterSession, [sender stringValue])];
  NSLog(@"reset sender");
  [sender setStringValue:@""];
}

- (void)appendOutput:(typename NSString *)text
{
  typename NSFont             *menlo13  = [NSFont fontWithName:@"Menlo-Regular" size:13];
  typename NSAttributedString *attrText = [[NSAttributedString alloc] initWithString:text 
                                                                          attributes:@{ NSFontAttributeName : menlo13 }];
  [self.textView.textStorage appendAttributedString:attrText];
  [self.textView scrollRangeToVisible:NSMakeRange([self.textView.textStorage length], 0)];
}

- (void)openDocument:(id)sender
{
  typename NSOpenPanel* panel = [NSOpenPanel openPanel];
  [panel setMessage:@"Select a Haskell module to load."];
  [panel setAllowedFileTypes:@[@"hs", @"lhs"]];
  [panel beginSheetModalForWindow:self.window completionHandler:^(typename NSInteger result){
    if (result == NSFileHandlingPanelOKButton) {

      typename NSArray* urls = [panel URLs];  
      [self appendOutput:loadModule(self.interpreterSession, [[urls firstObject] path])];
            
    }
  
  }];
}

@end
|]


objc_emit
