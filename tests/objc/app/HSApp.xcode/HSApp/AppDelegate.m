//
//  AppDelegate.m
//  HSApp
//
//  Created by Manuel M T Chakravarty on 13/04/13.
//  Copyright (c) 2013 Manuel M T Chakravarty. All rights reserved.
//

#import "AppDelegate.h"


@interface AppDelegate ()

@property NSTextView *textView;

- (void)appendOutput:(NSString *)text;

@end


@implementation AppDelegate

- (void)applicationDidFinishLaunching:(NSNotification *)aNotification
{
  self.textView = self.scrollView.documentView;
  [self appendOutput:@"Hello Wordl!"];
  NSLog(@"I did launch!");
}

- (IBAction)textFieldDidSend:(NSTextField *)sender
{
  [self appendOutput:[sender stringValue]];
  [sender setStringValue:@""];
}

- (void)appendOutput:(NSString *)text
{
  NSAttributedString *attrText = [[NSAttributedString alloc] initWithString:text];
  [self.textView.textStorage appendAttributedString:attrText];
}

@end
