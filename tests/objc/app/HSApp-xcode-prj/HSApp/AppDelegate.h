//
//  AppDelegate.h
//  HSApp
//
//  Created by Manuel M T Chakravarty on 13/04/13.
//  Copyright (c) 2013 Manuel M T Chakravarty. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface AppDelegate : NSObject <NSApplicationDelegate>

@property (weak, nonatomic) IBOutlet NSWindow     *window;
@property (weak, nonatomic) IBOutlet NSScrollView *scrollView;
@property (weak, nonatomic) IBOutlet NSTextField  *textField;

@end
