//
//  HelloWorldAppDelegate.h
//  HelloWorld
//
//  Created by Lars Yencken on 12/06/10.
//  Copyright 2010 University of Melbourne. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface HelloWorldAppDelegate : NSObject <NSApplicationDelegate> {
    NSWindow *window;
}

@property (assign) IBOutlet NSWindow *window;

@end
