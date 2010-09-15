//
//  HelloView.m
//  HelloWorld
//
//  Created by Lars Yencken on 12/06/10.
//  Copyright 2010 University of Melbourne. All rights reserved.
//

#import "HelloView.h"


@implementation HelloView

- (id)initWithFrame:(NSRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code here.
    }
    return self;
}

- (void)drawRect:(NSRect)dirtyRect {
    NSString* hello = @"Hello, World!";
	NSPoint point = NSMakePoint(15, 75);
	NSMutableDictionary* font_attributes = [NSMutableDictionary new];
	NSFont* font = [NSFont fontWithName:@"Futura-MediumItalic" size:42];
	[font_attributes setObject:font forKey:NSFontAttributeName];
	[hello drawAtPoint:point withAttributes:font_attributes];
	[font_attributes release];
}

@end
