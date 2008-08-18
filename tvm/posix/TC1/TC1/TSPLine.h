//
//  TSPLine.h
//  TC1
//
//  Created by Carl Ritson on 08/08/18.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/CoreAnimation.h>

enum {
	kLineInput,
	kLineOutput,
	kLineHidden,
	kLineBlocked,
	kLineWaiting,
	kLineIdle
};

@interface TSPLine : CALayer {
	int			type;
	int			state;
	id			parent;
}

- (id)initWithType:(int)lineType parent:(id)channel;
- (void)setState:(int)newState;
- (void)drawInContext:(CGContextRef)ctx;
- (void)updateLayout;

@end
