//
//  TSPChannel.h
//  TC1
//
//  Created by Carl Ritson on 08/08/04.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/CoreAnimation.h>
#import "TSPProcess.h"
#import "TSPLine.h"

enum {
	kChannelInput			= 0x1,
	kChannelOutput			= 0x2,
	kChannelBlockedInput	= 0x4,
	kChannelBlockedOutput	= 0x8,
	kChannelWaitingInput	= 0x10
};

@interface TSPChannel : CALayer {
	@public int		state;
	@public CGFloat	radius;
	@public CALayer	*centre;
	int				phase;
	TSPProcess		*parent;
	TSPProcess		*input;
	TSPLine			*inputLine;
	TSPProcess		*output;
	TSPLine			*outputLine;
	
}

@property(assign) int state;
@property(readonly) CGFloat radius;
@property(readonly) CALayer *centre;

- (id)initWithParent:(TSPProcess *)parent;
- (void)setInput:(TSPProcess *)process alternation:(bool)alt enable:(bool)onOff;
- (void)setInput:(TSPProcess *)process;
- (void)setOutput:(TSPProcess *)process;
- (TSPProcess *)getInput;
- (TSPProcess *)getOutput;
- (TSPProcess *)oppositeEndTo:(TSPProcess *)process;
- (CGPoint)inputPointInLayer:(CALayer *)layer;
- (CGPoint)outputPointInLayer:(CALayer *)layer;
- (void)unblocked;
- (void)deleteChannel;

@end
