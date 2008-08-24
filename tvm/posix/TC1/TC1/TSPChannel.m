//
//  TSPChannel.m
//  TC1
//
//  Created by Carl Ritson on 08/08/04.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "TSPChannel.h"
#import "TSPHelpers.h"


@implementation TSPChannel

@synthesize state;
@synthesize radius;
@synthesize centre;

- (id)initWithParent:(TSPProcess *)process;
{
	self = [super init];
	
	state	= 0;
	radius	= 5.0;
	centre	= [CALayer layer];
	phase	= 0;
	parent	= process;
	input	= nil;
	output	= nil;
	
	centre.backgroundColor = CGColorCreateGenericRGB(0.0,0.0,0.0,1.0);
	centre.cornerRadius = radius;
	centre.bounds = CGRectMake (0.0, 0.0, radius * 2.0, radius * 2.0);
	centre.position = CGPointMake (radius * 2.0, radius * 2.0);
	[self addSublayer:centre];
	
	self.name = @"channel";
	self.bounds = CGRectMake (0.0, 0.0, radius * 4.0, radius * 4.0);
	
	//self.autoresizingMask = kCALayerWidthSizable | kCALayerHeightSizable;
	//self.needsDisplayOnBoundsChange = YES;	
	//self.delegate = self;
	//self.masksToBounds = NO;
	
	inputLine = [[TSPLine alloc] initWithType:kLineInput parent:self];
	outputLine = [[TSPLine alloc] initWithType:kLineOutput parent:self];
	
	[parent addSublayer:self];
	[parent addSublayer:inputLine];
	[parent addSublayer:outputLine];
	
	return self;
}
	
- (void)setInput:(TSPProcess *)process alternation:(bool)alt enable:(bool)onOff
{
	state = (state | kChannelInput) & (~(kChannelWaitingInput | kChannelBlockedInput));
	if (input != process) {
		input = process;
		[parent setNeedsLayout];
	}
	if (alt) {
		if (onOff) {
			state |= kChannelWaitingInput;
			[inputLine setState:kLineWaiting];
		} else {
			[inputLine setState:kLineIdle];
		}
	} else {
		state |= kChannelBlockedInput;
		[inputLine setState:kLineBlocked];
		[input block];
	}
}

- (void)setInput:(TSPProcess *)process
{
	[self setInput:process alternation:FALSE enable:FALSE];
}

- (void)setOutput:(TSPProcess *)process
{
	state |= kChannelOutput | kChannelBlockedOutput;
	if (output != process) {
		output = process;
		[parent setNeedsLayout];
	}
	[outputLine setState:kLineBlocked];
	[output block];
}

- (TSPProcess *)getInput
{
	if ((state & kChannelInput) && input) {
		if (![input isComplete])
			return input;
		
		do {
			input = input.parent;
		} while (input && [input isComplete]);
		
		if (input && ![input isComplete])
			return input;
		input = nil;
		state &= ~kChannelInput;
	}
	return nil;
}

- (TSPProcess *)getOutput
{
	if ((state & kChannelOutput) && output) {
		if (![output isComplete])
			return output;
		
		do {
			output = output.parent;
		} while (output && [output isComplete]);
		
		if (output && ![output isComplete])
			return output;
		output = nil;
		state &= ~kChannelOutput;
	}
	return nil;
}

- (TSPProcess *)oppositeEndTo:(TSPProcess *)process
{
	if (input == process)
		return [self getOutput];
	else
		return [self getInput];
}

- (CGPoint)processPointInLayer:(TSPProcess *)processLayer fromLayer:(CALayer *)layer
{
	if (processLayer) {
		CGRect selfFrame = [layer convertRect:centre.frame fromLayer:self];
		CGRect procFrame = [layer convertRect:processLayer.border.frame fromLayer:processLayer];
		CGFloat selfX = CGRectGetMidX (selfFrame);
		CGFloat selfY = CGRectGetMidY (selfFrame);
		CGPoint target = CGPointMake (CGRectGetMidX (procFrame), CGRectGetMidY (procFrame));
		
		if (selfX < CGRectGetMinX (procFrame)) {
			target.x = CGRectGetMinX (procFrame);
		} else if (selfX > CGRectGetMaxX (procFrame)) {
			target.x = CGRectGetMaxX (procFrame);
		}
		
		if (selfY < CGRectGetMinY (procFrame)) {
			target.y = CGRectGetMinY (procFrame);
		} else if (selfY > CGRectGetMaxY (procFrame)) {
			target.y = CGRectGetMaxY (procFrame);
		}
		
		return target;
	} else {
		return CGPointZero;
	}	
}

- (CGPoint)inputPointInLayer:(CALayer *)layer
{
	return [self processPointInLayer:[self getInput] fromLayer:layer];
}

- (CGPoint)outputPointInLayer:(CALayer *)layer
{
	return [self processPointInLayer:[self getOutput] fromLayer:layer];
}

- (void)unblocked
{
	state &= ~ (kChannelBlockedInput | kChannelBlockedOutput);
	[input unblock];
	[inputLine setState:kLineIdle];
	[output unblock];
	[outputLine setState:kLineIdle];
}

- (void)deleteChannel
{
	[self removeFromSuperlayer];
	[inputLine removeFromSuperlayer];
	[outputLine removeFromSuperlayer];
	parent = nil;
}

@end
