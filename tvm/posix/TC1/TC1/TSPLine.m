//
//  TSPLine.m
//  TC1
//
//  Created by Carl Ritson on 08/08/18.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "TSPLine.h"
#import "TSPChannel.h"
#import "TSPHelpers.h"


@implementation TSPLine

- (id)initWithType:(int)lineType parent:(id)channel
{
	self = [super init];
	if (!self)
		return nil;
	
	type	= lineType;
	state	= kLineHidden;
	parent	= channel;

	self.anchorPoint = CGPointMake (0.0, 0.5);
	self.zPosition = -10.0;
	
	return self;
}

- (void)setState:(int)newState
{
	if (state != newState) {
		state = newState;
		[self setNeedsDisplay];
	}
}

- (void)drawInContext:(CGContextRef)ctx
{
	if (state == kLineHidden)
		return;
	
	CGContextSaveGState (ctx);
	
	if (state == kLineBlocked) {
		CGContextSetLineWidth (ctx, 2.0f);
		CGContextSetStrokeColorWithColor (ctx, [TSPHelpers black]);
	} else {
		CGContextSetLineWidth (ctx, 1.5f);
		CGContextSetStrokeColorWithColor (ctx, [TSPHelpers gray]);
	}
	
	CGSize size	= self.bounds.size;
	CGFloat xe, xo, x1;
	
	if (type == kLineOutput) {
		xo = 0.0;
		xe = size.width;
		x1 = xe - 10.0;
	} else {
		xo = size.width;
		xe = 0.0;
		x1 = 10.0;
	}
	
	CGFloat y	= size.height / 2.0;
	CGFloat y1	= y + 6.0;
	CGFloat y2	= y - 6.0;
	
	CGContextBeginPath (ctx);
	CGContextMoveToPoint (ctx, xo, y);
	CGContextAddLineToPoint (ctx, x1, y);
	CGContextAddLineToPoint (ctx, x1, y1);
	CGContextAddLineToPoint (ctx, xe, y);
	CGContextAddLineToPoint (ctx, x1, y2);
	CGContextAddLineToPoint (ctx, x1, y);
	CGContextStrokePath (ctx);
	
	CGContextRestoreGState (ctx);
}

- (void)updateLayout
{
	if (state == kLineHidden)
		return;
	
	TSPChannel *chan = (TSPChannel *) parent;
	CGPoint pE = CGPointMake (CGRectGetMidX (chan.centre.frame),  CGRectGetMidY (chan.centre.frame));
	CGPoint pS;
	
	pE = [self.superlayer convertPoint:pE fromLayer:chan];
	
	if (type == kLineInput) {
		pS = [chan inputPointInLayer:self.superlayer];
	} else {
		pS = [chan outputPointInLayer:self.superlayer];
	}
	
	CGFloat diffX	= pE.x - pS.x;
	CGFloat diffY	= pE.y - pS.y;
	CGFloat h		= sqrt ((diffX * diffX) + (diffY * diffY));
	CGFloat theta	= asin (diffY / h);
	
	h -= chan.centre.bounds.size.width / 2.0;

	//CGFloat x		= pS.x + (diffX * 0.5);
	//CGFloat y		= pS.y + (diffY * 0.5);
	if (diffX < 0)
		theta = 3.14571 - theta;
	
	self.transform = CATransform3DMakeRotation (theta, 0.0, 0.0, 1.0);
	
	/* XXX */
	self.position = pS;
	if (self.bounds.size.width != h) {
		self.bounds = CGRectMake (0.0, 0.0, h, 15.0);
		[self setNeedsDisplay];
	}
	
	//[self 
}

@end
