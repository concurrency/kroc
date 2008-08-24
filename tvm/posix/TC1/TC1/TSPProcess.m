//
//  TSPProcess.m
//  TC1
//
//  Created by Carl Ritson on 08/07/27.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "TSPProcess.h"
#import "TSPProcessLayout.h"
#import "TSPHelpers.h"
#import "TSPChannel.h"

@implementation TSPProcess

@synthesize parent;
@synthesize border;

- (id)initWithParent:(TSPProcess *)process {
    self = [super init];
    if (self) {
		parent = process;
		blocked = FALSE;
		complete = FALSE;
		
		self.layoutManager = [TSPProcessLayout layoutManager];
		
		border = [CALayer layer];
		border.bounds = CGRectMake (0.0, 0.0, 200.0, 60.0);
		border.name = @"border";
		border.borderWidth = 0.5;
		border.cornerRadius = 5;
		[self addSublayer:border];
		
		title = [CATextLayer layer];
		title.anchorPoint = CGPointMake (0.5, 0.5);
		title.bounds = CGRectMake (0.0, 0.0, 190.0, 20.0);
		title.position = CGPointMake (100.0, 50.0);
		title.name = @"title";
		title.string = @"";
		title.font = @"Lucida-Grande";
		title.fontSize = 13;
		title.alignmentMode = kCAAlignmentCenter;
		title.truncationMode = kCATruncationStart;
		title.foregroundColor = CGColorCreateGenericRGB(0.0,0.0,0.0,1.0);
		title.borderWidth = 0.5;
		title.cornerRadius = 5;
		[border addSublayer:title];
		
		/*
		heading = [CATextLayer layer];
		heading.bounds = CGRectMake (0.0, 0.0, 100.0, 11.0);
		heading.position = CGPointMake (190.0, 45.0);
		heading.anchorPoint = CGPointMake (1.0, 1.0);
		heading.string = @"";
		heading.alignmentMode = kCAAlignmentCenter;
		heading.truncationMode = kCATruncationMiddle;
		heading.fontSize = 11;
		heading.cornerRadius = 5.0;
		heading.borderWidth = 0.5;
		heading.foregroundColor = CGColorCreateGenericRGB (0.0, 0.0, 0.0, 1.0);
		heading.backgroundColor = CGColorCreateGenericRGB (1.0, 1.0, 1.0, 0.9);
		[border addSublayer:heading];
		*/
		
		code = [CATextLayer layer];
		code.bounds = CGRectMake (0.0, 0.0, 190.0, 36.0);
		code.position = CGPointMake (100.0, 20.0);
		code.name = @"codeLayer";
		code.string = @"\n\n";
		//code.font=@"Lucida-Grande";
		//code.frame=codeLayer.frame;
		//code.frame=CGRectMake(0.0, 0.0, 200.0, 200.0);
		//code.anchorPoint = CGPointMake (0.0, 0.0);
		//code.position = CGPointMake (0.0, 0.0);
		//code.bounds = CGRectMake (0.0, 0.0, codeWidth, codeHeight);
		code.fontSize = 12;
		code.alignmentMode = kCAAlignmentLeft;
		code.foregroundColor = CGColorCreateGenericRGB (0.0, 0.0, 0.0, 1.0);
		code.truncationMode = kCATruncationEnd;
		//code.backgroundColor = CGColorCreateGenericRGB (1.0, 1.0, 1.0, 0.8);
		code.zPosition = 1.0f;
		[border addSublayer:code];
		
		//[codeLayer addSublayer:code];
		//[code scrollRectToVisible:CGRectMake(0.0, width, height-80, height)];

		self.borderWidth = 0.0;
		
		//self.backgroundColor = CGColorCreateGenericRGB (1.0, 1.0, 1.0, 0.3);
		//self.shadowOpacity = 0.3;
		//self.backgroundColor = CGColorCreateGenericRGB (1.0f, 1.0f, 1.0f, 1.0f);
		//self.autoresizingMask = kCALayerWidthSizable | kCALayerHeightSizable;
		//self.needsDisplayOnBoundsChange = YES;
		
		[self layoutIfNeeded];
    }
    return self;
}

- (void)setTitle : (NSString*)newTitle
{
	title.string = newTitle;
}

- (void)setCodePosition:(NSString*)theHeading withLines:(NSArray*)newCode
{
	NSMutableAttributedString *codeString = [[NSMutableAttributedString alloc] initWithString:@""];
	NSMutableAttributedString *topLine = [[NSMutableAttributedString alloc] 
											initWithString:(NSString *)[newCode objectAtIndex:0]];
	NSMutableAttributedString *midLine = [[NSMutableAttributedString alloc] 
											initWithString:(NSString *)[newCode objectAtIndex:1]];
	NSMutableAttributedString *botLine = [[NSMutableAttributedString alloc] 
											initWithString:(NSString *)[newCode objectAtIndex:2]];
	NSAttributedString *newLine = [[NSAttributedString alloc] initWithString:@"\n"];
	
	[topLine addAttribute:NSForegroundColorAttributeName 
					value:[NSColor grayColor] 
					range:NSMakeRange (0, [topLine length])];
	[midLine addAttribute:NSForegroundColorAttributeName 
					value:[NSColor blackColor] 
					range:NSMakeRange (0, [midLine length])];
	[botLine addAttribute:NSForegroundColorAttributeName 
					value:[NSColor grayColor] 
					range:NSMakeRange (0, [botLine length])];
	
	//NSFont *boldFont = [NSFont boldSystemFontOfSize:11];
	//[midLine addAttribute:NSFontAttributeName value: range:range];
	
	[codeString appendAttributedString:topLine];
	[codeString appendAttributedString:newLine];
	[codeString appendAttributedString:midLine];
	[codeString appendAttributedString:newLine];
	[codeString appendAttributedString:botLine];
	
	//heading.string = theHeading; 
	if (blocked)
		[self unblock];
	code.string = codeString;
}

- (void)endProcess
{
	complete = YES;
	[self removeFromSuperlayer];
}

- (bool)isComplete
{
	return complete;
}

- (void)block
{
	blocked = true;
	border.opacity = 0.8;
	border.backgroundColor = CGColorCreateGenericRGB (0.8, 0.8, 0.8, 0.5);
	
	/*
	CIFilter *bloomFilter = [CIFilter filterWithName:@"bloom"];
	if (bloomFilter == nil) {
		bloomFilter = [CIFilter filterWithName:@"CIBloom"];
		[bloomFilter setDefaults];
		[bloomFilter setValue:[NSNumber numberWithFloat:2.5] forKey:@"inputRadius"];
		bloomFilter.name = @"bloom";
	}
	[border setFilters:[NSArray arrayWithObject:bloomFilter]];
	[border setNeedsDisplay];
	 */
}

- (void)unblock
{
	blocked = false;
	border.opacity = 1.0;
	border.backgroundColor = CGColorCreateGenericRGB (0.0, 0.0, 0.0, 0.0);
	
	//[border setFilters:nil];
	//[border setNeedsDisplay];
}

/*
static void drawArrowedLine (CGContextRef ctx, bool bold, CGPoint *source, CGPoint *target, CGFloat targetR)
{
	CGFloat targetX	= target->x;
	CGFloat targetY	= target->y;
	CGFloat diffX	= targetX - source->x;
	CGFloat diffY	= targetY - source->y;
	CGFloat h		= sqrt ((diffX * diffX) + (diffY * diffY));
	CGFloat nvX		= diffX / h;
	CGFloat nvY		= diffY / h;
	
	if (targetR > 0.0f) {
		h		= h - targetR;
		targetX = source->x + (nvX * h);
		targetY = source->y + (nvY * h);
	}
	
	CGFloat ppX = source->x + (nvX * (h - 10.0));
	CGFloat ppY = source->y + (nvY * (h - 10.0));
	
	if (bold) {
		CGContextSetLineWidth (ctx, 2.0f);
		CGContextSetStrokeColorWithColor (ctx, [TSPHelpers black]);
	} else {
		CGContextSetLineWidth (ctx, 1.5f);
		CGContextSetStrokeColorWithColor (ctx, [TSPHelpers gray]);
	}
	
	CGContextBeginPath (ctx);
	CGContextMoveToPoint (ctx, source->x, source->y);
	CGContextAddLineToPoint (ctx, ppX, ppY);
	CGContextAddLineToPoint (ctx, ppX - (nvY * 6.0), ppY + (nvX * 6.0));
	CGContextAddLineToPoint (ctx, targetX, targetY);
	CGContextAddLineToPoint (ctx, ppX + (nvY * 6.0), ppY - (nvX * 6.0));
	CGContextAddLineToPoint (ctx, ppX, ppY);
	CGContextStrokePath (ctx);
}

- (void)drawInContext:(CGContextRef)ctx
{
	CGContextSaveGState (ctx);
	CGContextSetLineWidth (ctx, 2.0f);
	CGContextSetStrokeColorWithColor (ctx, [TSPHelpers black]);
	
	for (CALayer *sublayer in self.sublayers) {
		if ([sublayer class] != [TSPChannel class])
			continue;
		
		TSPChannel *channel = (TSPChannel *) sublayer;
		CGPoint chan = CGPointMake (CGRectGetMidX (channel.centre.frame),  CGRectGetMidY (channel.centre.frame));
		
		chan = [self convertPoint:chan fromLayer:channel];
		
		if (channel.state & kChannelInput) {
			CGPoint point = [channel inputPointInLayer:self];
			drawArrowedLine (ctx, !!(channel.state & kChannelBlockedInput), &chan, &point, 0.0);
		}
		
		if (channel.state & kChannelOutput) {
			CGPoint point = [channel outputPointInLayer:self];
			drawArrowedLine (ctx, !!(channel.state & kChannelBlockedOutput), &point, &chan, channel.radius);
		}
	}
	
	CGContextRestoreGState (ctx);
}
*/

/*
- (id<CAAction>)actionForKey:(NSString *)theKey
{
	if ([theKey isEqualToString:@"bounds"]) {
		//[self setNeedsDisplay];
		
		return [super actionForKey:@"contents"];
	} else {
		id theAction = [super actionForKey:theKey];
		
		if (theAction) {
			NSLog (theKey);
			NSLog ([theAction className]);
			if ([theAction class] == [CABasicAnimation class]) {
				
			}
		}
		
		return theAction;
	}
}
*/

@end
