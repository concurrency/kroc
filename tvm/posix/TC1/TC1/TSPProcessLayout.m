//
//  TSPProcessLayout.m
//  TC1
//
//  Created by Carl Ritson on 08/07/30.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "TSPProcessLayout.h"
#import "TSPProcess.h"
#import "TSPChannel.h"
#import "TSPLine.h"
#import "LayoutArea.h"

@implementation TSPProcessLayout

+ (id)layoutManager
{
	return [[[self alloc] init] autorelease];
}

- (id)init
{
	return [super init];
}

static NSInteger connectivity (TSPChannel *chan, TSPProcess *source)
{
	TSPProcess *input = [chan valueForKey:@"inputLink"];
	TSPProcess *output = [chan valueForKey:@"outputLink"];
	NSInteger c = 0;
	if (input && input != source) {
		c += [((NSMutableArray *)[input valueForKey:@"channels"]) count];
	}
	if (output && input != source) {
		c += [((NSMutableArray *)[output valueForKey:@"channels"]) count];
	}
	return c;
}

static NSInteger channelSort (id chan1, id chan2, void *source)
{
	NSInteger c1 = connectivity ((TSPChannel *) chan1, (TSPProcess *) source);
	NSInteger c2 = connectivity ((TSPChannel *) chan2, (TSPProcess *) source);
    return c2 + c1;
}

static CGRect doLayout (CALayer *proc, NSUInteger mask, NSUInteger elementCount, NSArray *freeElements)
{
	CGFloat innerWidth = proc.frame.size.width;
	CGFloat innerHeight = proc.frame.size.height;
	
	CGFloat minX = -(innerWidth * 0.5);
	CGFloat maxX = (innerWidth * 0.5);
	CGFloat minY = -(innerHeight * 0.5);
	CGFloat maxY = (innerHeight * 0.5);
	
	NSMutableArray	*regions[4];
	NSUInteger		i;
	
	for (i = 0; i < 4; ++i) {
		regions[i] = [NSMutableArray arrayWithCapacity:elementCount];
	}
	
	[regions[0] addObject:[[LayoutArea alloc] initWithOrientation:(kOrientTop | kOrientLeft) 
															 size:CGSizeMake(0.0, 0.0) 
														 position:CGPointMake(maxX, maxY) 
														   stride:CGSizeMake(200.0, 60.0)]];
	[regions[1] addObject:[[LayoutArea alloc] initWithOrientation:(kOrientTop | kOrientRight) 
															 size:CGSizeMake(0.0, 0.0) 
														 position:CGPointMake(maxX, minY) 
														   stride:CGSizeMake(200.0, 60.0)]];
	[regions[2] addObject:[[LayoutArea alloc] initWithOrientation:(kOrientBottom | kOrientRight) 
															 size:CGSizeMake(0.0, 0.0) 
														 position:CGPointMake(minX, minY) 
														   stride:CGSizeMake(200.0, 60.0)]];
	[regions[3] addObject:[[LayoutArea alloc] initWithOrientation:(kOrientBottom | kOrientLeft) 
															 size:CGSizeMake(0.0, 0.0) 
														 position:CGPointMake(minX, maxY) 
														   stride:CGSizeMake(200.0, 60.0)]];
	
	NSMutableArray *rawChannels = [proc valueForKey:@"channels"];
	NSArray *channels = [rawChannels sortedArrayUsingFunction:channelSort context:proc];
	[proc setValue:channels forKey:@"channels"];
	
	i = (mask + 1) % 4;
	for (TSPChannel *chan in channels) {
		if ([[chan valueForKey:@"layedOut"] boolValue])
			continue;
		
		if (i == mask)
			i = (i + 1) % 4;
		
		LayoutArea *area = nil;
		NSUInteger j;
		
		for (j = 0; j < [regions[i] count]; ++j) {
			area = [regions[i] objectAtIndex:j];
			if ([area canHold:chan.bounds.size])
				break;
		}
		
		[area retain];
		[regions[i] removeObjectAtIndex:j];
		
		CGPoint chanP = [area fitAndSplit:chan.bounds.size remainderArray:regions[i]];
		
		[chan setValue:[NSNumber numberWithFloat:chanP.x] forKey:@"vX"];
		[chan setValue:[NSNumber numberWithFloat:chanP.y] forKey:@"vY"];
		[chan setValue:[NSNumber numberWithBool:TRUE] forKey:@"layedOut"];
		[chan setValue:[NSNumber numberWithUnsignedInt:i] forKey:@"region"];
		
		minX = MIN (minX, chanP.x - (chan.bounds.size.width * 0.5f));
		maxX = MAX (maxX, chanP.x + (chan.bounds.size.width * 0.5f));
		minY = MIN (minY, chanP.y - (chan.bounds.size.height * 0.5f));
		maxY = MAX (maxY, chanP.y + (chan.bounds.size.height * 0.5f));
		
		elementCount--;
		
		i = (i + 1) % 4;
	}
	
	for (TSPChannel *chan in channels) {
		i = [[chan valueForKey:@"region"] unsignedIntValue];
		
		TSPProcess *chanProc;
		
		if ((chanProc = [chan valueForKey:@"inputLink"]) == proc)
			chanProc = [chan valueForKey:@"outputLink"];
		
		if (chanProc && (![[chanProc valueForKey:@"layedOut"] boolValue])) {
			[chanProc setValue:[NSNumber numberWithBool:TRUE] forKey:@"layedOut"];
			
			CGRect bounds = doLayout (chanProc, (i + 2) % 4, elementCount, nil);
			LayoutArea *area = nil;
			NSUInteger j;
			
			for (j = 0; j < [regions[i] count]; ++j) {
				area = [regions[i] objectAtIndex:j];
				if ([area canHold:bounds.size])
					break;
			}
		
			[area retain];
			[regions[i] removeObjectAtIndex:j];
		
			CGPoint procP = [area fitAndSplit:bounds.size remainderArray:regions[i]];
			CGFloat oX = (procP.x - (bounds.size.width / 2.0)) + (-bounds.origin.x); 
			CGFloat oY = (procP.y - (bounds.size.height / 2.0)) + (-bounds.origin.y);
		
			[chanProc setValue:[NSNumber numberWithFloat:oX] forKey:@"vX"];
			[chanProc setValue:[NSNumber numberWithFloat:oY] forKey:@"vY"];
			
			minX = MIN (minX, procP.x - (bounds.size.width / 2.0f));
			maxX = MAX (maxX, procP.x + (bounds.size.width / 2.0f));
			minY = MIN (minY, procP.y - (bounds.size.height / 2.0f));
			maxY = MAX (maxY, procP.y + (bounds.size.height / 2.0f));
			
			elementCount--;
		}
	}
	
	if (freeElements) {
		//i = 0;
		for (CALayer *elem in freeElements) {
			if ([[elem valueForKey:@"layedOut"] boolValue])
				continue;
			
			if (i == mask)
				i = (i + 1) % 4;
			
			[elem setValue:[NSNumber numberWithBool:TRUE] forKey:@"layedOut"];
			
			CGRect bounds;
			if ([elem class] == [TSPProcess class]) {
				bounds = doLayout ((TSPProcess *)elem, (i + 2) % 4, elementCount, nil);
			} else {
				bounds = elem.bounds;
				bounds.origin.x = - (bounds.size.width / 2.0);
				bounds.origin.y = - (bounds.size.height / 2.0);
			}
				
			LayoutArea *area = nil;
			NSUInteger j;
		
			for (j = 0; j < [regions[i] count]; ++j) {
				area = [regions[i] objectAtIndex:j];
				if ([area canHold:bounds.size])
					break;
			}
		
			if (!area)
				break;
		
			[area retain];
			[regions[i] removeObjectAtIndex:j];
		
			CGPoint elemP = [area fitAndSplit:bounds.size remainderArray:regions[i]];
			CGFloat oX = (elemP.x - (bounds.size.width / 2.0)) + (-bounds.origin.x); 
			CGFloat oY = (elemP.y - (bounds.size.height / 2.0)) + (-bounds.origin.y);
			
			[elem setValue:[NSNumber numberWithFloat:oX] forKey:@"vX"];
			[elem setValue:[NSNumber numberWithFloat:oY] forKey:@"vY"];
		
			minX = MIN (minX, elemP.x - (bounds.size.width / 2.0f));
			maxX = MAX (maxX, elemP.x + (bounds.size.width / 2.0f));
			minY = MIN (minY, elemP.y - (bounds.size.height / 2.0f));
			maxY = MAX (maxY, elemP.y + (bounds.size.height / 2.0f));
			
			elementCount--;
			
			i = (i + 1) % 4;
		}
	}
	
	return CGRectMake (minX, minY, maxX - minX, maxY - minY);
}

static void doPositioning (CALayer *proc, NSArray *freeElements)
{
	NSArray *channels = [proc valueForKey:@"channels"];
	
	CGFloat x = proc.position.x;
	CGFloat y = proc.position.y;
	
	//fprintf (stderr, "%p (%.0f, %.0f)\n", proc, x, y);
	
	for (TSPChannel *chan in channels) {
		if ([[chan valueForKey:@"positioned"] boolValue])
			continue;
		
		CGFloat cX = [[chan valueForKey:@"vX"] floatValue];
		CGFloat cY = [[chan valueForKey:@"vY"] floatValue];
		
		chan.position = CGPointMake (x + cX, y + cY);
		[chan setValue:[NSNumber numberWithBool:TRUE] forKey:@"positioned"];
	}
	
	for (TSPChannel *chan in channels) {
		TSPProcess *chanProc;
		
		if ((chanProc = [chan valueForKey:@"inputLink"]) == proc)
			chanProc = [chan valueForKey:@"outputLink"];
		
		if (chanProc && (![[chanProc valueForKey:@"positioned"] boolValue])) {
			CGFloat pX = [[chanProc valueForKey:@"vX"] floatValue];
			CGFloat pY = [[chanProc valueForKey:@"vY"] floatValue];
			
			chanProc.anchorPoint = CGPointMake (0.5, 0.5);
			chanProc.position = CGPointMake (x + pX, y + pY);
			[chanProc setValue:[NSNumber numberWithBool:TRUE] forKey:@"positioned"];
			
			doPositioning (chanProc, nil);
		}
	}
	
	if (freeElements) {
		for (CALayer *elem in freeElements) {
			if ([[elem valueForKey:@"positioned"] boolValue])
				continue;
			
			CGFloat pX = [[elem valueForKey:@"vX"] floatValue];
			CGFloat pY = [[elem valueForKey:@"vY"] floatValue];
			
			elem.anchorPoint = CGPointMake (0.5, 0.5);
			elem.position = CGPointMake (x + pX, y + pY);
			[elem setValue:[NSNumber numberWithBool:TRUE] forKey:@"positioned"];
			
			if ([elem class] == [TSPProcess class])
				doPositioning ((TSPProcess *) elem, nil);
		}
	}
	
}

static void performLayout (CALayer *layer, CALayer *root, NSArray *processes, NSArray *channels, NSArray *all)
{	
	/* Setup */
	for (CALayer *sublayer in all) {
		[sublayer setValue:[NSNumber numberWithBool:FALSE] forKey:@"layedOut"];
		[sublayer setValue:[NSNumber numberWithBool:FALSE] forKey:@"positioned"];
	}
	
	/* Border */
	if ([layer class] == [TSPProcess class]) {
		if ([all count] == 0) {
			layer.borderWidth = 0.0f;
		} else {
			layer.borderWidth = 1.0f;
		}
	}
	
	/* Layout */
	[root setValue:[NSNumber numberWithBool:TRUE] forKey:@"layedOut"];
	[root setValue:[NSNumber numberWithBool:TRUE] forKey:@"positioned"];
	
	CGRect layoutBounds = doLayout (root, -1, [processes count] + [channels count], all);
	
	NSNumber *scaled = [layer valueForKey:@"scaled"];
	if (scaled && [scaled boolValue] && (layoutBounds.size.width > 200.0 || layoutBounds.size.height > 60.0)) {
		CGFloat scale = 200.0 / layoutBounds.size.width;
		scale = MIN (scale, 60.0 / layoutBounds.size.height);
		layer.borderWidth = 1.0 / scale;
		layer.bounds = CGRectMake (0.0, 0.0, layoutBounds.size.width, layoutBounds.size.height);
		layer.transform = CATransform3DMakeScale (scale, scale, scale);
	} else {
		layer.transform = CATransform3DIdentity;
	}
	
	layer.anchorPoint = CGPointMake (
									 layoutBounds.origin.x < 0.0 ? (-layoutBounds.origin.x) / layoutBounds.size.width : 0.0,
									 layoutBounds.origin.y < 0.0 ? (-layoutBounds.origin.y) / layoutBounds.size.height : 0.0
									 );
	layer.bounds = CGRectMake (0.0, 0.0, layoutBounds.size.width, layoutBounds.size.height);
	root.position = CGPointMake (0.0 - layoutBounds.origin.x, 0.0 - layoutBounds.origin.y);
	doPositioning (root, all);
	
	/* Clean-up */
	[root setValue:nil forKey:@"channels"];
	[layer setValue:nil forKey:@"channels"];
	for (TSPProcess *proc in processes) {
		[proc setValue:nil forKey:@"channels"];
	}
	for (TSPChannel *chan in channels) {
		[chan setValue:nil forKey:@"inputLink"];
		[chan setValue:nil forKey:@"outputLink"];
	}
}

- (void)layoutSublayersOfLayer:(CALayer *)layer
{	
	/* Segregate out processes and channels */
	NSUInteger arraySize		= [layer.sublayers count];
	NSMutableArray *all			= [NSMutableArray arrayWithCapacity:arraySize];
	NSMutableArray *processes	= [NSMutableArray arrayWithCapacity:arraySize];
	NSMutableArray *channels	= [NSMutableArray arrayWithCapacity:arraySize];
	NSMutableArray *lines		= [NSMutableArray arrayWithCapacity:arraySize];
	CALayer	*root				= nil;
	
	for (CALayer *sublayer in layer.sublayers) {
		if (sublayer.name == @"border") {
			root = sublayer;
		} else if ([sublayer class] == [TSPChannel class]) {
			[channels addObject:sublayer];
			[all addObject:sublayer];
		} else if ([sublayer class] == [TSPProcess class]) {
			[processes addObject:sublayer];
			[all addObject:sublayer];
		} else if ([sublayer class] == [TSPLine class]) {
			[lines addObject:sublayer];
		}
	}
	
	/* Build channel arrays and linkages */
	if (!root && ![processes count])
		return;
	else if (!root) {
		root = [processes objectAtIndex:0];
		[processes removeObjectAtIndex:0];
		[all removeObject:root];
	}
	
	[root setValue:[NSMutableArray arrayWithCapacity:[channels count]] forKey:@"channels"];
	[layer setValue:[root valueForKey:@"channels"] forKey:@"channels"];
	for (TSPProcess *proc in processes) {
		[proc setValue:[NSMutableArray arrayWithCapacity:[channels count]] forKey:@"channels"];
	}
	
	for (TSPChannel *chan in channels) {
		CALayer *inp = [chan getInput];
		CALayer *outp = [chan getOutput];
		
		if (inp) {
			while (inp.superlayer != layer && inp != layer)
				inp = inp.superlayer;
			if (inp == layer)
				inp = root;
			NSMutableArray *cs = [inp valueForKey:@"channels"];
			[chan setValue:inp forKey:@"inputLink"];
			[cs addObject:chan];
		}
		if (outp) {
			while (outp.superlayer != layer && outp != layer)
				outp = outp.superlayer;
			if (outp == layer)
				outp = root;
			NSMutableArray *cs = [outp valueForKey:@"channels"];
			[chan setValue:outp forKey:@"outputLink"];
			[cs addObject:chan];
		}
	}
	
	/* Do layout */
	[CATransaction begin];
	performLayout (layer, root, processes, channels, all);
	for (TSPLine *line in lines) {
		[line updateLayout];
	}
	[CATransaction commit];
	
	/* Propagate Updates */
	if ([layer.superlayer.layoutManager class] == [TSPProcessLayout class])
		[layer.superlayer setNeedsLayout];
	
	[layer setNeedsDisplay];
}

@end
