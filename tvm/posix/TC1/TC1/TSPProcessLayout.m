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

/*
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
*/

static void calculateForces (CALayer *root, NSArray *layers, bool channelCollisions)
{
	CGFloat k	= 2.0;
	CGFloat k2	= k * k;
	
	for (CALayer *layer in layers) {
		[layer setValue:[NSNumber numberWithFloat:0.0] forKey:@"fX"];
		[layer setValue:[NSNumber numberWithFloat:0.0] forKey:@"fY"];
	}
	
	NSUInteger count = [layers count];
	NSUInteger idx;
	for (idx = 0; idx < count; ++idx) {
		CALayer *layer		= [layers objectAtIndex:idx];
		CGFloat x			= [[layer valueForKey:@"pX"] floatValue];
		CGFloat y			= [[layer valueForKey:@"pY"] floatValue];
		CGFloat fx			= [[layer valueForKey:@"fX"] floatValue];
		CGFloat fy			= [[layer valueForKey:@"fY"] floatValue];
		CGRect thisBounds	= layer.frame;
		
		thisBounds.origin.x	= x - thisBounds.size.width / 2.0;
		thisBounds.origin.y = y - thisBounds.size.height / 2.0;
		
		/* Process <=> Channel Forces */
		if ([layer valueForKey:@"channels"]) {
			NSArray *channels = [layer valueForKey:@"channels"];
			for (TSPChannel *chan in channels) {
				CGFloat cx		= [[chan valueForKey:@"pX"] floatValue];
				CGFloat cy		= [[chan valueForKey:@"pY"] floatValue];
				
				CGPoint target = CGPointMake (CGRectGetMidX (thisBounds), CGRectGetMidY (thisBounds));
				
				if (cx < CGRectGetMinX (thisBounds)) {
					target.x = CGRectGetMinX (thisBounds);
				} else if (cx > CGRectGetMaxX (thisBounds)) {
					target.x = CGRectGetMaxX (thisBounds);
				}
				
				if (cy < CGRectGetMinY (thisBounds)) {
					target.y = CGRectGetMinY (thisBounds);
				} else if (cy > CGRectGetMaxY (thisBounds)) {
					target.y = CGRectGetMaxY (thisBounds);
				}
				
				CGFloat dX		= target.x - cx;
				CGFloat dY		= target.y - cy;
				CGFloat d		= sqrt (dX * dX + dY * dY);
				
				if (d > (chan.radius * 3.0)) {
					CGFloat cfx		= [[chan valueForKey:@"fX"] floatValue];
					CGFloat cfy		= [[chan valueForKey:@"fY"] floatValue];
					CGFloat f		= (d - k2) / k;
					
					fx -= MIN (0.05 * f, 1.0) * (dX / d);
					fy -= MIN (0.05 * f, 1.0) * (dY / d);
					
					if (channelCollisions)
						f = MIN (0.05 * f, 1.0);
					
					[chan setValue:[NSNumber numberWithFloat:(cfx + f * (dX / d))] forKey:@"fX"];
					[chan setValue:[NSNumber numberWithFloat:(cfy + f * (dY / d))] forKey:@"fY"];
				}
			}
		}
		
		if (layer == root)
			continue;
		
		/* Inter-node Forces */
		if (([layer class] != [TSPChannel class]) || channelCollisions) {
			CGRect collisionBounds = thisBounds;
			
			collisionBounds.origin.x -= 15.0;
			collisionBounds.origin.y -= 10.0;
			collisionBounds.size.width += 30.0;
			collisionBounds.size.height + 20.0;
			
			NSUInteger oidx;
			for (oidx = idx + 1; oidx < count; ++oidx) {
				CALayer	*other		= [layers objectAtIndex:oidx];
				
				if (!channelCollisions && ([other class] == [TSPChannel class]))
					continue;
				
				CGRect otherBounds	= other.frame;
				CGFloat ox			= [[other valueForKey:@"pX"] floatValue];
				CGFloat oy			= [[other valueForKey:@"pY"] floatValue];
			
				otherBounds.origin.x = ox - ((otherBounds.size.width / 2.0) + 15.0);
				otherBounds.origin.y = oy - ((otherBounds.size.height / 2.0) + 10.0);
				otherBounds.size.width += 30.0;
				otherBounds.size.height += 20.0;
				
				if (CGRectIntersectsRect (collisionBounds, otherBounds)) {
					CGFloat ofx		= [[other valueForKey:@"fX"] floatValue];
					CGFloat ofy		= [[other valueForKey:@"fY"] floatValue];
					
					CGFloat dX	= x - ox;
					CGFloat dY	= y - oy;
					CGFloat rX	= (collisionBounds.size.width + otherBounds.size.width) / 2.0;
					CGFloat rY	= (collisionBounds.size.height + otherBounds.size.height) / 2.0;
					
					CGFloat d2	= dX * dX + dY * dY;
					CGFloat d	= sqrt (d2);
					
					if (d < 0.01) {
						dX = 0.1;
						dY = 0.1;
						d2 = 0.1 * 0.1 + 0.1 * 0.1;
						d  = sqrt (dX * dX + dY * dY);
					}
				
					//CGFloat f = 5.0;
				
					CGFloat tX = (rX * 0.1) * (dX / d);
					CGFloat tY = (rY * 0.1) * (dY / d);
					
					fx += tX;
					fy += tY;
					
					if (other != root) {
						ofx -= tX;
						ofy -= tY;
					
						[other setValue:[NSNumber numberWithFloat:ofx] forKey:@"fX"];
						[other setValue:[NSNumber numberWithFloat:ofy] forKey:@"fY"];
					}
				}
			}
		}
		
		/* Holding pull */
		if ([layer valueForKey:@"oX"]) {
			CGFloat ax = [[layer valueForKey:@"oX"] floatValue];
			CGFloat ay = [[layer valueForKey:@"oY"] floatValue];
			CGFloat adX = x - ax;
			CGFloat adY = y - ay;
			CGFloat d	= sqrt (adX * adX + adY * adY);
			if (d > 0.0) {
				fx -= adX * 0.1;
				fy -= adY * 0.1;
			}
		}
		//fx += (x < 0.0 ? -0.5 : +0.5);
		//if (!channelCollisions)
		//	fy += (y < 0.0 ? -0.8 : +0.8);
		
		//NSLog (@"--");
		//NSLog ([[NSNumber numberWithFloat:fx] stringValue]);
		//NSLog ([[NSNumber numberWithFloat:fy] stringValue]);
		
		[layer setValue:[NSNumber numberWithFloat:fx] forKey:@"fX"];
		[layer setValue:[NSNumber numberWithFloat:fy] forKey:@"fY"];
	}
}
	
static CGFloat calculateAndApplyVelocity (NSArray *layers)
{
	CGFloat kE = 0.0;
	
	for (CALayer *layer in layers) {
		CGFloat px = [[layer valueForKey:@"pX"] floatValue];
		CGFloat py = [[layer valueForKey:@"pY"] floatValue];
		CGFloat vx = [[layer valueForKey:@"vX"] floatValue];
		CGFloat vy = [[layer valueForKey:@"vY"] floatValue];
		CGFloat fx = [[layer valueForKey:@"fX"] floatValue];
		CGFloat fy = [[layer valueForKey:@"fY"] floatValue];
		
		//if (isnan(vx) || isnan(fx) || isnan(vy) || isnan(fy))
		//	NSLog (@"badness");
		
		vx = vx * 0.5 + fx;
		vy = vy * 0.5 + fy;
		px = round ((px + vx) / 2.0) * 2.0;
		py = round ((py + vy) / 2.0) * 2.0;
		
		[layer setValue:[NSNumber numberWithFloat:vx] forKey:@"vX"];
		[layer setValue:[NSNumber numberWithFloat:vy] forKey:@"vY"];
		[layer setValue:[NSNumber numberWithFloat:px] forKey:@"pX"];
		[layer setValue:[NSNumber numberWithFloat:py] forKey:@"pY"];
		
		kE += (vx * vx) + (vy * vy);
	}
	
	//NSLog ([[NSNumber numberWithFloat:kE] stringValue]);
	
	return kE;
}

static CGRect calculateBounds (NSArray *layers)
{
	CGFloat minX = 0.0;
	CGFloat minY = 0.0;
	CGFloat maxX = 0.0;
	CGFloat maxY = 0.0;
	
	for (CALayer *layer in layers) {
		CGRect frame = layer.frame;
		
		CGFloat px = [[layer valueForKey:@"pX"] floatValue];
		CGFloat py = [[layer valueForKey:@"pY"] floatValue];
		
		minX = MIN (minX, px - (frame.size.width / 2.0f));
		maxX = MAX (maxX, px + (frame.size.width / 2.0f));
		minY = MIN (minY, py - (frame.size.height / 2.0f));
		maxY = MAX (maxY, py + (frame.size.height / 2.0f));
	}
	
	/*
	NSLog ([[NSNumber numberWithFloat:minX] stringValue]);
	NSLog ([[NSNumber numberWithFloat:maxX] stringValue]);
	NSLog ([[NSNumber numberWithFloat:minY] stringValue]);
	NSLog ([[NSNumber numberWithFloat:maxY] stringValue]);
	*/
	 
	return CGRectMake (minX, minY, maxX - minX, maxY - minY);
}

static CGRect doDirectedLayout (CALayer *root, NSArray *layers)
{
	CGFloat kE, dkE;
	int i;
	
	for (i = 0; i < 3; ++i) {
		calculateForces (root, layers, false);
		kE = calculateAndApplyVelocity (layers);
	}
	
	i = 0;
	do {
		calculateForces (root, layers, true);
		CGFloat nkE = calculateAndApplyVelocity (layers);
		dkE = nkE - kE;
		dkE *= dkE;
	} while (kE > 1.0 && dkE > 1.0 && ((++i) < 10));
	
	return calculateBounds (layers);
}

static void doPositioning (CGRect bounds, NSArray *layers)
{
	for (CALayer *layer in layers) {
		CGFloat px = [[layer valueForKey:@"pX"] floatValue];
		CGFloat py = [[layer valueForKey:@"pY"] floatValue];
		
		layer.anchorPoint	= CGPointMake (0.5, 0.5);
		layer.position		= CGPointMake (px - bounds.origin.x, py - bounds.origin.y);
	}
}

static void performLayout (CALayer *layer, CALayer *root, NSArray *processes, NSArray *channels, NSArray *all)
{	
	/* Setup */
	NSUInteger i = 0;
	for (CALayer *sublayer in all) {
		[sublayer setValue:[NSNumber numberWithBool:FALSE] forKey:@"layedOut"];
		[sublayer setValue:[NSNumber numberWithBool:FALSE] forKey:@"positioned"];
		
		if ([sublayer valueForKey:@"pX"]) {
			[sublayer setValue:[sublayer valueForKey:@"pX"] forKey:@"oX"];
		} else {
			CGFloat x = 10.0 * sin (0.6 * (CGFloat)i);
			//NSLog ([[NSNumber numberWithFloat:x] stringValue]);
			[sublayer setValue:[NSNumber numberWithFloat:x] forKey:@"pX"];
			[sublayer setValue:[NSNumber numberWithFloat:0.0] forKey:@"vX"];
		}
		
		if ([sublayer valueForKey:@"pY"]) {
			[sublayer setValue:[sublayer valueForKey:@"pY"] forKey:@"oY"];
		} else {
			CGFloat y = 10.0 * cos (0.6 * (CGFloat)i);
			//NSLog ([[NSNumber numberWithFloat:y] stringValue]);
			[sublayer setValue:[NSNumber numberWithFloat:y] forKey:@"pY"];
			[sublayer setValue:[NSNumber numberWithFloat:0.0] forKey:@"vY"];
		}
		
		i++;
	}
	
	/* Border */
	if ([layer class] == [TSPProcess class]) {
		if ([all count] > 1.0) {
			layer.borderWidth = 1.0f;
		} else {
			layer.borderWidth = 0.0f;
		}
	}
	
	CGRect layoutBounds = doDirectedLayout (root, all);
	
	NSNumber *scaled = [layer valueForKey:@"scaled"];
	if (scaled && [scaled boolValue] && (layoutBounds.size.width > 200.0 || layoutBounds.size.height > 60.0)) {
		CGFloat scale = 200.0 / layoutBounds.size.width;
		scale = MIN (scale, 60.0 / layoutBounds.size.height);
		[CATransaction begin];
		layer.borderWidth = 1.0f / scale;
		layer.transform = CATransform3DMakeScale (scale, scale, scale);
		layer.bounds = CGRectMake (0.0, 0.0, layoutBounds.size.width, layoutBounds.size.height);
		[CATransaction commit];
	} else {
		layer.transform = CATransform3DIdentity;
	}
	
	layer.anchorPoint = CGPointMake (
									 layoutBounds.origin.x < 0.0 ? (-layoutBounds.origin.x) / layoutBounds.size.width : 0.0,
									 layoutBounds.origin.y < 0.0 ? (-layoutBounds.origin.y) / layoutBounds.size.height : 0.0
									 );
	layer.bounds = CGRectMake (0.0, 0.0, layoutBounds.size.width, layoutBounds.size.height);
	//root.position = CGPointMake (0.0 - layoutBounds.origin.x, 0.0 - layoutBounds.origin.y);
	
	doPositioning (layoutBounds, all);
	
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
	NSUInteger arraySize		= [layer.sublayers count] + 1;
	NSMutableArray *all			= [NSMutableArray arrayWithCapacity:arraySize];
	NSMutableArray *processes	= [NSMutableArray arrayWithCapacity:arraySize];
	NSMutableArray *channels	= [NSMutableArray arrayWithCapacity:arraySize];
	NSMutableArray *lines		= [NSMutableArray arrayWithCapacity:arraySize];
	CALayer	*root				= nil;
	
	for (CALayer *sublayer in layer.sublayers) {
		if ([sublayer.name isEqualToString:@"border"]) {
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
	if (!root && ![processes count]) {
		return;
	} else if (!root) {
		root = [processes objectAtIndex:0];
	} else {
		[all addObject:root];
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
	//[CATransaction setValue:[NSNumber numberWithFloat:0.8f] forKey:kCATransactionAnimationDuration];
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
