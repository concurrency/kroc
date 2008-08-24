//
//  EventStream.m
//  TC1
//
//  Created by Carl Ritson on 08/08/02.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "EventStream.h"
#import "TSPProcessLayout.h"


@implementation EventStream

+ (NSArray *)loadStream:(NSString *)path
{
	NSData *plistData;
	NSString *error;
	NSPropertyListFormat format;
	id plist;
	
	plistData = [NSData dataWithContentsOfFile:path];
	plist = [NSPropertyListSerialization
			 propertyListFromData:plistData
			 mutabilityOption:NSPropertyListImmutable
			 format:&format
			 errorDescription:&error];
	
	if (!plist) {
		NSLog (error);
		[error release];
		return nil;
	} else {
		return (NSArray *)plist;
	}
}

- (id)initFromStream:(NSArray *)stream
{
	self = [super init];
	
	events = [stream retain];
	
	NSUInteger maxProcess = 0;
	NSUInteger maxChannel = 0;
	for (NSDictionary *op in events) {
		NSNumber *target = [op objectForKey:@"target"];
		NSNumber *channel = [op objectForKey:@"channel"];
		if (target) {
			NSUInteger val = [target unsignedIntegerValue];
			if (val > maxProcess)
				maxProcess = val;
		}
		if (channel) {
			NSUInteger val = [channel unsignedIntegerValue];
			if (val > maxChannel)
				maxChannel = val;
		}
	}
	
	idx			= 0;
	processes	= [NSMutableArray arrayWithCapacity:(maxProcess + 1)];
	channels	= [NSMutableArray arrayWithCapacity:(maxChannel + 1)];
	rootLayer	= [CALayer layer];
	rootLayer.layoutManager = [TSPProcessLayout layoutManager];
	
	NSUInteger i;
	for (i = 0; i <= maxProcess; ++i)
		[processes addObject:processes];
	for (i = 0; i <= maxChannel; ++i)
		[channels addObject:channels];
	
	return self;
}

+ (id)loadFromPath:(NSString *)path
{
	NSArray *stream = [EventStream loadStream:path];
	if (stream)
		return [[EventStream alloc] initFromStream:stream];
	else
		return nil;
}

- (CALayer *)getRootLayer
{
	return rootLayer;
}

- (void)nextEvent
{	
	if (idx >= [events count])
		return;
	
	NSDictionary	*event		= [events objectAtIndex:idx];
	NSString		*op			= [event objectForKey:@"op"];
	NSNumber		*target		= [event objectForKey:@"target"];
	NSUInteger		tidx		= [target unsignedIntegerValue];
	TSPProcess		*process	= [processes objectAtIndex:tidx];
	
	//NSLog (op);
	
	if ([op isEqualToString:@"position"]) {
		NSString	*heading	= [event objectForKey:@"heading"];
		NSArray		*lines		= [event objectForKey:@"lines"];
		
		[process setCodePosition:heading withLines:lines];
		
	} else if ([op isEqualToString:@"newp"]) {
		process = [[TSPProcess alloc] initWithParent:nil];
		
		[processes replaceObjectAtIndex:tidx withObject:process];
		
		[rootLayer addSublayer:process];
		[rootLayer layoutIfNeeded];
	} else if ([op isEqualToString:@"startp"]) {
		NSNumber	*parent		= [event objectForKey:@"parent"];
		NSUInteger	pidx		= [parent unsignedIntegerValue];
		TSPProcess	*pprocess	= [processes objectAtIndex:pidx];
		
		process	= [[TSPProcess alloc] initWithParent:pprocess];
		[process setValue:[NSNumber numberWithBool:TRUE] forKey:@"scaled"];
		
		[pprocess addSublayer:process];
		[processes replaceObjectAtIndex:tidx withObject:process];
		
	} else if ([op isEqualToString:@"symbol"]) {
		NSString	*symbol		= [event objectForKey:@"symbol"];
		
		[process setTitle:symbol];
	} else if ([op isEqualToString:@"endp"]) {
		[process endProcess];
	} else if ([op isEqualToString:@"addc"]) {
		NSUInteger	cidx		= [[event objectForKey:@"channel"] unsignedIntegerValue];
		TSPChannel	*channel	= [[TSPChannel alloc] initWithParent:process];
		
		[channels replaceObjectAtIndex:cidx withObject:channel];
		
	} else if ([op isEqualToString:@"input"]) {
		NSUInteger	cidx		= [[event objectForKey:@"channel"] unsignedIntegerValue];
		TSPChannel	*channel	= [channels objectAtIndex:cidx];
		
		[channel setInput:process];
	} else if ([op isEqualToString:@"output"]) {
		NSUInteger	cidx		= [[event objectForKey:@"channel"] unsignedIntegerValue];
		TSPChannel	*channel	= [channels objectAtIndex:cidx];
		
		[channel setOutput:process];
	} else if ([op isEqualToString:@"unblocked"]) {
		NSUInteger	cidx		= [[event objectForKey:@"channel"] unsignedIntegerValue];
		TSPChannel	*channel	= [channels objectAtIndex:cidx];
		
		[channel unblocked];
	} else if ([op isEqualToString:@"enable"]) {
		NSUInteger	cidx		= [[event objectForKey:@"channel"] unsignedIntegerValue];
		TSPChannel	*channel	= [channels objectAtIndex:cidx];
		
		[channel setInput:process alternation:TRUE enable:TRUE];
	} else if ([op isEqualToString:@"disable"]) {
		NSUInteger	cidx		= [[event objectForKey:@"channel"] unsignedIntegerValue];
		TSPChannel	*channel	= [channels objectAtIndex:cidx];
		
		[channel setInput:process alternation:TRUE enable:FALSE];
	} else if ([op isEqualToString:@"deletec"]) {
		NSUInteger	cidx		= [[event objectForKey:@"channel"] unsignedIntegerValue];
		TSPChannel	*channel	= [channels objectAtIndex:cidx];
		
		[channel deleteChannel];
	} else if ([op isEqualToString:@"wait"]) {
		[process block];
	}
	
	idx++;
}

- (void)dealloc
{
	[events release];
	//[processes removeAllObjects];
	[processes release];
	//[channels removeAllObjects];
	[channels release];
    [super dealloc];
}


@end
