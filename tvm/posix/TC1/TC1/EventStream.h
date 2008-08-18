//
//  EventStream.h
//  TC1
//
//  Created by Carl Ritson on 08/08/02.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/CoreAnimation.h>
#import "TSPProcess.h"
#import "TSPChannel.h"


@interface EventStream : NSObject {
	NSArray			*events;
	int				idx;
	NSMutableArray	*processes;
	NSMutableArray	*channels;
	CALayer			*rootLayer;
}

- (id)initFromStream:(NSArray *)stream;
+ (id)loadFromPath:(NSString *)path;
- (CALayer *)getRootLayer;
- (void)nextEvent;
- (void)dealloc;

@end
