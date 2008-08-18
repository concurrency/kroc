//
//  TSPProcess.h
//  TC1
//
//  Created by Carl Ritson on 08/07/27.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/CoreAnimation.h>

@interface TSPProcess : CALayer {
	@public TSPProcess		*parent;
	@public CALayer			*border;
	CATextLayer				*title;
	CATextLayer				*heading;
	CATextLayer				*code;
	bool					blocked;
	bool					complete;
}

@property(readonly) TSPProcess *parent;
@property(readonly) CALayer *border;
 
- (id)initWithParent:(TSPProcess *)process;
- (void)setTitle: (NSString*)newTitle;
- (void)setCodePosition:(NSString*)heading withLines:(NSArray*)newCode;
- (void)endProcess;
- (bool)isComplete;
- (void)block;
- (void)unblock;
//- (void)drawInContext:(CGContextRef)ctx;

@end
