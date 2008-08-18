//
//  LayoutArea.h
//  TC1
//
//  Created by Carl Ritson on 08/08/13.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/CoreAnimation.h>

enum kAreaOrientation {
	kOrientTop		= 0x1,
	kOrientBottom	= 0x2,
	kOrientLeft		= 0x4,
	kOrientRight	= 0x8
};

@interface LayoutArea : NSObject {
	@public CGPoint	position;
	@public int		orientation;
	@public CGSize	size;
	CGSize			stride;
}

@property(readonly) CGPoint position;
@property(readonly) int		orientation;
@property(readonly) CGSize	size;

- (id)initWithOrientation:(NSUInteger)initOrientation 
					 size:(CGSize)initSize 
				 position:(CGPoint)initPosition
				   stride:(CGSize)initStride;
- (bool)canHold:(CGSize)otherSize;
- (CGPoint)fitAndSplit:(CGSize)otherSize remainderArray:(NSMutableArray *)remainder;


@end
