//
//  LayoutArea.m
//  TC1
//
//  Created by Carl Ritson on 08/08/13.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "LayoutArea.h"


@implementation LayoutArea

@synthesize position;
@synthesize orientation;
@synthesize size;

- (id)initWithOrientation:(NSUInteger)initOrientation 
					 size:(CGSize)initSize 
				 position:(CGPoint)initPosition
				   stride:(CGSize)initStride
{
	self = [super init];

	position	= initPosition;
	orientation	= initOrientation;
	size		= initSize;
	stride		= initStride;
	
	return self;
}

- (bool)canHold:(CGSize)otherSize
{
	if (size.width != 0 && otherSize.width > size.width)
		return FALSE;
	if (size.height != 0 && otherSize.height > size.height)
		return FALSE;
	return TRUE;
}

- (CGPoint)fitAndSplit:(CGSize)otherSize remainderArray:(NSMutableArray *)remainder
{
	CGPoint p = position;
	
	otherSize.height = (ceilf (otherSize.height / stride.height)) * stride.height;
	otherSize.width = (ceilf (otherSize.width / stride.width)) * stride.width;
	
	if (orientation & kOrientTop)
		p.y -= otherSize.height / 2.0f;
	else if (orientation & kOrientBottom)
		p.y += otherSize.height / 2.0f;
	
	if (orientation & kOrientLeft)
		p.x += otherSize.width / 2.0f;
	else if (orientation & kOrientRight)
		p.x -= otherSize.width / 2.0f;
	
	if (CGSizeEqualToSize (otherSize, size)) {
		/* Exact fit */
		[self autorelease];
	} else {
		CGFloat xRem = size.width - otherSize.width;
		CGFloat yRem = size.height - otherSize.height;
		
		if (xRem == 0.0f) {
			/* Exact width fit */
			
			if (orientation & kOrientTop)
				position.y -= otherSize.height;
			else
				position.y += otherSize.height;
			if (yRem > 0.0f)
				size.height = yRem;
			
			[remainder addObject:self];
		} else if (yRem == 0.0f) {
			/* Exact height fit */
			
			if (orientation & kOrientLeft)
				position.x += otherSize.width;
			else
				position.x -= otherSize.height;
			if (xRem > 0.0f)
				size.width = xRem;
			
			[remainder addObject:self];
		} else {
			/* Cutting an infinite region */
			CGPoint newPosition = position;
			CGSize newSize = CGSizeMake (MAX (0.0, xRem), MAX (0.0, yRem));
			
			switch (orientation) {
				case (kOrientBottom | kOrientLeft):
					position.y += otherSize.height;
					newPosition.x += otherSize.width;
					newSize.height = otherSize.height;
					break;
				case (kOrientBottom | kOrientRight):
					position.x -= otherSize.width;
					newPosition.y += otherSize.height;
					newSize.width = otherSize.width;
					break;
				case (kOrientTop | kOrientLeft):
					position.x += otherSize.width;
					newPosition.y -= otherSize.height;
					newSize.width = otherSize.width;
					break;
				case (kOrientTop | kOrientRight):
					position.y -= otherSize.height;
					newPosition.x -= otherSize.width;
					newSize.height = otherSize.height;
					break;
			}
			
			LayoutArea *newArea = [[LayoutArea alloc] initWithOrientation:orientation
																	 size:newSize
																 position:newPosition
																   stride:stride];
			[remainder addObject:newArea];
			[remainder addObject:self];
		}
	}
	return p;
}

@end
