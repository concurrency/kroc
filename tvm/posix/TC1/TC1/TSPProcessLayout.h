//
//  TSPProcessLayout.h
//  TC1
//
//  Created by Carl Ritson on 08/07/30.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/CoreAnimation.h>

@interface TSPProcessLayout : NSObject {
	//CGSize layoutSize;
}

+ (id)layoutManager;
- (id)init;
//- (void)invalidateLayoutOfLayer:(CALayer *)layer;
- (void)layoutSublayersOfLayer:(CALayer *)layer;
//- (CGSize)preferredSizeOfLayer:(CALayer *)layer;

@end
