//
//  TSPHelpers.m
//  TC1
//
//  Created by Carl Ritson on 08/08/05.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "TSPHelpers.h"


@implementation TSPHelpers

+ (CGColorRef)black
{
	static CGColorRef black = NULL;
	if (black == NULL) {
		black = CGColorCreateGenericRGB (0.0, 0.0, 0.0, 1.0);
	}
	return black;
}

+ (CGColorRef)white
{
	static CGColorRef white = NULL;
	if (white == NULL) {
		white = CGColorCreateGenericRGB (1.0, 1.0, 1.0, 1.0);
	}
	return white;
}

+ (CGColorRef)gray
{
	static CGColorRef gray = NULL;
	if (gray == NULL) {
		gray = CGColorCreateGenericRGB (0.5, 0.5, 0.5, 0.5);
	}
	return gray;
}

@end
