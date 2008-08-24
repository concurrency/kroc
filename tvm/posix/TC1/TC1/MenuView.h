/*
 *  MenuView.h
 *  TC1
 *
 *  Created by Carl Ritson on 08/07/27.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#import <Cocoa/Cocoa.h>
#import <Quartz/Quartz.h>
#import <QuartzCore/CoreAnimation.h>
#import "EventStream.h"

@interface MenuView : NSView {
	CGPoint			viewOrigin;
	CAScrollLayer	*rootLayer;
	CALayer			*rootProcess;
	CALayer			*selected;
	bool			running;
	CGFloat			pace;
	EventStream		*eventStream;
} 

-(void)awakeFromNib; 
-(void)setupLayers;
-(void)loadEvents:(EventStream *)newStream;
-(void)open:(id)sender;
-(void)moveUp:(id)sender; 
-(void)moveDown:(id)sender;
-(void)moveRight:(id)sender;
-(void)moveLeft:(id)sender;
-(void)cancelOperation:(id)sender;
-(void)mouseDragged:(NSEvent *)event;
-(void)rightMouseDown:(NSEvent *)event;
-(void)adjustView;
-(NSDragOperation)draggingEntered:(id <NSDraggingInfo>)sender;
-(BOOL)performDragOperation:(id <NSDraggingInfo>)sender;
-(void)dealloc;

@end
