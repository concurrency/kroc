/*
 *  MenuView.h
 *  TC1
 *
 *  Created by Carl Ritson on 08/07/27.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#import <Cocoa/Cocoa.h> 
#import <QuartzCore/CoreAnimation.h> 

// the MenuView class is the view subclass that is inserted into 
// the window. It hosts the rootLayer, and responds to events 
@interface MenuView : NSView { 
	// contains the selected menu item index 
	NSInteger selectedIndex; 
	// the layer that contains the menu item layers 
	CALayer *menusLayer; 
	// the layer that is used for the selection display 
	CALayer *selectionLayer; 
	// the array of menu item names 
	NSArray *names; 
} 

-(void)awakeFromNib; 
-(void)setupLayers; 
-(void)changeSelectedIndex:(NSInteger)theSelectedIndex; 
-(void)moveUp:(id)sender; 
-(void)moveDown:(id)sender; 
-(void)dealloc; 
