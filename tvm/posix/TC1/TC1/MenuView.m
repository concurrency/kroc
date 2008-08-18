//
//  MenuView.m
//  TC1
//
//  Created by Carl Ritson on 08/07/27.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "MenuView.h"
#import "EventStream.h"


@implementation MenuView

-(void)awakeFromNib 
{
	eventStream	= nil;
	rootProcess	= nil;
	selected	= nil;
	pace		= 1.0;
	
	NSWindow *window = [self window];
	[window setContentResizeIncrements:NSMakeSize(20.0, 10.0)];
	[self setupLayers];
	[self loadEvents:@"/Users/cgr/src/kroc/branches/tsp/tvm/posix/sort_pump.plist"];
}

-(void)setupLayers
{
	[[self window] makeFirstResponder:self];
	
	rootLayer = [CAScrollLayer layer];
	rootLayer.backgroundColor = CGColorCreateGenericRGB (1.0f, 1.0f, 1.0f, 1.0f);

	[self setLayer:rootLayer];
	[self setWantsLayer:YES];
}

- (void)alertDidEnd:(NSAlert *)alert returnCode:(int)returnCode contextInfo:(void *)contextInfo
{
	/*
    if (returnCode == NSAlertFirstButtonReturn) {
    }
	*/
}

-(void)loadEvents:(NSString *)path
{	
	if (rootProcess) {
		[rootProcess removeFromSuperlayer];
		//[rootProcess release];
		rootProcess = nil;
	}
	//if (eventStream)
	//	[eventStream release];
	
	eventStream = [EventStream loadFromPath:path];
	if (eventStream) {
		rootProcess = [eventStream getRootLayer];
		rootProcess.position = CGPointMake (self.bounds.size.width / 2.0, self.bounds.size.height / 2.0);
	
		[rootLayer addSublayer:rootProcess];
		[rootLayer layoutIfNeeded];
	
		viewOrigin = CGPointMake (0.0, 0.0);
		[self adjustView];
		
		[self.window setTitle:path];
	} else {
		NSAlert *alert = [[[NSAlert alloc] init] autorelease];
		
		[alert addButtonWithTitle:@"OK"];
		[alert setMessageText:@"Unable to open file."];
		[alert setInformativeText:path];
		
		[alert setAlertStyle:NSCriticalAlertStyle];
		
		[alert beginSheetModalForWindow:[self window] modalDelegate:self didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:) contextInfo:nil];
	}
}

-(void)open:(id)sender
{
	NSOpenPanel* openDlg = [NSOpenPanel openPanel];
	
	[openDlg setAllowsMultipleSelection:NO];
	[openDlg setCanChooseFiles:YES];
	
	if ([openDlg runModalForDirectory:nil file:nil] == NSOKButton) {
		[self loadEvents:[[openDlg filenames] objectAtIndex:0]];
	}
}

-(void)moveUp:(id)sender
{
	viewOrigin = CGPointMake (0.0, 0.0);
	[self adjustView];
}

-(void)moveDown:(id)sender
{
	if (eventStream)
		[eventStream nextEvent];
}

-(void)advance
{
	@synchronized (self) {
		if (running && eventStream) {
			[eventStream nextEvent];
			[self performSelector:@selector(advance) withObject:nil afterDelay:pace];
		}
	}
}

-(void)moveRight:(id)sender
{
	@synchronized (self) {
		pace /= 2.0;
		if (pace <= 0.01)
			pace = 0.01;
		if (!running) {
			running = true;
			[self advance];
		}
	}
}

-(void)moveLeft:(id)sender
{
	@synchronized (self) {
		pace *= 2.0;
		if (pace > 1.0)
			pace = 1.0;
		if (!running) {
			running = true;
			[self advance];
		}
	}
}

-(void)cancelOperation:(id)sender
{
	@synchronized (self) {
		if (running) {
			running = false;
			[NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(advance) object:nil];
		}
	}
}

-(void)mouseDragged:(NSEvent *)event
{
	viewOrigin.x -= [event deltaX] * 1.0;
	viewOrigin.y += [event deltaY] * 1.0;
	[self adjustView];
}

-(void)rightMouseDown:(NSEvent *)event
{
	NSPoint p = [event locationInWindow];
	CALayer *hit = [rootLayer hitTest:CGPointMake (p.x, p.y)];
	if (hit) {
		bool modified = false;
		while (hit != rootLayer) {
			if ([hit class] == [TSPProcess class]) {
				if (hit.bounds.size.width > 200.0 || hit.bounds.size.height > 60.0) {
					NSNumber *scaled = [hit valueForKey:@"scaled"];
					if (scaled && [scaled boolValue]) {
						[hit setValue:[NSNumber numberWithBool:FALSE] forKey:@"scaled"];
						[hit setNeedsLayout];
						modified = true;
					} else {
						if (!modified) {
							[hit setValue:[NSNumber numberWithBool:TRUE] forKey:@"scaled"];
							[hit setNeedsLayout];
						}
						break;
					}
				}
			}
			hit = hit.superlayer;
		}
	}
}

/*
-(void)mouseDown:(NSEvent *)event
{
	NSPoint p = [event locationInWindow];
	CALayer *hit = [rootLayer hitTest:CGPointMake (p.x, p.y)];
	if (hit) {
		while (hit != rootLayer) {
			if ([hit class] == [TSPProcess class]) {
				//hit.borderColor = CGColorCreateGenericRGB(1.0, 0.0, 0.0, 1.0);
				if (hit == selected)
					break;
				CIFilter *bloomFilter = [CIFilter filterWithName:@"bloom"];
				if (bloomFilter == nil) {
					bloomFilter = [CIFilter filterWithName:@"CIBloom"];
					[bloomFilter setDefaults];
					[bloomFilter setValue:[NSNumber numberWithFloat:3.0] forKey:@"inputRadius"];
					bloomFilter.name = @"bloom";
				}
				[hit setFilters:[NSArray arrayWithObject:bloomFilter]];
				[hit setNeedsDisplay];
				break;
			}
			hit = hit.superlayer;
		}
		if (hit == rootLayer)
			hit = nil;
	}
	if (selected && hit != selected) {
		[selected setFilters:nil];
		[selected setNeedsDisplay];
	}
	selected = hit;
}
*/

-(void)viewDidMoveToWindow
{
	[[NSNotificationCenter defaultCenter] addObserver:self 
											 selector:@selector(windowResized:) name:NSWindowDidResizeNotification 
											   object:[self window]];
}

-(void)adjustView
{
	[rootLayer scrollToPoint:viewOrigin];
}

-(void)windowResized:(NSNotification *)notification;
{
	rootProcess.position = CGPointMake (floorf (self.bounds.size.width / 2.0), floorf (self.bounds.size.height / 2.0));
	[self performSelector:@selector(adjustView) withObject:nil afterDelay:1.0];
}

-(void)dealloc
{
	[[NSNotificationCenter defaultCenter] removeObserver:self];
	if (rootProcess)
		[rootProcess autorelease];
	if (eventStream)
		[eventStream autorelease];
	[rootLayer autorelease];
    [super dealloc];
}

@end
