/*
 * JEditSparkleNativeBridge.m
 * part of the Spakle plugin for the jEdit text editor
 * Copyright (C) 2009 Christian L. Jacobsen
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#include "org_transterpreter_jeditsparkleplugin_JEditSparklePlugin.h"
#include "JEditSparkleHelper.h";
#include <Foundation/Foundation.h>
#include <AppKit/AppKit.h>

/* This code is based on:
 * http://elliotth.blogspot.com/2007_09_01_archive.html
 * which is a lot simpler than this method, which I also looked at:
 * http://www.rkuntz.org/pmwiki.php?n=Code.SparkleJava
 */

JNIEXPORT void JNICALL Java_org_transterpreter_jeditsparkleplugin_JEditSparklePlugin_initSparkle(JNIEnv *a, jobject b)
{    
    bool haveBundle = ([[NSBundle mainBundle] objectForInfoDictionaryKey:@"CFBundleName"] != nil);
    
    NSString* path = [[[NSBundle mainBundle] privateFrameworksPath] 
    	stringByAppendingPathComponent: @"Sparkle.framework"];
    
    NSBundle* bundle = [NSBundle bundleWithPath:path];
    
    Class suUpdaterClass = [bundle classNamed:@"SUUpdater"];
    id suUpdater = [[suUpdaterClass alloc] init];
    
    NSMenu* menu = [[NSApplication sharedApplication] mainMenu];
    NSMenu* applicationMenu = [[menu itemAtIndex:0] submenu];
    NSMenuItem* checkForUpdatesMenuItem = [[NSMenuItem alloc]
                                            initWithTitle:@"Check for Updates..."
                                            action:@selector(checkForUpdates:)
                                            keyEquivalent:@""];
    if (haveBundle) {
        [checkForUpdatesMenuItem setEnabled:YES];
        [checkForUpdatesMenuItem setTarget:suUpdater];
    }
    // 0 => top, 1 => after "About...", 2 => after separator, 3 => after "Preferences...".
    // FIXME: should search for "Preferences...", because it might not be there!
    [applicationMenu insertItem:checkForUpdatesMenuItem atIndex:2];

    JEditSparkleHelper *h = [[JEditSparkleHelper alloc] initWithUpdater:(id)suUpdater];
    [h performSelectorOnMainThread:@selector(initSparkle)
                                withObject:nil 
                                waitUntilDone:NO];
}
