/*
 * JEditSparkleHelper.m
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

#include "JEditSparkleHelper.h";

@implementation JEditSparkleHelper

-(id) initWithUpdater:(id) updater
{
	self = [super init];
	suUpdater = updater;
	return self;
}

-(void)initSparkle
{
    /* FIXME: Would be nice to get rid of warnings when calling methods
     *        on suUpdater, could link against Sparkle, thought that would
     *      not fix this method call, as it is not exposed anyway...
     */
    [suUpdater applicationDidFinishLaunching:nil];

    /*
     * FIXME: The above is technically a bit naughty, as it is not a public fn.
     *        however, if I don't call that, I have to do a relatively large 
     *        amount of work myself in order to get equivalent functionality.
     *        I don't want to essentially re-implement the logic in 
     *        applicationDidFinishLaunching, and so I hope that logic will get 
     *        exposed properly in a future Sparkle update, at which point we 
     *        should stop calling applicationDidFinishLaunching directly.
     *        The code below will also work, though it won't present the nice
     *        would you like to enable automatic updates and send profile info 
     *        dlg box which calling applicationDidFinishLaunching will.
     */
    /*
    [suUpdater setAutomaticallyChecksForUpdates:YES];
    [suUpdater setSendsSystemProfile:YES];
    [suUpdater setUpdateCheckInterval:60.0*60.0*10];
    if([suUpdater lastUpdateCheckDate] == nil)
    {
	    //[suUpdater checkForUpdatesInBackground];
    }
    [suUpdater resetUpdateCycle];
    */
    
    // The userdefaults don't seem to always get synced (unless I do 
    // it manually) !?
    [[NSUserDefaults standardUserDefaults] synchronize];

}
@end
