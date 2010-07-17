/*
 * JEditWinSparkleNativeBridge.m
 * part of the Windows Spakle plugin for the jEdit text editor
 * Copyright (C) 2010 Christian L. Jacobsen
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

#include "org_transterpreter_jeditwinsparkleplugin_JEditWinSparklePlugin.h"
#include "winsparkle.h"
#include <wchar.h>


JNIEXPORT void JNICALL Java_org_transterpreter_jeditwinsparkleplugin_JEditWinSparklePlugin_init
  (JNIEnv *, jobject)
{
	win_sparkle_set_app_details(L"A company", L"An app", L"1.1");
	win_sparkle_set_appcast_url("http://winsparkle.org/example/appcast.xml");
	win_sparkle_init();
}
