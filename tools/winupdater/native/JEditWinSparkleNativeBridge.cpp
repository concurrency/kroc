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

#include <windows.h>

#define WIDE_LEN 1000

JNIEXPORT void JNICALL Java_org_transterpreter_jeditwinsparkleplugin_JEditWinSparklePlugin_init
  (JNIEnv *env, jobject obj, jstring company, jstring app, jstring version, jstring url)
{
	char c[1000], a[1000], v[1000], u[4000];
	wchar_t wc[WIDE_LEN], wa[WIDE_LEN], wv[WIDE_LEN];
	int len;

        len = env->GetStringLength(company);
        env->GetStringUTFRegion(company, 0, len, c);
	MultiByteToWideChar(CP_UTF8, 0, c, len, wc, WIDE_LEN);

        len = env->GetStringLength(app);
        env->GetStringUTFRegion(app, 0, len, a);
	MultiByteToWideChar(CP_UTF8, 0, a, len, wa, WIDE_LEN);

        len = env->GetStringLength(version);
        env->GetStringUTFRegion(version, 0, len, v);
	MultiByteToWideChar(CP_UTF8, 0, v, len, wv, WIDE_LEN);

        len = env->GetStringLength(url);
        env->GetStringUTFRegion(url, 0, len, u);

	win_sparkle_set_app_details(wc, wa, wv);
	win_sparkle_set_appcast_url(u);

	win_sparkle_init();
}


JNIEXPORT void JNICALL Java_org_transterpreter_jeditwinsparkleplugin_JEditWinSparklePlugin_unload
  (JNIEnv *, jobject)
{
  win_sparkle_cleanup();
}
