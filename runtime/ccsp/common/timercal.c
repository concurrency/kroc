/*
 *	timercal.c -- CPU timer calibration
 *	Copyright (C) 2001, 2005 Fred Barnes <frmb@kent.ac.uk>
 *	              2007 University of Kent
 *	Based on cputimerutil by Fred Barnes, which was:
 *	Based on code from Marcel Boosten in MESH
 *
 *	This program is free software; you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation; either version 2 of the License, or
 *	(at your option) any later version.
 *
 *	This program is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with this program; if not, write to the Free Software
 *	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef ENABLE_CPU_TIMERS

#ifdef OOS_BUILD
/*{{{ OOS definitions */
#include <oos_funcs.h>
#include <kernel.h>
/*}}}*/
#else /* !OOS_BUILD */
/*{{{ regular definitions */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#ifdef TARGET_OS_DARWIN
#include <sys/sysctl.h>
#endif

#include <kernel.h>

typedef struct {
	unsigned long long timeparts[4];
	struct timeval t_start;
	struct timeval t_finish;
} timeinit;

#define TIMEINIT_COUNT 1000
/*}}}*/
#endif /* !OOS_BUILD */

/*{{{ variables */
static unsigned int cpu_factor;
static unsigned int cpu_khz;
/*}}}*/

#ifdef OOS_BUILD
/*{{{  static bool read_clock_from_oos (double *cpu_speed)*/
/*
 *	read the clock from the OOS interface
 */
static bool read_clock_from_oos (double *cpu_speed)
{
	*cpu_speed = oos_cpu_speed_mhz ();
	return true;
}
/*}}}*/
#else /* !OOS_BUILD*/
/*{{{  static bool read_clock_from_proc (double *cpu_speed)*/
/*
 *	attempts to read the clock from /proc/cpuinfo
 */
static bool read_clock_from_proc (double *cpu_speed)
{
	FILE *fp;
	char buf[128];

	fp = fopen ("/proc/cpuinfo", "r");
	if (fp == NULL)
		return false;

	do {
		if (fgets (buf, sizeof buf, fp) == NULL) {
			fclose (fp);
			return 0;
		}
	} while (sscanf (buf, "cpu MHz : %lf", cpu_speed) != 1);

	fclose (fp);
	
	return true;
}
/*}}}*/
/*{{{  static bool read_clock_from_files (double *cpu_speed)*/
/*
 *	attempts to read the clock from a config file
 */
static bool read_clock_from_files (double *cpu_speed)
{
	FILE *fp;
	char fn[FILENAME_MAX];
	const char *home = getenv("HOME");
	int n;

	fp = fopen (".kroc_clock", "r");
	if (fp == NULL && home != NULL) {
		snprintf (fn, sizeof fn, "%s/.kroc_clock", home);
		fp = fopen (fn, "r");
	}
	if (fp == NULL) {
		fp = fopen ("/etc/kroc_clock", "r");
	}
	if (fp == NULL)
		return false;

	n = fscanf (fp, "%lf", cpu_speed);
	fclose (fp);

	if (n == 1) {
		return true;
	} else {
		return false;
	}
}
/*}}}*/
#ifdef TARGET_OS_DARWIN
/*{{{  static bool read_clock_from_sysctl (double *cpu_speed)*/
/*
 *	read the clock via sysctl
 */
static bool read_clock_from_sysctl (double *cpu_speed)
{
	size_t len = sizeof(int);
	int mib[2] = { CTL_HW, HW_CPU_FREQ }; 
	int clock;
	
	if (sysctl (mib, 2, &clock, &len, 0, 0) < 0) {
		return false;
	} else if (len != sizeof(clock)) {
		return false;
	} else if (!clock) {
		return false;
	}
	
	*cpu_speed = ((double) ((unsigned int) clock)) / 1000000.0;
	
	return true;
}
/*}}}*/
#endif /* TARGET_OS_DARWIN */
#ifdef TARGET_CPU_386
/*{{{  static unsigned long long read_tsc (void)*/
/*
 *	reads the CPU timer
 */
static unsigned long long read_tsc (void)
{
	union {
		struct {
			unsigned int p0;
			unsigned int p1;
		} bits;
		unsigned long long time;
	} u;
	__asm__ __volatile__ (".byte 0x0f,0x31" : "=a" (u.bits.p0), "=d" (u.bits.p1));

	return u.time;
}
/*}}}*/
/*{{{  static bool calc_cpuspeed (timeinit *t, double *cpu_speed)*/
/*
 *	calculates CPU speed
 */
static bool calc_cpuspeed (timeinit *t, double *cpu_speed)
{
	double day_diff;

	day_diff = (1e6 * (t->t_finish.tv_sec - t->t_start.tv_sec)) + (double)(t->t_finish.tv_usec - t->t_start.tv_usec);
	*cpu_speed = (double)(t->timeparts[3] - t->timeparts[1]) / day_diff;
	if (((t->timeparts[3] - t->timeparts[2]) < 10000) && ((t->timeparts[1] - t->timeparts[0]) < 10000)) {
		return true;
	} else {
		return false;
	}
}
/*}}}*/
/*{{{  static bool measure_clock_speed (double *cpu_speed)*/
/*
 *	calibrates the clock using spinning time-delays
 */
static bool measure_clock_speed (double *cpu_speed)
{
	int i, n_valid = 0;
	double sum = 0.0;
	timeinit tdata[TIMEINIT_COUNT];

	for (i = 0; i < TIMEINIT_COUNT; i++) {
		tdata[i].timeparts[0] = read_tsc ();
		gettimeofday (&tdata[i].t_start, NULL);
		tdata[i].timeparts[1] = read_tsc ();
	}
	sleep (2);
	for (i = 0; i < TIMEINIT_COUNT; i++) {
		double speed;

		tdata[i].timeparts[2] = read_tsc ();
		gettimeofday (&tdata[i].t_finish, NULL);
		tdata[i].timeparts[3] = read_tsc ();
		if (calc_cpuspeed (&tdata[i], &speed)) {
			sum += speed;
			n_valid++;
		}
	}

	if (n_valid) {
		*cpu_speed = sum / n_valid;
		return true;
	} else {
		return false;
	}
}
/*}}}*/
#endif /* TARGET_CPU_386 */
#endif /* !OOS_BUILD */

/*{{{  void ccsp_initial_cpu_speed (unsigned int *factor, unsigned int *khz)*/
/*
 * 	Provides access to any calculated or load CPU speed values.
 */
void ccsp_initial_cpu_speed (unsigned int *factor, unsigned int *khz)
{
	*factor = cpu_factor;
	*khz = cpu_khz;
}
/*}}}*/
/*{{{  bool calibrate_timers (void)*/
/*
 *	Perform CPU timer calibration, trying several approaches until we find
 *	one that works.
 *	Return 1 on success, 0 on failure.
 */
bool ccsp_calibrate_timers (void)
{
	double clock_speed;

#ifdef OOS_BUILD
	if (read_clock_from_oos (&clock_speed)) {
		/* OK */
	} else
#else /* !OOS_BUILD*/
#ifdef TARGET_OS_DARWIN
	if (read_clock_from_sysctl (&clock_speed)) {
		/* OK */
	} else
#endif /* TARGET_OS_DARWIN */
	if (read_clock_from_proc (&clock_speed)) {
		/* OK */
	} else if (read_clock_from_files (&clock_speed)) {
		/* OK */
	} else
#ifdef TARGET_CPU_386
	if (measure_clock_speed (&clock_speed)) {
		/* OK */
	} else
#endif /* TARGET_CPU_386 */
#endif /* !OOS_BUILD */
	if (true) {
		/* Fall through */
		return false;
	}

	cpu_factor = (unsigned long) ((double) (1LL << 32) / clock_speed);
	cpu_khz = (unsigned long) (clock_speed * 1024.0);

	return true;
}
/*}}}*/

#endif /* ENABLE_CPU_TIMERS */

