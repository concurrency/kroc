/* BareMichael SCC baremetal framework.
 * Copyright (C) 2012.  All rights reserved. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <ext/stdio_filebuf.h>
#include <unistd.h>
#include <signal.h>
#include <iomanip>

#define MAXATTEMPTS 4
#define PBSIZE 65536
#define HEADTAIL 0x1ec000000 /* address of head */
#define PBOFFSET 0x1ec001000 /* address of beginning of print buf */
#define NUMCORES 48

using namespace std;

void ex_program(int sig);

int main() {
	(void) signal(SIGINT, ex_program);
	int attempts = 0, i, lastCoreToPrint = -1;
	ifstream datafile;
	char str[100], substr[10], memline[25], compare[25], lastChar[NUMCORES], toPrint;
	long int head[NUMCORES], tail[NUMCORES], minmemget, maxmemget;
	char wrapped = 0;

	FILE *pipe = NULL;

	/* compare and substr[8] won't ever change */
	sprintf(compare, "%016lx", HEADTAIL);
	substr[8] = '\0';

	while(1) {
//	sprintf(str, "sccDump -d 0 0x%lx %d 4", HEADTAIL, 16*NUMCORES);
//	TODO: delete next line and uncomment prev line when 128-bit write restriction bug of sccKit 1.4.1 is fixed
	sprintf(str, "sccDump -d 0 0x%lx %d 4", HEADTAIL, 32*NUMCORES);
	if (!(pipe = popen(str, "r"))) {
		printf("MIKETERM ERROR trying to do popen(\"sccDump ...\") [head/tail].  Bye!\n");
		exit(1);
	}
	// TODO: try/catch the following constructions
	__gnu_cxx::stdio_filebuf<char> pipe_buf(pipe, ios_base::in);
	istream pipe_stream(&pipe_buf);

	if (pipe_stream.fail()) {
		printf("MIKETERM ERROR opening pipe_stream [head/tail].  Bye!\n");
		exit(1);
	}

	/* iterate through lines of pipe stream to find head and tail */
	while ( pipe_stream.getline(str, 100) ) {
		if (!strncmp(str, compare, 16)) { /* correct line */
			/* get all heads and tails */
			for (i=0; i<NUMCORES; i++) {
				strncpy(substr, &str[19], 8);
				head[i] = strtol(substr, NULL, 16);
				/* next line for tail */
//				pipe_stream.getline(str, 100);
//	TODO: delete next line and uncomment prev line when 128-bit write restriction bug of sccKit 1.4.1 is fixed
				pipe_stream.getline(str, 100); pipe_stream.getline(str, 100);
				strncpy(substr, &str[19], 8);
				tail[i] = strtol(substr, NULL, 16);
//				pipe_stream.getline(str, 100);
//	TODO: delete next line and uncomment prev line when 128-bit write restriction bug of sccKit 1.4.1 is fixed
				pipe_stream.getline(str, 100); pipe_stream.getline(str, 100);
			}
			break;
		}
	}

	pclose(pipe);
	
	for (i=0; i<NUMCORES; i++) {
		if (head[i] != tail[i]) {
			/* time to print stuff! */
			/* if different core was last one to print, then we \n */
			if (lastCoreToPrint != i) {
				cout << "\n" << flush;
				/* if different core was last one to print, AND our last character was not a new line, we must announce ourselves. If lastChar WAS a new line, then the announcement will take place below (still before the next char is printed). */
				if (lastChar[i] != '\n') {
					cout << "\033[1;31m[" << setfill('0') << setw(2) << i << "]:\033[0;39m " << flush;
				}
			}
			lastCoreToPrint = i;
			/* find out how much print buffer to dump */
			if (head[i] < tail[i]) { /* dump all */
				minmemget = 0;
				maxmemget = PBSIZE;
			} else {
				minmemget = tail[i] - (tail[i] % 32);
				maxmemget = head[i] + (32 - (head[i] % 32));
			}

reDump:
			sprintf(str, "sccDump -d 0 0x%lx %ld",
					minmemget + PBOFFSET + PBSIZE*i, maxmemget - minmemget);
			if (!(pipe = popen(str, "r"))) {
				printf("MIKETERM ERROR trying to do popen(\"sccDump ...\") [buffer].  Bye!\n");
				exit(1);
			}
			// TODO: try/catch the following constructions
			__gnu_cxx::stdio_filebuf<char> pipe_buf(pipe, ios_base::in);
			istream pipe_stream(&pipe_buf);

			if (pipe_stream.fail()) {
				printf("MIKETERM ERROR opening pipe_stream [buffer].  Bye!\n");
				exit(1);
			}

			/* Now parse and print the character data */
			str[0] = '\0';
			while (head[i] != tail[i]) {
				/* is str already the correct line? */
				sprintf(memline, "%016lx",
						(PBOFFSET + PBSIZE*i + tail[i]) - ((PBOFFSET + PBSIZE*i + tail[i]) % 8) );
				if (strncmp(str, memline, 16)) {
					/* load str with next line */
					if (!pipe_stream.getline(str, 100)) {
						/* if haven't wrapped yet, then
						 * wrap to beginning and search once more */
						if (!wrapped) {
							wrapped = 1;
							goto reDump;
						} else { /* else, terminate gracefully */
							printf("MIKETERM ERROR: Couldn't find appropriate line in buffer. Bye!\n");
							printf("  head = %ld; tail = %ld\n", head[i], tail[i]);
							exit(1);
						}
					}
					continue;
				}

				/* str is now correct line. */
				wrapped = 0;
				toPrint = (char)strtol(&str[19 + (tail[i] % 8) * 3], NULL, 16);
				if (lastChar[i] == '\n') {
					cout << "\033[1;31m[" << setfill('0') << setw(2) << i << "]:\033[0;39m " << flush;
				}
				cout << toPrint << flush;
				lastChar[i] = toPrint;

				tail[i]++;
				if (tail[i] >= PBSIZE) tail[i] = 0;
			}

			pclose(pipe);

			/* use sccWrite to adjust tail up to new value */
//			sprintf(str, "sccWrite -d 0 0x%lx 0x%lx > /dev/null", HEADTAIL+8 + 16*i, tail[i]);
//	TODO: delete next line and uncomment prev line when 128-bit write restriction bug of sccKit 1.4.1 is fixed
			sprintf(str, "sccWrite -d 0 0x%lx 0x%lx > /dev/null", HEADTAIL+16 + 32*i, tail[i]);
			while (system(str)) {
				if (++attempts >= MAXATTEMPTS) {
					printf("MIKETERM ERROR: Too many failed attempts at sccWrite.  Bye!\n");
					exit(1);
				}
			}
			attempts = 0;

			/* should probably check that tail out and make sure it really wrote...
			 * but, not going to. */
		}
	}

	/* sleep 1 second before looping */
	sleep(1);
	}

	return 0;
}

/* This executes if ctrl-c has been pressed. */
void ex_program(int sig) {
	printf("\nThanks for flying MikeTerm!\n");
	exit(0);
//	(void) signal(SIGINT, SIG_DFL);
}
