/*
 *	ioport: serial port interface
 *	Copyright (C) 2008  Carl Ritson <cgr@kent.ac.uk>
 *
 *	This library is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU Lesser General Public
 *	License as published by the Free Software Foundation, either
 *	version 2 of the License, or (at your option) any later version.
 *
 *	This library is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	Lesser General Public License for more details.
 *
 *	You should have received a copy of the GNU Lesser General Public
 *	License along with this library.  If not, see
 *	<http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include <sys/termios.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/ioctl.h>

enum {
	PARITY_NONE	= 0,
	PARITY_ODD	= 1,
	PARITY_EVEN	= 2,
	FLOW_NONE	= 0,
	FLOW_HARD	= 1,
	FLOW_SOFT	= 2
};

static speed_t make_speed (int baud)
{
	switch (baud) {
		case 1200:	return B1200;
		case 2400:	return B2400;
		case 4800:	return B4800;
		case 9600:	return B9600;
		case 19200:	return B19200;
		case 38400:	return B38400;
		#if defined(B57600)
		case 57600:	return B57600;
		#endif
		#if defined(B115200)
		case 115200:	return B115200;
		#endif
		default:	return B0;
	}
}


static int configure_serial_port (int fd, 
			int baud, 
			int parity, 
			int databits, 
			int stopbits, 
			int flow)
{
	struct termios tio;
	int ret;

	/*
	fprintf (stderr, "fd = %d, baud = %d, parity = %d, databits = %d, stopbits = %d, flow = %d\n", fd, baud, parity, databits, stopbits, flow);
	*/

	if ((ret = tcgetattr (fd, &tio))) {
		fprintf (stderr, "tcgetattr failed %d\n", ret);
		return -1;
	}

	/*
	fprintf (stderr, "old: iflag %x, oflag %x, cflag %x, lflag %x, speed %d\n",
		(unsigned int) tio.c_iflag, 
		(unsigned int) tio.c_oflag,
		(unsigned int) tio.c_cflag,
		(unsigned int) tio.c_lflag,
		(unsigned int) tio.c_ispeed
	);
	*/

	cfmakeraw (&tio);
	cfsetispeed (&tio, make_speed (baud));
	cfsetospeed (&tio, make_speed (baud));
	tio.c_iflag &= ~(IXON | IXOFF | IXANY);
	tio.c_iflag &= ~(INPCK | ISTRIP | IGNPAR);
	tio.c_iflag |= IGNBRK;
	tio.c_lflag = 0;
	tio.c_oflag = 0;

	tio.c_cc[VMIN] = 1;
	tio.c_cc[VTIME] = 1;

	tio.c_cflag |= CREAD | CLOCAL;
	tio.c_cflag &= ~CSIZE;
	switch (databits) {
		case 5: tio.c_cflag |= CS5; break;
		case 6: tio.c_cflag |= CS6; break;
		case 7: tio.c_cflag |= CS7; break;
		default: 
			tio.c_cflag |= CS8; break;
	}

	if (stopbits == 2)
		tio.c_cflag |= CSTOPB;
	else
		tio.c_cflag &= ~CSTOPB;

	tio.c_cflag &= ~(PARENB | PARODD);
	switch (parity) {
		case PARITY_EVEN: 
			tio.c_cflag |= PARENB; break;
		case PARITY_ODD:
			tio.c_cflag |= PARENB | PARODD; break;
		default:
			tio.c_iflag |= IGNPAR; break;
	}

	/*
	#if defined (CRTSCTS)
	tio.c_cflag	&= ~(CRTSCTS);
	#endif
	switch (flow) {
		#if defined (CRTSCTS)
		case FLOW_HARD:
			tio.c_cflag |= CRTSCTS; 
			break;
		#endif
		case FLOW_SOFT:
			tio.c_iflag |= IXON | IXOFF;
			break;
	}
	*/
	/*
	fprintf (stderr, "new: iflag %x, oflag %x, cflag %x, lflag %x, speed %d\n",
		(unsigned int) tio.c_iflag, 
		(unsigned int) tio.c_oflag,
		(unsigned int) tio.c_cflag,
		(unsigned int) tio.c_lflag,
		(unsigned int) tio.c_ispeed
	);
	*/
	
	tcflush (fd, TCIOFLUSH);
	if ((ret = tcsetattr (fd, TCSANOW, &tio))) {
		fprintf (stderr, "tcsetattr failed %d\n", ret);
	}
	if ((ret = tcgetattr (fd, &tio))) {
		fprintf (stderr, "tcgetattr failed %d\n", ret);
		return -1;
	}

	/*
	fprintf (stderr, "result: iflag %x, oflag %x, cflag %x, lflag %x, speed %d\n",
		(unsigned int) tio.c_iflag, 
		(unsigned int) tio.c_oflag,
		(unsigned int) tio.c_cflag,
		(unsigned int) tio.c_lflag,
		(unsigned int) tio.c_ispeed
	);
	*/

#if 0
	/* Assert RTS */
	#if defined(TIOCM_RTS) && defined(TIOCMODG)
	{
		int mcs=0;

		ioctl (fd, TIOCMODG, &mcs);
		mcs |= TIOCM_RTS;
		ioctl (fd, TIOCMODS, &mcs);
	}
	#endif
	#ifdef TIOCSRTS 
	ioctl (fd, TIOCSRTS, 0);
	#endif

	/* Be double sure CLOCAL and HUPCL are set */
	tcgetattr (fd, &tio);
	tio.c_cflag |= CLOCAL | HUPCL;
	tcsetattr (fd, TCSANOW, &tio);

	/* Flush buffers */
	#ifdef TCFLSH
	ioctl (fd, TCFLSH, 2);
	#endif
	#ifdef TIOCFLUSH
	{
		int out = 0;
		ioctl (fd, TIOCFLUSH, &out);
	}
	#endif

	/* Toggle DTR line */
	#if defined(TIOCCDTR) && defined(TIOCSDTR)
	ioctl (fd, TIOCCDTR, 0);
	sleep (1);
	ioctl (fd, TIOCSDTR, 0);
	#else
	{
		struct termios old;
		tcgetattr (fd, &old);
		tcgetattr (fd, &tio);
		cfsetospeed (&tio, B0);
		cfsetispeed (&tio, B0);
		tcsetattr (fd, TCSANOW, &tio);
		sleep (1);
		tcsetattr (fd, TCSANOW, &old);
	}
	#endif
#endif

	return 0;
}

static int wait_and_read (int timeout, int fd, char *buffer, int buflen)
{
	struct timeval tv;
	fd_set fds;
	int ret;

	tv.tv_sec = timeout / 1000000;
	tv.tv_usec = timeout - (tv.tv_sec * 1000000);

	FD_ZERO (&fds);
	FD_SET (fd, &fds);

	if ((ret = select (fd + 1, &fds, NULL, NULL, &tv)) > 0) {
		return read (fd, buffer, buflen);
	}
	
	return ret;
}

/* PROC B.configure.serial.port (VAL INT fd, baud, parity, data.bits, stop.bits, flow, RESULT INT result) = 3 */
void _configure_serial_port (int ws[])
{
	*((int *)ws[6]) = configure_serial_port (
		ws[0], ws[1], ws[2], ws[3], ws[4], ws[5]
	);
}

/* PROC B.wait.and.read (VAL INT timeout, fd, []BYTE buffer, RESULT INT result) = 3 */
void _wait_and_read (int ws[])
{
	*((int *)ws[4]) = wait_and_read (
		ws[0], ws[1], (char *) ws[2], ws[3]
	);
}
