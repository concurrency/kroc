// Platform specific header

#if defined(WIN32) || defined(__CYGWIN__)
#	include <windows.h>
#	define SLEEP Sleep
#	define TIMEVAL unsigned long
#else
#	define SLEEP m_sleep
#	include <sys/time.h> // time measurement
#	include <time.h> // time measurement
#	define TIMEVAL struct timeval
#endif

