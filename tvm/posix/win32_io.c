
#include "tvm_posix.h"

/* Types for the function hooks */
typedef int (*CHAR_AVAILABLE)(void);
typedef BYTE (*READ_CHAR)(void);

/* Function hooks */
static CHAR_AVAILABLE char_available_win32;
static READ_CHAR read_char_win32;
 
/* EOF For pipes */
static int pipe_eof = 0;

/****************************************************************************
 ******************************** CONSOLE ***********************************
 ****************************************************************************/

/* Checks to see if a character is available in the console input buffer.
 *
 * The following, possibly bad, assumptions are made:
 *   - One key event is put into the ReadConsoleA buffer per event seen by
 *     peek, even if REPEAT > 1
 *   - Key events are not put into ReadConsoleA's buffer on keyup events
 * Given that peek:
 *   - Both keyup and keydown events
 *   - Events for any key (ie shift, ctrl, etc)
 *   - Possibly multiple events on key held down
 *   - Possibly a one or more events with a repeat value > 0 on key held down
 * And that we are using ReadConsoleA to read characters, which:
 *   - Only gives us ASCII key events, not control keys (shift, ctrl, etc)
 */
static int char_available_console_win32(void)
{
	DWORD count;
	INPUT_RECORD data;
	
	/* GetNumberOfConsoleInputEvents:
		 http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/getnumberofconsoleinputevents.asp 
	   PeekConsoleInput:
		 http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/peekconsoleinput.asp
	   */
	
	/* Peek at the next event */
	PeekConsoleInputA(GetStdHandle(STD_INPUT_HANDLE), &data, 1, &count);
	/*
	printf("avail (1): count %d; uChar '%c' (%x); rep %d; down %d \n", 
		count, 
		data.Event.KeyEvent.uChar.AsciiChar, 
		data.Event.KeyEvent.uChar.AsciiChar,
		data.Event.KeyEvent.wRepeatCount,
		data.Event.KeyEvent.bKeyDown);
	*/
	
	/* We need to check to see if we have a char event, and not
	   something else, like a mouse event, or console resize event */
	while(count > 0 && (data.EventType != KEY_EVENT || 
		data.Event.KeyEvent.uChar.AsciiChar == 0 ||
		data.Event.KeyEvent.bKeyDown == 0))
	{
		/* Remove the event we just looked at */
		ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), &data, 1, &count);
		/* Peek at the next event */
		PeekConsoleInputA(GetStdHandle(STD_INPUT_HANDLE), &data, 1, &count);
		
		/*
		printf("avail (2): count %d; uChar '%c' (%x); rep %d; down %d \n", 
			count, 
			data.Event.KeyEvent.uChar.AsciiChar, 
			data.Event.KeyEvent.uChar.AsciiChar,
			data.Event.KeyEvent.wRepeatCount,
			data.Event.KeyEvent.bKeyDown);
			*/
	}
	
	/*
	printf("avail (3): count %d; uChar '%c' (%x); rep %d; down %d \n", 
		count, 
		data.Event.KeyEvent.uChar.AsciiChar, 
		data.Event.KeyEvent.uChar.AsciiChar,
		data.Event.KeyEvent.wRepeatCount,
		data.Event.KeyEvent.bKeyDown);
	*/
		
	/* No more events? */
	if(count == 0)
	{
		/* We ran out of events, and none were keyboard events */
		return 0;
	}
	
	/* Otherwise we got a keyboard event waiting */
	return 1;
}
	
	
/* Read a character from the console input buffer */	
static BYTE read_char_console_win32(void)
{
	DWORD count;
	char ch;
	
	/* Read the next available event (which should be a character) */
	ReadConsoleA(GetStdHandle(STD_INPUT_HANDLE), &ch, 1, &count, 0);
	/* printf("read %c\n", ch); */
	
	return ch;
}


/****************************************************************************
 ********************************** PIPE ************************************
 ****************************************************************************/

/* Right, according to MSDN, the console functions may not be the right things
 * to use here, ie:
 * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/readconsole.asp
 * says that: 
 *   Although ReadConsole can only be used with a console input buffer
 *   handle, ReadFile can be used with other handles (such as files or
 *   pipes). ReadConsole fails if used with a standard handle that has been
 *   redirected to be something other than a console handle.
 * which could very well be whats happening here!!!! Considering that it
 * works when I run the programs in a console, and they dont work when I run
 * them from within Java.... it would make sense to think that we are
 * actually dealing with a pipe at that point!
 * Which means that all the console functions that I am using might be failing....
 *
 * It turns out of course, that I cannot get the console functions to work on
 * pipes, nor pipe functions to work on cosoles. So we have to detect which we
 * need, and do the right thing.
 */

static int char_available_pipe_win32(void)
{
	DWORD count;
	BOOL result;
		
	/* Peek at the next event */
	result = PeekNamedPipe(GetStdHandle(STD_INPUT_HANDLE), NULL, 0, NULL, &count, NULL);
	/* printf("avail: %d; result %d\n", count, result); */
	
	/* Something went wrong while peeking, lets assume that it was an EOF */
	if(result == 0)
	{
		pipe_eof = 1;
		return 1;
	}
	
	/* No more events? */
	if(count == 0)
	{
		/* We ran out of events, and none were character events */
		return 0;
	}
	
	/* Otherwise we got a character event waiting */
	return 1;
}
	
static BYTE read_char_pipe_win32(void)
{
	DWORD count;
	char ch;
	
	if(pipe_eof)
	{
		return 0xFF; /* Output FF to signify EOF */
	}
	
	ReadFile(GetStdHandle(STD_INPUT_HANDLE), &ch, 1, &count, 0);
	/* printf("read: '%c' 0x%2x\n", ( ((ch < ' ') || (ch > '~'))?'.':ch ), ch); */
	
	return ch;
}


/****************************************************************************
 ******************************** EXPORTED **********************************
 ****************************************************************************/

/* Initialise the windows 32 terminal. Depending on the success of this operation, 
 * we are going to decide what kind of Input method we are going to be using, console
 * or pipe.
 */
void init_terminal(void)
{
	BOOL result;
	
	/* SetConsoleMode: 
	   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/setconsolemode.asp */
	   
	/* For input, we dont want echo, mouse input, window input, or line input, but
	   we do want Windows to process crtl-c, etc for us (ENABLE_PROCESSED_INPUT) */
	result = SetConsoleMode(GetStdHandle(STD_INPUT_HANDLE), 
		!ENABLE_ECHO_INPUT      | !ENABLE_LINE_INPUT | 
		!ENABLE_MOUSE_INPUT     | ENABLE_PROCESSED_INPUT |
		!ENABLE_WINDOW_INPUT    /* | !ENABLE_INSERT_MODE |
		!ENABLE_QUICK_EDIT_MODE | ENABLE_EXTENDED_FLAGS |
		ENABLE_AUTO_POSITION */);
		
	/* For output we want Windows to process BELL etc for us */
	SetConsoleMode(GetStdHandle(STD_OUTPUT_HANDLE), ENABLE_PROCESSED_OUTPUT);
	
	/* If we were successful, we use the CONSOLE API, otherwise the PIPE API */
	if(result)
	{
		char_available_win32 = char_available_console_win32;
		read_char_win32 = read_char_console_win32;
	}
	else
	{
		char_available_win32 = char_available_pipe_win32;
		read_char_win32 = read_char_pipe_win32;
	}
}

/* There is nothing to do here */
void restore_terminal(void)
{
	/* Empty */
}

/* Calls the appropriate char_available for console or piped I/O */
int char_available(void)
{
	return char_available_win32();
}

/* Calls the appropriate read_char for console or piped I/O */
BYTE read_char(void)
{
	return read_char_win32();
}

