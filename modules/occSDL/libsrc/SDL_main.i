%module SDL_main
%{
#include <SDL_main.h>
%}

/* Main include header for the SDL library */


#include "SDL_main.h"
#include "SDL_types.h"
#include "SDL_getenv.h"
#include "SDL_error.h"
#include "SDL_rwops.h"
#include "SDL_timer.h"
#include "SDL_audio.h"
#include "SDL_cdrom.h"
#include "SDL_joystick.h"
#include "SDL_events.h"
#include "SDL_video.h"
#include "SDL_byteorder.h"
#include "SDL_version.h"

#include "begin_code.h"

/* These are the flags which may be passed to SDL_Init() -- you should
   specify the subsystems which you will be using in your application.
*/
#define	SDL_INIT_TIMER		0x00000001
#define SDL_INIT_AUDIO		0x00000010
#define SDL_INIT_VIDEO		0x00000020
#define SDL_INIT_CDROM		0x00000100
#define SDL_INIT_JOYSTICK	0x00000200
#define SDL_INIT_NOPARACHUTE	0x00100000	/* Don't catch fatal signals */
#define SDL_INIT_EVENTTHREAD	0x01000000	/* Not supported on all OS's */
#define SDL_INIT_EVERYTHING	0x0000FFFF

/* This function loads the SDL dynamically linked library and initializes 
 * the subsystems specified by 'flags' (and those satisfying dependencies)
 * Unless the SDL_INIT_NOPARACHUTE flag is set, it will install cleanup
 * signal handlers for some commonly ignored fatal signals (like SIGSEGV)
 */
extern  int  SDL_Init(Uint32 flags);

/* This entire function is only needed on the mac and will dissapear in SDL 1.3 */
%runtime %{
    #ifdef COCOA_HELPER
    extern void SDLCocoaInit();  /* To be called before any SDL functions */
    #endif
    int  occ_SDL_Init(Uint32 flags) {
      #ifdef COCOA_HELPER
      const SDL_version *sdl_version = SDL_Linked_Version();
      if(sdl_version->major <= 1 && sdl_version->minor <= 2) {
        SDLCocoaInit();
      }
      #endif
      /* Force NOPARACHUTE - it messes with the tvm's fast timer. */
      return SDL_Init(flags | SDL_INIT_NOPARACHUTE);
    }
%}
int occ_SDL_Init(Uint32 flags);

/* This function initializes specific SDL subsystems */
extern  int  SDL_InitSubSystem(Uint32 flags);

/* This function cleans up specific SDL subsystems */
extern  void  SDL_QuitSubSystem(Uint32 flags);

/* This function returns mask of the specified subsystems which have
   been initialized.
   If 'flags' is 0, it returns a mask of all initialized subsystems.
*/
extern  Uint32  SDL_WasInit(Uint32 flags);

/* This function cleans up all initialized subsystems and unloads the
 * dynamically linked library.  You should call it upon all exit conditions.
 */
extern  void  SDL_Quit(void);

/* Ends C function definitions when using C++ */
#include "close_code.h"

