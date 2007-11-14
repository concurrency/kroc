%module occSDL
%{
#include <SDL.h>
%}
%include SDL_main.i
%include SDL_types.i
%include SDL_video.i
%include SDL_keysym.i
%include SDL_keyboard.i
%include SDL_joystick.i
%include SDL_audio.i
%include SDL_events.i
%include SDL_error.i
