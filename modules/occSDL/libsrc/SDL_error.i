%module SDL_error
%{
#include <SDL_error.h>
%}

/* Public functions */
//The function below needs to be accessed via occ_SDL_GetError
//extern  void  SDL_SetError(const char *fmt, ...);

// custom wrapper for sdl_geterror
%runtime %{
	inline static void occ_SDL_GetError(char err_str[], int err_str_len, int *str_len)
	{
  	char *err = SDL_GetError();

  	*str_len = 0;

  	while(*str_len < err_str_len && err[*str_len] != '\0')
    	err_str[*str_len] = err[(*str_len)++];
		return;
	}
%}

extern void occ_SDL_GetError(char err_str[], int err_str_len, int *str_len);

//extern  char *  SDL_GetError(void);
extern  void  SDL_ClearError(void);



