%module SDL_error
%{
#include <SDL_error.h>
%}

/* Public functions */
//extern  void  SDL_SetError(const char *fmt, ...);

%runtime %{
inline static int occ_SDL_GetError(char err_str[], int err_str_len) {
        const char *err = SDL_GetError();

        int len = 0;
        while (len < err_str_len && err[len] != '\0') {
                err_str[len] = err[len];
                len++;
        }

        return len;
}
%}
extern int occ_SDL_GetError(char err_str[], int err_str_len);

extern  char *  SDL_GetError(void);
extern  void  SDL_ClearError(void);



