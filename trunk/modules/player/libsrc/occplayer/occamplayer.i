%module occamplayer
/*Typedefs that are not found in stdint.i*/
typedef unsigned int uint;
typedef unsigned int size_t;

/*Stuff specifically for occam*/

/*Wrapper functions which allow you to get a string as a return value*/
%runtime %{
	inline void occ_playerc_error_str(char err_str[], int len)
	{
		char *tmp = playerc_error_str();
		strncpy(err_str, tmp, len);
		return;
	}
%}

void occ_playerc_error_str(char err_str[], int len);

%runtime %{
	inline void occ_playerc_lookup_name(char err_str[], int len, int code)
	{
		char *tmp = playerc_lookup_name(code);
		strncpy(err_str, tmp, len);
		return;
	}
%}

void occ_playerc_lookup_name(char err_str[], int len, int code);
