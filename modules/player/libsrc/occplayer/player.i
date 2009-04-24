%module occplayer
%include "stdint.i"
%{
#include <libplayercore/player.h>
#include <libplayerc/playerc.h>
%}

// XXX: This shouldn't be necessary, but some Player functions use it.
typedef int size_t;

// Mark calls that might block as blocking.
%feature("blocking", "B") playerc_client_request;
%feature("blocking", "B") playerc_client_read;

%include <libplayercore/player.h>
%include <libplayercore/player_interfaces.h>
%include <libplayerc/playerc.h>
