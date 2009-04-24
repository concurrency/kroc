%module occplayer
%include "stdint.i"
%{
#include <libplayercore/player.h>
#include <libplayerc/playerc.h>
%}

// XXX: This shouldn't be necessary, but some Player functions use it.
typedef long size_t;

%include <libplayercore/player.h>
%include <libplayercore/player_interfaces.h>
%include <libplayerc/playerc.h>
