#ifndef HANDLERS_H
#define HANDLERS_H

/* keyboard handler */
#define ws_size kbh_ws_size
#define vs_size kbh_vs_size
#define ms_size kbh_ms_size
#define inst_size kbh_inst_size
#define transputercode kbh_transputercode
#include "kbhandler.h"
#undef transputercode
#undef inst_size
#undef ms_size
#undef vs_size
#undef ws_size

/* screen handler */
#define ws_size scrh_ws_size
#define vs_size scrh_vs_size
#define ms_size scrh_ms_size
#define inst_size scrh_inst_size
#define transputercode scrh_transputercode
#include "scrhandler.h"
#undef transputercode
#undef inst_size
#undef ms_size
#undef vs_size
#undef ws_size

/* error handler */
#define ws_size errh_ws_size
#define vs_size errh_vs_size
#define ms_size errh_ms_size
#define inst_size errh_inst_size
#define transputercode errh_transputercode
#include "errhandler.h"
#undef transputercode
#undef inst_size
#undef ms_size
#undef vs_size
#undef ws_size

#endif /* HANDLERS_H */
