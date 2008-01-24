/**
 * Basic JTAG driver public functions
 *
 * (c) 01/2005 Martin Strubel <hackfin@section5.ch>
 *
 * UNSTABLE! (API may change)
 *
 * $Id: jtag.h 381 2006-08-29 23:11:47Z strubi $
 *
 */

#ifndef DLL_API
#if defined (WIN32)
#	if defined (DLL_EXPORTS)
#		define DLL_API __declspec(dllexport)
#	else
#		define DLL_API __declspec(dllimport)
#	endif
#else
#	define DLL_API
#endif
#endif

struct _jtag_controller;
#define JTAG_CTRL struct _jtag_controller *


////////////////////////////////////////////////////////////////////////////
// JTAG init/exit

/** \addtogroup JTAG
 * @{ */

/** Open JTAG controller
 *
 * \param argv   A null terminated array of char[] with initialization
 *               options.
 *
 * \param c      Controller handle returned
 *
 * \return       Number of detected devices, negative error code else
 *
 */

DLL_API
int           jtag_open(char * const *argv, JTAG_CTRL *c);


/** Detect scan chain
 *
 * \param c      Controller handle
 *
 * \return       Number of detected devices or error if < 0
 */

DLL_API
int           jtag_detect(JTAG_CTRL c);

/** Reset JTAG controller
 *
 * This function affects all devices on the chain!
 *
 * \param cycles   Number of cycles to stay in reset
 */

DLL_API
int           jtag_reset(JTAG_CTRL c, int cycles);

/** Close JTAG controller
 *
 * Closes the JTAG controller and frees all resources
 */


DLL_API
void          jtag_close(JTAG_CTRL c);

/** Get JTAG chain property
 *
 * Return value is only valid after call to jtag_open().
 *
 * \param token  Currently supported tokens are:
 *               #JTAG_NDEVICES, #JTAG_STATE, #JTAG_IR, #JTAG_IRLENGTH.
 *
 * \param value  Pointer to structure to be filled out. See token definition.
 *
 * \return       #ERR_UNSUPPORTED if property unknown. If property is
 *               read or writeonly, #ERR_READ / #ERR_WRITE are returned.
 *
 */

DLL_API
int           jtag_get(JTAG_CTRL c, int token, void *value);

/** JTAG configuration
 *
 * \attention This API call replaces emulation_config()
 *
 * Currently, only JTAG clock speed can be changed.
 *
 * \param token   one of [#JTAG_CLKSPEED]
 * \param value   JTAG_CLKSPEED: 0: fastest (6MHz), 255: slowest
 *
 */

DLL_API
int           jtag_config(JTAG_CTRL jtag, int token, int value);

/** Tokens for Emulation Control
 * ( jtag_config(), jtag_get() )
 *
 * In brackets, the data type is listed.
 * This is important for the jtag_get() function.
 *
 * \ingroup JTAG
 */

enum {
	JTAG_NDEVICES =   1,        ///< Number of JTAG devices in chain [int]
	JTAG_STATE,                 ///< Current JTAG state [int]
	JTAG_IR,                    ///< JTAG Instruction register [JtagRegister *]
	JTAG_IRLENGTH,              ///< Length of JTAG instruction register [int]
	// Configurable parameters
	JTAG_CLKSPEED =  32,        ///< JTAG clock speed (wait cycles) [int]
};

/* @} */
