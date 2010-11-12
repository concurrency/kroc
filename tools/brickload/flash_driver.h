/* 
 *  flash_driver.h -- part of brickload NXT/RCX firmware and bytecode tool
 *  Copyright (C) 2010  Carl Ritson <cgr@kent.ac.uk>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#define ROM_BASE			(0x00100000)
#define RAM_BASE			(0x00200000)

#define PAGE_SIZE			256
#define INIT_VAR_ADDR			(RAM_BASE + 0x2200)
#define PAGE_N_ADDR			(RAM_BASE + 0x2204)
#define PAGE_BUF_ADDR			(RAM_BASE + 0x2300)
#define FLASH_DRIVER_ADDR 		(RAM_BASE + 0x2000)

#define _AT91C_PMC_MCKR			((unsigned int) 0xfffffc30)
#define _AT91C_PMC_SR			((unsigned int) 0xfffffc68)
#define AT91C_PMC_MCKRDY		((unsigned int) 0x7)

#define _AT91C_MC_FCR			((unsigned int) 0xffffff64)
#define _AT91C_MC_FMR			((unsigned int) 0xffffff60)
#define _AT91C_MC_FSR			((unsigned int) 0xffffff68)
#define AT91C_MC_FRDY			((unsigned int) 0x1)
#define AT91C_MC_FCMD_START_PROG	((unsigned int) 0x1)
#define AT91C_MC_FCMD_UNLOCK		((unsigned int) 0x4)

#define FLASH_KEY			((unsigned int) (0x5a << 24))

#define MC_UNLOCK_MODE			((0x5 << 16) | (0x1 << 8))
#define MC_WRITE_MODE			((0x34 << 16) | (0x1 << 8))
#define MC_UNLOCK_CMD			(FLASH_KEY | AT91C_MC_FCMD_UNLOCK)

#ifndef DRIVER_BUILD
/* Built from sources in flash_nxt/ */
const uint8_t flash_driver[] = {
	0x21, 0xd8, 0xa0, 0xe3, 0x00, 0x40, 0x2d, 0xe9,
	0x00, 0x00, 0x00, 0xeb, 0x00, 0x80, 0xbd, 0xe8,
	0x00, 0x30, 0xe0, 0xe3, 0x97, 0x20, 0x13, 0xe5,
	0x01, 0x00, 0x12, 0xe3, 0xfc, 0xff, 0xff, 0x0a,
	0x1c, 0x30, 0x9f, 0xe5, 0x1c, 0x10, 0x9f, 0xe5,
	0x01, 0x26, 0x43, 0xe2, 0x04, 0x00, 0x93, 0xe4,
	0x23, 0x2c, 0x42, 0xe2, 0x01, 0x00, 0x53, 0xe1,
	0x00, 0x00, 0x82, 0xe5, 0xf9, 0xff, 0xff, 0x1a,
	0x1e, 0xff, 0x2f, 0xe1, 0x00, 0x23, 0x20, 0x00,
	0x00, 0x24, 0x20
};
#endif /* !DRIVER_BUILD */
