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

#define ROM_BASE (0x00100000)
#define RAM_BASE (0x00200000)

#define PAGE_SIZE		256
#define PAGE_N_ADDR		(RAM_BASE + 0x1000)
#define PAGE_BUF_ADDR		(RAM_BASE + 0x2000)
#define FLASH_DRIVER_ADDR 	(RAM_BASE)

const uint8_t flash_driver[] = { 0x00 };
