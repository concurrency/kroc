/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 *  camera.c - OV9655 camera setup for SRV-1 robot
 *    Copyright (C) 2005-2007  Surveyor Corporation
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details (www.gnu.org/licenses)
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

#define SSYNC asm("ssync;")

#include <cdefBF537.h>
#include "i2cwrite.h"

#define POL_C            0x0000
#define POL_S            0x0000
#define PPI_SYNCS        0x0020  // 2 or 3 syncs
#define PPI_INPUT_MODE   0x000C  // non ITU-R 656 mode
#define PPI_DATA_LEN     0x0000
#define PPI_PACK         0x0080
#define PPI_ENABLE       0x0001
#define DMA_FLOW_MODE    0x1000
#define DMA_2D           0x0010
#define DMA_WDSIZE_16    0x0004
#define DMA_WDSIZE_8     0x0000
#define DMA_WNR          0x0002  
#define DMA_ENABLE       0x0001

typedef struct {
    void *pNext;
    unsigned char *pBuf;
    short dConfig;
} dma_desc_list;

dma_desc_list dlist1;
dma_desc_list dlist2;

int camera_init(unsigned char *buf1, unsigned char *buf2, short pixels, short lines)
{
    unsigned short itmp = *pPORTG_FER;
    *pPORTG_FER |= 0x00FF;  // PPI[7:0]
    *pPORTG_FER |= 0x00FF;  // PPI[7:0]
    itmp = *pPORTF_FER;
    *pPORTF_FER |= 0x8300;  // PF.15 = PPI_CLK    PF.9 = FS1     PF.8 = FS2
    *pPORTF_FER |= 0x8300;

    *pPPI_CONTROL =
        POL_S | POL_C | PPI_DATA_LEN | PPI_PACK | PPI_SYNCS | PPI_INPUT_MODE;
    *pPPI_COUNT = (pixels * 2) - 1;  // data format is YUYV, so there are 2 bytes per pixel
    *pPPI_DELAY = 0;
    *pPPI_FRAME = lines;
    SSYNC;

    dlist1.dConfig = FLOW_LARGE | NDSIZE_5 | DMA_WDSIZE_16 | DMA_2D | DMA_WNR | DMA_ENABLE ;
    dlist1.pBuf = buf1;
    dlist1.pNext = &dlist2;

    dlist2.dConfig = FLOW_LARGE | NDSIZE_5 | DMA_WDSIZE_16 | DMA_2D | DMA_WNR | DMA_ENABLE ;
    dlist2.pBuf = buf2;
    dlist2.pNext = &dlist1;

    //*pDMA0_CONFIG = DMA_FLOW_MODE | DMA_WDSIZE_16 | DMA_2D | DMA_WNR;
    //*pDMA0_START_ADDR = buf1;
    *pDMA0_X_COUNT = pixels;
    *pDMA0_X_MODIFY = 2;
    *pDMA0_Y_COUNT = lines;
    *pDMA0_Y_MODIFY = 2;  
    SSYNC;

    *pDMA0_CURR_DESC_PTR = &dlist1;
    *pDMA0_NEXT_DESC_PTR = &dlist2;
    SSYNC;
    *pDMA0_CONFIG = FLOW_LARGE | NDSIZE_5 | DMA_WDSIZE_16 | DMA_2D | DMA_WNR;

    return 0;
}

void camera_start(void)
{
    // enable transfers
    *pDMA0_CONFIG |= DMA_ENABLE;
    SSYNC;
    *pPPI_CONTROL |= PPI_ENABLE;
    SSYNC;
}

void camera_stop(void)
{
    // disable transfers
    *pDMA0_CONFIG &= 0xFFFE;
    SSYNC;
    *pPPI_CONTROL &= 0xFFFE;
    SSYNC;
}

