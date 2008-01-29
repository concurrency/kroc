#define MAX_BLOBS  16
#define MAX_COLORS 16
#define MIN_BLOB_SIZE 5

#define index(xx, yy)   (yy * imgWidth + xx) * 2

extern void vgrab(unsigned char *,unsigned int, unsigned int, 
    unsigned int, unsigned int, unsigned int);
extern void vscan(unsigned char *, unsigned int), vfind(unsigned char *, unsigned int);
extern void vnum(unsigned char *, unsigned int), vblob(unsigned char *, unsigned int);
unsigned int ymax[MAX_COLORS], ymin[MAX_COLORS], umax[MAX_COLORS], umin[MAX_COLORS], vmax[MAX_COLORS], vmin[MAX_COLORS];
unsigned int blobx1[MAX_BLOBS], blobx2[MAX_BLOBS], bloby1[MAX_BLOBS], bloby2[MAX_BLOBS], blobcnt[MAX_BLOBS];
unsigned int tvect[80];



void vgrab(unsigned char *decode_buf, unsigned int ii, unsigned int x1, 
    unsigned int x2, unsigned int y1, unsigned int y2) {
  unsigned int xx, yy, y, u, v;
  
  ymax[ii] = 0;
  ymin[ii] = 255;
  umax[ii] = 0;
  umin[ii] = 255;
  vmax[ii] = 0;
  vmin[ii] = 255;

  for (yy=y1; yy<=y2; yy++) {   
    for (xx=x1; xx<=x2; xx++) {
      y = (unsigned int)decode_buf[index(xx,yy)];
      u = (unsigned int)decode_buf[index(xx,yy)+5120];
      v = (unsigned int)decode_buf[index(xx,yy)+10240];
      if (ymax[ii] < y)
        ymax[ii] = y;
      if (ymin[ii] > y)
        ymin[ii] = y;
      if (umax[ii] < u)
        umax[ii] = u;
      if (umin[ii] > u)
        umin[ii] = u;
      if (vmax[ii] < v)
        vmax[ii] = v;
      if (vmin[ii] > v)
        vmin[ii] = v;
    }
  }
}

void vscan(unsigned char *decode_buf, unsigned int ii) {
  unsigned int xx, yy, y, u, v;

  for (xx=0; xx<80; xx++) {   // for each column, measure distance to first mismatch from initial samples
    tvect[xx] = 64;            //      max distance will be 64 pixels (height of image)
    for (yy=0; yy<64; yy++) {
      y = (unsigned int)decode_buf[index(xx,yy)];
      u = (unsigned int)decode_buf[index(xx,yy)+5120];
      v = (unsigned int)decode_buf[index(xx,yy)+10240];
      if ((y < ymin[ii])
       || (y > ymax[ii]) 
       || (u < umin[ii]) 
       || (u > umax[ii]) 
       || (v < vmin[ii]) 
       || (v > vmax[ii])) {
        tvect[xx] = yy;
        break;
      }
    }
  }
}

void vnum(unsigned char *decode_buf, unsigned int ii) {
  unsigned int xx, yy, y, u, v;

  for (xx=0; xx<80; xx++) {   // for each column, count number of pixels that match selected color
    tvect[xx] = 0;
    for (yy=0; yy<64; yy++) {
      y = (unsigned int)decode_buf[index(xx,yy)];
      u = (unsigned int)decode_buf[index(xx,yy)+5120];
      v = (unsigned int)decode_buf[index(xx,yy)+10240];
      if ((y >= ymin[ii])
       && (y <= ymax[ii]) 
       && (u >= umin[ii]) 
       && (u <= umax[ii]) 
       && (v >= vmin[ii]) 
       && (v <= vmax[ii]))
        tvect[xx]++;
    }
  }
}

void vfind(unsigned char *decode_buf, unsigned int ii) {
  unsigned int xx, yy, y, u, v;

  for (xx=0; xx<80; xx++) {   // for each column, measure distance to first match of target color
    tvect[xx] = 0xff;            //      max distance will be 63 pixels (height of image) - return of 0xff means no match
    for (yy=0; yy<64; yy++) {
      y = (unsigned int)decode_buf[index(xx,yy)];
      u = (unsigned int)decode_buf[index(xx,yy)+5120];
      v = (unsigned int)decode_buf[index(xx,yy)+10240];
      if ((y >= ymin[ii])
       && (y <= ymax[ii]) 
       && (u >= umin[ii]) 
       && (u <= umax[ii]) 
       && (v >= vmin[ii]) 
       && (v <= vmax[ii])) {
        tvect[xx] = yy;
        break;
      }
    }
  }
}

void vblob(unsigned char *decode_buf, unsigned int ii) {
  unsigned int xx, yy, y, u, v, jj, count, bottom, top, tmp;

  for (jj=0; jj<MAX_BLOBS; jj++) {
    blobcnt[jj] = 0;
    blobx1[jj] = 80;
    blobx2[jj] = 0;
    bloby1[jj] = 64;
    bloby2[jj] = 0;
  }
    
  jj = 0;  // jj indicates the current blob being processed
  for (xx=0; xx<80; xx++) {
    count = 0;
    bottom = 64;
    top = 0;
    for (yy=0; yy<64; yy++) {
      y = (unsigned int)decode_buf[index(xx,yy)];
      u = (unsigned int)decode_buf[index(xx,yy)+5120];
      v = (unsigned int)decode_buf[index(xx,yy)+10240];
      if ((y >= ymin[ii])
       && (y <= ymax[ii]) 
       && (u >= umin[ii]) 
       && (u <= umax[ii]) 
       && (v >= vmin[ii]) 
       && (v <= vmax[ii])) {
        count++;
        if (bottom > yy)
          bottom = yy;
        if (top < yy)
          top = yy;
      }
    }
    if (count) {
      if (bloby1[jj] > bottom)
        bloby1[jj] = bottom;
      if (bloby2[jj] < top)
        bloby2[jj] = top;
      if (blobx1[jj] > xx)
        blobx1[jj] = xx;
      if (blobx2[jj] < xx)
        blobx2[jj] = xx;
      blobcnt[jj] += count;
    } else {
      if (blobcnt[jj])  // move to next blob if a gap is found
        jj++;
      if (jj > (MAX_BLOBS-1))
        goto blobbreak;
    }
  }
blobbreak:   // now sort blobs by size, largest to smallest pixel count
  for (xx=0; xx<=jj; xx++) {
    if (blobcnt[xx] == 0)  // no more blobs, so exit
      return;
    for (yy=xx; yy<=jj; yy++) {
      if (blobcnt[yy] == 0)
        break;
      if (blobcnt[xx] < blobcnt[yy]) {
        tmp = blobcnt[xx];
        blobcnt[xx] = blobcnt[yy];
        blobcnt[yy] = tmp;
        tmp = blobx1[xx];
        blobx1[xx] = blobx1[yy];
        blobx1[yy] = tmp;
        tmp = blobx2[xx];
        blobx2[xx] = blobx2[yy];
        blobx2[yy] = tmp;
        tmp = bloby1[xx];
        bloby1[xx] = bloby1[yy];
        bloby1[yy] = tmp;
        tmp = bloby2[xx];
        bloby2[xx] = bloby2[yy];
        bloby2[yy] = tmp;
      }
    }
  }
}

