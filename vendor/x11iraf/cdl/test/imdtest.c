#include <stdio.h>
#define CDL_LIBRARY_SOURCE
#include "cdl.h"

#define	TEST_PATTERN 	-1

/* Utility macros.  */
#define max(a,b)        (a > b ? a : b)
#define min(a,b)        (a < b ? a : b)


/* IMDTEST -- Test routines for the IMD package.
 */

main()
{
	IMDPtr 	imd, imd_open();
	char	key, cmd[2], name[SZ_NAME], title[SZ_NAME];
	int	i, j, zt=1, frame=1, fbconfig=2, color=201;
	int	debug = 1, nx, ny, ix=128, iy=128, wcs=0;
	int	xarray[1024], yarray[1024];
	float	rx=128., ry=128.;
	float	a, b, c, d, tx, ty, z1, z2;
	uchar	pix, raster[1024*1024];

	imd_setDebug (debug);
	com_setDebug (debug);
	if ((imd = imd_open ((char *)getenv("IMTDEV"))) == (IMDPtr) NULL)
	    exit ();

	cmd[0] = 'd';
	cmd[1] = '\n';
	print_help();
	do {
	    switch (cmd[0]) {
	    case '+':					/* ENABLE DEBUG   */
		imd_setDebug (++debug);
		break;

	    case '-':					/* DISABLE DEBUG  */
		imd_setDebug (--debug);
		break;

	    case 's':					/* SAMPLE CURSOR  */
		sleep (2);
		if (imd_readCursor (imd, 1, &rx, &ry, &wcs, &key))
		    printf ("...returns an error\n");
		printf ("cursor: x=%g  y=%g  key='%c'\n", rx, ry, key);
		break;

	    case 'c':					/* SET CURSOR	  */
		sleep (2);
		if (imd_setCursor (imd, 256, 256))
		    printf ("...returns an error\n");
		break;

	    case 'C':					/* READ CURSOR	  */
		if (imd_readCursor (imd, 0, &rx, &ry, &wcs, &key))
		    printf ("...returns an error\n");
		ix = (int) rx;
		iy = (int) ry;
		printf ("cursor: x=%g/%d  y=%g/%d  key='%c'\n", 
		    rx, ix, ry, iy,  key);
		break;

	    case 'd':					/* WRITE IMAGE	  */
		printf ("creating raster....\n");
		/*
		make_raster (raster, 1024, 1024, TEST_PATTERN);
		*/
		make_raster (raster, 512, 512, TEST_PATTERN);
		imd_setName (imd, "test image");
		imd_setTitle (imd, "test title");
		printf ("displaying raster....\n");
		/*
		if (imd_displayImage(imd,raster,1024,1024,frame,fbconfig,1))
		    printf ("...returns an error\n");
		*/
		if (imd_displayImage(imd,raster,512,512,frame,fbconfig,1))
		    printf ("...returns an error\n");
		break;

	    case 'D':					/* READ IMAGE 	  */
		if (imd_readImage (imd, raster, &nx, &ny))
		    printf ("...returns an error\n");
		printf ("corners: nx=%d  ny=%d  LL = [%d]  UR = [%d]\n",
		    nx, ny, raster[0], raster[nx*ny-1]);
		if (imd_displayImage(imd,raster,nx,ny,frame,fbconfig,1))
		    printf ("...returns an error\n");
		break;

	    case 'e':					/* ERASE FRAME	  */
		if (imd_clearFrame (imd))
		    printf ("...returns an error\n");
		break;

	    case 'F':					/* READ FB 	  */
		if (imd_readFrameBuffer (imd, raster, &nx, &ny))
		    printf ("...returns an error\n");
		printf ("fb corners: nx=%d  ny=%d  LL = [%d]  UR = [%d]\n",
		    nx, ny, raster[0], raster[nx*ny-1]);
		if (imd_displayImage(imd,raster,nx,ny,frame,fbconfig,1))
		    printf ("...returns an error\n");
		break;

	    case 'm':					/* MARK POINT 	  */
		for (key='z'; key != 'q'; ) {
		    if (imd_readCursor (imd, 0, &rx, &ry, &wcs, &key))
		        printf ("...returns an error\n");
		    ix = (int) rx;
		    iy = (int) ry;
		    printf ("cursor: x=%g/%d  y=%g/%d  key='%c'\n", 
		        rx, ix, ry, iy,  key);
		    make_raster (raster, 16, 16, color);
		    color = (color+1 > 209 ? 201 : color+1);
		    if (imd_writeSubRaster (imd, ix-8, iy-8, 16, 16, raster))
		        printf ("...returns an error\n");
		}
		break;

	    case 'n':					/* NEXT FRAME 	  */
		frame = (frame+1 > 4 ? 1 : frame+1);
		if (imd_setFrame (imd, frame))
		    printf ("...returns an error\n");
		printf ("imd->frame = %d\n", imd->frame);
		break;

	    case 'N':					/* NEXT CONFIG 	  */
		fbconfig = (fbconfig+1 > 12 ? 1 : fbconfig+1);
		if (imd_setFBConfig (imd, fbconfig))
		    printf ("...returns an error\n");
		i = imd->fbconfig - 1;
		printf ("imd->fbconfig = %d/%d [%d %d %d %d]\n", imd->fbconfig,
		    i, imd->fbtab[i]->config, imd->fbtab[i]->nframes,
		    imd->fbtab[i]->width, imd->fbtab[i]->height);
		break;

	    case 'p':					/* PREVIOUS FRAME */
		frame = (frame-1 < 1 ? 4 : frame-1);
		if (imd_setFrame (imd, frame))
		    printf ("...returns an error\n");
		printf ("imd->frame = %d\n", imd->frame);
		break;

	    case 'P':					/* PREV CONFIG    */
		if (imd_setFBConfig (imd, fbconfig))
		    printf ("...returns an error\n");
		fbconfig = (fbconfig-1 < 1 ? 12 : fbconfig)-1;
		i = imd->fbconfig - 1;
		printf ("imd->fbconfig = %d/%d [%d %d %d %d]\n", imd->fbconfig,
		    i, imd->fbtab[i]->config, imd->fbtab[i]->nframes,
		    imd->fbtab[i]->width, imd->fbtab[i]->height);
		break;

	    case 'q':					/* QUIT 	  */
		goto quit;
	    case 'f':					/* (FAST) REGION  */
		make_raster (raster, 256, 256, TEST_PATTERN);
		for (i=0; i < 256; i++) 	/* diagonal 	*/
		    (void) imd_writeSubRaster(imd,i,i,256,256,raster);
		for (; i > 0; i--) 		/* left 	*/
		    (void) imd_writeSubRaster(imd,i,256,256,256,raster);
		for (i=256; i > 0; i--) 	/* down 	*/
		    (void) imd_writeSubRaster(imd,0,i,256,256,raster);
		for (i=0; i < 256; i++) 	/* right 	*/
		    (void) imd_writeSubRaster(imd,i,0,256,256,raster);
		for (i=0; i < 256; i++) 	/* up 		*/
		    (void) imd_writeSubRaster(imd,256,i,256,256,raster);
		break;
	    case 'r':					/* WRITE REGION	  */
		make_raster (raster, 256, 256, TEST_PATTERN);
		for (i=0; i < 100; i++) {
		    if (imd_writeSubRaster (imd,103+i,103+i,256,256,raster)) {
		        printf ("...returns an error\n");
			break;
		    }
		}

		/*  Old test to make sure we clip properly.

		make_raster (raster, 32, 32, color);
		if (imd_writeSubRaster (imd, -16, -16, 32, 32, raster))
		    printf ("...returns an error\n");
		if (imd_writeSubRaster (imd, -16, 496, 32, 32, raster))
		    printf ("...returns an error\n");
		if (imd_writeSubRaster (imd, 496, -16, 32, 32, raster))
		    printf ("...returns an error\n");
		if (imd_writeSubRaster (imd, 496, 496, 32, 32, raster))
		    printf ("...returns an error\n");
		*/
		color = (color+1 > 209 ? 201 : color+1);
		break;

	    case 'R':					/* READ REGION 	  */
		if (imd_readSubRaster (imd, -8, -8, 16, 16, raster))
		    printf ("...returns an error\n");
		for (i=15; i > 0; i--) {
		    for (j=0; j < 16; j++) {
			printf ("%3d ", raster[i * 16 + j]);
			raster[i * 16 + j] = 0;
		    }
		    printf ("\n");
		}
		if (imd_writeSubRaster (imd, -8, -8, 16, 16, raster))
		    printf ("...returns an error\n");
		break;

	    case 'w':					/* SET WCS 	  */
		if (imd_setWCS (imd, "imaname", "imtitle", 1., 0., 0., -1., 0.,
		    0., 1., 255., 1))
		        printf ("...returns an error\n");
		break;

	    case 'W':					/* GET WCS 	  */
		if (imd_setWCS (imd, name, title, &a, &b, &c, &d, &tx, &ty,
		    &z1, &z2, &zt))
		        printf ("...returns an error\n");
		printf ("name='%s' title='%s'\n a=%g b=%g c=%g d=%g tx=%g ty=%g z1=%g z2=%g zt=%d\n", name, title, a, b, c, d, tx, ty, z1, z2, zt);
		break;

	    case '?':
		print_help();
		break;

	    case '1':					/* WRITE CLIPPING  */
		printf ("creating raster....\n");
		make_raster (raster, 512, 512, TEST_PATTERN);
        	imd_setFrame (imd, frame);
        	imd_clearFrame (imd);
        	imd_setFBConfig (imd, fbconfig);
		imd_setName (imd, "test image");
		imd_setTitle (imd, "test title");
		printf ("displaying raster....\n");
		if (imd_writeImage(imd,raster,512,512,-8, -8))
		    printf ("...returns an error\n");
		break;

	    case '2':					/* READ CLIPPING  */
		if (imd_readSubRaster (imd, -8, -8, 16, 16, raster))
		    printf ("...returns an error\n");
		for (i=15; i >= 0; i--) {
		    for (j=0; j < 16; j++)
			printf ("%3d ", raster[i * 16 + j]);
		    printf ("\n");
		}
		if (imd_writeSubRaster (imd, -8, -8, 16, 16, raster))
		    printf ("...returns an error\n");
		break;

	    case 'a':
		clock ();
		for (i=0, ix=16, iy=16; i < 1024; i++) {
		    ix = xarray[i] = (ix+10 > 500 ? 16 : ix + 10);
		    iy = yarray[i] = (ix == 16 ? iy + 16: iy);
		}
		for (i=0; i < 1024; i++) {
		    make_raster (raster, 8, 8, color);
		    color = (color+1 > 209 ? 201 : color+1);
		    if (imd_writeSubRaster (imd, xarray[i], yarray[i],
			8, 8, raster))
		            printf ("...returns an error\n");
		}
		printf ("takes %d microseconds\n", clock());
	    }
	    printf ("Command: ");

	  scanf ("%s", cmd);
	} while (cmd[0] != 'q');

quit:	(void) imd_close (imd);	
}


print_help ()
{
	    printf ("\n");
	    printf ("	c -  set cursor     ");
	    printf ("	C -  read cursor    ");
	    printf ("	d -  write image    "); printf ("\n");
	    printf ("	D -  read image     ");
	    printf ("	e -  erase frame    ");
	    printf ("	F -  read frame buf "); printf ("\n");
	    printf ("	m -  mark points    ");
	    printf ("	n -  next frame     ");
	    printf ("	N -  next config    "); printf ("\n");
	    printf ("	p -  previous frame ");
	    printf ("	P -  prev config    ");
	    printf ("	q -  quit           "); printf ("\n");
	    printf ("	r -  write region   ");
	    printf ("	R -  read region    ");
	    printf ("	s -  sample cursor  "); printf ("\n");
	    printf ("	w -  set wcs        ");
	    printf ("	W -  get wcs        ");
	    printf ("	? -  print help     "); printf ("\n");
	    printf ("	+ -  enable debug   ");
	    printf ("	- -  disable debug  "); printf ("\n");
}


make_raster (raster, nx, ny, color)
uchar *raster;
int	nx, ny, color;
{
	register uchar pix;
	register int i, j;
	register float scale;

	if (color > 0) {
	    /* Build a solid color */
    	    for (i = 0; i < nx; i++) {
                for (j = 0; j < ny; j++) {
            	    raster[i * nx + j] = (uchar) color;
                }
    	    } 
	} else {
	    /* Make a test pattern. */
    	    for (i = 0; i < nx; i++) {
                for (j = 0; j < ny; j++) {
/* Diagonal ramp
*/
            	    scale =  200. / (float)(ny) / 2.;
            	    pix =  (uchar) max(2, (min(200,(scale*i + scale*j))));
/* Vertical ramp
            	    scale =  200. / (float)(ny);
            	    pix =  (uchar) max(2, (min(200,(scale * i))));
*/
/* Horizontal ramp
            	    scale =  200. / (float)(nx);
            	    pix =  (uchar) max(2, (min(200,(scale * j))));
*/
            	    raster[i * nx + j] = pix;
                }
    	    } 
	}
}

