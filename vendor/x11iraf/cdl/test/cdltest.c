#include <stdio.h>
#define CDL_LIBRARY_SOURCE
#include "cdl.h"

/* CDLTEST -- Test routines for the CDL package.
 */

#define	TEST_PATTERN 	-1

/* Utility macros.  */
#define max(a,b)        (a > b ? a : b)
#define min(a,b)        (a < b ? a : b)

extern	int	cdl_debug;

main()
{
	CDLPtr 	cdl;
	char	key, cmd[2], name[SZ_NAME], title[SZ_NAME], obj[SZ_NAME];
	int	i, j, zt=1, frame=1, fbconfig=1, color=3, lwidth=1, twidth=1;
	int	debug = 0, nx, ny, bitpix, ix=128, iy=128, lstyle=0;
	int	lx, ly, ux, uy;
	int	fb_w, fb_h, nframes, wcs;
	int	xarray[1024], yarray[1024];
	float	rx=128., ry=128., sx, sy;
	float	a, b, c, d, tx, ty, z1=0.0, z2=0.0;
	int	snx, sny, dnx, dny, dx, dy;
	uchar	*pix, *c_ras;
	double 	sin();

	cdl = cdl_open ((char *)getenv("IMTDEV"));
	if (cdl == (CDLPtr) NULL)
	    exit ();

	c_ras = (uchar *) malloc (1024 * 1024);

	cmd[0] = 'f';
	cmd[1] = '\n';
	print_help();
	do {
	    switch (cmd[0]) {
	    case '<':					/* ENABLE DEBUG   */
		cdl_setDebug (1);
		break;

	    case '>':					/* DISABLE DEBUG  */
		cdl_setDebug (0);
		break;

	    case 'S':					/* SAMPLE CURSOR  */
		(void) cdl_readCursor (cdl, 1, &rx, &ry, &wcs, &key);
		printf ("cursor: x=%g  y=%g  wcs=%d  key='%d' (%c)\n", 
		    rx, ry, wcs, key, key);
		break;

	    case 'C':					/* CLIP TEST TOP */
		if (cdl_readSubRaster (cdl, 128, -32, 128, 128, &c_ras))
		    printf ("...read returns an error\n");
		if (cdl_writeSubRaster (cdl, 128, -32, 128, 128, c_ras))
		    printf ("...write returns an error\n");
		if (cdl_readSubRaster (cdl, 128, -8, 16, 16, &c_ras))
		    printf ("...read returns an error\n");
		for (i=0;i<16;i++) {
		    for (j=0; j<16; j++)
			printf ("%3d ", c_ras[i*16+j]);
		    printf ("\n");
		}
		printf ("\n");
		if (cdl_readSubRaster (cdl, 128, 504, 16, 16, &c_ras))
		    printf ("...read returns an error\n");
		for (i=0;i<16;i++) {
		    for (j=0; j<16; j++)
			printf ("%3d ", c_ras[i*16+j]);
		    printf ("\n");
		}
		break;

	    case 'c':					/* CLEAR FRAME	  */
		if (cdl_clearFrame (cdl))
		    printf ("...returns an error\n");
		break;

            case 'd':                                   /* DELETE MARKER  */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_deleteMark (cdl, ix, iy))
                   printf ("...returns an error\n");
                break;

	    case 'D':					/* DELETE OVERLAY */
		(void) cdl_clearOverlay (cdl);
		break;

	    case 'f':					/* DISPLAY FITS	  */
		if (cdl_displayFITS (cdl, "dpix.fits", frame, FB_AUTO, 1))
		    printf ("...returns an error\n");
		break;

	    case 'g':					/* READ IMAGE 	  */
		if (cdl_readImage (cdl, &c_ras, &nx, &ny))
		    printf ("...returns an error\n");
		printf ("corners: nx=%d  ny=%d  LL = [%d]  UR = [%d]\n",
		    nx, ny, c_ras[0], c_ras[nx*ny-1]);
		if (cdl_displayPix (cdl, c_ras, nx, ny, 8, frame, FB_AUTO, 0))
		    printf ("...returns an error\n");
		break;

	    case 'G':					/* READ FB 	  */
		if (cdl_readFrameBuffer (cdl, &c_ras, &nx, &ny))
		    printf ("...returns an error\n");
		printf ("fb corners: nx=%d  ny=%d  LL = [%d]  UR = [%d]\n",
		    nx, ny, c_ras[0], c_ras[nx*ny-1]);
		if (cdl_displayPix (cdl, c_ras, nx, ny, 8, frame, FB_AUTO, 0))
		    printf ("...returns an error\n");
		break;

	    case 'i':					/* DISPLAY IRAF	  */
		if (cdl_displayIRAF (cdl, "examples/dpix.imh", 1, frame,
		    FB_AUTO, 0))
		        printf ("...returns an error\n");
		break;

            case 'm':                                   /* MARK POINT     */
                i = 1;
                while (cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key) != 'q') {
                    ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                    printf ("cursor: x=%g/%d  y=%g/%d  key='%c'\n", 
                        rx, ix, ry, iy,  key);
                    if (cdl_markPoint (cdl, ix, iy, i++, 7, M_STAR, C_GREEN))
                        printf ("...returns an error\n");
                }
                break;

	    case 'n':					/* NEXT FRAME 	  */
		frame = (frame+1 > 16 ? 1 : frame+1);
		cdl_setFrame (cdl, frame);
		printf ("set cdl->frame = %d\n", cdl->frame);
		cdl_getFrame (cdl, &frame);
		printf ("get cdl->frame = %d\n", frame);
		break;

	    case 'N':					/* NEXT CONFIG 	  */
		fbconfig = (fbconfig+1 > 12 ? 1 : fbconfig+1);
		cdl_setFBConfig (cdl, fbconfig);
		i = cdl->fbconfig - 1;
		printf ("cdl->fbconfig = %d/%d [%d %d %d %d]\n", cdl->fbconfig,
		    i, cdl->imd->fbtab[i]->config, cdl->imd->fbtab[i]->nframes,
		    cdl->imd->fbtab[i]->width, cdl->imd->fbtab[i]->height);
		break;

	    case 'p':					/* PREVIOUS FRAME */
		frame = (frame-1 < 1 ? 4 : frame-1);
		cdl_setFrame (cdl, frame);
		printf ("set cdl->frame = %d\n", cdl->frame);
		cdl_getFrame (cdl, &frame);
		printf ("get cdl->frame = %d\n", frame);
		break;

	    case 'P':					/* PREV CONFIG    */
		cdl_setFBConfig (cdl, fbconfig);
		fbconfig = (fbconfig-1 < 1 ? 12 : fbconfig)-1;
		i = cdl->fbconfig - 1;
		printf ("cdl->fbconfig = %d/%d [%d %d %d %d]\n", cdl->fbconfig,
		    i, cdl->imd->fbtab[i]->config, cdl->imd->fbtab[i]->nframes,
		    cdl->imd->fbtab[i]->width, cdl->imd->fbtab[i]->height);
		break;

	    case 'q':					/* QUIT 	  */
		goto quit;

	    case 'r':					/* READ CURSOR	  */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
		printf("cursor: x=%g/%d  y=%g/%d  wcs = %d key='%c' frame=%d\n",
		    rx, ix, ry, iy,  wcs, key, wcs / 100);
		break;

	    case 's':					/* TEXT STRING	  */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
		if (cdl_markText (cdl, -20, iy, "testing 123", 1., 0.0, C_RED))
		    printf ("...returns an error\n");
		break;

	    case 't':					/* TEST IMAGE     */
		printf ("creating raster....\n");
		make_raster (c_ras, 512, 512, TEST_PATTERN);
		cdl_setName (cdl, "test image");
		cdl_setTitle (cdl, "test title");
		printf ("displaying raster....\n");
		if (cdl_displayPix (cdl, c_ras, 512, 512, 8, frame,fbconfig,0))
		    printf ("...returns an error\n");
		break;

	    case 'T':					/* TEST TILE      */
		if (cdl_readFITS ("dpix.fits", &pix, &nx, &ny,
		    &bitpix, obj))
		        printf ("...returns an error\n");
		printf ("nx=%d ny=%d bitpix=%d\n", nx, ny, bitpix);
		cdl_clearFrame (cdl);
		cdl_selectFB (cdl, 2*nx, 2*ny, &fbconfig, &fb_w, &fb_h,
		    &nframes, 1);
		if (cdl_setWCS (cdl, "imaname", "imtitle", 1., 0., 0., -1., 0.,
		    (float) 2*ny, 1., 255., 1))
		        printf ("...returns an error\n");
		cdl_computeZscale (cdl, pix, nx ,ny, bitpix, &z1, &z2);
		cdl_zscaleImage (cdl, &pix, nx ,ny, bitpix, z1, z2);
		if (cdl_writeSubRaster (cdl, 0, 0, nx, ny, pix))
		    printf ("...returns an error\n");
		if (cdl_writeSubRaster (cdl, nx, 0, nx, ny, pix))
		    printf ("...returns an error\n");
		if (cdl_writeSubRaster (cdl, 0, ny, nx, ny, pix))
		    printf ("...returns an error\n");
		if (cdl_writeSubRaster (cdl, nx, ny, nx, ny, pix))
		    printf ("...returns an error\n");
		break;

	    case 'w':					/* SET WCS 	  */
		if (cdl_setWCS (cdl, "imname", "imtitle", 1., 0., 0., -1., 0.,
		    512., 1., 255., 1))
		        printf ("...returns an error\n");
		break;

	    case 'W':					/* GET WCS 	  */
		if (cdl_getWCS (cdl, name, title, &a, &b, &c, &d, &tx, &ty,
		    &z1, &z2, &zt))
		        printf ("...returns an error\n");
		printf ("name='%s' title='%s'\n a=%g b=%g c=%g d=%g ",
		    name, title, a, b, c, d);
		printf ("tx=%g ty=%g z1=%g z2=%g zt=%d\n", tx, ty, z1, z2, zt);
		break;

            case 'b':                                   /* MARK BOX      */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                lx = (int) (rx + 0.5); 		ly = (int) (ry + 0.5);
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ux = (int) (rx + 0.5); 		uy = (int) (ry + 0.5);
                printf ("cursor: x=%d/%d y=%d/%d\n", lx,ux,ly,uy);
                if (cdl_markBox (cdl, lx, ly, ux, uy, 0, C_RED))
                   printf ("...returns an error\n");
                break;

            case 'B':                                   /* FILL BOX      */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                lx = (int) (rx + 0.5); 		ly = (int) (ry + 0.5);
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ux = (int) (rx + 0.5); 		uy = (int) (ry + 0.5);
                printf ("cursor: x=%d/%d y=%d/%d\n", lx,ux,ly,uy);
                if (cdl_markBox (cdl, lx, ly, ux, uy, 1, C_RED))
                   printf ("...returns an error\n");
                break;

            case 'L':                                   /* MARK LINE STYLES */
                break;

            case 'l':                                   /* MARK LINE     */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                lx = (int) (rx + 0.5); 		ly = (int) (ry + 0.5);
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ux = (int) (rx + 0.5); 		uy = (int) (ry + 0.5);
                printf ("cursor: x=%d/%d y=%d/%d\n", lx,ux,ly,uy);
                if (cdl_markLine (cdl, lx, ly, ux, uy, C_YELLOW))
                   printf ("...returns an error\n");
                break;

            case '[':
		cdl_setTextWidth (cdl, twidth = (twidth-1 < 1 ? 1 : --twidth));
		break;

            case ']':
		cdl_setTextWidth (cdl, ++twidth);
		break;

            case ',':
		cdl_setLineWidth (cdl, lwidth = (lwidth-1 < 1 ? 1 : --lwidth));
		break;

            case '.':
		cdl_setLineWidth (cdl, ++lwidth);
		break;

            case '{':
		cdl_setLineStyle (cdl, lstyle = (lstyle-1 < 1 ? 1 : --lstyle));
		break;

            case '}':
		cdl_setLineStyle (cdl, ++lstyle);
		break;

            case '(':                                   /* MARK POLYLINE  */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);     iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
	  	for (i=0; i < 145; i++) {
		    xarray[i] = ix + i;
		    yarray[i] = iy + (int)(32*sin((double)(i*0.17453)));
	        }
                if (cdl_markPolyline (cdl, xarray, yarray, 145, C_GREEN))
                   printf ("...returns an error\n");
                break;

            case ')':                                   /* MARK POLYGON   */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);     iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
		xarray[0] = ix - 9; 	yarray[0] = iy - 9;
		xarray[1] = ix + 0; 	yarray[1] = iy + 9;
		xarray[2] = ix + 9; 	yarray[2] = iy - 9;
                if (cdl_markPolygon (cdl, xarray, yarray, 3, 1, C_GREEN))
                   printf ("...returns an error\n");
                break;

            case 'e':                                   /* MARK ELLIPSE    */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_markEllipse (cdl, ix, iy, 11, 7, 45.0, 0, C_RED))
                   printf ("...returns an error\n");
                break;

            case 'E':                                   /* MARK ELLIPSE    */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_markEllipAnnuli (cdl, ix, iy, 11, 7, 45.0, 2, 5, C_RED))
                    printf ("...returns an error\n");
                break;

            case 'o':                                   /* MARK CIRCLE    */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_markCircle (cdl, ix, iy, 27, 0, C_YELLOW))
                   printf ("...returns an error\n");
                break;

            case 'O':                                   /* MARK CIRCLE    */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_markCircAnnuli (cdl, ix, iy, 13, 2, 5, C_YELLOW))
                   printf ("...returns an error\n");
                break;

            case 'M':                                   /* MARK ALL PTS    */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                cdl_markPoint (cdl,ix+00,iy,   0,13, M_BOX, C_GREEN);
                cdl_markPoint (cdl,ix+15,iy,   0,13, M_PLUS, C_GREEN);
                cdl_markPoint (cdl,ix+30,iy,   0,13, M_CROSS, C_GREEN);
                cdl_markPoint (cdl,ix+45,iy,   0,13, M_DIAMOND,C_GREEN);
                cdl_markPoint (cdl,ix+60,iy,   0,13, M_CIRCLE, C_GREEN);
                cdl_markPoint (cdl,ix+75,iy,   0,13, M_STAR, C_GREEN);
                cdl_markPoint (cdl,ix+90,iy,   0,13, M_HLINE, C_GREEN);
                cdl_markPoint (cdl,ix+105,iy,  0,13, M_VLINE, C_GREEN);
                cdl_markPoint (cdl,ix+120,iy,  0,13, M_HBLINE, C_GREEN);
                cdl_markPoint (cdl,ix+135,iy,  0,13, M_VBLINE, C_GREEN);
                cdl_markPoint (cdl,ix+00,iy-16,0,13, M_FILL|M_BOX, C_GREEN);
                cdl_markPoint (cdl,ix+45,iy-16,0,13, M_FILL|M_DIAMOND,C_GREEN);
                cdl_markPoint (cdl,ix+60,iy-16,0,13, M_FILL|M_CIRCLE, C_GREEN);
                cdl_markPoint (cdl,ix+90,iy-16,0,13, M_VLINE|M_HLINE, C_GREEN);
                cdl_markPoint (cdl,ix+105,iy-16,0,13, M_VLINE|M_HLINE, C_GREEN);
                cdl_markPoint (cdl,ix+120,iy-16,0,13,M_VBLINE|M_HBLINE,C_GREEN);
                cdl_markPoint (cdl,ix+135,iy-16,0,13,M_VBLINE|M_HBLINE,C_GREEN);
                cdl_markPoint (cdl,ix+00,iy-32,0,13, M_PLUS|M_BOX, C_GREEN);
                cdl_markPoint (cdl,ix+45,iy-32,0,13, M_PLUS|M_DIAMOND,C_GREEN);
                cdl_markPoint (cdl,ix+60,iy-32,0,13, M_PLUS|M_CIRCLE, C_GREEN);
                cdl_markPoint (cdl,ix+90,iy-32,0,13, M_HLINE|M_POINT, C_GREEN);
                cdl_markPoint (cdl,ix+105,iy-32,0,13, M_VLINE|M_POINT, C_GREEN);
                cdl_markPoint (cdl,ix+120,iy-32,0,13, M_HBLINE|M_POINT,C_GREEN);
                cdl_markPoint (cdl,ix+135,iy-32,0,13, M_VBLINE|M_POINT,C_GREEN);
                cdl_markPoint (cdl,ix+00,iy-48,0,13, M_CROSS|M_BOX, C_GREEN);
                cdl_markPoint (cdl,ix+45,iy-48,0,13, M_CROSS|M_DIAMOND,C_GREEN);
                cdl_markPoint (cdl,ix+60,iy-48,0,13, M_CROSS|M_CIRCLE, C_GREEN);
                cdl_markPoint (cdl,ix+90,iy-48,0,13, M_VLINE|M_HLINE|M_POINT,
		    C_GREEN);
                cdl_markPoint (cdl,ix+105,iy-48,0,13, M_HLINE|M_VLINE|M_POINT,
		    C_GREEN);
                cdl_markPoint (cdl,ix+120,iy-48,0,13, M_VBLINE|M_HBLINE|M_POINT,
		    C_GREEN);
                cdl_markPoint (cdl,ix+135,iy-48,0,13, M_HBLINE|M_VBLINE|M_POINT,
		    C_GREEN);
                break;

            case '+':                                   /* MARK PLUS      */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_markPoint (cdl, ix, iy, 1, 7, M_PLUS, C_KHAKI))
                   printf ("...returns an error\n");
                break;

            case 'x':                                   /* MARK CROSS     */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);     iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_markPoint (cdl, ix, iy, 1, 7, M_CROSS, C_CYAN))
                   printf ("...returns an error\n");
                break;

            case '*':                                   /* MARK STAR     */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);     iy = (int) (ry + 0.5);
                printf ("cursor: x=%g/%d y=%g/%d key='%c'\n", rx,ix,ry,iy,key);
                if (cdl_markPoint (cdl, ix, iy, 1, 7, M_STAR, C_GREEN))
                   printf ("...returns an error\n");
                break;

	    case '-':
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);     iy = (int) (ry + 0.5);
                (void) cdl_markPoint (cdl,ix, iy, 0, 30, M_HBLINE, C_RED);
		break;

	    case '|':
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);     iy = (int) (ry + 0.5);
                (void) cdl_markPoint (cdl,ix, iy, 0, 30, M_VBLINE, C_RED);
		break;

	    case '=':
		cdl_readFrameBuffer (cdl, &pix, &nx, &ny);
		cdl_printPixToFile (cdl, "foo.eps", pix, nx, ny, 1);
		break;

            case '&':
                clock ();
                for (i=0, ix=16, iy=16; i < 512; i++) {
                    ix = xarray[i] = (ix+10 > 500 ? 16 : ix + 10);
                    iy = yarray[i] = (ix == 16 ? iy + 16: iy);
                }
                for (i=0; i < 512; i++) {
                    if (cdl_markPoint (cdl, xarray[i], yarray[i], 0, 7, 
			M_PLUS, C_RED))
                            printf ("...returns an error\n");
                }
                printf ("takes %d microseconds\n", clock());
		break;
 
	    case 'Q':
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
		printf("cursor: x=%g/%d  y=%g/%d  wcs = %d key='%c' frame=%d\n",
		    rx, ix, ry, iy,  wcs, key, wcs / 100);

                (void) cdl_queryMap (cdl, wcs, name, &sx, &sy, &snx, &sny,
			&dx, &dy, &dnx, &dny, obj);
                printf ("\tregion='%s' ref='%s'\n", name, obj);
                printf ("\tsrc = %g,%g,%d,%d   dest = %d,%d,%d,%d\n",
                     sx, sy, snx, sny, dx, dy, dnx, dny);

		break;

	    case '?':
		print_help();
		break;

	    case '1':					/* TEXT STRING	  */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
		if (cdl_markText (cdl, ix, iy, "test", 2,  0.0, C_GREEN))
		    printf ("...returns an error\n");
		if (cdl_markText (cdl, ix-5, iy+15, "test", 2, 45.0, C_GREEN))
		    printf ("...returns an error\n");
		if (cdl_markText (cdl, ix-20, iy+30, "test", 2, 90.0, C_GREEN))
		    printf ("...returns an error\n");
		if (cdl_markText (cdl, ix-35, iy+30, "test", 2, 135.0, C_GREEN))
		    printf ("...returns an error\n");
		if (cdl_markText (cdl, ix-40, iy+15, "test", 2, 180.0, C_GREEN))
		    printf ("...returns an error\n");
		if (cdl_markText (cdl, ix-40, iy-10, "test", 2, 225.0, C_GREEN))
		    printf ("...returns an error\n");
		if (cdl_markText (cdl, ix-30, iy-25, "test", 2, 270.0, C_GREEN))
		    printf ("...returns an error\n");
		if (cdl_markText (cdl, ix-15, iy-15, "test", 2, 315.0, C_GREEN))
		    printf ("...returns an error\n");
		break;

	    case '2':					/* TEXT STRING	  */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
		if (cdl_markText (cdl, ix, iy, "testing", 2, 180.0, C_GREEN))
		    printf ("...returns an error\n");
		break;

	    case '3':					/* BLINKING MARKER */
                (void) cdl_readCursor (cdl, 0, &rx, &ry, &wcs, &key);
                ix = (int) (rx + 0.5);      iy = (int) (ry + 0.5);
		cdl_blinkCircle (cdl, ix, iy, 10, C_YELLOW, 5);
		break;

            case '4':                                   /* BLINKING MARKER */
               (void) cdl_setCursor (cdl, 100, 100, 0);
                break;
	    }
	    printf ("Command: ");

	  /* gets(cmd); */
	  scanf ("%s", cmd);
	} while (cmd[0] != 'q');

quit:	(void) cdl_close (cdl);	
}

cdl_blinkCircle (cdl, x, y, r, color, nblinks)
CDLPtr 	cdl;
int 	x;
int 	y;
int 	r;
int 	color;
int 	nblinks;
{
	char *init, *mark, *blink;
	int  i, dim = 2*r + 2, DIM=4*r + 2;

	init =  (char *) malloc (DIM*DIM);
	mark =  (char *) malloc (dim*dim);
	blink = (char *) malloc (DIM*DIM);

	cdl_readSubRaster (cdl, x-dim, y-dim, DIM, DIM, &init); 

	cdl_setLineWidth (cdl, 3);
        cdl_markCircle (cdl, x, y, 2*r-1, 0, color);
	cdl_readSubRaster (cdl, x-dim, y-dim, DIM, DIM, &blink); 
	cdl_setLineWidth (cdl, 1);

        cdl_markCircle (cdl, x, y, r, 0, color);
	cdl_readSubRaster (cdl, x-r, y-r, 2*r, 2*r, &mark); 

	for (i=0; i<nblinks; i++) {
	    cdl_writeSubRaster (cdl, x-dim, y-dim, DIM, DIM, blink); 
	    usleep (333*1000);
	    cdl_writeSubRaster (cdl,  x-r, y-r, 2*r, 2*r, mark);
	    usleep (333*1000);
	    cdl_writeSubRaster (cdl, x-dim, y-dim, DIM, DIM, init); 
	    usleep (333*1000);
	}
	cdl_writeSubRaster (cdl,  x-r, y-r, 2*r, 2*r, mark);

	free ((char *)init);
	free ((char *)blink);
	free ((char *)mark);
}


print_help ()
{
	printf ("\n");
	printf ("    b -  mark box         ");
	printf ("    B -  fill box         ");
	printf ("    c -  clear frame      "); printf ("\n");
	printf ("    d -  delete marker    ");
	printf ("    D -  delete overlay   ");
	printf ("    e -  mark ellipse     "); printf ("\n");
	printf ("    E -  mark ellip ann   ");
	printf ("    f -  display FITS     ");
	printf ("    g -  read image       "); printf ("\n");
	printf ("    G -  read fb          ");
	printf ("    i -  display iraf     ");
	printf ("    l -  mark line        "); printf ("\n");
	printf ("    m -  mark point       ");
	printf ("    n -  next frame       ");
	printf ("    N -  next config      "); printf ("\n");
	printf ("    M -  mark all pts     ");
	printf ("    o -  mark circle      ");
	printf ("    O -  mark circle ann  "); printf ("\n");
	printf ("    p -  previous frame   ");
	printf ("    P -  prev config      ");
	printf ("    q -  quit             "); printf ("\n");
	printf ("    r -  read cursor      ");
	printf ("    s -  text string      ");
	printf ("    S -  sample cursor    "); printf ("\n");
	printf ("    t -  test image       ");
	printf ("    T -  tile image       ");
	printf ("    w -  set wcs          "); printf ("\n");
	printf ("    W -  get wcs          ");
	printf ("    ( -  mark polyline    ");
	printf ("    ) -  mark polygon     "); printf ("\n");
	printf ("    + -  mark plus        ");
	printf ("    x -  mark cross       ");
	printf ("    * -  mark star        "); printf ("\n");
	printf ("    = -  test hardcopy    ");
	printf ("    & -  time 512 points  ");
	printf ("    > -  enable debug     "); printf ("\n");
	printf ("    < -  disable debug    ");
	printf ("    ? -  help             ");
	printf ("    , -  dec linewidth    "); printf ("\n");
	printf ("    . -  inc linewidth    ");
	printf ("    [ -  dec textwidth    ");
	printf ("    ] -  inc textwidth    "); printf ("\n");
	printf ("    { -  dec linestyle    ");
	printf ("    } -  inc linestyle    ");
	printf ("    Q -  query mapping    "); printf ("\n");
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

