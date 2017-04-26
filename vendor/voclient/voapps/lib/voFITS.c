#include <string.h>
#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include "voApps.h"
#include "fitsio.h"


#define  MAX_IMAGES      20480           /* max images to process        */

#define  dabs(x)         ((x<0.0?-x:x))


/**
 *  Private methods.
 */
static void  vot_printFrameInfo (FILE *fd, frameInfo *im);
static  int  vot_getFrameWcs (fitsfile *fptr, frameInfo *info);

extern  int  vot_fileType (char *name);




/**
 *  VOT_IMAGEINFO -- Get information about a FITS file structure and WCS.
 *  
 *  @fn      info = vot_imageInfo (char *name, int do_all)
 *
 *  @brief            Get information about a FITS file structure and WCS.
 *  @param   name     name of FITS file to open
 *  @param   do_all   do all extensions in an MEF file?
 *  @return  	      pointer to ImInfo structure
 */
ImInfo *
vot_imageInfo (char *name, int do_all)
{
    fitsfile *fptr;
    ImInfo   *info = (ImInfo *) NULL;
    long      naxes[3] = {0, 0, 0}, nrows=0;
    int	      nextns=0, naxis=0, bitpix=0, extnum=0;
    int       hdupos=0, hdutype=0, ncols=0, i=0, status=0;
    double    cxsum=0.0, cysum=0.0, rxsum=0.0, rysum=0.0;


    /*  Check for file existence.
     */
    if (access (name, F_OK) != 0) {
	fprintf (stderr, "Error: cannot open image '%s'\n", name);
	return ((ImInfo *) NULL);
    }
    if (vot_fileType (name) != VOT_FITS) {
	fprintf (stderr, "Error: file '%s' is not a FITS image\n", name);
	return ((ImInfo *) NULL);
    }

    info = (ImInfo *) calloc (1, sizeof (ImInfo));
    if (fits_open_file (&fptr, name, READONLY, &status) == 0) {
	fits_get_num_hdus (fptr, &nextns, &status);
        fits_get_hdu_num (fptr, &hdupos);    /* get the current HDU position */

	strncpy (info->imname, name, strlen(name));
	info->nextend = (nextns - 1);
	info->extns = (frameInfo *) calloc (nextns, sizeof (frameInfo));

        for (; !status; hdupos++) {    /* Main loop for each HDU */
            fits_get_hdu_type (fptr, &hdutype, &status); /* Get the HDU type */

            if (hdutype == IMAGE_HDU) {    /* primary array or image HDU */
                fits_get_img_param (fptr, 10, &bitpix, &naxis, naxes, &status);

		if (hdupos == 0) {				/* PHU	*/
		    info->frame.is_image = 1;
		    info->frame.is_table = 0;
		    info->frame.naxis    = naxis;
		    info->frame.bitpix   = bitpix;
		    for (i=0; i < naxis; i++)
			info->frame.naxes[i] = naxes[i];

		} else {					/* EHU	*/
		    extnum = hdupos - 1;
		    info->extns[extnum].is_image = 1;
		    info->extns[extnum].is_table = 0;
		    info->extns[extnum].extnum   = extnum;
		    info->extns[extnum].naxis    = naxis;
		    info->extns[extnum].bitpix   = bitpix;
		    for (i=0; i < naxis; i++)
			info->extns[extnum].naxes[i] = naxes[i];

		    if (vot_getFrameWcs (fptr, &info->extns[extnum]) == 0)
		        info->extns[extnum].has_wcs = 1;
		}

            } else {					/* a table HDU */
		if (hdupos > 0) {				/* EHU	*/
                    fits_get_num_rows (fptr, &nrows, &status);
                    fits_get_num_cols (fptr, &ncols, &status);

		    extnum = hdupos - 1;
		    info->extns[extnum].is_image = 0;
		    info->extns[extnum].is_table = 1;
		    info->extns[extnum].naxis    = 2;
		    info->extns[extnum].bitpix   = 0;
		    info->extns[extnum].naxes[0] = ncols;
		    info->extns[extnum].naxes[1] = nrows;
		}

#ifdef GET_TBLINFO
                printf ("%s Table:  %d columns x %ld rows\n", 
		    ((hdutype==ASCII_TBL) ? "ASCII" : "Binary"); ncols, nrows);
                printf(" COL NAME             FORMAT\n");
                for (i = 1; i <= ncols; i++) {
                    fits_make_keyn ("TTYPE", i, keyname,&status);
                    fits_read_key (fptr, TSTRING, keyname,colname,NULL,&status);
                    fits_make_keyn ("TFORM", i, keyname,&status);
                    fits_read_key (fptr, TSTRING, keyname,coltype,NULL,&status);
                    printf(" %3d %-16s %-16s\n", i, colname, coltype);
                }
#endif
            }

	    /*  Move to next extension.
	     */
            fits_movrel_hdu (fptr, 1, NULL, &status); 
        }

        if (status == END_OF_FILE)
            status = 0;    			/* reset normal error 	   */

    } else if (status)    			/* print any error message */
        fits_report_error (stderr, status);


    /*  Compute the values for the entire frame.
     */
    info->frame.lx = info->frame.ly =  360.0;
    info->frame.ux = info->frame.uy = -360.0;

    info->frame.cx     = info->extns[0].cx;
    info->frame.cy     = info->extns[0].cy;
    info->frame.lx     = info->extns[0].lx;
    info->frame.ly     = info->extns[0].ly;
    info->frame.ux     = info->extns[0].ux;
    info->frame.uy     = info->extns[0].uy;
    info->frame.rotang = info->extns[0].rotang;
    info->frame.xrval  = info->extns[0].xrval;
    info->frame.yrval  = info->extns[0].yrval;
    info->frame.xrpix  = info->extns[0].xrpix;
    info->frame.yrpix  = info->extns[0].yrpix;
    info->frame.radius = info->extns[0].radius;
		    
    if (nextns == 1) {
        for (i=0; i < naxis; i++)
	    info->frame.naxes[i] = info->extns[0].naxes[i];
    }
    memcpy (&info->frame.xc[0], &info->extns[0].xc[0], (sizeof(double)*4));
    memcpy (&info->frame.yc[0], &info->extns[0].yc[0], (sizeof(double)*4));

    for (i=1; i < nextns; i++) {
	if (info->extns[i].lx < info->frame.lx) 
	    info->frame.lx = info->extns[i].lx;
	if (info->extns[i].ly < info->frame.ly) 
	    info->frame.ly = info->extns[i].ly;

	if (info->extns[i].ux > info->frame.ux) 
	    info->frame.ux = info->extns[i].ux;
	if (info->extns[i].uy > info->frame.uy) 
	    info->frame.uy = info->extns[i].uy;
	cxsum += info->extns[i].cx;
	cysum += info->extns[i].cy;

	rxsum += info->extns[i].naxes[0];
	rysum += info->extns[i].naxes[1];

	info->frame.scale  = info->extns[i].scale;
	info->frame.rotang = info->extns[i].rotang;
	strcpy (info->frame.ctype, info->extns[i].ctype);
    }

    if (nextns > 1) {
        info->frame.xrval = info->frame.cx = (cxsum / (double) nextns);
        info->frame.yrval = info->frame.cy = (cysum / (double) nextns);
        info->frame.xrpix = info->frame.radius / 
		(info->frame.scale / 3600.) / 2.0;
        info->frame.yrpix = info->frame.radius / 
		(info->frame.scale / 3600.) / 2.0;
    } else {
        info->frame.xrval = info->frame.cx = info->extns[0].cx;
        info->frame.yrval = info->frame.cy = info->extns[0].cy;
        info->frame.xrpix = info->extns[0].xrpix;
        info->frame.yrpix = info->extns[0].yrpix;
	strcpy (info->frame.ctype, info->extns[0].ctype);
    }

    info->frame.width  = ((info->frame.ux+360.) - (info->frame.lx+360.));
    info->frame.height = ((info->frame.uy+ 90.) - (info->frame.ly+ 90.));
    info->frame.radius = sqrt (
	(info->frame.cx - info->frame.lx) * 
	(info->frame.cx - info->frame.lx) + 
	(info->frame.cy - info->frame.ly) * 
	(info->frame.cy - info->frame.ly) );
    info->frame.xc[0]  = info->frame.lx;   info->frame.yc[0] = info->frame.ly;
    info->frame.xc[1]  = info->frame.lx;   info->frame.yc[1] = info->frame.uy;
    info->frame.xc[2]  = info->frame.ux;   info->frame.yc[2] = info->frame.uy;
    info->frame.xc[3]  = info->frame.ux;   info->frame.yc[3] = info->frame.ly;


    fits_close_file (fptr, &status);
    return ( (ImInfo *) info);
}


/**
 *  VOT_IMAGENEXTNS -- Get the number of extensions in an MEF file.
 *
 *  @fn      nextn = vot_imageNExtns (char *name)
 *
 *  @brief            Get the number of extensions in an MEF file.
 *  @param   name     name of FITS file to open
 *  @return           number of extensions in an MEF file
 */
int
vot_imageNExtns (char *image)
{
    fitsfile *fptr;
    int   nextns = -1, status = 0;


    if (fits_open_file (&fptr, image, READONLY, &status) == 0) {
	fits_get_num_hdus (fptr, &nextns, &status);
        fits_close_file (fptr, &status);
    }

    return (nextns);
}


/**
 *  VOT_PRINTIMAGEINFO -- Print the image information.
 *
 *  @fn      vot_printImageInfo (FILE *fd, ImInfo *im)
 *
 *  @brief            Print the image information.
 *  @param   fd       output file descriptor (or stdout/stderr)
 *  @param   im       image information structure
 *  @return           nothing
 */
void
vot_printImageInfo (FILE *fd, ImInfo *im)
{
    register int i;

    fprintf (fd, "Name: %s    nextns = %d\n", im->imname, im->nextend);

    fprintf (fd, "Frame:\n");   im->frame.extnum = -1;
        vot_printFrameInfo (fd, &im->frame);

    for (i=1; i < im->nextend; i++)
	vot_printFrameInfo (fd, &im->extns[i]);
}


/**
 *  VOC_FREEIMAGEINFO -- Free the image information structure.
 *
 *  @fn      vot_freeImageInfo (ImInfo *im)
 *
 *  @brief            Free the image information structure.
 *  @param   im       image information structure
 *  @return           nothing
 */
void
vot_freeImageInfo (ImInfo *im)
{
    free ((void *) im->extns);            /* extension structs    */
    free ((void *) im);                   /* main image structs   */
}



/****************************************************************************
 ***   				Private procedures
 ****************************************************************************/

/**
 *  VOT_PRINTFRAMEINFO -- Print information about a specific frame.
 */
static void
vot_printFrameInfo (FILE *fd, frameInfo *im)
{
    if (im->extnum >= 0) {
        fprintf (fd, "\nExt: %d  dims[%d] = %d %d %d\t",
            im->extnum, im->naxis, im->naxes[0], im->naxes[1], im->naxes[2]);
        fprintf (fd, "  image: %d   table: %d   has_wcs: %d   flip: %d\n",
            im->is_image, im->is_table, im->has_wcs, im->axflip);
    }

    fprintf (fd, 
	"  center:  %8.4f %8.4f   ll: %8.4f %8.4f    ur: %8.4f %8.4f\n",
        im->cx, im->cy, im->lx, im->ly, im->ux, im->uy);
    fprintf (fd, "  corners: %8.4f %8.4f       %8.4f %8.4f\n",
	im->xc[1], im->yc[1], im->xc[2], im->yc[2]);
    fprintf (fd, "\t   %8.4f %8.4f       %8.4f %8.4f\n",
        im->xc[0], im->yc[0], im->xc[3], im->yc[3]);
    fprintf (fd, 
	"  crval: %8.4f %8.4f   crpix: %8.4f %8.4f  ctype: '%s'\n",
        im->xrval, im->yrval, im->xrpix, im->yrpix, im->ctype);
    fprintf (fd, 
	"  w/h: %8.4f %8.4f   radius: %8.4f  rot: %8.4f  scale: %8.4f\n",
        dabs(im->width), dabs(im->height), im->radius, im->rotang, im->scale); 
}


/**
 *  VOT_GETFRAMEWCS -- Get the WCS information for a given frame.
 */
static int  
vot_getFrameWcs (fitsfile *fptr, frameInfo *info)
{
    double   xrval=0.0, yrval=0.0, xrpix=0.0, yrpix=0.0, xpix=0.0, ypix=0.0;
    double   xinc=0.0, yinc=0.0, rot=0.0, scale=0.0, xrot=0.0, yrot=0.0;
    double   cx=0.0, cy=0.0, lx=0.0, ly=0.0, ux=0.0, uy=0.0;
    double   cd11=0.0, cd12=0.0, cd21=0.0, cd22=0.0, cdelt1=0.0, cdelt2=0.0;
    int      i, axflip=0, status = 0;
    char     str[32], ctype[5], comment[80];


    /*  Get the header WCS keywords.
     */
    fits_read_img_coord (fptr, &xrval, &yrval, &xrpix,
               &yrpix, &xinc, &yinc, &rot, ctype, &status);

    info->xrval    = xrval;
    info->yrval    = yrval;
    info->xrpix    = xrpix;
    info->yrpix    = yrpix;
    info->has_wcs  = 1;
    info->is_image = 1;
    strcpy (info->ctype, ctype);

    xpix = (double) 0.5;                        /*   Lower-left         */
    ypix = (double) 0.5;
    status = 0;
    fits_pix_to_world (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, 
	rot, ctype, &info->xc[0], &info->yc[0], &status);

    xpix = (double) 0.5;                        /*   Upper-left         */
    ypix = (double) info->naxes[1] - 0.5;
    status = 0;
    fits_pix_to_world (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, 
	rot, ctype, &info->xc[1], &info->yc[1], &status);

    xpix = (double) info->naxes[0] - 0.5;       /*   Upper-right        */
    ypix = (double) info->naxes[1] - 0.5;
    status = 0;
    fits_pix_to_world (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, 
	rot, ctype, &info->xc[2], &info->yc[2], &status);

    xpix = (double) info->naxes[0] - 0.5;       /*   Lower-right        */
    ypix = (double) 0.5;
    status = 0;
    fits_pix_to_world (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, 
	rot, ctype, &info->xc[3], &info->yc[3], &status);

    xpix = (double) info->naxes[0] / 2. - 0.5;	/*  Center		*/
    ypix = (double) info->naxes[1] / 2. - 0.5;
    status = 0;
    fits_pix_to_world (xpix, ypix, xrval, yrval, xrpix, yrpix, xinc, yinc, 
	rot, ctype, &info->cx, &info->cy, &status);
    info->cx = xrval;
    info->cy = yrval;

    /*  Get center and cone radius.
     */
    lx = info->lx = info->xc[0];		/*  Lower-left		*/
    ly = info->ly = info->yc[0];
    ux = info->ux = info->xc[2];		/*  Upper-right		*/
    uy = info->uy = info->yc[2];
    cx = info->cx;
    cy = info->cy;
    status = 0;
    if ((i = fits_read_key_dbl(fptr, "CD1_1", &cd11, comment, &status))==0) {
        fits_read_key_dbl (fptr, "CD1_2", &cd12, comment, &status);
        fits_read_key_dbl (fptr, "CD2_1", &cd21, comment, &status);
        fits_read_key_dbl (fptr, "CD2_2", &cd22, comment, &status);

        scale = 3600.0 * sqrt ((cd11*cd11+cd21*cd21+cd12*cd12+cd22*cd22) / 2.);
	xrot  = dabs (atan2 ( cd21, cd11));
	yrot  = dabs (atan2 (-cd12, cd22));
	rot   = (xrot + yrot) / 2.0;
    } else {
	/*  Old-style keywords.
	 */
	status = 0;
        if (!fits_read_key_dbl (fptr, "CDELT1", &cdelt1, comment, &status)) {
             fits_read_key_dbl (fptr, "CDELT2", &cdelt2, comment, &status);

	    scale = 3600.0 * sqrt ((cdelt1*cdelt1 + cdelt2*cdelt2) / 2.);

            if (!fits_read_key_dbl (fptr, "CROTA1", &xrot, comment, &status)) {
                 fits_read_key_dbl (fptr, "CROTA2", &yrot, comment, &status);
		     rot   = (xrot + yrot) / 2.0;
            }
        } else
	    info->has_wcs  = 0;
    }

    status = 0;
    memset (str, 0, 32);
    if ((i = fits_read_key_str(fptr, "CTYPE1", str, comment, &status))==0) {
	if (strncasecmp (str,"DEC",3) == 0 || strncasecmp (str,"LAT",3) == 0)
	    axflip = 1;
    }

    /*  For a bad/approximate WCS, compute in rough coords.
     */
    if ( (lx == cx) && (ly == cy) ) {
	double s = (scale / 3600.0);

        lx = info->lx = cx - (info->naxes[0]/2 * s); /* Lower-left	*/
        ly = info->ly = cy - (info->naxes[1]/2 * s);
        ux = info->ux = cx + (info->naxes[0]/2 * s); /* Upper-right	*/
        uy = info->uy = cy + (info->naxes[1]/2 * s);

        info->xc[0] = cx - (info->naxes[0]/2 * s); /* Lower-left	*/
        info->yc[0] = cy - (info->naxes[1]/2 * s);

        info->xc[1] = cx - (info->naxes[0]/2 * s); /* Upper-left	*/
        info->yc[1] = cy + (info->naxes[1]/2 * s);

        info->xc[2] = cx + (info->naxes[0]/2 * s); /* Upper-right	*/
        info->yc[2] = cy + (info->naxes[1]/2 * s);

        info->xc[3] = cx + (info->naxes[0]/2 * s); /* Lower-right	*/
        info->yc[3] = cy - (info->naxes[1]/2 * s);
    }

    /*  FIXME -- Doens't handle rotation properly. */
    info->width  = dabs((ux - lx));		/* in degrees		*/
    info->height = dabs((uy - ly));
    info->radius = sqrt ((cx - lx) * (cx - lx) + (cy - ly) * (cy - ly));
    info->rotang = rot;
    info->scale  = scale;
    info->axflip = axflip;

    return (info->has_wcs);
}


/*****************************************************************************
 *  Program main.
 ****************************************************************************/
#ifdef UNIT_TEST
int 
main (int argc, char *argv[])
{
    ImInfo *img = (ImInfo *) NULL;


    if ( (img = vot_imageInfo (argv[1], 1)) ) {
	vot_printImageInfo (stdout, img);
	vot_freeImageInfo (img);
    }

    return (0);
}
#endif
