include <imhdr.h>
include <mwset.h>
include <math.h>

define  SZ_KEYWORD  	8
define	SZ_PLATECOEFF	20
define	SZ_CDMATX	4

# AT_MKDSS -- Compute the FITS WCS from the general plate solution for a 
# DSS image. This routine assumes that the geometry of the DSS image has not
# been modified since it was extracted, i.e. it has not been shifed,rotated,
# scaled, transposed etc. This routine has been adapted from one in the STSDAS
# GASP package, whose maim limitation for IRAF purposes was that it bypassed
# the IRAF MWCS routines. Return OK it the header is successfully updated,
# ERR otherwise.

int procedure at_mkdss (im, update, verbose)

pointer		im			#I the DSS image descriptor
bool		update			#I update rather than list  the wcs ?
bool		verbose			#I verbose mode ?

double	amdx[SZ_PLATECOEFF]		# the RA plate solution coefficients
double	amdy[SZ_PLATECOEFF]		# the DEC plate solution coefficients
double	plate_cen_x			# the x center position in microns
double	plate_cen_y			# the y center position in microns
double	plate_cen_ra			# the RA plate center in radians
double	plate_cen_dec			# the DEC plate center in radians
double	x_pixel_size			# the x step size in microns
double	y_pixel_size			# the y step size in microns
double	plate_scale			# the plate sclae in arcsec / mm
double	im_x_center_pix 		# the x of ll corner of scanned plate
double	im_y_center_pix 		# the y of ll corner of scanned plate
double	ra_s				# the plate center RA in seconds
double	dec_s				# the plate center DEC in seconds
double  object_mag			# the object magnitude
double	object_col			# the object color 
int	ra_h, ra_m			# the plate center RA hours, minutes
int	dec_d, dec_m			# the plate center DEC degrees, minutes
char	dec_sign			# the plate center DEC sign +/-
int	xcorner				# the ll x of image w/r to plate
int	ycorner				# the ll y of image w/r to plate
int	xsize				# naxis1
int	ysize				# naxis2

double	crpix1, crpix2, crval1, crval2, cdmatx[SZ_CDMATX]
int	i
char	parname[SZ_KEYWORD]

double	imgetd()
real	imgetr()
int	imaccf(), imgeti()
errchk	imgetr(), imgetd(), imgeti()

begin
	# Check that the image is 2D, if not it is not a DSS image.
	if (IM_NDIM(im) != 2)
	    return (ERR)

	# See if image header contains the general plate solution.
	if (imaccf (im,"PPO3    ") == NO)
	    return (ERR)

	# If we have an old DSS image, i.e. the one with the CRPIX rather
	# than CNPIX keywords, rename CRPIX to CNPIX and proceed.
	# this keyword to CNPIX and proceed.

	if (imaccf (im,"CRPIX1") == YES || imaccf (im, "CRPIX2") == YES) {
	    if (imaccf (im,"CRVAL1") == YES || imaccf (im, "CRVAL2") == YES) {
	        if (imaccf (im,"CD1_1") == NO && imaccf (im, "CD1_2") == NO &&
		    imaccf (im, "CD2_1") == NO && imaccf (im, "CD2_2") == NO) {
		    # This is the case when we have CRPIX, CRVAL and no CD
		    # so, proceed to calculate the WCS again.
	            iferr (crpix1 = imgetr (im, "CRPIX1"))
			return (ERR)
	            iferr (crpix2 = imgetr (im, "CRPIX2"))
			return (ERR)
	            call imdelf (im, "CRPIX1")
	            call imaddr (im, "CNPIX1", real (crpix1))
	            call imdelf (im, "CRPIX2")
	            call imaddr (im, "CNPIX2", real (crpix2))
	        }
	    } else {
	         iferr (crpix1 = imgetr (im, "CRPIX1"))
	     	     return (ERR)
	         iferr (crpix2 = imgetr (im, "CRPIX2"))
	     	     return (ERR)
	         call imdelf (im, "CRPIX1")
	         call imaddr (im, "CNPIX1", real (crpix1))
	         call imdelf (im, "CRPIX2")
	         call imaddr (im, "CNPIX2", real (crpix2))
	    }
	}
	if (imaccf (im,"CNPIX1") == NO || imaccf (im, "CNPIX2") == NO )
	    return (ERR)

	# Get the plate solution.
	iferr {

	    # Get the plate center parameters.
	    plate_cen_x = imgetd (im, "PPO3    ")
	    plate_cen_y = imgetd (im, "PPO6    ")
	    x_pixel_size = imgetd (im, "XPIXELSZ")
	    y_pixel_size = imgetd (im, "YPIXELSZ")
	    plate_scale = imgetd (im, "PLTSCALE")
	    ra_h = imgeti (im, "PLTRAH  ")
	    ra_m = imgeti (im, "PLTRAM  ")
	    ra_s = imgetd (im, "PLTRAS  ")
	    call imgstr (im, "PLTDECSN", dec_sign, 1)
	    dec_d = imgeti (im, "PLTDECD ")
	    dec_m = imgeti (im, "PLTDECM ")
	    dec_s = imgetd (im, "PLTDECS ")
	    plate_cen_ra = DDEGTORAD ((ra_h + ra_m / 60.0d0 + ra_s /
	        3600.0d0) * 15.0d0)
	    plate_cen_dec = DDEGTORAD (dec_d + dec_m / 60.0d0 + dec_s /
	        3600.0d0)
	    if (dec_sign == '-')
	       plate_cen_dec = -plate_cen_dec
 
	    # Get general plate solution coefficients
	    do i = 1, SZ_PLATECOEFF {
	        call sprintf (parname, SZ_KEYWORD, "AMDX%d")
	            call pargi(i)
	        amdx[i] = imgetd (im, parname)
	    }
	    do i = 1, SZ_PLATECOEFF {
	        call sprintf (parname, SZ_KEYWORD, "AMDY%d")
	           call pargi(i)
	        amdy[i] = imgetd (im, parname)
	    }
	    xcorner = imgetr (im, "CNPIX1")
	    ycorner = imgetr (im, "CNPIX2")
	    object_mag = 0.0d0
	    object_col = 0.0d0
	} then
	    return (ERR)

	xsize   = IM_LEN(im,1)
	ysize   = IM_LEN(im,2) 
	crpix1  = xsize / 2.0d0
	crpix2  = ysize / 2.0d0

	# Center of image w/r to original lower left corner of scanned plate.
	im_x_center_pix = xcorner + (xsize / 2.0d0) - 0.5d0
	im_y_center_pix = ycorner + (ysize / 2.0d0) - 0.5d0

	# Calculate equatorial coordinates for the center of subset giving
	# the complete plate solution w/r to the original lower left corner.
	call ccgseq (plate_cen_ra,
     		     plate_cen_dec,
		     plate_cen_x,
		     plate_cen_y,
		     x_pixel_size,
		     y_pixel_size,
		     plate_scale,
		     amdx,
		     amdy,
		     im_x_center_pix,
		     im_y_center_pix,
		     object_mag,
		     object_col,
		     crval1,
		     crval2)

	
	# Calculate CD matrix values for the input subset from the original
	# plate solution.
	call calcds (plate_cen_ra,
		  plate_cen_dec,
		  plate_cen_x,
		  plate_cen_y,
		  xcorner,
		  ycorner,
		  x_pixel_size,
		  y_pixel_size,
		  plate_scale,
		  xsize,
		  ysize,	
		  crval1,
		  crval2,
		  amdx,
		  amdy,
		  cdmatx)

	# Update the image header.
	crval1 = DRADTODEG (crval1)
	crval2 = DRADTODEG (crval2)

	if (verbose || ! update) 
	    call printf ("    Converting DSS wcs to FITS wcs\n")
	call at_dmwcs (im, crpix1, crpix2, crval1, crval2, cdmatx, update)

	return (OK)
end

define  NEWCD     Memd[ncd+(($2)-1)*ndim+($1)-1]

# AT_DMWCS -- Create new image WCS from the approximation to the DSS plate
# solution.  This routine assumes that the geometry of the DSS image has
# not been changed since since the image has been exracted from the image
# survey.

procedure at_dmwcs (im, xref, yref, lngref, latref, cdmatx, update)

pointer im                      #I pointer to the input image
double  xref, yref              #I the reference point in pixels
double  lngref, latref          #I the  reference point in degrees
double  cdmatx[ARB]             #I CD1_1, CD1_2, CD2_1, CD2_2
bool	update			#I update rather than list the wcs ?

pointer	mw, mwnew
pointer	sp, projstr, r, w, cd, ltm, ltv, iltm, nr, ncd, axno, axval, axes 
int	ndim, ax1, ax2, naxes
pointer	mw_openim(), mw_open()
int	mw_stati()
errchk	mw_newsystem()

begin
	mw = mw_openim (im)
	ndim = mw_stati (mw, MW_NPHYSDIM)

        # Allocate working memory for the vectors and matrices.
        call smark (sp)
	call salloc (projstr, SZ_LINE, TY_CHAR)
        call salloc (r, ndim, TY_DOUBLE)
        call salloc (w, ndim, TY_DOUBLE)
        call salloc (cd, ndim * ndim, TY_DOUBLE)
        call salloc (ltm, ndim * ndim, TY_DOUBLE)
        call salloc (ltv, ndim, TY_DOUBLE)
        call salloc (iltm, ndim * ndim, TY_DOUBLE)
        call salloc (nr, ndim, TY_DOUBLE)
        call salloc (ncd, ndim * ndim, TY_DOUBLE)
        call salloc (axno, IM_MAXDIM, TY_INT)
        call salloc (axval, IM_MAXDIM, TY_INT)
        call salloc (axes, IM_MAXDIM, TY_INT)

        # Open the new wcs.
        mwnew = mw_open (NULL, ndim)
        call mw_gsystem (mw, Memc[projstr], SZ_LINE)
        iferr {
            call mw_newsystem (mw, "image", ndim)
        } then {
            call mw_newsystem (mwnew, Memc[projstr], ndim)
        } else {
            call mw_newsystem (mwnew, "image", ndim)
        }

        # Set the LTERM.
        call mw_gltermd (mw, Memd[ltm], Memd[ltv], ndim)
        call mw_sltermd (mwnew, Memd[ltm], Memd[ltv], ndim)

        # Store the old axis map for later use.
        call mw_gaxmap (mw, Memi[axno], Memi[axval], ndim)

        # Get the 2 logical axes.
        call mw_gaxlist (mw, 03B, Memi[axes], naxes)
        ax1 = Memi[axes]
        ax2 = Memi[axes+1]

        # Set the axes and projection type.
        call sprintf (Memc[projstr], SZ_LINE,
            "axis 1: axtype = ra axis 2: axtype = dec ")
        call mw_swtype (mwnew, Memi[axes], ndim, "tan", Memc[projstr])

	# Set the reference point world coordinates.
        Memd[w+ax1-1] = lngref
        Memd[w+ax2-1] = latref

        # Set the reference point pixel coordinates.
        Memd[nr+ax1-1] = xref
        Memd[nr+ax2-1] = yref

        # Compute the new CD matrix.
        NEWCD(ax1,ax1) = cdmatx[1] # xscale * cos (DEGTORAD(xrot)) / 3600.0d0
        NEWCD(ax2,ax1) = cdmatx[2] # -yscale * sin (DEGTORAD(yrot)) / 3600.0d0
        NEWCD(ax1,ax2) = cdmatx[3] # xscale * sin (DEGTORAD(xrot)) / 3600.0d0
        NEWCD(ax2,ax2) = cdmatx[4] # yscale * cos (DEGTORAD(yrot)) / 3600.0d0

	# List the new wcs.
	if (! update)
	    call at_mwshow (mwnew, Memd[ltv], Memd[ltm], Memd[w], Memd[nr],
	        Memd[ncd], ndim) 

        # Recompute and store the new wcs.
        call mw_saxmap (mwnew, Memi[axno], Memi[axval], ndim)
        call mwmmuld (Memd[ncd], Memd[ltm], Memd[cd], ndim)
        call mwinvertd (Memd[ltm], Memd[iltm], ndim)
        call asubd (Memd[nr], Memd[ltv], Memd[r], ndim)
        call mwvmuld (Memd[iltm], Memd[r], Memd[nr], ndim)
        call mw_swtermd (mwnew, Memd[nr], Memd[w], Memd[cd], ndim)

	# Update the image wcs.
	if (update)
	    call mw_saveim (mwnew, im)

        call mw_close (mwnew)
        call mw_close (mw)

        call sfree (sp)
end
