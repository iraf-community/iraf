include <fset.h>
include <imhdr.h>
include <math.h>
include <mwset.h>
include <math/gsurfit.h>
include "../../lib/skywcs.h"

procedure t_imcctran ()

bool	verbose, update
double	tilng, tilat, tolng, tolat, xscale, yscale, xrot, yrot, xrms, yrms
int	imlist, nxgrid, nygrid, npts, instat, outstat, ndim, fitstat, axbits
pointer	sp, imtemplate, insystem, outsystem, image
pointer	im, mwin, cooin, mwout, cooout, ctin, ctout
pointer	r, w, cd, ltm, ltv, iltm, nr, ncd, jr
pointer	ix, iy, ox, oy, ilng, ilat, olng, olat

bool	clgetb()
double	rg_rmsdiff()
int	fstati(), imtopen(), imtgetim(), sk_decim(), sk_decwcs(), mw_stati()
int	clgeti(), sk_stati(), rg_cdfit()
pointer	immap(), rg_xytoxy(), mw_newcopy()

begin
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (imtemplate, SZ_FNAME, TY_CHAR)
	call salloc (insystem, SZ_FNAME, TY_CHAR)
	call salloc (outsystem, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Get the list of images and output coordinate system.
	call clgstr ("image", Memc[imtemplate], SZ_FNAME)
	call clgstr ("outsystem", Memc[outsystem], SZ_FNAME)

	# Get the remaining parameters.
	nxgrid = clgeti ("nx")
	nygrid = clgeti ("ny")
	npts = nxgrid * nygrid
	verbose = clgetb ("verbose")
	update = clgetb ("update")

	# Loop over the list of images
	imlist = imtopen (Memc[imtemplate])
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image after removing any section notation.
	    call imgimage (Memc[image], Memc[image], SZ_FNAME)
	    if (update)
	        im = immap (Memc[image], READ_WRITE, 0)
	    else
	        im = immap (Memc[image], READ_ONLY, 0)
	    if (verbose) {
		call printf ("INPUT IMAGE: %s\n")
		    call pargstr (Memc[image])
	    }

	    # Create the input system name.
	    call sprintf (Memc[insystem], SZ_FNAME, "%s logical")
		call pargstr (Memc[image])

	    # Open the input image coordinate system.
	    instat = sk_decim (im, "logical", mwin, cooin)
	    if (verbose) {
		if (instat == ERR || mwin == NULL)
		    call printf ("Error decoding the input coordinate system\n")
		call sk_iiprint ("Insystem", Memc[insystem], mwin, cooin)
	    }
	    if (instat == ERR || mwin == NULL) {
		if (mwin != NULL)
		    call mw_close (mwin)
		#call mfree (cooin, TY_STRUCT)
		call sk_close (cooin)
		call imunmap (im)
		next
	    }

	    # Open the output coordinate system.
	    outstat = sk_decwcs (Memc[outsystem], mwout, cooout, cooin)
	    if (verbose) {
	        if (outstat == ERR || mwout != NULL)
		    call printf (
		        "Error decoding the output coordinate system\n")
	        call sk_iiprint ("Outsystem", Memc[outsystem], mwout, cooout)
	    }
	    if (outstat == ERR || mwout != NULL) {
	        if (mwout != NULL)
		    call mw_close (mwout)
	        #call mfree (cooout, TY_STRUCT)
		call sk_close (cooout)
	        call sfree (sp)
	        return
	    }

	    # Get the dimensionality of the wcs.
	    ndim = mw_stati (mwin, MW_NPHYSDIM)

	    # Allocate working memory for the vectors and matrices.
	    call malloc (r, ndim, TY_DOUBLE)
	    call malloc (w, ndim, TY_DOUBLE)
	    call malloc (cd, ndim * ndim, TY_DOUBLE)
	    call malloc (ltm, ndim * ndim, TY_DOUBLE)
	    call malloc (ltv, ndim, TY_DOUBLE)
	    call malloc (iltm, ndim * ndim, TY_DOUBLE)
	    call malloc (nr, ndim, TY_DOUBLE)
	    call malloc (jr, ndim, TY_DOUBLE)
	    call malloc (ncd, ndim * ndim, TY_DOUBLE)

	    # Allocate working memory for the grid points.
	    call malloc (ix, npts, TY_DOUBLE)
	    call malloc (iy, npts, TY_DOUBLE)
	    call malloc (ilng, npts, TY_DOUBLE)
	    call malloc (ilat, npts, TY_DOUBLE)
	    call malloc (ox, npts, TY_DOUBLE)
	    call malloc (oy, npts, TY_DOUBLE)
	    call malloc (olng, npts, TY_DOUBLE)
	    call malloc (olat, npts, TY_DOUBLE)

	    # Compute the original logical to world transformation.
            call mw_gltermd (mwin, Memd[ltm], Memd[ltv], ndim)
	    call mw_gwtermd (mwin, Memd[r], Memd[w], Memd[cd], ndim)
            call mwvmuld (Memd[ltm], Memd[r], Memd[nr], ndim)
            call aaddd (Memd[nr], Memd[ltv], Memd[nr], ndim)
            call mwinvertd (Memd[ltm], Memd[iltm], ndim)
            call mwmmuld (Memd[cd], Memd[iltm], Memd[ncd], ndim)

	    # Output the current image wcs.
	    if (verbose && ! update) {
	        call printf ("\n")
	        call rg_wcsshow (mwin, "Current", Memd[ltv], Memd[ltm], Memd[w],
		    Memd[nr], Memd[ncd], ndim)
	    }

	    # Compute the logical and world coordinates of the input image
	    # grid points.
	    call rg_rxyl (Memd[ix], Memd[iy], nxgrid, nygrid, 1.0d0,
		double(sk_stati(cooin, S_NLNGAX)), 1.0d0,
		double(sk_stati(cooin, S_NLATAX)))
	    ctin = rg_xytoxy (mwin, Memd[ix], Memd[iy], Memd[ilng], Memd[ilat],
		npts, "logical", "world", sk_stati (cooin, S_XLAX),
		sk_stati (cooin, S_YLAX))

	    # Transfrom the input image grid points to the new world coordinate
	    # system.
	    call rg_lltransform (cooin, cooout, Memd[ilng], Memd[ilat],
		Memd[olng], Memd[olat], npts)

	    # Initialize the output transfrom.
	    mwout = mw_newcopy (mwin)

	    # Compute the new world coordinates of the reference point and
	    # update the reference point vector.
	    if (sk_stati(cooin, S_PLNGAX) < sk_stati(cooin, S_PLATAX)) {
	        tilng = Memd[w+sk_stati(cooin,S_PLNGAX)-1]
	        tilat = Memd[w+sk_stati(cooin,S_PLATAX)-1]
	    } else {
	        tilng = Memd[w+sk_stati(cooin,S_PLATAX)-1]
	        tilat = Memd[w+sk_stati(cooin,S_PLNGAX)-1]
	    }
	    call rg_lltransform (cooin, cooout, tilng, tilat, tolng, tolat, 1)
	    if (sk_stati(cooout, S_PLNGAX) < sk_stati(cooout, S_PLATAX)) {
	        Memd[w+sk_stati(cooout,S_PLNGAX)-1] = tolng
	        Memd[w+sk_stati(cooout,S_PLATAX)-1] = tolat
	    } else {
	        Memd[w+sk_stati(cooout,S_PLNGAX)-1] = tolat
	        Memd[w+sk_stati(cooout,S_PLATAX)-1] = tolng
	    }
	    
	    # Set the cd matrix.
	    #axbits = 2 ** (sk_stati (cooout, S_XLAX) - 1) +
	        #2 ** (sk_stati (cooout, S_YLAX) - 1)
	    #call rg_setcd (mwout, Memd[cd], Memd[cd], ndim, axbits)

	    # Set the terms.
	    call mw_swtermd (mwout, Memd[r], Memd[w], Memd[cd], ndim)

	    # Compute the new x and y values.
	    ctout = rg_xytoxy (mwout, Memd[olng], Memd[olat], Memd[ox],
	        Memd[oy], npts, "world", "logical", sk_stati (cooout, S_XLAX),
		sk_stati (cooout, S_YLAX))

	    # Subtract off the origin and compute the coordinate system
	    # rotation angle and scale factor.
	    call asubkd (Memd[ix], Memd[nr+sk_stati(cooin, S_XLAX)-1],
	        Memd[ix], npts) 
	    call asubkd (Memd[iy], Memd[nr+sk_stati(cooin, S_YLAX)-1],
	        Memd[iy], npts) 
	    call asubkd (Memd[ox], Memd[nr+sk_stati(cooout, S_XLAX)-1],
	        Memd[ox], npts) 
	    call asubkd (Memd[oy], Memd[nr+sk_stati(cooout, S_YLAX)-1],
	        Memd[oy], npts) 
	    fitstat = rg_cdfit (Memd[ix], Memd[iy], Memd[ox], Memd[oy], npts,
	        xscale, yscale, xrot, yrot)

	    if (fitstat == OK) {

		# Modify the cd matrix.
	        axbits = 2 ** (sk_stati (cooout, S_XLAX) - 1) +
	            2 ** (sk_stati (cooout, S_YLAX) - 1)
	        call rg_mwxyrot (mwout, xscale, yscale, xrot, yrot, Memd[ncd],
		    Memd[cd], ndim, axbits)
	        call mwmmuld (Memd[cd], Memd[ltm], Memd[ncd], ndim)
		call mwinvertd (Memd[ltm], Memd[iltm], ndim)
		call asubd (Memd[nr], Memd[ltv], Memd[r], ndim)
		call mwvmuld (Memd[iltm], Memd[r], Memd[jr], ndim)
	        call mw_swtermd (mwout, Memd[jr], Memd[w], Memd[ncd], ndim)

	        # Compute and print the goodness of fit estimate.
	        if (verbose) {
		    call mw_ctfree (ctout)
	            ctout = rg_xytoxy (mwout, Memd[olng], Memd[olat],
		        Memd[ox], Memd[oy], npts, "world", "logical",
		        sk_stati (cooout, S_XLAX), sk_stati (cooout, S_YLAX))
	    	    call aaddkd (Memd[ix], Memd[nr+sk_stati(cooout, S_XLAX)-1],
	        	Memd[ix], npts) 
	    	    call aaddkd (Memd[iy], Memd[nr+sk_stati(cooout, S_YLAX)-1],
	        	Memd[iy], npts) 
	            xrms = rg_rmsdiff (Memd[ox], Memd[ix], npts)
	            yrms = rg_rmsdiff (Memd[oy], Memd[iy], npts)
		}

	        # Recompute and store the new wcs if update is enabled.
	        if (update) {
		    call sk_hdrsavim (cooout, mwout, im)
		    call mw_saveim (mwout, im)
	        } else if (verbose)
	            call rg_wcsshow (mwin, "New", Memd[ltv], Memd[ltm], Memd[w],
		        Memd[nr], Memd[cd], ndim)

		if (verbose) {
		    call printf (
	                "Crval%d,%d: %h, %h -> %h, %h dd:mm:ss.s\n")
			call pargi (sk_stati(cooout,S_PLNGAX))
			call pargi (sk_stati(cooout,S_PLATAX))
			call pargd (tilng)
			call pargd (tilat)
			call pargd (tolng)
			call pargd (tolat)
		    call printf ("    Scaling: Xmag: %0.6f Ymag: %0.6f ")
			call pargd (xscale)
			call pargd (yscale)
			call printf ("Xrot: %0.3f Yrot: %0.3f degrees\n")
		        call pargd (xrot)
		        call pargd (yrot)
		    call printf (
		    "    Rms: X fit: %0.7g pixels  Y fit: %0.7g pixels\n")
		        call pargd (xrms)
		        call pargd (yrms)
		    call printf ("\n")
	        }

	    } else
		call printf ("Error fitting the scaling factors angle\n")

	    # Free the memory.
	    call mfree (r, TY_DOUBLE)
            call mfree (w, TY_DOUBLE)
            call mfree (cd, TY_DOUBLE)
            call mfree (ncd, TY_DOUBLE)
            call mfree (nr, TY_DOUBLE)
            call mfree (jr, TY_DOUBLE)
            call mfree (ltm, TY_DOUBLE)
            call mfree (ltv, TY_DOUBLE)
            call mfree (iltm, TY_DOUBLE)

            call mfree (ix, TY_DOUBLE)
            call mfree (iy, TY_DOUBLE)
            call mfree (ilng, TY_DOUBLE)
            call mfree (ilat, TY_DOUBLE)
            call mfree (ox, TY_DOUBLE)
            call mfree (oy, TY_DOUBLE)
            call mfree (olng, TY_DOUBLE)
            call mfree (olat, TY_DOUBLE)

	    # Clean up various data stuctures.
	    if (mwin != NULL)
	        call mw_close (mwin)
	    call sk_close (cooin)
	    if (mwout != NULL)
	        call mw_close (mwout)
	    call sk_ctypeim (cooout, im)
	    call sk_close (cooout)
	    call imunmap (im)
	}

	call imtclose (imlist)

	call sfree (sp)
end


# RG_WCSSHOW -- Print a quick summary of the current wcs.

procedure rg_wcsshow (mwin, label, ltv, ltm, w, r, cd, ndim)

pointer	mwin			#I pointer to the current wcs
char	label[ARB]		#I name of the input label
double	ltv[ARB]		#I the lterm offsets
double	ltm[ndim,ARB]		#I the lterm rotation matrix
double	w[ARB]			#I the fits crval parameters
double	r[ARB]			#I the fits crpix parameters
double	cd[ndim,ARB]		#I the fits rotation matrix
int	ndim			#I the dimension of the wcs

int	i,j
pointer	sp, str
errchk	mw_gwattrs()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print the image name and current wcs.
	call printf ("%s wcs\n")
	    call pargstr (label)

	# Print the axis banner.
	call printf ("    Axis   ")
	do i = 1, ndim {
	    call printf ("%10d  ")
		call pargi (i)
	}
	call printf ("\n")

	# Print the crval parameters.
	call printf ("    Crval  ")
	do i = 1, ndim {
	    call printf ("%10.4f  ")
		call pargd (w[i])
	}
	call printf ("\n")

	# Print the crpix parameters.
	call printf ("    Crpix  ")
	do i = 1, ndim {
	    call printf ("%10.2f  ")
		call pargd (r[i])
	}
	call printf ("\n")

	# Print the cd matrix.
	do i = 1, ndim {
	    call printf ("    Cd %d   ")
		call pargi (i)
	    do j = 1, ndim {
	        call printf ("%10.4g  ")
		    call pargd (cd[j,i])
	    }
	    call printf ("\n")
	}

	call printf ("\n")

	call sfree (sp)
end


# RG_CDFIT -- Compute the cd matrix and shift vector required to realign
# the transformed coordinate systems.

int procedure rg_cdfit (xref, yref, xin, yin, npts, xscale, yscale, xrot, yrot)

double	xref[ARB]			#I the input x reference vector
double	yref[ARB]			#I the input y reference vector
double	xin[ARB]			#I the input x vector
double	yin[ARB]			#I the input y vector
int	npts				#I the number of points
double	xscale, yscale			#O the x and y scale factors
double	xrot				#O the rotation angle in degrees
double	yrot				#O the rotation angle in degrees

int	fitstat
double	xshift, yshift
pointer	sp, wts
int	rg_ffit()

begin
	call smark (sp)
	call salloc (wts, npts, TY_DOUBLE)
	call amovkd (1.0d0, Memd[wts], npts)

	fitstat = rg_ffit (xref, yref, xin, yin, Memd[wts], npts,
	        xshift, yshift, xscale, yscale, xrot, yrot)
	if (fitstat == ERR) {
	    xrot = INDEFD
	    yrot = INDEFD
	    xscale = INDEFD
	    yscale = INDEFD
	}

	call sfree (sp)
	return (fitstat)
end


# RG_FFIT -- Compute the x and y shift, th x and y scale, and the x and y
# rotation angle required to match one set of coordinates to another.

int procedure rg_ffit (xref, yref, xin, yin, wts, npts, xshift, yshift,
	xmag, ymag, xrot, yrot)

double	xref[ARB]	#I reference image x values
double	yref[ARB]	#I reference image y values
double	xin[ARB]	#I input image x values
double	yin[ARB]	#I input image y values
double	wts[ARB]	#I array of weights
int	npts		#I number of points
double	xshift, yshift	#O the x and y shifts
double	xmag, ymag	#O the x and y scale factors
double	xrot, yrot	#O the rotation angles

double	xmin, xmax, ymin, ymax
int	xier, yier, ier
pointer	sx1, sy1

begin
	# Compute the data limits.
	call alimd (xref, npts, xmin, xmax)
	call alimd (yref, npts, ymin, ymax)

	# Compute the x fit.
	call dgsinit (sx1, GS_POLYNOMIAL, 2, 2, GS_XNONE, xmin, xmax,
	    ymin, ymax)
	call dgsfit (sx1, xref, yref, xin, wts, npts, WTS_USER, xier)

	# Compute the y fit.
	call dgsinit (sy1, GS_POLYNOMIAL, 2, 2, GS_XNONE, xmin, xmax,
	    ymin, ymax)
	call dgsfit (sy1, xref, yref, yin, wts, npts, WTS_USER, yier)

	# Compute the geometric parameters.
	if (xier != OK || yier != OK) {
	    xshift = INDEFD
	    yshift = INDEFD
	    xmag = INDEFD
	    ymag = INDEFD
	    xrot = INDEFD
	    yrot = INDEFD
	    ier = ERR
	} else {
	    call geo_lcoeffd (sx1, sy1, xshift, yshift, xmag, ymag, xrot, yrot)
	    ier = OK
	}

	call dgsfree (sx1)
	call dgsfree (sy1)
	return (ier)
end


define	CDIN	icd[$1,$2]
define	CDOUT	ocd[$1,$2]

# RG_MWXYROT -- Scale and rotate the CD matrix by specifying the x and y scale
# factors in dimensionless units and the rotation angle in degrees.  Since only
# x and y scale factors and one rotation angle can be specified, this routine
# is useful only useful for a 2D transformation

procedure rg_mwxyrot(mw, xmag, ymag, xtheta, ytheta, icd, ocd, ndim, axbits)

pointer	mw			#I pointer to MWCS descriptor
double	xmag, ymag		#I the x and y scaling factors
double	xtheta			#I the x rotation angle, degrees
double	ytheta			#I the y rotation angle, degrees
double	icd[ndim,ARB]		#U the input CD matrix
double	ocd[ndim,ARB]		#U the output CD matrix
int	ndim			#I dimensions of the CD matrix
int	axbits			#I bitflags defining axes to be rotated

double	d_thetax, d_thetay, costx, sintx, costy, sinty
int	axis[IM_MAXDIM], naxes, ax1, ax2, axmap
int	mw_stati()
errchk	syserr

begin
	# Convert axis bitflags to axis list and get the two axes.
	call mw_gaxlist (mw, axbits, axis, naxes)
	axmap = mw_stati (mw, MW_USEAXMAP)
	call mw_seti (mw, MW_USEAXMAP, NO)
	ax1 = axis[1]
	ax2 = axis[2]

	# Rotate the CD matrix.
	d_thetax = DEGTORAD(xtheta)
	d_thetay = DEGTORAD(ytheta)
	costx = cos (d_thetax)
	sintx = sin (d_thetax)
	costy = cos (d_thetay)
	sinty = sin (d_thetay)
	call amovd (icd, ocd, ndim * ndim)

	CDOUT(ax1,ax1) = xmag * costx * CDIN(ax1,ax1) -
	     xmag * sintx * CDIN(ax2,ax1)
	CDOUT(ax2,ax1) = ymag * sinty * CDIN(ax1,ax1) +
	    ymag * costy * CDIN(ax2,ax1)
	CDOUT(ax1,ax2) = xmag * costx * CDIN(ax1,ax2) -
	    xmag * sintx * CDIN(ax2,ax2)  
	CDOUT(ax2,ax2) = ymag * sinty * CDIN(ax1,ax2) +
	    ymag * costy * CDIN(ax2,ax2) 

	call mw_seti (mw, MW_USEAXMAP, axmap)
end


# RG_RMSDIFF -- Compute the standard deviation of the difference between 2
# vectors

double procedure rg_rmsdiff (a, b, npts)

double	a[ARB]			#I the first input vector
double	b[ARB]			#I the second input vector
int	npts			#I the number of points

int	i
double	sum, rms

begin
	sum = 0.0d0
	do i = 1, npts
	    sum = sum + (a[i] - b[i]) ** 2

	if (npts <= 1)
	    rms = INDEFD
	else
	    rms = sqrt (sum / (npts - 1))

	return (rms)
end

