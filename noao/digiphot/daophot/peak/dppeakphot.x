include	<imhdr.h>
include <tbset.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"
include "../lib/peakdef.h"


# DP_PEAKPHOT -- Fit the PSF to a single star.

procedure dp_peakphot (dao, im, tp, tpout, tprej, ap_text)

pointer	dao			# pointer to the daophot structure
pointer	im			# the input image descriptor
int	tp			# the input photometry file descriptor
int	tpout			# the ouput photometry file descriptor
int	tprej			# the rejections file descriptor
bool	ap_text			# which style of photometry

real	rel_bright, xold, yold, x, y, dx, dy, mag, sky, errmag
real	chi, sharp, radius, itx, ity, otx, oty
pointer	psffit, key, sp, subim, colpoint, indices, fields, perror
int	id, in_nrow, instar, lowx, lowy, nxpix, nypix, niter, out_nrow
int	rout_nrow, nterm, ier, plen

int	tbpsta(), dp_rrphot(), dp_pkfit(), dp_gpkerr()
pointer	dp_gsubrast()

begin
	# Get the daophot pointers.
	psffit = DP_PSFFIT (dao)

	# Store the original fitting radius.
	radius = DP_FITRAD(dao)

	# Check that the fitting radius is less than the psf radius.
	DP_FITRAD(dao) = min (DP_FITRAD(dao), DP_PSFRAD(dao))
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)

	# Allocate working space.
	call smark (sp)
	call salloc (indices, NAPPAR, TY_INT)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (colpoint, PK_NOUTCOL, TY_INT)
	call salloc (perror, SZ_FNAME, TY_CHAR)

	# Initialze the output table.
	if (DP_TEXT(dao) == YES) {
	    call dp_xnewpeak (dao, tpout)
	    if (tprej != NULL)
	        call dp_xnewpeak (dao, tprej)
	} else {
	    call dp_tnewpeak (dao, tpout, Memi[colpoint])
	    if (tprej != NULL)
	        call dp_tnewpeak (dao, tprej, Memi[colpoint])
	}

	# Intialize the input table.
	if (ap_text) {
	    call pt_kyinit (key)
	    Memi[indices] = DP_PAPID
	    Memi[indices+1] = DP_PAPXCEN
	    Memi[indices+2] = DP_PAPYCEN
	    Memi[indices+3] = DP_PAPMAG1
	    Memi[indices+4] = DP_PAPSKY
	    call dp_gappsf (Memi[indices], Memc[fields], NAPRESULT)
	    in_nrow = 0
	} else {
	    call dp_tpkinit (tp, Memi[indices])
	    in_nrow = tbpsta (tp, TBL_NROWS)
	}

	# Initialize the photometry file reading code.
	instar = 0

	# Initialize the fitting code.
	if (DP_RECENTER(dao) == YES)
	    nterm = 3
	else
	    nterm = 1
	if (DP_FITSKY(dao) == YES)
	    nterm = nterm + 1
	call dp_mempk (dao, nterm)

	out_nrow = 0
	rout_nrow = 0
	repeat {

	    # Read in the photometry for a single star.
	    if (dp_rrphot (tp, key, Memc[fields], Memi[indices], id, itx,
	        ity, sky, mag, instar, in_nrow) == EOF)
		break

	    # Convert to and from logical coordinates.
	    call dp_win (dao, im, itx, ity, x, y, 1)
	    call dp_wout (dao, im, x, y, otx, oty, 1)

	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
		    "Star: %5d X: %8.2f Y: %8.2f Mag: %8.2f Sky: %8.2f\n")
		    call pargi (id)
		    call pargr (otx)
		    call pargr (oty)
		    call pargr (mag)
		    call pargr (sky)
	    }

	    # Check that the center is defined.
	    if (IS_INDEFR(x) || IS_INDEFR(y)) {
		if (DP_VERBOSE(dao) == YES) {
		    call printf (
		        "\tWarning: X and/or Y for star %d are undefined\n")
		        call pargi (id)
		}
		next
	    }

    	    # Read in the subraster.
	    subim = dp_gsubrast (im, x, y, DP_FITRAD(dao), lowx, lowy,
	        nxpix, nypix)
	    if (subim == NULL) {
		if (DP_VERBOSE(dao) == YES) {
		    call printf (
		        "\tWarning: Cannot read in image data for star %d\n")
		        call pargi (id)
		}
		next
	    }

	    # Save the old x and y values for use with the variable psf
	    # option.
	    xold = x
	    yold = y
	    call dp_wpsf (dao, im, xold, yold, xold, yold, 1)

	    # Compute the relative centers and the relative brightness and
	    # fit the star.
	    if (IS_INDEFR(sky)) {

		ier = PKERR_INDEFSKY

	    } else {

	        x = x - lowx + 1.0
	        y = y - lowy + 1.0
	        dx = (xold - 1.0) / DP_PSFX(psffit) - 1.0
	        dy = (yold - 1.0) / DP_PSFY(psffit) - 1.0
	        if (IS_INDEFR(mag))
	            mag = DP_PSFMAG (psffit) + DELTA_MAG
	        rel_bright = DAO_RELBRIGHT (psffit, mag)
	        ier = dp_pkfit (dao, Memr[subim], nxpix, nypix, DP_FITRAD(dao),
		    x, y, dx, dy, rel_bright, sky, errmag, chi, sharp, niter)
	        x = x + lowx - 1.0
	        y = y + lowy - 1.0
	    }

	    call dp_wout (dao, im, x, y, otx, oty, 1)

	    if (ier != PKERR_OK) {

		# Set fitting parameters to INDEF.
		mag = INDEFR
		niter = 0
		errmag = INDEFR
		chi = INDEFR
		sharp = INDEFR

		if (DP_VERBOSE(dao) == YES) {
		    switch (ier) {
		    case PKERR_INDEFSKY:
		        call printf (
		        "\tWarning: The sky value for star %d is undefined\n")
		            call pargi (id)
		    case PKERR_NOPIX:
		        call printf (
		            "\tWarning: Too few pixels to fit star %d\n")
		            call pargi (id)
		    case PKERR_SINGULAR:
		        call printf (
		            "\tWarning: Singular matrix computed for star %d\n")
		            call pargi (id)
		    case PKERR_FAINT:
		        call printf ("\tWarning: Star %d too faint\n")
		            call pargi (id)
		    default:
		        call printf (
		           "\tWarning: Unknown error detected for star %d\n")
		            call pargi (id)
		    }
		}

	    } else {

	        # Compute the results.
	        mag = DP_PSFMAG (psffit) - 2.5 * log10 (rel_bright)
	        errmag = 1.085736 * errmag / rel_bright
		if (errmag >= 2.0)
		    errmag = INDEFR

	    	if (DP_VERBOSE (dao) == YES) {
             	        call printf (
		  "\tFIT:  Star: %5d X: %8.2f Y: %8.2f Mag: %8.2f Sky =%8.2f\n")
	    	            call pargi (id)
		    	    call pargr (otx)
		    	    call pargr (oty)
		   	    call pargr (mag)
		    	    call pargr (sky)
		}
	    }

	    # Get the error code.
	    plen = dp_gpkerr (ier, Memc[perror], SZ_FNAME)

	    # Now write the results to the output photometry or rejections
	    # file.
	    if (DP_TEXT(dao) == YES) {
		if ((tprej != NULL) && (ier != PKERR_OK))
		    call dp_xpkwrite (tprej, id, otx, oty, mag, errmag, sky,
		        niter, chi, sharp, ier, Memc[perror], plen)
		else
		    call dp_xpkwrite (tpout, id, otx, oty, mag, errmag, sky,
		        niter, chi, sharp, ier, Memc[perror], plen)
	    } else {
		if ((tprej != NULL) && (ier != PKERR_OK)) {
	            rout_nrow = rout_nrow + 1
		    call dp_tpkwrite (tprej, Memi[colpoint], id, otx, oty, mag,
		        errmag, sky, niter, chi, sharp, ier,
			Memc[perror], plen, rout_nrow)
		} else {
	            out_nrow = out_nrow + 1
		    call dp_tpkwrite (tpout, Memi[colpoint], id, otx, oty, mag,
		        errmag, sky, niter, chi, sharp, ier, Memc[perror],
			plen, out_nrow)
		}
	    }
	}

	# Free the text file descriptor.
	if (ap_text)
	    call pt_kyfree (key)

	# Restore the original fitting radius.
	DP_FITRAD(dao) = radius
	DP_SFITRAD(dao) = DP_FITRAD(dao) * DP_SCALE(dao)

	call sfree (sp)
end


# DP_GPKERR -- Set the PEAK task error code string.

int procedure dp_gpkerr (ier, perror, maxch)

int	ier		# the integer error code
char	perror		# the output error code string
int	maxch		# the maximum size of the error code string

int	plen
int	gstrcpy()

begin
	switch (ier) {
	case PKERR_OK:
	    plen = gstrcpy ("No_error", perror, maxch)
	case PKERR_INDEFSKY:
	    plen = gstrcpy ("Bad_sky", perror, maxch)
	case PKERR_NOPIX:
	    plen = gstrcpy ("Npix_too_few", perror, maxch)
	case PKERR_SINGULAR:
	    plen = gstrcpy ("Singular", perror, maxch)
	case PKERR_FAINT:
	    plen = gstrcpy ("Too_faint", perror, maxch)
	default:
	    plen = gstrcpy ("No_error", perror, maxch)
	}

	return (plen)
end
