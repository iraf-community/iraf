include	<imhdr.h>
include <tbset.h>
include "../lib/apsel.h"
include "../lib/daophotdef.h"

define	NCOLUMN 9
define	DELTA_MAG	5.0

# DP_PEAKPHOT -- Fit the PSF to a single star.

procedure dp_peakphot (dao, im, tp, tpout, ap_text)

pointer	dao			# pointer to the daophot structure
pointer	im			# pointer to the input image
pointer	tp			# photometry file descriptor
pointer	tpout			# pointer to ouput photometry file
bool	ap_text			# which style of photometry

int	id, nrow, instar, lowx, lowy, nxpix, nypix, niter, star
pointer	psffit, key, sp, subim, colpoint, indices, fields
real	radius, rel_bright, xold, yold, x, y, dx, dy, mag, sky, errmag
real	chi, sharp

int	tbpsta(), dp_rrphot()
pointer	dp_gsubrast()

begin
	# Get the other daophot pointers.
	psffit = DP_PSFFIT (dao)

	call smark (sp)
	call salloc (indices, NAPPAR, TY_INT)
	call salloc (fields, SZ_LINE, TY_CHAR)
	call salloc (colpoint, NCOLUMN, TY_INT)

	# Initialze the output table.
	if (DP_TEXT(dao) == YES)
	    call dp_xnewpeak (dao, tpout)
	else
	    call dp_tnewpeak (dao, tpout, Memi[colpoint])

	# Determine the size of subraster to be read.
	radius = DP_PSFRAD (dao) + DP_FITRAD(dao) + 1

	# Intialize the input table.
	if (ap_text) {
	    call pt_kyinit (key)
	    Memi[indices] = DP_PAPID
	    Memi[indices+1] = DP_PAPXCEN
	    Memi[indices+2] = DP_PAPYCEN
	    Memi[indices+3] = DP_PAPMAG1
	    Memi[indices+4] = DP_PAPSKY
	    call dp_gappsf (Memi[indices], Memc[fields], NAPRESULT)
	    nrow = 0
	} else {
	    call dp_tpkinit (tp, Memi[indices])
	    nrow = tbpsta (tp, TBL_NROWS)
	}

	# Initialize the photometry file reading code.
	instar = 0

	# Initialize the fitting code.
	call dp_ipkfit ()

	star = 0
	repeat {

	    # Read in the photometry for a single star.
	    if (dp_rrphot (tp, key, Memc[fields], Memi[indices], id, x,
	        y, sky, mag, instar, nrow) == EOF)
		break

	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
		    "Star: %5d X: %8.2f Y: %8.2f Mag: %8.2f Sky: %8.2f\n")
		    call pargi (id)
		    call pargr (x)
		    call pargr (y)
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
	    subim = dp_gsubrast (im, x, y, radius, lowx, lowy, nxpix, nypix)
	    if (subim == NULL) {
		if (DP_VERBOSE(dao) == YES) {
		    call printf (
		        "\tWarning: Cannot read in image data for star %d\n")
		        call pargi (id)
		}
		next
	    }

	    # Save the old x values.
	    xold = x
	    yold = y

	    # Compute the relative centers and the relative brightness and
	    # fit the star.
	    if (IS_INDEFR(sky)) {
		niter = 0

	    } else {

	        x = x - lowx + 1.0
	        y = y - lowy + 1.0
	        dx = xold - DP_XPSF(psffit)
	        dy = yold - DP_YPSF(psffit)
	        if (IS_INDEFR(mag))
	            mag = DP_PSFMAG (psffit) + DELTA_MAG
	        rel_bright = DAO_RELBRIGHT (psffit, mag)

	        call dp_pkfit (dao, Memr[subim], nxpix, nypix, DP_VARPSF(dao),
		    x, y, dx, dy, rel_bright, sky, errmag, chi, sharp, niter)
	    }

	    if (niter <= 0) {

		# Set mags and fitting parameters to INDEF.
		x = xold
		y = yold
		mag = INDEFR
		errmag = INDEFR
		niter = 0
		chi = INDEFR
		sharp = INDEFR

		if (DP_VERBOSE(dao) == YES) {
		    if (IS_INDEFR(sky)) {
		        call printf (
		        "\tWarning: The sky value for star %d is undefined\n")
		            call pargi (id)
		    } else {
		        call printf (
		            "\tWarning: Error computing fit for star %d\n")
		            call pargi (id)
		    }
		}

	    } else {

	        # Compute the results.
	        x = x + lowx - 1.0
	        y = y + lowy - 1.0
	        mag = DP_PSFMAG (psffit) - 2.5 * log10 (rel_bright)
	        errmag = 1.086 * errmag / rel_bright
		if (errmag >= 2.0)
		    errmag = INDEFR

	    	if (DP_VERBOSE (dao) == YES) {
             	        call printf (
		  "\tFIT:  Star: %5d X: %8.2f Y: %8.2f Mag: %8.2f Sky =%8.2f\n")
	    	            call pargi (id)
		    	    call pargr (x)
		    	    call pargr (y)
		   	    call pargr (mag)
		    	    call pargr (sky)
		}
	    }

	    # Now write the results to the table.
	    star = star + 1
	    if (DP_TEXT(dao) == YES)
		call dp_xpkwrite (tpout, id, x, y, mag, errmag, sky, niter, chi,
		    sharp)
	    else
		call dp_tpkwrite (tpout, Memi[colpoint], id, x, y, mag, errmag,
		    sky, niter, chi, sharp, star)
	}

	# Free the peak fitting code.
	call dp_fpkfit()

	if (ap_text)
	    call pt_kyfree (key)

	call sfree (sp)
end
