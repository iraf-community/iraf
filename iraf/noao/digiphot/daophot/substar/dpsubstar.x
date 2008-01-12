include <mach.h>
include	<imhdr.h>
include "../lib/daophotdef.h"
include "../lib/apseldef.h"

define	EXPAND	8

# DP_SUBSTAR -- Subtract the scaled and shifted PSF from the data

procedure dp_substar (dao, inim, exfd, ex_text, outim)

pointer	dao			# pointer to the DAOPHOT structure
pointer	inim			# pointer to the input image
int	exfd			# exclude file descriptor
bool	ex_text			# text or table exclude file
pointer	outim			# pointer to the output image

real	pradius, psfradsq, x, y, dxfrom_psf, dyfrom_psf, mag, tx, ty
real	rel_bright, maxgdata
pointer	apsel, psffit, buf, sp, index
int	i, id, line1, line2, nline_buf, x1, x2, y1, y2
int	lowy, highy, offset, nstars, ier
int	dp_restars()

begin
	# Get the daophot pointers.
	apsel = DP_APSEL (dao)
	psffit = DP_PSFFIT (dao)

	# Exit gracefully if there are no stars.
	#if (DP_APNUM(apsel) <= 0) {
	    #call printf ("The number of stars in the photometry list is %d\n")
		#call pargi (DP_APNUM(apsel))
	    #return
	#}

	# Check for stars to be excluded.
	if (exfd != NULL) {
	    if (dp_restars (dao, inim, exfd, ex_text) <= 0)
		;
	}

	# Compute the size of subraster to read from the PSF image.
	if (DP_PSFSIZE(dao) == 0)
	    pradius = DP_PSFRAD(dao)
	else
	    pradius = (real (DP_PSFSIZE(psffit) - 1) / 2.0 - 1.0) / 2.0
	psfradsq = pradius * pradius

	# Set the maximum good bad limit.
	if (IS_INDEFR (DP_MAXGDATA(dao)))
	    maxgdata = MAX_REAL
	else
	    maxgdata = DP_MAXGDATA(dao)

	# Get some working memory.
	call smark (sp)
	call salloc (index, DP_APNUM (apsel), TY_INT)
	 
	# Sort the photometry on increasing Y.
	if (DP_APNUM(apsel) > 0)
	    call quick (Memr[DP_APYCEN(apsel)], DP_APNUM(apsel), Memi[index],
	        ier)

	# Initialize the boundary of the buffer.
	buf = NULL
	line1 = 0
	line2 = 0
	nline_buf = EXPAND * pradius

	nstars = 0
	do i = 1, DP_APNUM (apsel) {

	    # Get the data for the next star.
	    offset = Memi[index+i-1] - 1
	    x = Memr[DP_APXCEN(apsel)+offset]
	    y = Memr[DP_APYCEN(apsel)+i-1]
	    id = Memi[DP_APID(apsel)+offset]
	    mag = Memr[DP_APMAG (apsel)+offset]
	    call dp_wpsf (dao, inim, x, y, dxfrom_psf, dyfrom_psf, 1)
	    dxfrom_psf = (dxfrom_psf - 1.0) / DP_PSFX(psffit) - 1.0
	    dyfrom_psf = (dyfrom_psf - 1.0) / DP_PSFY(psffit) - 1.0

	    # Reject star is the magnitude is INDEF.
	    if (IS_INDEFR(x) || IS_INDEFR(y) || IS_INDEFR(mag)) {
	        if (DP_VERBOSE(dao) == YES) {
		    if (IS_INDEFR(x) || IS_INDEFR(y)) {
			tx = x
			ty = y
		    } else
	                call dp_wout (dao, inim, x, y, tx, ty, 1)
	            call printf (
		    "REJECTING   - Star:%5d X =%8.2f Y =%8.2f Mag =%8.2f\n")
	   	        call pargi (id)
		        call pargr (tx)
		        call pargr (ty)
		        call pargr (mag)
 	        }
		next
	    }
	    
	    # Print out the verbose message.
	    if (DP_VERBOSE(dao) == YES) {
	        call dp_wout (dao, inim, x, y, tx, ty, 1)
	        call printf (
		    "SUBTRACTING - Star:%5d X =%8.2f Y =%8.2f Mag =%8.2f\n")
	   	    call pargi (id)
		    call pargr (tx)
		    call pargr (ty)
		    call pargr (mag)
 	    }

	    # Determine the range of lines required.
	    lowy = max (1, int (y - pradius) + 1)
	    highy = min (IM_LEN (inim, 2), int (y + pradius))
	    if (highy > line2) {
		line1 = max (1, lowy)
		line2 = min (line1 + nline_buf, IM_LEN (inim, 2))
		call dp_gimbufr (inim, outim, line1, line2, buf, false)
	    }

	    # Change coordinates to reference frame of buffer.
	    y = y - line1 + 1.0
	    y1 = max (1, int (y - pradius) + 1)
	    y2 = min (line2 - line1 + 1, int (y + pradius))
	    x1 = max (1, int (x - pradius) + 1)
	    x2 = min (IM_LEN (inim, 1), int (x + pradius))

	    # Computee the relative brightness.
	    rel_bright = DAO_RELBRIGHT (psffit, mag)

	    # Subtract this star.
	    call dp_sstar (dao, Memr[buf], int (IM_LEN(inim,1)), nline_buf,
	        x1, x2, y1, y2, x, y, psfradsq, rel_bright, dxfrom_psf,
		dyfrom_psf, maxgdata)

	    nstars = nstars + 1
	}

	# Flush the remaining lines in the image buffer.
	call dp_gimbufr (inim, outim, y1, y2, buf, true)

	# Summarize data on the number of stars subtracted.
	if (DP_VERBOSE(dao) == YES) {
	    call printf (
	        "\nA total of %d stars were subtracted out of a possible %d\n")
		call pargi (nstars)
		call pargi (DP_APNUM(apsel))
	}

	# Free memory.
	call sfree (sp)
end


# DP_SSTAR -- Subtract the star from the image.

procedure dp_sstar (dao, data, nx, ny, x1, x2, y1, y2, xstar, ystar, psfradsq,
	rel_bright, dxfrom_psf, dyfrom_psf, maxgdata)

pointer	dao				# pointer to the daophot structure
real	data[nx,ny]			# sata buffer
int	nx, ny				# size of buffer
int	x1, x2, y1, y2			# area of interest
real	xstar, ystar			# position of star to subtract
real	psfradsq			# PSF radius ** 2
real	rel_bright			# relative brightness of star
real	dxfrom_psf, dyfrom_psf		# not currently used
real	maxgdata			# maximum good data

int	ix, iy
pointer	psffit
real	dx, dy, dxsq, dysq, radsq, dvdx, dvdy
real	dp_usepsf()

begin
	psffit = DP_PSFFIT(dao)
	do iy = y1, y2 {
            dy = real (iy) - ystar
	    dysq = dy * dy
	    do ix = x1, x2 {
		if (data[ix,iy] > maxgdata)
		    next
	        dx = real (ix) - xstar
		dxsq = dx * dx
		radsq = dxsq + dysq
		if (radsq >= psfradsq) {
		    if (dx > 0.0)
			break
		    next
		}
		data[ix,iy] = data[ix,iy] - rel_bright *
		    dp_usepsf (DP_PSFUNCTION(psffit), dx, dy,
		    DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		    Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		    DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit),
		    dxfrom_psf, dyfrom_psf, dvdx, dvdy)
	    }
	}
end
