include <mach.h>
include	<imhdr.h>
include "../lib/apsel.h"
include "../lib/daophot.h"
include "../lib/daophotdef.h"

define	EXPAND	4

# DP_SUBSTAR -- Subtract the scaled and shifted PSF from the data

procedure dp_substar (dao, psf_tp, inim, outim)

pointer	dao			# pointer to the DAOPHOT structure
int	psf_tp			# pointer to PSF image
pointer	inim			# pointer to the input image
pointer	outim			# pointer to the output image

int	i, id, line1, line2, nline_buf, x1, x2, y1, y2
int	lowy, highy, offset, nstars
pointer	apsel, psffit, buf, sp, index
real	x, y, xpsf, ypsf, xfrom_psf, yfrom_psf, mag
real	radius, psfradsq, rel_bright, mingdata, maxgdata
real	imgetr()

begin
	# Get the daophot pointers.
	apsel = DP_APSEL (dao)
	psffit = DP_PSFFIT (dao)

	# Exit gracefully if there are no stars.
	if (DP_APNUM(apsel) <= 0) {
	    call printf ("The number of stars in the photometry list is %d\n")
		call pargi (DP_APNUM(apsel))
	    return
	}

	# Get some memory
	call smark (sp)
	call salloc (index, DP_APNUM (apsel), TY_INT)
	 
	# Compute the size of subraster to read from the PSF image.
	radius = DP_PSFRAD(dao)
	psfradsq = radius * radius
	xpsf = imgetr (psf_tp, "XPSF")
	ypsf = imgetr (psf_tp, "YPSF")
	if (IS_INDEFR (DP_MINGDATA(dao)))
	    mingdata = -MAX_REAL
	else
	    mingdata = DP_MINGDATA(dao)
	if (IS_INDEFR (DP_MAXGDATA(dao)))
	    maxgdata = MAX_REAL
	else
	    maxgdata = DP_MAXGDATA(dao)

	# Sort the photometry on increasing Y. Index points to the
	# appropriate x and magnitudes.
	call quick (Memr[DP_APYCEN (apsel)], DP_APNUM (apsel), Memi[index])

	# Initialize the boundary of the buffer.
	buf = NULL
	line1 = 0
	line2 = 0
	nline_buf = EXPAND * radius

	nstars = 0
	do i = 1, DP_APNUM (apsel) {

	    offset = Memi[index+i-1] - 1
	    x = Memr[DP_APXCEN(apsel)+offset]
	    y = Memr[DP_APYCEN(apsel)+i-1]
	    id = Memi[DP_APID(apsel)+offset]
	    mag = Memr[DP_APMAG (apsel)+offset]
	    xfrom_psf = x - xpsf
	    yfrom_psf = y - ypsf

	    if (IS_INDEFR(x) || IS_INDEFR(y) || IS_INDEFR(mag)) {
	        if (DP_VERBOSE(dao) == YES) {
	            call printf (
		        "REJECTING  - Star:%5d X =%8.2f Y =%8.2f Mag =%8.2f\n")
	   	        call pargi (id)
		        call pargr (x)
		        call pargr (y)
		        call pargr (mag)
 	        }
		next
	    }
	    
	    if (DP_VERBOSE(dao) == YES) {
	        call printf (
		    "SUBTRACTING - Star:%5d X =%8.2f Y =%8.2f Mag =%8.2f\n")
	   	    call pargi (id)
		    call pargr (x)
		    call pargr (y)
		    call pargr (mag)
 	    }

	    # Determine boundaries of the subraster and make sure that
	    # this piece of data is available. Update the buffer if
	    # appropriate.

	    lowy = max (1, int (y - radius) - 1)
	    highy = min (int (y + radius) + 1, IM_LEN (inim, 2))
	    if (highy > line2) {
		line1 = max (1, lowy - 1)
		line2 = line1 + nline_buf
		line2 = min (line2, IM_LEN (inim, 2))
		call dp_mgbufl2r (inim, outim, line1, line2, buf, false)
	    }

	    # Change coordinates to reference frame of buffer.
	    y = y - line1 + 1.0
	    y1 = y - radius - 1
	    y2 = y + radius + 1
	    x1 = x - radius - 1
	    x2 = x + radius + 1

	    x1 = max (1, x1)
	    x2 = min (IM_LEN (inim, 1), x2)
	    y1 = max (1, y1)
	    y2 = min (line2 - line1 + 1, y2)

	    # Computee the relative brightness.
	    rel_bright = DAO_RELBRIGHT (psffit, mag)

	    # Subtract this star.
	    call dp_sstar (Memr[buf], int (IM_LEN(inim, 1)), nline_buf, x1, x2,
	        y1, y2, x, y, psfradsq, rel_bright, psffit, xfrom_psf,
		yfrom_psf, DP_VARPSF(dao), mingdata, maxgdata)

	    nstars = nstars + 1
	}

	# Flush the remaining lines in the image buffer.
	call dp_mgbufl2r (inim, outim, y1, y2, buf, true)

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

procedure dp_sstar (data, nx, ny, x1, x2, y1, y2, xstar, ystar, psfradsq,
	rel_bright, psffit, xfrom_psf, yfrom_psf, varpsf, mingdata, maxgdata)

real	data[nx,ny]			# sata buffer
int	nx, ny				# size of buffer
int	x1, x2, y1, y2			# area of interest
real	xstar, ystar			# position of star to subtract
real	psfradsq			# PSF radius ** 2
real	rel_bright			# relative brightness of star
pointer	psffit				# pointer to PSF structurer
real	xfrom_psf, yfrom_psf		# not currently used
int	varpsf				# variable psf
real	mingdata			# minimum good data (not used)
real	maxgdata			# maximum good data

int	ix, iy
real	dx, dy, dxsq, dysq, radsq, value, dvdx, dvdy
real	dp_evalpsf()

begin
	do iy = y1, y2 {
            dy = real (iy) - ystar
	    dysq = dy * dy
	    do ix = x1, x2 {
		if (data[ix,iy] > maxgdata)
		    next
	        dx = real (ix) - xstar
		dxsq = dx * dx
		radsq = dxsq + dysq
		if (radsq <= psfradsq) {
		    value = rel_bright * dp_evalpsf (dx, dy, psffit,
		        xfrom_psf, yfrom_psf, varpsf, dvdx, dvdy)
		    data[ix,iy] = data[ix,iy] - value
		}
	    }
	}
end
