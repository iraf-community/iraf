include	<smw.h>
include	"ecidentify.h"

define	NBIN	10	# Bin parameter for mode determination

# EC_SHIFT -- Determine a shift by correlating feature user positions
# with peaks in the image data.

double procedure ec_shift (ec)

pointer	ec			# EC pointer

int	i, j, k, ap, order, nx, ndiff, find_peaks()
real	d, dmin
double	pix, ec_center(), ec_fitpt()
pointer	x, y, diff
errchk	malloc, realloc, find_peaks

begin
	ndiff = 0
	call malloc (x, EC_NCOLS(ec), TY_REAL)
	call malloc (y, EC_NCOLS(ec), TY_DOUBLE)
	do k = 1, EC_NLINES(ec) {
	    call ec_gline (ec, k)
	    ap = APS(ec,k)
	    order = ORDERS(ec,k)

	    # Find the peaks in the image data.
	    i = max (5, EC_MAXFEATURES(ec) / EC_NLINES(ec))
	    nx = find_peaks (IMDATA(ec,1), Memr[x], EC_NPTS(ec), 0.,
	        int (EC_MINSEP(ec)), 0, i, 0., false)

	    # Center the peaks and convert to user coordinates.
	    j = 0
	    do i = 1, nx {
		pix = Memr[x+i-1]
	        pix = ec_center (ec, pix, EC_FWIDTH(ec), EC_FTYPE(ec))
	        if (!IS_INDEFD (pix)) {
	            Memd[y+j] = ec_fitpt (ec, ap, pix)
		    j = j + 1
	        }
	    }
	    nx = j

	    # Compute differences with feature list.
	    do i = 1, EC_NFEATURES(ec) {
		if (APN(ec,i) != ap)
		    next
		if (ndiff == 0)
		    call malloc (diff, nx, TY_REAL)
		else 
		    call realloc (diff, ndiff+nx, TY_REAL)
		do j = 1, nx {
		    Memr[diff+ndiff] = (Memd[y+j-1] - FIT(ec,i)) * order
		    ndiff = ndiff + 1
		}
	    }
	}
	call mfree (x, TY_REAL)
	call mfree (y, TY_DOUBLE)

	# Sort the differences and find the mode.
	call asrtr (Memr[diff], Memr[diff], ndiff)

	dmin = Memr[diff+ndiff-1] - Memr[diff]
	do i = 0, ndiff-NBIN-1 {
	    j = i + NBIN
	    d = Memr[diff+j] - Memr[diff+i]
	    if (d < dmin) {
		dmin = d
		pix = Memr[diff+i] + d / 2.
	    }
	}
	call mfree (diff, TY_REAL)

	return (pix)
end
