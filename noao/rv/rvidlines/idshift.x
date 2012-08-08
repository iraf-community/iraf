include	"identify.h"

define	NBIN	10	# Bin parameter for mode determination

# ID_SHIFT -- Determine a shift by correlating feature user positions
# with peaks in the image data.

double procedure id_shift (id)

pointer	id			# ID pointer

int	i, j, npeaks, ndiff, find_peaks()
real	d, dmin
double	pix, id_center(), id_fitpt()
pointer	x, y, diff
errchk	malloc, find_peaks

begin
	# Find the peaks in the image data and center.
	call malloc (x, ID_NPTS(id), TY_REAL)
	npeaks = find_peaks (IMDATA(id,1), Memr[x], ID_NPTS(id), 0.,
	    int (ID_MINSEP(id)), 0, ID_MAXFEATURES(id), 0., false)

	# Center the peaks and convert to user coordinates.
	call malloc (y, npeaks, TY_DOUBLE)
	j = 0
	do i = 1, npeaks {
	    pix = id_center (id, double(Memr[x+i-1]), 1, ID_FWIDTH(id),
		ID_FTYPE(id), NO)
	    if (!IS_INDEFD (pix)) {
	        Memd[y+j] = id_fitpt (id, pix)
		j = j + 1
	    }
	}
	npeaks = j

	# Compute differences with feature list.
	ndiff = npeaks * ID_NFEATURES(id)
	call malloc (diff, ndiff, TY_REAL)
	ndiff = 0
	do i = 1, ID_NFEATURES(id) {
	    do j = 1, npeaks {
		Memr[diff+ndiff] = Memd[y+j-1] - FIT(id,i)
		ndiff = ndiff + 1
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
