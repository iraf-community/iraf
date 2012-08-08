include	"identify.h"

define	NBIN	10	# Bin parameter for mode determination

# ID_SHIFT1 -- Determine a shift by correlating feature user positions
# with peaks in the image data.

double procedure id_shift1 (id)

pointer	id			# ID pointer

int	i, j, npeaks, ndiff, id_peaks()
real	d, dmin
double	pix, id_center(), id_fitpt()
pointer	x, y, diff
errchk	malloc, id_peaks

begin
	# Find the peaks in the image data and center.
	call malloc (x, ID_NPTS(id), TY_REAL)
	npeaks = id_peaks (id, IMDATA(id,1), Memr[x], ID_NPTS(id), 0.,
	    int (ID_MINSEP(id)), 0, ID_MAXFEATURES(id), 0., false)

	# Center the peaks and convert to user coordinates.
	call malloc (y, npeaks, TY_DOUBLE)
	j = 0
	do i = 1, npeaks {
	    pix = id_center (id, double(Memr[x+i-1]), ID_FWIDTH(id),
		ID_FTYPE(id))
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


# ID_SHIFT -- Determine a shift using the AID_SHIFT algorithm.  This
# differs from AID_SHIFT in that the input ID pointer is unchanged
# (same dispersion function and features) but a shift is computed and
# returned.

double procedure id_shift (id, crsearch cdsearch)

pointer	id		#I ID pointer
double	crsearch	#I Search range
double	cdsearch	#I Search range

int	marker
double	shift, asumd()
pointer	new, id_getid()
errchk	aid_shift

begin
	call stmark (ID_STP(id), marker)
	call id_saveid (id, "backup")

	# Find the shift.
	shift = INDEFD
	iferr {
	    call aid_shift (id, crsearch, cdsearch)
	    call malloc (new, ID_NPTS(id), TY_DOUBLE)
	    call amovd (FITDATA(id,1), Memd[new], ID_NPTS(id))
	    if (id_getid (id, "backup") == NULL)
		call error (1, "Error getting saved record")
	    call asubd (FITDATA(id,1), Memd[new], Memd[new], ID_NPTS(id))
	    shift = asumd (Memd[new], ID_NPTS(id)) / ID_NPTS(id)
	    call mfree (new, TY_DOUBLE)
	} then {
	    if (id_getid (id, "backup") == NULL)
		call error (1, "Error getting saved record")
	}

	call stfree (ID_STP(id), marker)
	return (shift)
end
