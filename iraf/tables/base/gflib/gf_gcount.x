include <imio.h>
include	"gf.h"

#* HISTORY *
#* B.Simon	30-Sep-98	Rewriten to support all image types
#* B.Simon	12-Jul-00	Call to gf_groups removed

# GF_GCOUNT -- Count the number of groups in a file
#
# The number of "groups" is the value of gcount in a geis file, the
# number of extensions in a fits file with extensions, and one in all 
# other iamge formats, including simple fits files. All fits extensions 
# must be images or this procedure will return erroneous results.
 
int procedure gf_gcount (im)

pointer	im		# i: image descriptor
#--
int	hi, lo, mid, imtype

int	imaccf(), imgeti(), gf_imtype(), gf_hasgroup()

begin
	# If this is a new image, we cannot count the groups
	# Rely on the values of GCOUNT or NEXTEND instead

	imtype = gf_imtype (im)
	if (IM_ACMODE(im) == NEW_COPY || IM_ACMODE(im) == NEW_FILE) {
	    lo = 1
	    switch (imtype) {
	    case GEIS_FMT:
		if (imaccf (im, "GCOUNT") == YES)
		    lo = imgeti (im, "GCOUNT")
	    case FITS_FMT:
		if (imaccf (im, "NEXTEND") == YES)
		    lo = imgeti (im, "NEXTEND")
		else if (imaccf (im, "GCOUNT") == YES)
		    lo = imgeti (im, "GCOUNT")
	    }

	    return (lo)
	}

	# Check to see if image is in group format or has extensions

	if (imtype != GEIS_FMT && imtype != FITS_FMT)
	    return (1)

	# Otherwise determine the number of groups by brute force 
	# search. The search strategy is to form a bracket such 
	# that group LO is in the image and group HI is not.
	# Then narrow the bracket until HI is just greater
	# than LO. Initial value of HI indicates that a bracket 
	# has not yet been found

	lo = 1
	hi = 0

	# Use NEXTEND as an initial guess if it is in the header

	if (gf_imtype (im) == FITS_FMT && imaccf (im, "NEXTEND") == YES) {
	    mid = imgeti (im, "NEXTEND")

	    if (mid > 0) {
		if (gf_hasgroup (im, mid) == NO) {
		    hi = mid

		} else {
		    lo = mid
		    mid = mid + 1

		    if (gf_hasgroup (im, mid) == NO) {
			hi = mid
		    } else {
			lo = mid
		    }
		}
	    }
	}

	# Search for a group not in the image to form a bracket

	while (hi < lo) {
	    mid = 2 * lo

	    if (gf_hasgroup (im, mid) == NO) {
		hi = mid
	    } else {
		lo = mid
	    }
	}

	# Use binary search to narrow bracket to adjacent groups

	while (hi - lo > 1) {
	    mid = (hi + lo) / 2

	    if (gf_hasgroup (im, mid) == NO) {
		hi = mid
	    } else {
		lo = mid
	    }
	}

	# Return lower end of bracket

	return (lo)
end
