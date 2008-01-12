include "template.h"

# TP_COUNT -- Count the number of groups in an image
#
# B.Simon	02-Oct-98	Original
# B.Simon	26-Apr-99	Check value of NEXTEND before using

int procedure tp_count (root)

char	root[ARB]	# i: image name minus any sections
#--
int	imtype, count, lo, hi, mid
pointer	sp, image, im

int	imgeti(), imaccf(), tp_imtype(), tp_hasgroup()
pointer	immap()

begin
	# If the image is not a geis or fits file it can only have one group

	imtype = tp_imtype (root) 
	if (imtype != TP_GEIS && imtype != TP_FITS)
	    return (1)

	# Open the image to read the number of groups or extensions
	# as recorded in the appropriate header keyword

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	call strcpy (root, Memc[image], SZ_FNAME)
	call strcat ("[0]", Memc[image], SZ_FNAME)

	iferr (im = immap (Memc[image], READ_ONLY, NULL)) {
	    # If image can't be opened, report an error condition

	    count = ERR

	} else if (imtype == TP_GEIS) {
	    # Number of groups is trustworthy, report it to user

	    count = imgeti (im, "GCOUNT")
	    call imunmap (im)

	} else {
	    # Number of extensions is not, it must be checked

	    lo = 1
	    hi = 0

	    # Check number of extensions

	    if (imaccf (im, "NEXTEND") == YES) {
		mid = imgeti (im, "NEXTEND")

		if (mid > 0) {
		    if (tp_hasgroup (root, mid) == NO) {
			hi = mid

		    } else {
			lo = mid
			mid = mid + 1

			if (tp_hasgroup (root, mid) == NO) {
			    hi = mid
			} else {
			    lo = mid
			}
		    }
		}
	    }

	    # Find bracket for number of extensions

	    while (hi < lo) {
		mid = 2 * lo

		if (tp_hasgroup (root, mid) == NO) {
		    hi = mid
		} else {
		    lo = mid
		}
	    }

	    # Use binary search to find actual number of extensions

	    while (hi - lo > 1) {
		mid = (hi + lo) / 2

		if (tp_hasgroup (root, mid) == NO) {
		    hi = mid
		} else {
		    lo = mid
		}
	    }

	    count = lo
	    call imunmap (im)
	}

	call sfree (sp)
	return (count)
end

# TP_HASGROUP -- Determine if group is preent in image

int procedure tp_hasgroup (root, index)

char	root[ARB]	# i: image name
int	index		# i: index of group to check
#--
int	has
pointer	sp, image, im

pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	call sprintf (Memc[image], SZ_FNAME, "%s[%d]")
	call pargstr (root)
	call pargi (index)

	iferr (im = immap (Memc[image], READ_ONLY, NULL)) {
	    has = NO
	} else {
	    call imunmap (im)
	    has = YES
	}

	call sfree (sp)
	return (has)
end
