include "../lib/daophotdef.h"
include "../lib/psfdef.h"

# DP_PSUBRAST -- Procedure to fetch the PSF star data and check for bad
# pixel values.

pointer procedure dp_psubrast (dao, im, x1, x2, y1, y2, subsky)

pointer	dao		# pointer to the daophot structure
pointer	im		# pointer to the input image
int	x1, x2		# x limits of the extracted subraster
int	y1, y2		# y limits of the extracted subraster
int	subsky		# subtract the sky

int	box_size
pointer	psf, buf
int	dp_chksr()
pointer	dp_subrast()

begin
	# Initialize.
	psf = DP_PSF(dao)
	buf = NULL

	box_size = DP_SZLOOKUP(psf)
 	buf = dp_subrast (im, int (DP_CUR_PSFX(psf)), int (DP_CUR_PSFY(psf)),
	    box_size, x1, x2, y1, y2)
	if (buf == NULL)
	    return (NULL)

	# Check for bad pixels in subraster, compute the min and maximum.
	if (dp_chksr (dao, Memr[buf], box_size, box_size) == ERR) {
	    call mfree (buf, TY_REAL)
	    return (NULL)
	}
	
	# Subtract the sky from the data subraster.
	if (subsky == YES)
	    call asubkr (Memr[buf], DP_CUR_PSFSKY(psf), Memr[buf], box_size *
	        box_size)

	return (buf)
end
