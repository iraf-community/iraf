include <imhdr.h>
include <mwset.h>
include "imtab.h"

# This file contains three routines:  itb_wcs_init and the single and
# double precision routines itb_ctranr & itb_ctrand.
#
# Phil Hodge, 30-Sep-1993  Subroutines created.

# itb_wcs_init -- open wcs, etc
# This routine gets the wcs, turns axis mapping off, and initializes
# the transformation for physical or world coordinates.  The dimension
# of the original image and the mapping from logical to physical axis 
# numbers are returned for use by itb_ctrand and itb_ctranr.

procedure itb_wcs_init (im, wcs_type, mw, ct, ax, wcsdim)

pointer im			# i: imhdr pointer for image
int	wcs_type		# i: wcs type
pointer mw, ct			# o: mwcs pointers
int	ax[IM_MAXDIM]		# o: ax[i] is physical axis for logical axis i
int	wcsdim			# o: dimension of physical image coord system
#--
int	axno[IM_MAXDIM]		# axno[j] is logical axis for physical axis j
int	axval[IM_MAXDIM]	# axval[j] is value if axno[j] is zero
int	ndim			# number of "logical" axes
int	i, j
pointer	mw_openim(), mw_sctran()
int	mw_stati()

begin
	if (wcs_type == IMTAB_NO_WCS || wcs_type == IMTAB_LOGICAL) {
	    mw = NULL
	    ct = NULL
	    wcsdim = IM_NDIM(im)
	    return
	}

	# Get the wcs.
	mw = mw_openim (im)

	# Set up the transformation.
	call mw_seti (mw, MW_USEAXMAP, NO)
	if (wcs_type == IMTAB_PHYSICAL)
	    ct = mw_sctran (mw, "logical", "physical", 0)
	else if (wcs_type == IMTAB_WORLD)
	    ct = mw_sctran (mw, "logical", "world", 0)
	wcsdim = mw_stati (mw, MW_NPHYSDIM)
	ndim = IM_NDIM(im)

	# Get the logical axis number corresponding to each physical axis.
	call mw_gaxmap (mw, axno, axval, wcsdim)

	# Invert axno:  get the physical axis number for each logical axis.
	do i = 1, ndim				# initialize
	    ax[i] = 0
	do j = 1, wcsdim {
	    do i = 1, ndim {
		if (axno[j] == i) {
		    ax[i] = j
		    break
		}
	    }
	}

	# Check to be sure each axis was found.
	do i = 1, ndim {
	    if (ax[i] < 1)
		call error (1, "itb_mwcs_init:  an axis was not found")
	}
end

# itb_ctran -- translate coordinates with axis mapping = NO
# This routine translates "logical" coordinates to "physical" or "world".
# Axis mapping must have been turned off, and the mapping from logical
# to physical axes is given by the array AX:  if I is a logical axis
# number, AX[I] is the corresponding physical axis number.  Each element
# of the array IN must have been initialized to one by the calling routine.
# Separate single and double precision versions are included.

procedure itb_ctrand (im, ct, ax, in, incoords, outcoords, wcsdim)

pointer im			# i: imhdr pointer
pointer ct			# i: coordinate transformation pointer
int	ax[wcsdim]		# i: "logical" to "physical" mapping
double	in[IM_MAXDIM]		# io: copy of incoords but includes axis mapping
double	incoords[wcsdim]	# i: input "logical" coordinates
double	outcoords[wcsdim]	# o: output coordinates
int	wcsdim			# i: length of incoords & outcoords arrays
#--
int	i

begin
	if (ct == NULL) {
	    call amovd (incoords, outcoords, wcsdim)
	    return
	}

	# Take account of axis mapping; i is the logical axis number.
	do i = 1, IM_NDIM(im)
	    in[ax[i]] = incoords[i]

	call mw_ctrand (ct, in, outcoords, wcsdim)
end

procedure itb_ctranr (im, ct, ax, in, incoords, outcoords, wcsdim)

pointer im			# i: imhdr pointer
pointer ct			# i: coordinate transformation pointer
int	ax[wcsdim]		# i: "logical" to "physical" mapping
real	in[IM_MAXDIM]		# io: copy of incoords but includes axis mapping
real	incoords[wcsdim]	# i: input "logical" coordinates
real	outcoords[wcsdim]	# o: output coordinates
int	wcsdim			# i: length of incoords & outcoords arrays
#--
int	i

begin
	if (ct == NULL) {
	    call amovr (incoords, outcoords, wcsdim)
	    return
	}

	# Take account of axis mapping; i is the logical axis number.
	do i = 1, IM_NDIM(im)
	    in[ax[i]] = incoords[i]

	call mw_ctranr (ct, in, outcoords, wcsdim)
end
