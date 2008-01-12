include <imio.h>
include <imhdr.h>
include <mach.h>
include "gi.h"

define  SZ_KEYWORD   8
# GI_RESET -- Procedure to reset some of the STF descriptor values
# to be able to create an multigroup image with a gpb but not
# writing the latter until all the pixels are in first.
#
# BS Jun 1997: Removed calls to gi_gstfval and gi_pstfval

procedure gi_reset (im)

pointer im	# Image descriptor

pointer stf
int	totpix, i, ngroups, compress, blklen, pixoff
int	psize, szgroup, dim

int	open(), sizeof()

begin

	stf = IM_KDES(im)
	ngroups = STF_GCOUNT(stf)
	if (ngroups > 1) {
	    # The groups are stack in the last dimension.
	    dim = IM_NDIM(im)

	    # Diminish output dimensionality by one.
	    IM_NDIM(im) = dim - 1
	    IM_LEN(im,dim) = 1
	}
	STF_NAXIS(stf) = IM_NDIM(im)

	#Calculate size of each group in the pixel file, in chars
	totpix = IM_LEN(im,1)
	do i = 2, IM_NDIM(im)
	   totpix = totpix * IM_LEN(im,i)

	psize = STF_PSIZE(stf)
	szgroup = totpix * sizeof(IM_PIXTYPE(im)) + 
		  psize / (SZB_CHAR * NBITS_BYTE)

	STF_SZGROUP(stf) = szgroup

	#We tell stf not to create the default GPB.
	STF_NEWIMAGE(stf) = NO
	STF_GROUP(stf) = 1

	do i = 1, IM_NDIM(im)
	    STF_LENAXIS(stf,i) = IM_LEN(im,i)

        # Open pixel file 
        call falloc (IM_PIXFILE(im), szgroup*ngroups)
	STF_PFD(stf) = open (IM_PIXFILE(im), READ_WRITE, BINARY_FILE)

	IM_PFD(im) = STF_PFD(stf)
	compress = YES
	blklen = 1
	pixoff = 1
	call imioff (im, pixoff, compress, blklen)
	call imsetbuf (IM_PFD(im), im)


end
