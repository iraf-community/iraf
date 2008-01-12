include	<imhdr.h>
include <mach.h>
include <fset.h>
include <tbset.h>
include	"rfits.h"

# RGF_READ_TFITS --  Read FITS table in row order as character strings.
# This routine is called only when the user has asked to convert
# a FITS file with an extra dimension and a table attached to it
# to a multigroup file.

procedure rgi_read_tfits (fits_fd, im, gcount, ext)

int	fits_fd		# FITS file descriptor
pointer im		# Image descriptor
int	gcount		# number of groups
pointer	ext		# Extension data structure

int	i, rowlen, blksize, nch
int	gn, pcount
pointer	sp, bfp, pc, pw 

int	rft_init_read_pixels(), rft_read_pixels()
int	npix_record, fstati(), gi_gstfval()
int	rec_count

errchk	salloc, sfree, rft_init_read_pixels, rft_read_pixels
errchk	smark

include	"rfits.com"
include "tab.com"

begin

	rowlen = EXT_ROWLEN(ext)
	rec_count = 0

	pcount = gi_gstfval (im, "PCOUNT")

	call smark (sp)
	call salloc (bfp, rowlen, TY_CHAR)

	npix_record = len_record * FITS_BYTE / EXT_BITPIX(ext)
	i = rft_init_read_pixels (npix_record, EXT_BITPIX(ext), LSBF, TY_CHAR)
	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1

	# Put EOS at the end, rft_red_pixels does not put one at rowlen.
	Memc[bfp+rowlen] = EOS
	pc = EXT_PCOL(ext)
	pw = EXT_PCW(ext)
	do gn = 1, gcount {

	    # Read in table row
	    nch = rft_read_pixels (fits_fd, Memc[bfp], rowlen,
	        rec_count, blksize)
	    if (nch != rowlen)
		call printf ("Error reading FITS data\n")

	    # Write table row
	    call gi_crgpb (im, Memc[bfp], Memi[pc], Memi[pw], gn)
	}

	call sfree (sp)

end
