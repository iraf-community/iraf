include	<imhdr.h>
include <mach.h>
include <fset.h>
include <tbset.h>
include	"rfits.h"

# RGI_READ_TFITS --  Read FITS table in row order as character strings.
#
# This routine is called only when the user has asked to convert
# a FITS file with an extra dimension and a table attached to it
# to a multigroup file.

procedure rgi_read_tfits (fits_fd, im, gcount, ext)

int	fits_fd		# FITS file descriptor
pointer im		# Image descriptor
int	gcount		# number of groups
pointer	ext		# Extension data structure

int	i, rowlen, blksize, nch, maxout
int	gn, pcount, rec_count, npix_record
pointer	sp, input, output

int	rft_init_read_pixels(), rft_read_pixels()
int	fstati(), gi_gstfval()

errchk	rft_init_read_pixels, rft_read_pixels, rgi_fix_nulls

include	"rfits.com"

begin
	# Allocate buffers to hold gpb values

	rowlen = EXT_ROWLEN(ext)
	maxout = rowlen + SZ_LINE

	call smark (sp)
	call salloc (input, rowlen, TY_CHAR)
	call salloc (output, maxout, TY_CHAR)

	# Put EOS at the end of input buffer
	# Rft_read_pixels does not put one at rowlen

	Memc[input+rowlen] = EOS

	# Initialize variables used to read buffer

	npix_record = len_record * FITS_BYTE / EXT_BITPIX(ext)
	i = rft_init_read_pixels (npix_record, EXT_BITPIX(ext), LSBF, TY_CHAR)

	blksize = fstati (fits_fd, F_SZBBLK)
	if (mod (blksize, 2880) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1
	
	rec_count = 0
	pcount = gi_gstfval (im, "PCOUNT")

	do gn = 1, gcount {

	    # Read in table row
	    nch = rft_read_pixels (fits_fd, Memc[input], rowlen,
				   rec_count, blksize)

	    if (nch != rowlen)
		call printf ("Error reading FITS data\n")

	    # Convert null values in row

	    call rgi_fix_nulls (ext, Memc[input], pcount, 
				    Memc[output], maxout)

	    # Write table row to group parameter block

	    call gi_crgpb (im, gn, Memc[output])
	}

	call sfree (sp)

end

# RGI_FIX_NULLS -- Replace null values in the extension buffer

procedure rgi_fix_nulls (ext, input, ncol, output, maxout)

pointer	ext		# i: extension data strc
char	input[ARB]	# i: input buffer
int	ncol		# i: number of columns in extension table
char	output[ARB]	# o: output buffer with null values replaced
int	maxout		# i: declared length of output buffer
#--
int	icol, inoff, outoff, len
pointer	pc, pl, pn, pd, nullval

int	strlen(), gstrcpy(), cmp_null()

begin
	# Get info from extension data structure

	pc = EXT_PBCOL (ext)
	pl = EXT_PCW(ext)
	pn = EXT_PNULL(ext)
	pd = EXT_PDTYPE(ext)

	# Look at each value in the input buffer and replace it with the
	# appropriate value if the field is null. Note that both the
	# input and output arrays are character buffers, but the fields in 
	# input buffer are not null terminated while those in the output 
	# buffer are. 

	outoff = 1
	do icol = 0, ncol-1 {
	    # Compare each buffer value to the value representing a null

	    nullval = pn + icol * SZ_COLUNITS
	    inoff = Memi[pc+icol]
	    len = Memi[pl+icol]

	    if (strlen (Memc[nullval]) == 0) {
		# No null value, copy input to output
		outoff = gstrcpy (input[inoff], output[outoff], len) +
			 outoff + 1

	    } else {
		if (cmp_null(input[inoff], Memc[nullval], len) == 0) {
		    # Buffer value is not null, copy input to output
		    outoff = gstrcpy (input[inoff], output[outoff], len) +
			     outoff + 1

		} else if (Memi[pd+icol] == TY_BOOL){
		    # Null boolean, replace with false
		    outoff = gstrcpy ("F", output[outoff], maxout-outoff) +
			     outoff + 1

		} else if (Memi[pd+icol] == TY_CHAR) {
		    # Null character string, replace with zero length string
		    output[outoff] = EOS
		    outoff = outoff + 1

		} else {
		    # Null number, replace with INDEF
		    outoff = gstrcpy ("INDEF", output[outoff], maxout-outoff) +
			     outoff + 1
		}
	    }
	}

	if (outoff >= maxout)
	    call error (1, "RGI_FIX_NULLS: GPB buffer overflow")

end
