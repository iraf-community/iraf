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

long	l_val0, l_val1
long	i, nch, rec_count
size_t	rowlen, maxout, blksize, npix_record
int	gn, pcount
pointer	sp, inputp, output

long	rft_init_read_pixels(), rft_read_pixels(), fstatl(), lmod()
int	gi_gstfval()

errchk	rft_init_read_pixels, rft_read_pixels, rgi_fix_nulls

include	"rfits.com"

begin
	# Allocate buffers to hold gpb values

	rowlen = EXT_ROWLEN(ext)
	maxout = rowlen + SZ_LINE

	call smark (sp)
	call salloc (inputp, rowlen, TY_CHAR)
	call salloc (output, maxout, TY_CHAR)

	# Put EOS at the end of input buffer
	# Rft_read_pixels does not put one at rowlen

	Memc[inputp+rowlen] = EOS

	# Initialize variables used to read buffer

	npix_record = len_record * FITS_BYTE / EXT_BITPIX(ext)
	i = rft_init_read_pixels (npix_record, EXT_BITPIX(ext), LSBF, TY_CHAR)

	blksize = fstatl (fits_fd, F_SZBBLK)
	l_val0 = blksize
	l_val1 = 2880
	if (lmod (l_val0, l_val1) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1
	
	rec_count = 0
	pcount = gi_gstfval (im, "PCOUNT")

	do gn = 1, gcount {

	    # Read in table row
	    nch = rft_read_pixels (fits_fd, Memc[inputp], rowlen,
				   rec_count, blksize)

	    if (nch != rowlen)
		call printf ("Error reading FITS data\n")

	    # Convert null values in row

	    call rgi_fix_nulls (ext, Memc[inputp], pcount, 
				Memc[output], maxout)

	    # Write table row to group parameter block

	    call gi_crgpb (im, gn, Memc[output])
	}

	call sfree (sp)

end

# RGI_FIX_NULLS -- Replace null values in the extension buffer

procedure rgi_fix_nulls (ext, inputb, ncol, output, maxout)

pointer	ext		# i: extension data strc
char	inputb[ARB]	# i: input buffer
int	ncol		# i: number of columns in extension table
char	output[ARB]	# o: output buffer with null values replaced
size_t	maxout		# i: declared length of output buffer
#--
long	outoff, len, inoff
int	icol
pointer	pc, pl, pn, pd, nullval

int	strlen(), cmp_null()
long	rgi_gstrcpy()

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
	    inoff = Meml[pc+icol]
	    len = Meml[pl+icol]

	    if (strlen (Memc[nullval]) == 0) {
		# No null value, copy input to output
		outoff = rgi_gstrcpy (inputb[inoff], output[outoff], len) +
			 outoff + 1

	    } else {
		if (cmp_null(inputb[inoff], Memc[nullval], len) == 0) {
		    # Buffer value is not null, copy input to output
		    outoff = rgi_gstrcpy (inputb[inoff], output[outoff], len) +
			     outoff + 1

		} else if (Memi[pd+icol] == TY_BOOL){
		    # Null boolean, replace with false
		    outoff = rgi_gstrcpy ("F", output[outoff], maxout-outoff) +
			     outoff + 1

		} else if (Memi[pd+icol] == TY_CHAR) {
		    # Null character string, replace with zero length string
		    output[outoff] = EOS
		    outoff = outoff + 1

		} else {
		    # Null number, replace with INDEF
		    outoff = rgi_gstrcpy ("INDEF", output[outoff], maxout-outoff) +
			     outoff + 1
		}
	    }
	}

	if (outoff >= maxout)
	    call error (1, "RGI_FIX_NULLS: GPB buffer overflow")

end

long procedure rgi_gstrcpy (s1, s2, maxch)

char	s1[ARB], s2[ARB]
long	maxch

long	i

begin
	do i = 1, maxch {
	    s2[i] = s1[i]
	    if (s2[i] == EOS)
		return (i - 1)
	}

	s2[maxch+1] = EOS
	return (maxch)
end
