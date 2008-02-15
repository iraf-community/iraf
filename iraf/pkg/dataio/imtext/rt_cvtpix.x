# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include "imtext.h"

# RT_CONVERT_PIXELS -- Called once for each text file to be converted.  All
# pixels in the text file are converted to image pixels.

procedure rt_convert_pixels (tf, im, format, pixels)

int	tf		# File descriptor of input text file
pointer	im		# Pointer to image header
int	format		# Format of text pixels (integer/floating point)
int	pixels		# Get pixels from input text file?

pointer	bufptr, sp, word1, pattern
int	stat, nlines, npix, i
long	v[IM_MAXDIM], start
int	impnll(), impnld(), impnlx()
int	fscan(), stridxs(), patmatch(), patmake()
long	note()

errchk	impnll, impnld, impnlx
errchk	rt_get_lineptr, rt_output_line, fscan, seek, amovkl

begin
	# Determine if text file pixels were written with an integer, complex
	# or floating point format.  This information may have been already
	# determined from the header.  If not, the first pixel is read
	# from text file.  If it contains a decimal point, the character E,
	# or a + or - sign not in the first position, it is a floating point
	# number.  Complex numbers are assumed to be written as "(r,i)".

	if (pixels == YES && format == UNSET) {
	    call smark (sp)
	    call salloc (word1,   SZ_LINE, TY_CHAR)
	    call salloc (pattern, SZ_LINE, TY_CHAR)

	    # Note position so we can return to it
	    start = note (tf)

	    stat = fscan (tf)
	    call gargwrd (Memc[word1], SZ_LINE)
	    if (patmake ("[DdEe]", Memc[pattern], SZ_LINE) == ERR)
		call error (7, "Error creating format pattern")

	    if (stridxs ("(", Memc[word1]) > 0)
		format = CPX_FORM
	    else if (stridxs (".", Memc[word1]) > 0)
		format = FP_FORM
	    else if (patmatch (Memc[word1], Memc[pattern]) > 0)
		format = FP_FORM
	    else if (stridxs ("+", Memc[word1]) > 1)
		format = FP_FORM
	    else if (stridxs ("-", Memc[word1]) > 1)
		format = FP_FORM
	    else
		format = INT_FORM

	    call sfree (sp)
	    call seek (tf, start)
	}

	# Pixel type may not have been set by this point either...
	if (IM_PIXTYPE(im) == UNSET) {
	    switch (format) {
	    case FP_FORM:
		IM_PIXTYPE(im) = TY_REAL
	    case INT_FORM:
		IM_PIXTYPE(im) = TY_INT
	    case CPX_FORM:
		IM_PIXTYPE(im) = TY_COMPLEX
	    default:
		call error (0, "Unrecognized format type")
	    }
	}

	nlines = 1
	do i = 2, IM_NDIM(im)
	    nlines = nlines * IM_LEN (im, i)
	call amovkl (long(1), v, IM_MAXDIM)
	npix = IM_LEN (im, 1)

	# Initialize text buffer
	call rt_rinit

	switch (IM_PIXTYPE(im)) {
	case TY_SHORT, TY_INT, TY_USHORT, TY_LONG:
	    do i = 1, nlines {
	        stat = impnll (im, bufptr, v) 
		if (pixels == YES)
		    call rt_output_linel (tf, format, bufptr, npix)
		else
		    call aclrl (Meml[bufptr], npix)
	    }
	case TY_REAL, TY_DOUBLE:
	    do i = 1, nlines {
	        stat = impnld (im, bufptr, v)
		if (pixels == YES)
		    call rt_output_lined (tf, format, bufptr, npix)
		else
		    call aclrd (Memd[bufptr], npix)
	    }
	case TY_COMPLEX:
	    do i = 1, nlines {
	        stat = impnlx (im, bufptr, v)
		if (pixels == YES)
		    call rt_output_linex (tf, format, bufptr, npix)
		else
		    call aclrx (Memx[bufptr], npix)
	    }
	default:
	    call error (0, "Image pixel type unset")
	}
end
