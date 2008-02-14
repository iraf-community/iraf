# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>

# WTI_PUTREAL -- Output pixels to a text file in a floating point format.
# Pixels are output in storage order for images of any dimension (leftmost
# subscript varying fastest).  We do not bother to implement a different
# datapath for each image pixel datatype because the execution time is
# entirely dominated by the binary to character conversion, and because we
# need type double pixels for DTOC anyhow.

procedure wti_putreal (im, tx, maxll, decpl, fmtchar, width)

pointer	im		# pointer to image file
int	tx		# file descriptor of output text file
int	maxll		# maximum length of output text line
int	decpl		# number of decimal places of precision
int	fmtchar		# type of encoding (efg)
int	width		# field width of each number (0=free format)

char	numbuf[MAX_DIGITS]
int	npix, ip, j, ndigits, nspaces, maxch
pointer	sp, obuf, op, pix, cp
long	v[IM_MAXDIM]
int	imgnld(), dtoc()
errchk	imgnld, putline

begin
	call smark (sp)
	call salloc (obuf, maxll+1, TY_CHAR)

	call amovkl (long(1), v, IM_MAXDIM)
	npix = IM_LEN(im,1)
	op = obuf

	while (imgnld (im, pix, v) != EOF) {
	    do j = 1, npix {
		# Encode the number.
		if (width <= 0)
		    maxch = MAX_DIGITS
		else
		    maxch = width

		ndigits = dtoc (Memd[pix+j-1], numbuf, MAX_DIGITS,
		    decpl, fmtchar, maxch)

		# Determine the number of spaces needed to right justify the
		# field.  If the field width is zero the output is free format
		# and we always output a single space.

		if (width <= 0)
		    nspaces = 1
		else
		    nspaces = width - ndigits

		# Break the output line if insufficient space remains on the
		# line.

		if (op-obuf + ndigits + nspaces > maxll) {
		    Memc[op] = '\n'
		    Memc[op+1] = EOS
		    call putline (tx, Memc[obuf])
		    op = obuf
		}

		# Append sufficient blanks to right justify the number in
		# the given field.
		do cp = op, op + nspaces - 1
		    Memc[cp] = ' '
		op = op + nspaces

		# Append the number to the output line.
		do ip = 1, ndigits
		    Memc[op+ip-1] = numbuf[ip]
		op = op + ndigits
	    }
	}

	# Break the last line if there is anything on it.
	if (op > obuf) {
	    Memc[op] = '\n'
	    Memc[op+1] = EOS
	    call putline (tx, Memc[obuf])
	}

	call sfree (sp)
end
