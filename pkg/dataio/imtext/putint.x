# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	<ctype.h>

# WTI_PUTINT -- Output pixels to a text file in integer format.  Pixels are
# output in storage order for images of any dimension (leftmost subscript
# varying fastest).

procedure wti_putint (im, tx, maxll, width)

pointer	im		# pointer to image file
int	tx		# file descriptor of output text file
int	maxll		# maximum length of output text line
int	width		# field width of each number (0=free format)

char	numbuf[MAX_DIGITS]
int	npix, ip, j, ndigits
pointer	sp, obuf, op, pix
long	v[IM_MAXDIM]
int	imgnll(), ltoc()
errchk	imgnll, putline

begin
	call smark (sp)
	call salloc (obuf, maxll+1, TY_CHAR)

	call amovkl (long(1), v, IM_MAXDIM)
	npix = IM_LEN(im,1)
	op = obuf

	if (width <= 0) {
	    # If the encoding is free format call LTOC to encode the number,
	    # compute the number of spaces required to right justify the
	    # numeric string in the specified field width, then move the
	    # spaces and the number into the output line.

	    while (imgnll (im, pix, v) != EOF) {
		do j = 1, npix {
		    # Encode the number.
		    ndigits = ltoc (Meml[pix+j-1], numbuf, MAX_DIGITS)

		    # Break output line if insufficient space remains.
		    if (op-obuf + ndigits + 1 > maxll) {
			Memc[op] = '\n'
			Memc[op+1] = EOS
			call putline (tx, Memc[obuf])
			op = obuf
		    }

		    # Append a blank and the number to the output line.
		    if (op > obuf) {
			Memc[op] = ' '
			op = op + 1
		    }
		    do ip = 1, ndigits
			Memc[op+ip-1] = numbuf[ip]
		    op = op + ndigits
		}
	    }

	} else {
	    # Fixed format.  Encode the integer number from right to left
	    # in the given field, blank filling at the left.  Note that
	    # fancy formats such as left justify or zero fill are not
	    # presently supported (and are probably not worth it here).

	    while (imgnll (im, pix, v) != EOF) {
		do j = 1, npix {
		    # Break output line if insufficient space remains.
		    if (op-obuf + width > maxll) {
			Memc[op] = '\n'
			Memc[op+1] = EOS
			call putline (tx, Memc[obuf])
			op = obuf
		    }

		    # Encode the number in the output field.
		    call wti_encode_l (Meml[pix+j-1], Memc[op], width)
		    op = op + width
		}
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


# WTI_ENCODE_L -- Encode a long integer number as a decimal integer, right
# justified with blank fill in the indicated field.  Since we know the field
# width in advance we can encode the number from right to left (least
# significant digits first), without having to reverse the digits and copy
# the string as is the case with LTOC.
procedure wti_encode_l (lval, out, w)

long	lval			# number to be encoded
char	out[w]			# output field (NOT EOS DELIMITED)
int	w			# field width

bool	neg
int	op, i
long	val, quotient
define	overflow_ 91

begin
	if (IS_INDEFL (lval)) {
	    if (w < 5)
		goto overflow_
	    call amovc ("INDEF", out[w-4], 5)
	    op = w - 5

	} else {
	    neg = (lval < 0)
	    if (neg)
		val = -lval
	    else
		val = lval

	    # Output digits from right to left.
	    do i = w, 1, -1 {
		quotient = val / 10
		out[i] = TO_DIGIT (val - quotient * 10)
		val = quotient
		if (val == 0) {
		    op = i - 1
		    break
		}
	    }

	    # Add minus sign if negative.
	    if (neg) {
		if (op > 0)
		    out[op] = '-'
		op = op - 1
	    }

	    # Check for overflow.
	    if (op < 0 || val > 0)
		goto overflow_
	}
	    
	# Blank fill at left.
	do i = op, 1, -1
	    out[i] = ' '

	return

overflow_
	# Number was too large to fit in the given field width.
	do i = 1, w
	    out[i] = '*'
end
