# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>
include	"glabax.h"

define	SZ_MANTISSA	3		# "10" or "-10"
define	SZ_EXPONENT	4		# largest is "-999"


# GLB_LOGLAB -- Draw a tick label in log units at the given position.
# A log tick is a power of ten, e.g.  10^2, where the ^ signifies that
# the 2 is to be drawn one half character height higher than the 10.

procedure glb_loglab (gp, sx, sy, val, fmt, scaling)

pointer	gp			# graphics descriptor
real	sx, sy			# NDC coords of label
real	val			# value to be encoded (not the log of)
char	fmt[ARB]		# tick label gtext format (justification)
int	scaling			# type of scaling on axis

bool	zero
char	mantissa[SZ_MANTISSA]
char	exponent[SZ_EXPONENT]
int	len_mantissa, len_exponent, ip, hj, vj
real	logval, char_height, char_width, left, xpos, ypos, txsize

bool	fp_equalr()
real	elogr(), gstatr(), ggetr()
int	strlen(), strmatch(), itoc()

begin
	# Compute the log value to be encoded.
	if (scaling == LOG)
	    logval = log10 (val)
	else {
	    logval = elogr (val)
	    zero = fp_equalr (logval, 0.0)
	}

	txsize = gstatr (gp, G_TXSIZE)

	# Get char height and width in NDC coords.
	char_height = ggetr (gp, "ch")
	if (char_height < EPSILON)
	    char_height = DEF_CHARHEIGHT
	char_height = char_height * txsize

	char_width = ggetr (gp, "cw")
	if (char_width < EPSILON)
	    char_width = DEF_CHARWIDTH
	char_width = char_width * txsize

	# Encode the mantissa and exponent strings.
	if (zero) {
	    call strcpy ("0", mantissa, SZ_MANTISSA)
	} else if (logval < 0 && scaling == ELOG) {
	    call strcpy ("-10", mantissa, SZ_MANTISSA)
	    logval = abs (logval)
	} else
	    call strcpy ("10", mantissa, SZ_MANTISSA)

	len_mantissa = strlen (mantissa)
	if (zero)
	    len_exponent = 0
	else
	    len_exponent = itoc (nint(logval), exponent, SZ_EXPONENT)

	# Determine type of horizontal justification required.
	ip = strmatch (fmt, "hj=")
	if (ip <= 0)
	    hj = 'c'
	else
	    hj = fmt[ip]

	# Determine type of vertical justification required.
	ip = strmatch (fmt, "vj=")
	if (ip <= 0)
	    vj = 'c'
	else
	    vj = fmt[ip]

	# On devices with adjustable character sizes the most pleasing results
	# are obtained if the digits "10" are nicely aligned on the vertical
	# axis, regardless of the actual number of characters in the exponent
	# string, minus signs etc (this type of alignment is more natural
	# because the exponent is printed at half size).  Hence if we are on
	# a vertical axis (hj != c) fix the number of characters in the two
	# strings so that the alignment comes out the same regardless of the
	# actual number of chars in either field.  The length of the exponent
	# field is not completely fixed, rather we allow a little more space
	# if the exponent is large.  For small exponents len_exponent=1.

	if (hj != 'c') {
	    len_mantissa = 2
	    len_exponent = (len_exponent + 1) / 2
	}

	# Compute XPOS, the NDC X coord of the point halfway between the
	# last char of the mantissa and the first char of the exponent.

	switch (hj) {
	case 'l':
	    left = sx
	case 'r':
	    left = sx - (len_mantissa + len_exponent) * char_width
	default:
	    left = sx - ((len_mantissa + len_exponent) * char_width) / 2.0
	}

	xpos = left + len_mantissa * char_width

	# Compute YPOS, the NDC Y coord of the center of a mantissa character
	# and of the bottom of an exponent character.  Using the same coordinate
	# to address both positions makes the label come out the same regardless
	# of the plot magnification, even on a device where the character size
	# is fixed by the hardware.

	switch (vj) {
	case 'b':
	    ypos = sy + char_height / 2.0
	case 't':
	    ypos = sy - char_height / 2.0
	default:
	    ypos = sy
	}

	# Draw the mantissa.
	call gtext (gp, xpos, ypos, mantissa, "hj=r,vj=c")

	# Draw the exponent if there is one.
	if (!zero) {
	    call gsetr (gp, G_TXSIZE, txsize / 2.0)
	    call gtext (gp, xpos, ypos, exponent, "hj=l;vj=b")
	    call gsetr (gp, G_TXSIZE, txsize)
	}
end
