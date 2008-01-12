include <mach.h>
include <imhdr.h>

define	FONTWIDE	6
define	FONTHIGH	7
define	SZ_LOOKUP	128
define	SZ_FONT		455
define	SZ_PIXARY	5

# MK_TEXTIM -- Write a text string into an image using a pixel font for speed.
# Characters are made twice as big as the font by doubling in both axes.

procedure mk_textim (im, s, x, y, xmag, ymag, value, center)

pointer	im				# image to put the text in.
char	s[ARB]				# text to put in the image.
int	x, y				# x, y position in the image.
int	xmag, ymag			# x, y magnification values.
int	value				# value to use in image for text.
int	center				# center the string

int	numrow, numcol, numchars, fonthigh, fontwide, xinit, yinit
int	i, l, ch, nchar, line, ip, pixary[SZ_PIXARY]
pointer	lineget, lineput

int	strlen()
pointer	imgl2s(), impl2s()
errchk	imgl2s, impl2s

begin
	# Find the length of the string.
	numchars = strlen (s)
	if (numchars <= 0)
	    return

	# Calculate height and width of magnified font.
	fonthigh = FONTHIGH * ymag
	fontwide = FONTWIDE * xmag

	# Check for row/col out of bounds.
	numcol= IM_LEN(im,1)
	numrow = IM_LEN(im,2)

	# Compute the initial position of the string truncating characters
	# is necessary.
	if (center == YES)
	    xinit = x - fontwide * numchars / 2
	else
	    xinit = x
	for (ip = 1; ip <= numchars; ip = ip + 1) {
	    if (xinit >= 1)
		break
	    xinit = xinit + fontwide 
	}

	# Return if beginning of string is off image.
	if (xinit < 1 || xinit > numcol)
	    return

	# Truncate the string.
	if (xinit > numcol - fontwide * (numchars - ip + 1)) {
	    numchars = int ((numcol - xinit) / fontwide)
	    if (numchars <= 0)
	        return
	}

	# Return if the text does not fit in the image.
	if (center == YES)
	    yinit = y - fonthigh * numchars / 2
	else
	    yinit = y
	if ((yinit <= 0) || (yinit > numrow - fonthigh))
	    return

	# For each line of the text (backward).
	for (i = 1; i <= 7; i = i + 1) {

	    line = yinit + (i-1) * ymag

	    do l = 1, ymag {

	        # Get and put the line of the image.
	        lineput = impl2s (im, line+(l-1))
	        lineget = imgl2s (im, line+(l-1))
	        call amovs (Mems[lineget], Mems[lineput], numcol)

	        # Put out the font.
	        do ch = ip, numchars {
	            nchar = int (s[ch])
		    call mk_pixbit (nchar, 8 - i, pixary)
		    call mk_putpix (pixary, Mems[lineput], numcol,
			xinit+(ch-1)*fontwide, value, xmag)
	        }

	    }
	}
end


# MK_PIXBIT -- Look up which bits should be set for this character on this line.

procedure mk_pixbit (code, line, bitarray)

int	code		# character we are writing
int	line		# line of the character we are writing
int	bitarray[ARB]	# bit-array to receive data

int	pix, i
short	asciilook[SZ_LOOKUP], font[SZ_FONT]
int	bitupk()

include	"pixelfont.inc"
include	"asciilook.inc"

begin
	pix = font[asciilook[code+1]+line-1]
	bitarray[5] = bitupk (pix, 1, 1)
	bitarray[4] = bitupk (pix, 4, 1)
	bitarray[3] = bitupk (pix, 7, 1)
	bitarray[2] = bitupk (pix, 10, 1)
	bitarray[1] = bitupk (pix, 13, 1)
end


# MK_PUTPIX -- Put one line of one character into the data array.

procedure mk_putpix (pixary, array, size, position, value, xmag)

int	pixary[ARB]		# array of pixels in character
int	size, position		# size of data array
short	array[size]		# data array in which to put character line
int	value			# value to use for character pixels
int	xmag			# x-magnification of text

int	i, k, x

begin
	do i = 1, 5 {
	    if (pixary[i] == 1) {
		x = position + (i-1) * xmag
		do k = 1, xmag
		    array[x+(k-1)] = value
	    }
	}
end


# MK_TLIMITS -- Compute the column and line limits of a text string.

procedure mk_tlimits (str, x, y, xmag, ymag, ncols, nlines, x1, x2, y1, y2)

char	str[ARB]		# string to be written to the image
int	x, y			# starting position of the string
int	xmag, ymag		# magnification factor
int	ncols, nlines		# dimensions of the image
int	x1, x2			# column limits
int	y1, y2			# line limits

begin
	x1 = max (1, min (y, ncols))
	x2 = min (ncols, max (1, y + 5 * xmag))
	y1 = max (1, min (y, nlines))
	y2 = min (nlines, max (1, y + 6 * ymag))
end
