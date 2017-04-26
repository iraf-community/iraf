include <mach.h>
include <imhdr.h>

define	FONTWIDE	6
define	FONTHIGH	7
define	MAXSTRING	100

# TEXTIM -- Write a text string into an image using a pixel font for speed.
# Characters are made twice as big as the font by doubling in both axes.

procedure textim (im, s, x, y, xmag, ymag, value, zerobgnd, bgndvalu)

pointer	im				# Image to put the text in.
char	s[MAXSTRING]			# Text to put in the image.
int	x, y				# x, y position in the image.
int	xmag, ymag			# x, y magnification values.
int	value				# Value to use in image for text.
int	zerobgnd			# Flag to tell if we should zero bgnd.
int	bgndvalu			# Background value to use.

int	numrow, numcol, numchars
int	fonthigh, fontwide
int	i, l, ch
int	nchar, line
int	pixary[5]
pointer	lineget, lineput

short	tshort
int	strlen()
pointer	imgl2s(), impl2s()
errchk	imgl2s, impl2s

begin
	# Find the length of the string (if there aren't any chars, return).
	numchars = strlen (s)
	if (numchars <= 0)
	    return

	# Calculate height and width of magnified font.
	fonthigh = FONTHIGH * ymag
	fontwide = FONTWIDE * xmag

	# Check for row/col out of bounds.
	numcol= IM_LEN(im,1)
	numrow = IM_LEN(im,2)

	if (x <= 0) {
	    call printf ("Warning: Image text deleted, column <= 0.\n")
	    return
	}

	if (x > numcol - fontwide*numchars) {
	    call printf ("Warning: Image text truncated or deleted\n")
	    numchars = int((numcol - x)/fontwide)
	    if (numchars <= 0)
	        return
	}

	if ((y <= 0) || (y > numrow - fonthigh)) {
	    call printf ("Warning: Image text deleted, wrong row number.\n")
	    return
	}

	# For each line of the text (backward).
	for (i=7; i>=1; i=i-1) {
	    line = y+(8-i)*ymag-1

	    do l = 1, ymag {

	        # Get and put the line of the image.
	        lineget = imgl2s (im, line+(l-1))
	        lineput = impl2s (im, line+(l-1))

	        # Copy input array or the background value to output array.
	        if (zerobgnd == 1) {
		    tshort = bgndvalu
	            call amovks (tshort, Mems[lineput+x-1],
		        fontwide*numchars)
	        } else
	            call amovs (Mems[lineget], Mems[lineput], numcol)

	        # Put the font.
	        do ch = 1, numchars {
	            nchar = int(s[ch])
		    call pixbit (nchar, i, pixary)
		    call putpix (pixary, Mems[lineput], numcol,
			x+(ch-1)*fontwide, value, xmag)
	        }
	    }		# End of do on l.
	}
end


# PUTPIX -- Put one line of one character into the data array.

procedure putpix (pixary, array, size, position, value, xmag)

int	pixary[5]		# array of pixels in character
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
