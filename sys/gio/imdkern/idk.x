# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <chars.h>
include <imhdr.h>
include <gki.h>

.help idk
.nf ---------------------------------------------------------------------------
IDK -- Simple image display graphics interface.  The purpose of this
interface is to provide a means of drawing into a graphics overlay in
an image display server via the IRAF data stream interface.  The
interface works by rasterizing the GKI metacode, reading the display
frame buffer, merging the graphics raster with the frame buffer, and
writing back the raster to the frame buffer.

       g_out = idk_open (frame, color, tty)	# device open
	      idk_close (g_out)			# device close
	      idk_flush (g_out)			# flush output

	      idk_frame (g_out)			# start a new frame
	       idk_move (g_out, x, y)		# move to (x,y)
	       idk_draw (g_out, x, y)		# draw a vector to (x,y)
	  idk_linewidth (g_out, width)		# set line width (>=1)

The procedures comprising the top end of the IDK interface are summarized
above and the code is included in this file.  These procedures could be
rewritten by the user to talk directly to a graphics device if desired,
although the metacode file interface is likely to be simpler in most cases.

The size of the bitmap is taken from the size of the display frame
buffer.  Values of the frame buffer are set to the specified color index
for each set bitmap pixel.  The final displayed color depends on the
display server.

The following graphcap fields apply:

	DB	have the kernel print debug messages during execution
	LO	width in device pixels of a line of size 1.0
	LS	difference in device pixels between line sizes
	CI	color index, i.e., the frame buffer pixel value
	FN	display frame number

.endhelp ----------------------------------------------------------------------

# NOTE -- The mf_physbit lookup table, used to map logical screen bits into
# physical bits in the bitmap (for NB != 8) is equivalenced to the mf_obuf
# array which is not otherwise used for bitmap devices.  The length of the
# mf_obuf array must therefore be >= PX.

define	mf_physbit	mf_obuf		# union these two arrays [[[NOTE]]]
define	BPW		32		# nbits in an integer
define	LEN_FBUF	(8192*8192/BPW)	# max size bitmap / frame buffer
define	LEN_OBUF	8192		# max size of buffer line
define	SZ_DDSTR	256		# max size graphcap.DD
define	SZ_OSCMD	256		# OS dispose command from graphcap.DD
define	IOLINES		64		# image lines per i/o transfer


# IDK_OPEN -- Open the metacode file.  Open the frame buffer as an image.
# Initialize the bitmap based on the size of the frame.

int procedure idk_open (a_frame, a_color, tty)

int	a_color			#I display device color index
int	a_frame			#I display buffer frame number
pointer	tty			#I pointer to graphcap descriptor

real	x, y
char	strval[1]
int	byte, off, i, j
int	wcs, key, frame, color

bool	ttygetb()
real	ttygetr()
int	imd_mapframe(), ttygeti(), shifti(), imdrcur()
errchk	imd_mapframe, ttygetr, ttygeti, ttygetb
include	"idk.com"

begin
	frame = a_frame
	color = a_color

	# The DB flag may be set in the graphcap entry for an IMD device to
	# print debug messages during execution.

	mf_debug = ttygetb (tty, "DB")
	if (mf_debug) {
	    call eprintf ("idk: open frame %d, color = %d\n")
		call pargi (frame)
		call pargi (color)
	}

	mf_update = false
	
	# If the frame number was not specified as a parameter see if it is
	# specified in the graphcap, else try to query the display to determine
	# the current display frame and plot into that.

	if (frame <= 0)
	    iferr (frame = ttygeti (tty, "FN"))
		frame = 0
	if (frame <= 0)
	    if (imdrcur ("stdimage", x, y, wcs, key, strval, 1, 0, NO) >= 0)
		frame = max (1, wcs / 100)
	    else
		frame = 1

	# Find the color index in graphcap?
	if (color < 0)
	    color = max(0, ttygeti (tty, "CI"))

	# Map the frame buffer as an image.
	mf_fd = imd_mapframe (frame, READ_WRITE, YES)

	# Initialize bitmap parameters.
	mf_pxsize   = IM_LEN(mf_fd, 1)
	mf_pysize   = IM_LEN(mf_fd, 2)
	mf_xorigin  = 0
	mf_yorigin  = 0
	mf_wxsize   = IM_LEN(mf_fd, 1) - 1
	mf_wysize   = IM_LEN(mf_fd, 2) - 1
	mf_nbpb     = 8

	# Line width parameters.
	mf_lworigin = max (1, ttygeti (tty, "LO"))
	mf_lwslope  = ttygetr (tty, "LS")

	# Size of the frame buffer.
	mf_lenframe = (mf_pxsize * mf_pysize + BPW-1) / BPW

	mf_color = color
	mf_linewidth = mf_lworigin

	# Initial "pen" position.
	mf_cx = 0
	mf_cy = 0

	mf_xmin = mf_xorigin
	mf_ymin = mf_yorigin
	mf_xmax = mf_xmin + mf_wxsize
	mf_ymax = mf_ymin + mf_wysize

	mf_xscale = real(mf_wxsize) / real(GKI_MAXNDC)
	mf_yscale = real(mf_wysize) / real(GKI_MAXNDC)

	if (mf_lenframe > LEN_FBUF)
	    call error (1, "imdkern: bitmap too large")

	# Initialize the bit mask table.
	do j = 1, (BPW/NBITS_BYTE)
	    do i = 1, NBITS_BYTE {
		off = (j - 1) * NBITS_BYTE
		mf_bitmask[off+i] = shifti (1, off + NBITS_BYTE - i)
	    }

	# Initialize the bit offset lookup table.  This gives the physical
	# x-offset into the lookup table of each addressable x-coordinate
	# on the device.  If NB is NBITS_BYTE the mapping is one-to-one.
	# Note that the table contains zero-indexed bit offsets.

	do i = 1, mf_pxsize {
	    byte = (i - 1) / mf_nbpb
	    mf_physbit[i] = min (mf_pxsize,
		byte * NBITS_BYTE + (i - (byte * mf_nbpb))) - 1
	}

	if (mf_debug) {
	    call eprintf ("bitmap [%d,%d] origin=[%d,%d] wsize=[%d,%d]\n")
		call pargi (mf_pxsize);  call pargi (mf_pysize)
		call pargi (mf_xorigin); call pargi (mf_yorigin)
		call pargi (mf_wxsize);  call pargi (mf_wysize)
	}

	return (mf_fd)
end


# IDK_CLOSE -- Update the display frame buffer and close the display.

procedure idk_close (fd)

int	fd			# output stream [NOT USED]

errchk	idk_frame, imunmap
include	"idk.com"

begin
	if (mf_debug)
	    call eprintf ("close device\n")

	call idk_frame (mf_fd)

	if (mf_fd != NULL) {
	    call imunmap (mf_fd)
	    mf_fd = NULL
	}
end


# IDK_FLUSH -- Flush any buffered metacode output.

procedure idk_flush (fd)

int	fd			# output stream [NOT USED]
include	"idk.com"

begin
	if (mf_fd != NULL)
	    call imflush (mf_fd)
end


# IDK_FRAME -- Output a frame.  Overlay the bitmap on the frame buffer.
# Map the display frame as an image section and process the bitmap line by
# line.

procedure idk_frame (fd)

int	fd			# output stream [NOT USED]

int	x1, x2, y1, y2
int	bmw			# Bitmap word offset
int	npix			# Pixels in local I/O buffer
int	fbp			# Frame buffer section offset
int	fbp0
int	i, j
int	line
pointer	ob, ib

pointer	imps2s(), imgs2s()
include	"idk.com"

begin
	# Ignore frame commands if frame is empty.
	if (!mf_update)
	    return

	if (mf_debug) {
	    call eprintf ("Write the frame, color = %d\n")
		call pargi (mf_color)
	}

	# Write the bitmap to the output frame buffer.

	y2 = 0
	for (y1=1;  y2 < mf_pysize;  y1=y1+IOLINES) {
	    # For each buffer section of the frame.
	    y2 = min (y1 + IOLINES-1, mf_pysize)
	    x1 = 1
	    x2 = mf_pxsize

	    # Map the frame section.
	    ob = imps2s (mf_fd, x1, x2, y1, y2)
	    ib = imgs2s (mf_fd, x1, x2, y1, y2)

	    npix = mf_pxsize * (y2 - y1 + 1)

	    if (ob != ib)
		# Copy the input buffer to the output buffer
		call amovs (Mems[ib], Mems[ob], npix)

	    do line = y1, y2 {
		# Each line in the local frame buffer section
		fbp0 = (line - y1) * mf_pxsize

		do i = 1, mf_pxsize / BPW {
		    # Each word in the bitmap line.
		    bmw = (line - 1) * mf_pxsize / BPW + i

		    if (mf_fbuf[bmw] != 0) {
			do j = 1, BPW {
			    # Each bit in the bitmap word.

			    if (and (mf_fbuf[bmw], mf_bitmask[j]) != 0) {
				# An ON bit.
				fbp = fbp0 + (i-1) * BPW + j
				Mems[ob+fbp-1] = mf_color
			    }
			}
		    }
		}
	    }
	}

	mf_update = false
end


# IDK_MOVE -- Output a pen move instruction.

procedure idk_move (fd, x, y)

int	fd			# output stream [NOT USED]
int	x, y			# point to move to

include	"idk.com"

begin
	mf_cx = x
	mf_cy = y

	# Convert to zero indexed coordinates and clip at boundary.
	# Allow room for line width shift near boundary.

	mf_cx = max (mf_xmin, min (mf_xmax,
	    int (mf_cx * mf_xscale) + mf_xorigin))
	mf_cy = max (mf_ymin, min (mf_ymax,
	    int (mf_cy * mf_yscale) + mf_yorigin))
end


# IDK_DRAW -- Output a pen draw instruction.

procedure idk_draw (fd, a_x, a_y)

int	fd			# output stream [NOT USED]
int	a_x, a_y		# point to draw to

int	xshift, yshift, dx, dy
int	new_x, new_y, x1, y1, x2, y2, n, i
include	"idk.com"

begin
	new_x = a_x
	new_y = a_y

	if (!mf_update) {
	    # We are called when the first drawing instruction is output for a
	    # new frame.  We clear the bitmap.

	    # Zero out all the bits in a bitmap.
	    call aclri (mf_fbuf, mf_lenframe)

	    mf_update = true
	}

	# Convert to zero indexed coordinates and clip at boundary.
	# Allow room for line width shift near boundary.

	new_x = max (mf_xmin, min (mf_xmax,
	    int (new_x * mf_xscale) + mf_xorigin))
	new_y = max (mf_ymin, min (mf_ymax,
	    int (new_y * mf_yscale) + mf_yorigin))

	if (mf_linewidth <= 1)
	    call idk_vector (mf_cx, mf_cy, new_x, new_y)
	else {
	    # Redraw the vector several times with small normal shifts to
	    # produce a wider line.

	    xshift = 0
	    yshift = 0

	    if (abs (new_x - mf_cx) > abs (new_y - mf_cy)) {
		dx = 0
		dy = 1
	    } else {
		dx = 1
		dy = 0
	    }

	    do i = 1, mf_linewidth {
		x1 = mf_cx + xshift
		y1 = mf_cy + yshift
		x2 = new_x + xshift
		y2 = new_y + yshift

		call idk_vector (x1, y1, x2, y2)

		n = (i + 1) / 2
		if (and (i, 1) == 0) {
		    xshift =  dx * n
		    yshift =  dy * n
		} else {
		    xshift = -dx * n
		    yshift = -dy * n
		}
	    }
	}

	# Update the current pen position, and set the update flag so that
	# the bitmap will be written to the output file.

	mf_cx = new_x
	mf_cy = new_y
end


# IDK_VECTOR -- Write a vector (line) of unit width into the bitmap.  The line
# endpoints are expressed in physical device coordinates.

procedure idk_vector (a_x1, a_y1, a_x2, a_y2)

int	a_x1, a_y1			# start point of line
int	a_x2, a_y2			# end point of line

real	dydx, dxdy
long	fbit, wbit, word
int	wpln, mask, dx, dy, x, y, x1, y1, x2, y2, or()
include	"idk.com"

begin
	x1 = a_x1; y1 = a_y1
	x2 = a_x2; y2 = a_y2

	dx = x2 - x1
	dy = y2 - y1

	if (abs(dx) > abs(dy)) {
	    if (x1 > x2) {
		x1 = a_x2; x2 = a_x1; dx = -dx
		y1 = a_y2; y2 = a_y1; dy = -dy
	    }

	    if (dy == 0 && mf_nbpb == NBITS_BYTE) {
		# Somewhat optimized code for the case of a horiz. vector.

		fbit = y1 * mf_pxsize + x1
		word = fbit / BPW
		wbit = and (fbit, BPW-1)

		do x = x1, x2 {
		    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
		    wbit = wbit + 1
		    if (wbit >= BPW) {
			wbit = 0
			word = word + 1
		    }
		}

	    } else {
		# The general case for a mostly-X vector.

		dydx = real(dy) / real(dx)
		do x = x1, x2 {
		    y = int ((x - x1) * dydx) + y1
		    fbit = y * mf_pxsize + mf_physbit[x+1]
		    word = fbit / BPW
		    wbit = and (fbit, BPW-1)
		    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
		}
	    }

	} else if (dy != 0) {
	    if (y1 > y2) {
		x1 = a_x2; x2 = a_x1; dx = -dx
		y1 = a_y2; y2 = a_y1; dy = -dy
	    }

	    if (dx == 0) {
		# Optimized code for the case of a vertical vector.

		fbit = y1 * mf_pxsize + mf_physbit[x1+1]
		word = fbit / BPW + 1
		wbit = and (fbit, BPW-1)
		wpln = (mf_pxsize + BPW-1) / BPW
		mask = mf_bitmask[wbit+1]

		do y = y1, y2 {
		    mf_fbuf[word] = or (mf_fbuf[word], mask)
		    word = word + wpln
		}

	    } else {
		# The general case of a mostly-Y vector.

		dxdy = real(dx) / real(dy)
		do y = y1, y2 {
		    x = int ((y - y1) * dxdy) + x1
		    fbit = y * mf_pxsize + mf_physbit[x+1]
		    word = fbit / BPW
		    wbit = and (fbit, BPW-1)
		    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
		}
	    }

	} else {
	    # Plot a single point (dx=dy=0).

	    fbit = y1 * mf_pxsize + mf_physbit[x1+1]
	    word = fbit / BPW
	    wbit = and (fbit, BPW-1)
	    mf_fbuf[word+1] = or (mf_fbuf[word+1], mf_bitmask[wbit+1])
	}
end


# IDK_LINEWIDTH -- Output a line width set instruction.

procedure idk_linewidth (fd, width)

int	fd			# output stream [NOT USED]
int	width			# new line width

int	gap
include	"idk.com"

begin
	# Set the line width in device pixels.
	mf_linewidth = max (1, mf_lworigin + int ((width-1) * mf_lwslope))

	# Set the clipping limits.  Allow for shifting to widen lines.
	gap = mf_linewidth / 2
	mf_xmin = mf_xorigin + gap
	mf_ymin = mf_yorigin + gap
	mf_xmax = mf_xorigin + mf_wxsize - gap
	mf_ymax = mf_yorigin + mf_wysize - gap
end
