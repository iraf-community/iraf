# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<fset.h>
include	<imio.h>

# IMSETBUF -- Set the FIO file buffer size for the pixel storage file.
# We always make the buffer size equal to an integral number of image lines
# if possible.  The actual number of images lines chosen depends on the
# type of access expected and the size of an image line.  If image lines are
# very large the FIO buffer will be shorter than a line.  We also compute
# IM_FAST, the flag determining whether or not direct access to the FIO
# buffer is permissible.

procedure imsetbuf (fd, im)

int	fd			# pixel storage file
pointer	im			# image header pointer

int	opt_bufsize, max_bufsize, dev_blksize
int	szline, bufsize, nlines
int	fstati(), sizeof()

begin
	IM_FAST(im) = NO

	szline  = IM_PHYSLEN(im,1) * sizeof (IM_PIXTYPE(im))
	max_bufsize = fstati (fd, F_MAXBUFSIZE)
	opt_bufsize = fstati (fd, F_OPTBUFSIZE)
	dev_blksize = fstati (fd, F_BLKSIZE)

	# Compute max number of image lines that will fit in default buffer.
	if (max_bufsize > 0)
	    nlines = min(max_bufsize,max(opt_bufsize,IM_VBUFSIZE(im))) / szline
	else
	    nlines = IM_VBUFSIZE(im) / szline

	# Compute final number of image lines in buffer.
	if (nlines == 0) {
	    # Image lines are very long.  Use a buffer smaller than a line.
	    call fseti (fd, F_ADVICE, SEQUENTIAL)
	    return
	} else if (IM_VADVICE(im) == RANDOM) {
	    # Always buffer at least one line if the lines are short.
	    nlines = 1
	}

	# An integral number of image lines fit inside the default size
	# buffer.  Tell FIO the minimum size buffer to use.  FIO will actually
	# allocate a slightly larger buffer if bufsize is not an integral
	# number of device blocks.

	bufsize = nlines * szline
	call fseti (fd, F_BUFSIZE, bufsize)

	# Tell FIO to align the first file buffer to the offset of the first
	# image line, else it will have done us no good to size the FIO buffer
	# to fit an integral number of image lines.  Note that we cannot do
	# this, however, unless the first image line is aligned on a device
	# block boundary.

	if (dev_blksize > 1)
	    if (mod (IM_PIXOFF(im)-1, dev_blksize) == 0)
		call fseti (fd, F_FIRSTBUFOFF, IM_PIXOFF(im))

	# If a FIO buffer will hold at least two image lines, if no image
	# section was given, if there is only one input line buffer, if
	# we are not going to be referencing out of bounds, and the pixel
	# data is properly aligned in the pixel file, then FAST i/o (directly
	# into the FIO buffer) is possible provided no datatype conversion
	# or byte swapping is desired or required.  If all these criteria
	# are true enable fast i/o.

	if ((bufsize / szline >= 2 && IM_SECTUSED(im) == NO) &&
	    (IM_VNBUFS(im) == 1 && IM_VNBNDRYPIX(im) == 0) &&
	    (mod (IM_PIXOFF(im), sizeof(IM_PIXTYPE(im)))) == 1 &&
	    IM_SWAP(im) == NO) {

	    IM_FAST(im) = YES
	}
end
