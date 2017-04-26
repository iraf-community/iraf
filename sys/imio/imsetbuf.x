# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<fset.h>
include	<imio.h>

# IMSETBUF -- Set the FIO file buffer size for the pixel storage file.
# We always make the buffer size equal to an integral number of image lines
# if possible.  The actual number of image lines chosen depends on the
# type of access expected and the size of an image line.  If image lines are
# very large the FIO buffer will be shorter than a line.  We also compute
# IM_FAST, the flag determining whether or not direct access to the FIO
# buffer is permissible.

procedure imsetbuf (fd, im)

int	fd			# pixel storage file
pointer	im			# image header pointer

long	imsize, bufoff, blkoff
int	maxlines, bufsize, szline, nlines, i
int	opt_bufsize, max_bufsize, dev_blksize
int	fstati(), sizeof()

begin
	IM_FAST(im) = NO

	max_bufsize = fstati (fd, F_MAXBUFSIZE)
	opt_bufsize = fstati (fd, F_OPTBUFSIZE)
	dev_blksize = max (1, fstati (fd, F_BLKSIZE))

	szline = IM_PHYSLEN(im,1) * sizeof(IM_PIXTYPE(im))
	imsize = szline
	do i = 2, IM_NDIM(im)
	    imsize = imsize * IM_PHYSLEN(im,i)

	# Compute the suggested buffer size.  If bufsize is set externally
	# and buffrac is disabled (zero) then we try to use the bufsize
	# value given.  If buffrac is enabled then we compute a bufsize
	# based on this, and use the larger of this value or the default
	# bufsize, but not more than DEF_MAXFIOBUFSIZE.  The parameter
	# buffrac specifies the size of an image buffer as a fraction,
	# in percent, of the total size of the image.
	#
	# For example if buffrac=10, the default buffer size will be either
	# "bufsize", or 10% of the full image size, whichever is larger, but
	# not more than DEF_MAXFIOBUFSIZE.  The intent of buffrac is to
	# provide an adaptive mechanism for adjusting the size of the image
	# buffers to match the image being accessed.  For small images the
	# buffer will be the default bufsize (or less if the image is 
	# smaller than this).  For very large images the buffer size will 
	# increase until the builtin default maximum value DEF_MAXFIOBUFSIZE 
	# is reached.  If more control is needed, buffrac can be set to zero,
	# and bufsize will specify the buffer size to be used.  Even if
	# buffrac is enabled, bufsize can be set to a large value to force
	# a large buffer to be used.

	bufsize = IM_VBUFSIZE(im)
	if (IM_VBUFFRAC(im) > 0)
	    bufsize = max(bufsize, min(IM_VBUFMAX(im),
		imsize / 100 * min(100,IM_VBUFFRAC(im)) ))

	# Compute max number of image lines that will fit in default buffer.
	if (max_bufsize > 0)
	    nlines = min(max_bufsize,max(opt_bufsize,bufsize)) / szline
	else
	    nlines = bufsize / szline

	# Compute final number of image lines in buffer.
	if (nlines == 0) {
	    # Image lines are very long.  Use a buffer smaller than a line.
	    call fseti (fd, F_ADVICE, SEQUENTIAL)
	    return
	} else if (IM_VADVICE(im) == RANDOM) {
	    # Always buffer at least one line if the lines are short.
	    nlines = 1
	}

	# Don't make the buffer any larger than the image.
	maxlines = 1
	do i = 2, IM_NDIM(im)
	    maxlines = maxlines * IM_PHYSLEN(im,i)
	nlines = min (nlines, maxlines)

	# Tell FIO to align the first file buffer to the device block
	# containing the first image line.  Ideally the image line will
	# start on a block boundary but this does not have to be the case.

	bufoff = (IM_PIXOFF(im) - 1) / dev_blksize * dev_blksize + 1
	blkoff = IM_PIXOFF(im) - bufoff
	call fseti (fd, F_FIRSTBUFOFF, bufoff)

	# An integral number of image lines fit inside the default size
	# buffer.  Tell FIO the minimum size buffer to use.  FIO will actually
	# allocate a slightly larger buffer if bufsize is not an integral
	# number of device blocks.

	bufsize = blkoff + nlines * szline
	call fseti (fd, F_BUFSIZE, bufsize)

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
IM_FAST(im) = NO
	}
end
