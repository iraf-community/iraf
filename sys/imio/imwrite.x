# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include	<imio.h>

define	SZ_ZBUF		50

# IMWRITE -- Write data to the pixel storage file.  Bounds checking has
# already been performed by the time IMWRITE is called.  If writing beyond
# EOF (new image), write zeros until the indicated offset is reached.

procedure imwrite (imdes, buf, nchars, offset)

pointer	imdes
char	buf[ARB]
int	nchars
long	offset

int	fd
char	zbuf[SZ_ZBUF]
long	start, i
long	fstatl()
errchk	write, seek, fstatl
data	zbuf /SZ_ZBUF*0,0/

begin
	fd = IM_PFD(imdes)

	# Get file size.  If writing beyond end of file (file_size+1),
	# write out blocks of zeros until the desired offset is reached.
	# The IM_FILESIZE parameter in the image descriptor is not always
	# up to date, but does provide a lower bound on the size of the pixel
	# storage file.

	if (offset >= IM_FILESIZE(imdes))
	    IM_FILESIZE(imdes) = fstatl (fd, F_FILESIZE)

	if (offset-1 <= IM_FILESIZE(imdes)) {
	    # Write within bounds of file, or at EOF.

	    call seek (fd, offset)
	    call write (fd, buf, nchars)

	} else {
	    # Write beyond EOF.

	    IM_FILESIZE(imdes) = fstatl (fd, F_FILESIZE)
	    start = IM_FILESIZE(imdes) + 1

	    call seek (fd, start)
	    do i = start, offset, SZ_ZBUF
		call write (fd, zbuf, min (SZ_ZBUF, offset-i))

	    call write (fd, buf, nchars)
	    IM_FILESIZE(imdes) = fstatl (fd, F_FILESIZE)
	}
end
