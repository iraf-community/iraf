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

char	zbuf[SZ_ZBUF]
int	fd
long	pos
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

	if (IM_FILESIZE(imdes) == 0)
	    IM_FILESIZE(imdes) = fstatl (fd, F_FILESIZE)

	if (offset-1 <= IM_FILESIZE(imdes))
	    call seek (fd, offset)
	else {
	    IM_FILESIZE(imdes) = fstatl (fd, F_FILESIZE)
	    do pos = IM_FILESIZE(imdes)+1, offset, SZ_ZBUF
		call write (fd, zbuf, min (SZ_ZBUF, offset-pos))
	}

	call write (fd, buf, nchars)
end
