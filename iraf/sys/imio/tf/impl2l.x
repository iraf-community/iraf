# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<imhdr.h>
include	<imio.h>

# IMPL2? -- Put a line to an apparently two dimensional image.  If there
# is only one input buffer, no image section, we are not referencing out of
# bounds, and no datatype conversion needs to be performed, directly access
# the pixels to reduce the overhead per line.

pointer procedure impl2l (im, linenum)

pointer	im			# image header pointer
int	linenum			# line to be written

int	fd, nchars
long	vs[2], ve[2], offset
pointer	bp, impgsl(), fwritep()
errchk	imopsf, imerr

begin
	repeat {
	    if (IM_FAST(im) == YES && IM_PIXTYPE(im) == TY_LONG) {
		fd = IM_PFD(im)
		if (fd == NULL) {
		    call imopsf (im)
		    next
		}
		if (linenum < 1 || linenum > IM_LEN(im,2))
		    call imerr (IM_NAME(im), SYS_IMREFOOB)

		offset = (linenum - 1) * IM_PHYSLEN(im,1) * SZ_LONG +
		    IM_PIXOFF(im)
		nchars = IM_PHYSLEN(im,1) * SZ_LONG
		ifnoerr (bp = (fwritep (fd, offset, nchars) - 1) / SZ_LONG + 1)
		    return (bp)
	    }

	    vs[1] = 1
	    ve[1] = IM_LEN(im,1)
	    vs[2] = linenum
	    ve[2] = linenum

	    return (impgsl (im, vs, ve, 2))
	}
end
