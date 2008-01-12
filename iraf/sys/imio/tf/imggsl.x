# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

# IMGGS? -- Get a general section.

pointer procedure imggsl (imdes, vs, ve, ndim)

pointer	imdes
long	vs[IM_MAXDIM], ve[IM_MAXDIM]
int	ndim
long	totpix
pointer	bp, imggsc()
errchk	imggsc

begin
	bp = imggsc (imdes, vs, ve, ndim, TY_LONG, totpix)
	if (IM_PIXTYPE(imdes) != TY_LONG)
	    call imupkl (Meml[bp], Meml[bp], totpix, IM_PIXTYPE(imdes))
	return (bp)
end
