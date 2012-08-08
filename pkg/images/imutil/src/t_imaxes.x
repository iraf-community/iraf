# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

define	SZ_PARAM	5


# IMAXES -- Determine the number and lengths of the axes of an image.
# Called from CL scripts.  This routine will go away when we get DBIO
# access from the CL.

procedure t_imaxes()

char	imname[SZ_FNAME]
char	param[SZ_PARAM]
int	i
pointer	im
pointer	immap()

begin
	call clgstr ("image", imname, SZ_FNAME)
	im = immap (imname, READ_ONLY, 0)

	call clputi ("ndim", IM_NDIM(im))

	do i = 1, IM_MAXDIM {
	    call sprintf (param, SZ_PARAM, "len%d")
		call pargi (i)
	    call clputl (param, IM_LEN(im,i))
	}

	call imunmap (im)
end
