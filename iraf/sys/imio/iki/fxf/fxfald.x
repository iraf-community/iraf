# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
			       

# FXF_ALTMD -- Scale a double array.

procedure fxf_altmd (a, b, npix, bscale, bzero)

int	a[ARB]			#I input array
double	b[ARB]			#O output array
int	npix			#I number of pixels
double	bscale, bzero		#I scaling parameters

int	i

begin
	### int and double are not the same size so if this operation is
	### to allow an in-place conversion it must go right to left instead
	### of left to right.

	do i = npix, 1, -1
	    b[i] = a[i] * bscale + bzero
end
