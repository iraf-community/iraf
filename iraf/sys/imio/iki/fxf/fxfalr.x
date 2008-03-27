# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# FXF_ALTMR -- Scale a real array.

procedure fxf_altmr (a, b, npix, bscale, bzero)

int	a[ARB]			#I input array
real	b[ARB]			#O output array
size_t	npix			#I number of pixels
double	bscale, bzero		#I scaling parameters

long	i

begin
	do i = 1, npix
	    b[i] = a[i] * bscale + bzero
end
