# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
			       

# FXF_ALTMD -- Scale a double array.

procedure fxf_altmd (a_i, a_l, b, npix, fit_pixtype, bscale, bzero)

int	a_i[ARB]		#I input array (int)
long	a_l[ARB]		#I input array (long)
double	b[ARB]			#O output array
size_t	npix			#I number of pixels
int	fit_pixtype		#I input pixtype of FITS
double	bscale, bzero		#I scaling parameters

long	i

begin
	### int and double are not the same size so if this operation is
	### to allow an in-place conversion it must go right to left instead
	### of left to right.

	if ( fit_pixtype == TY_LONG ) {
	    do i = 1, npix {
		b[i] = a_l[i] * bscale + bzero
	    }
	} else {
	    do i = npix, 1, -1 {
		b[i] = a_i[i] * bscale + bzero
	    }
	}
end
