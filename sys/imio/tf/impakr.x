# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impakr (a, b, npix, dtype)

real	a[npix]
int	b[npix], npix, dtype

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtru (a, b, npix)
	case TY_SHORT:
	    call achtrs (a, b, npix)
	case TY_INT:
	    call achtri (a, b, npix)
	case TY_LONG:
	    call achtrl (a, b, npix)
	case TY_REAL:
	    call achtrr (a, b, npix)
	case TY_DOUBLE:
	    call achtrd (a, b, npix)
	case TY_COMPLEX:
	    call achtrx (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end
