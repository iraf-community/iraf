# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impaks (a, b, npix, dtype)

short	a[npix]
int	b[npix], npix, dtype

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtsu (a, b, npix)
	case TY_SHORT:
	    call achtss (a, b, npix)
	case TY_INT:
	    call achtsi (a, b, npix)
	case TY_LONG:
	    call achtsl (a, b, npix)
	case TY_REAL:
	    call achtsr (a, b, npix)
	case TY_DOUBLE:
	    call achtsd (a, b, npix)
	case TY_COMPLEX:
	    call achtsx (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end
