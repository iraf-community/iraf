# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impakx (a, b, npix, dtype)

complex	a[npix]
int	b[npix], npix, dtype

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtxu (a, b, npix)
	case TY_SHORT:
	    call achtxs (a, b, npix)
	case TY_INT:
	    call achtxi (a, b, npix)
	case TY_LONG:
	    call achtxl (a, b, npix)
	case TY_REAL:
	    call achtxr (a, b, npix)
	case TY_DOUBLE:
	    call achtxd (a, b, npix)
	case TY_COMPLEX:
	    call achtxx (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end
