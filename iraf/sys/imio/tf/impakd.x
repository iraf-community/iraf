# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impakd (a, b, npix, dtype)

double	a[npix]
int	b[npix], npix, dtype

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtdu (a, b, npix)
	case TY_SHORT:
	    call achtds (a, b, npix)
	case TY_INT:
	    call achtdi (a, b, npix)
	case TY_LONG:
	    call achtdl (a, b, npix)
	case TY_REAL:
	    call achtdr (a, b, npix)
	case TY_DOUBLE:
	    call achtdd (a, b, npix)
	case TY_COMPLEX:
	    call achtdx (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end
