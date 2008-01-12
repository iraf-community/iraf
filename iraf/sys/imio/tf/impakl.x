# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impakl (a, b, npix, dtype)

long	a[npix]
int	b[npix], npix, dtype

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtlu (a, b, npix)
	case TY_SHORT:
	    call achtls (a, b, npix)
	case TY_INT:
	    call achtli (a, b, npix)
	case TY_LONG:
	    call achtll (a, b, npix)
	case TY_REAL:
	    call achtlr (a, b, npix)
	case TY_DOUBLE:
	    call achtld (a, b, npix)
	case TY_COMPLEX:
	    call achtlx (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end
