# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMUPK? -- Convert an array of pixels of datatype DTYPE into the datatype
# specified by the IMUPK? suffix character.

procedure imupkr (a, b, npix, dtype)

real	b[npix]
int	a[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtur (a, b, npix)
	case TY_SHORT:
	    call achtsr (a, b, npix)
	case TY_INT:
	    call achtir (a, b, npix)
	case TY_LONG:
	    call achtlr (a, b, npix)
	case TY_REAL:
	    call achtrr (a, b, npix)
	case TY_DOUBLE:
	    call achtdr (a, b, npix)
	case TY_COMPLEX:
	    call achtxr (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end


