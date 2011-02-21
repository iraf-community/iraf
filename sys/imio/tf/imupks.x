# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMUPK? -- Convert an array of pixels of datatype DTYPE into the datatype
# specified by the IMUPK? suffix character.

procedure imupks (a, b, npix, dtype)

short	b[npix]
int	a[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtus (a, b, npix)
	case TY_SHORT:
	    call achtss (a, b, npix)
	case TY_INT:
	    call achtis (a, b, npix)
	case TY_LONG:
	    call achtls (a, b, npix)
	case TY_REAL:
	    call achtrs (a, b, npix)
	case TY_DOUBLE:
	    call achtds (a, b, npix)
	case TY_COMPLEX:
	    call achtxs (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end


