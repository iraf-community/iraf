# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMUPK? -- Convert an array of pixels of datatype DTYPE into the datatype
# specified by the IMUPK? suffix character.

procedure imupki (a, b, npix, dtype)

int	b[npix]
int	a[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtui (a, b, npix)
	case TY_SHORT:
	    call achtsi (a, b, npix)
	case TY_INT:
	    call achtii (a, b, npix)
	case TY_LONG:
	    call achtli (a, b, npix)
	case TY_REAL:
	    call achtri (a, b, npix)
	case TY_DOUBLE:
	    call achtdi (a, b, npix)
	case TY_COMPLEX:
	    call achtxi (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end


