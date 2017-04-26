# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMUPK? -- Convert an array of pixels of datatype DTYPE into the datatype
# specified by the IMUPK? suffix character.

procedure imupkd (a, b, npix, dtype)

double	b[npix]
int	a[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtud (a, b, npix)
	case TY_SHORT:
	    call achtsd (a, b, npix)
	case TY_INT:
	    call achtid (a, b, npix)
	case TY_LONG:
	    call achtld (a, b, npix)
	case TY_REAL:
	    call achtrd (a, b, npix)
	case TY_DOUBLE:
	    call achtdd (a, b, npix)
	case TY_COMPLEX:
	    call achtxd (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end


