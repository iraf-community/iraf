# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMUPK? -- Convert an array of pixels of datatype DTYPE into the datatype
# specified by the IMUPK? suffix character.

procedure imupkl (a, b, npix, dtype)

long	b[npix]
int	a[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtul (a, b, npix)
	case TY_SHORT:
	    call achtsl (a, b, npix)
	case TY_INT:
	    call achtil (a, b, npix)
	case TY_LONG:
	    call achtll (a, b, npix)
	case TY_REAL:
	    call achtrl (a, b, npix)
	case TY_DOUBLE:
	    call achtdl (a, b, npix)
	case TY_COMPLEX:
	    call achtxl (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end


