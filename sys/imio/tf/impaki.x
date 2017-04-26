# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impaki (a, b, npix, dtype)

int	a[npix]
int	b[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtiu (a, b, npix)
	case TY_SHORT:
	    call achtis (a, b, npix)
	case TY_INT:
	    if (SZ_INT == SZ_INT32)
	        call achtii (a, b, npix)
	    else {
		call malloc (bp, npix, TY_INT)
	        call achtii (a, Memi[bp], npix)
		call ipak32 (Memi[bp], b, npix)
		call mfree (bp, TY_INT)
	    }
	case TY_LONG:
	    if (SZ_INT == SZ_INT32)
	        call achtil (a, b, npix)
	    else {
		call malloc (bp, npix, TY_LONG)
	        call achtil (a, Meml[bp], npix)
		call ipak32 (Meml[bp], b, npix)
		call mfree (bp, TY_LONG)
	    }
	case TY_REAL:
	    call achtir (a, b, npix)
	case TY_DOUBLE:
	    call achtid (a, b, npix)
	case TY_COMPLEX:
	    call achtix (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end
