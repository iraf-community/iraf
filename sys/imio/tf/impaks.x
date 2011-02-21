# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impaks (a, b, npix, dtype)

short	a[npix]
int	b[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtsu (a, b, npix)
	case TY_SHORT:
	    call achtss (a, b, npix)
	case TY_INT:
	    if (SZ_INT == SZ_INT32)
	        call achtsi (a, b, npix)
	    else {
		call malloc (bp, npix, TY_INT)
	        call achtsi (a, Memi[bp], npix)
		call ipak32 (Memi[bp], b, npix)
		call mfree (bp, TY_INT)
	    }
	case TY_LONG:
	    if (SZ_INT == SZ_INT32)
	        call achtsl (a, b, npix)
	    else {
		call malloc (bp, npix, TY_LONG)
	        call achtsl (a, Meml[bp], npix)
		call ipak32 (Meml[bp], b, npix)
		call mfree (bp, TY_LONG)
	    }
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
