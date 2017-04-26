# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impakl (a, b, npix, dtype)

long	a[npix]
int	b[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtlu (a, b, npix)
	case TY_SHORT:
	    call achtls (a, b, npix)
	case TY_INT:
	    if (SZ_INT == SZ_INT32)
	        call achtli (a, b, npix)
	    else {
		call malloc (bp, npix, TY_INT)
	        call achtli (a, Memi[bp], npix)
		call ipak32 (Memi[bp], b, npix)
		call mfree (bp, TY_INT)
	    }
	case TY_LONG:
	    if (SZ_INT == SZ_INT32)
	        call achtll (a, b, npix)
	    else {
		call malloc (bp, npix, TY_LONG)
	        call achtll (a, Meml[bp], npix)
		call ipak32 (Meml[bp], b, npix)
		call mfree (bp, TY_LONG)
	    }
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
