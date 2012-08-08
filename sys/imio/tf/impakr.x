# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# IMPAK? -- Convert an array of pixels of a specific datatype to the 
# datatype given as the final argument.

procedure impakr (a, b, npix, dtype)

real	a[npix]
int	b[npix], npix, dtype

pointer	bp

begin
	switch (dtype) {
	case TY_USHORT:
	    call achtru (a, b, npix)
	case TY_SHORT:
	    call achtrs (a, b, npix)
	case TY_INT:
	    if (SZ_INT == SZ_INT32)
	        call achtri (a, b, npix)
	    else {
		call malloc (bp, npix, TY_INT)
	        call achtri (a, Memi[bp], npix)
		call ipak32 (Memi[bp], b, npix)
		call mfree (bp, TY_INT)
	    }
	case TY_LONG:
	    if (SZ_INT == SZ_INT32)
	        call achtrl (a, b, npix)
	    else {
		call malloc (bp, npix, TY_LONG)
	        call achtrl (a, Meml[bp], npix)
		call ipak32 (Meml[bp], b, npix)
		call mfree (bp, TY_LONG)
	    }
	case TY_REAL:
	    call achtrr (a, b, npix)
	case TY_DOUBLE:
	    call achtrd (a, b, npix)
	case TY_COMPLEX:
	    call achtrx (a, b, npix)
	default:
	    call error (1, "Unknown datatype in imagefile")
	}
end
