# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# WCS_GFTERM -- Compute the output FITS CRPIX, CRVAL, and CD arrays from the
# MWCS LTERM and WTERM. Note that the CD matrix terms are still transposed
# from the usual Fortran order.

procedure wcs_gfterm (mw, crpix, crval, cd, ndim)

pointer mw              			#i the input mwcs pointer
double  crpix[ndim]     			#o the output FITS CRPIX array
double  crval[ndim]     			#o the output FITS CRVAL array
double  cd[ndim,ndim]   			#o the output FITS CD matrix
int     ndim            			#i the dimensionality of the wcs

pointer sp, r, wcd, ltv, ltm, iltm
pointer	alert, errmsg
int	i, errcode

int	errget()

errchk	mw_gwtermd, mw_gltermd

begin
	call smark (sp)
	call salloc (r, ndim, TY_DOUBLE)
	call salloc (wcd, ndim * ndim, TY_DOUBLE)
	call salloc (ltv, ndim, TY_DOUBLE)
	call salloc (ltm, ndim * ndim, TY_DOUBLE)
	call salloc (iltm, ndim * ndim, TY_DOUBLE)

	iferr {
	    call mw_gwtermd (mw, Memd[r], crval, Memd[wcd], ndim)
	    call mw_gltermd (mw, Memd[ltm], Memd[ltv], ndim)
	    call mwvmuld (Memd[ltm], Memd[r], crpix, ndim)
	    call aaddd (crpix, Memd[ltv], crpix, ndim)
	    call mwinvertd (Memd[ltm], Memd[iltm], ndim)
	    call mwmmuld (Memd[wcd], Memd[iltm], cd, ndim)

	} then {
	    call salloc (alert, SZ_LINE, TY_CHAR)
	    call salloc (errmsg, SZ_LINE, TY_CHAR)

	    # Set up a default value.
	    call aclrd (cd, ndim*ndim)
	    for (i=1; i <= ndim; i=i+1) {
	        crpix[i] = 1.0d0
	        crval[i] = 1.0d0
	        cd[i,i] = 1.0d0
	    }

            # Send alert to the GUI.
	    errcode = errget (Memc[errmsg], SZ_LINE)
	    call sprintf (Memc[alert], SZ_FNAME, "%s\n\"%s\"")
		call pargstr ("Error decoding image WCS:")
		call pargstr (Memc[errmsg])
            call ism_alert (Memc[alert], "", "")
	}

	call sfree (sp)
end
