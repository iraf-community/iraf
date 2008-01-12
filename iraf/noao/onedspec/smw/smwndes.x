include	<imhdr.h>
include	<smw.h>


# SMW_NDES -- Convert NDSPEC WCS into EQUISPEC WCS.
# This requires that the logical dispersion axis be 1.

procedure smw_ndes (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U Input NDSPEC SMW, output EQUISPEC SMW

int	i, pdim1, pdim2, daxis, ap, beam, dtype, nw, axes[2]
real	aplow[2], aphigh[2]
double	w1, dw, z
pointer	sp, key, str, lterm1, lterm2, coeff, mw1, mw2, mw_open()
errchk	mw_open, mw_gltermd, mw_gwtermd, smw_open, smw_saxes, smw_gwattrs
data	axes/1,2/, coeff/NULL/

begin
	# Require the dispersion to be along the first logical axis.
	if (SMW_LAXIS(smw,1) != 1)
	    return

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (lterm1, 15, TY_DOUBLE)
	call salloc (lterm2, 15, TY_DOUBLE)

	# Set the MWCS.  Only the logical and world transformations along
	# the dispersion axis are transfered.

	pdim1 = SMW_PDIM(smw)
	pdim2 = IM_NDIM(im)
	daxis = SMW_PAXIS(smw,1)
	mw1 = SMW_MW(smw,0)

	mw2 = mw_open (NULL, pdim2)
	call mw_newsystem (mw2, "equispec", pdim2)
	call mw_swtype (mw2, axes, pdim2, "linear", "")
	ifnoerr (call mw_gwattrs (mw1, daxis, "label", Memc[str], SZ_LINE))
	    call mw_swattrs (mw2, 1, "label", Memc[str])
	ifnoerr (call mw_gwattrs (mw1, daxis, "units", Memc[str], SZ_LINE))
	    call mw_swattrs (mw2, 1, "units", Memc[str])
	ifnoerr (call mw_gwattrs (mw1, daxis, "units_display", Memc[str],
	    SZ_LINE))
	    call mw_swattrs (mw2, 1, "units_display", Memc[str])

	call mw_gltermd (mw1, Memd[lterm1+pdim1], Memd[lterm1], pdim1)
	call mw_gltermd (mw2, Memd[lterm2+pdim2], Memd[lterm2], pdim2)
	Memd[lterm2] = Memd[lterm1+daxis-1]
	Memd[lterm2+pdim2] = Memd[lterm1+pdim1+(pdim1+1)*(daxis-1)]
	call mw_sltermd (mw2, Memd[lterm2+pdim2], Memd[lterm2], pdim2)

	call mw_gwtermd (mw1, Memd[lterm1], Memd[lterm1+pdim1],
	     Memd[lterm1+2*pdim1], pdim1)
	call mw_gwtermd (mw2, Memd[lterm2], Memd[lterm2+pdim2],
	     Memd[lterm2+2*pdim2], pdim2)
	Memd[lterm2] = Memd[lterm1+daxis-1]
	Memd[lterm2+pdim2] = Memd[lterm1+pdim1+daxis-1]
	Memd[lterm2+2*pdim2] = Memd[lterm1+2*pdim1+(pdim1+1)*(daxis-1)]
	call mw_swtermd (mw2, Memd[lterm2], Memd[lterm2+pdim2],
	     Memd[lterm2+2*pdim2], pdim2)

	# Set the EQUISPEC SMW.
	IM_LEN(im,2) = SMW_NSPEC(smw)
	IM_LEN(im,3) = SMW_NBANDS(smw)
	call smw_open (mw2, NULL, im)
	do i = 1, SMW_NSPEC(smw) {
	    call smw_gwattrs (smw, i, 1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, coeff)
	    call smw_swattrs (mw2, i, 1, ap, beam, dtype, w1, dw, nw, z,
		aplow, aphigh, Memc[coeff])
	}
	call mfree (coeff, TY_CHAR)

	call smw_close (smw)
	smw = mw2

	call sfree (sp)
end
