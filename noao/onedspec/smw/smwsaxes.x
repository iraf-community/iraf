include	<imhdr.h>
include	<mwset.h>
include	<smw.h>


# SMW_SAXES -- Set axes parameters based on previously set dispersion axis.
# If the dispersion axis has been excluded for NDSPEC allow another axis to
# be chosen with a warning.  For EQUISPEC and MULTISPEC require the dispersion
# to be 1 and also to be present.

procedure smw_saxes (smw, smw1, im)

pointer	smw			#I SMW pointer
pointer	smw1			#I Template SMW pointer
pointer	im			#I Template IMIO pointer

int	i, pdim, ldim, paxis, laxis, nw, dtype, nspec
real	smw_c1tranr()
double	w1, dw
pointer	sp, str, axno, axval, r, w, cd, mw, ct, smw_sctran()
int	mw_stati(), imgeti()
bool	streq(), fp_equald()
errchk	smw_sctran

begin
	# If a template SMW pointer is specified just copy the axes parameters.
	if (smw1 != NULL) {
	    call strcpy (Memc[SMW_APID(smw1)], Memc[SMW_APID(smw)], SZ_LINE)
	    SMW_NSPEC(smw) = SMW_NSPEC(smw1)
	    SMW_NBANDS(smw) = SMW_NBANDS(smw1)
	    SMW_TRANS(smw) = SMW_TRANS(smw1)
	    call amovi (SMW_PAXIS(smw1,1), SMW_PAXIS(smw,1), 3)
	    SMW_LDIM(smw) = SMW_LDIM(smw1)
	    call amovi (SMW_LAXIS(smw1,1), SMW_LAXIS(smw,1), 3)
	    call amovi (SMW_LLEN(smw1,1), SMW_LLEN(smw,1), 3)
	    call amovi (SMW_NSUM(smw1,1), SMW_NSUM(smw,1), 2)

	    mw = SMW_MW(smw,0)
	    SMW_PDIM(smw) = mw_stati (mw, MW_NDIM)
	    if (SMW_PDIM(smw) > SMW_PDIM(smw1))
	       do i = SMW_PDIM(smw1)+1, SMW_PDIM(smw)
		   SMW_PAXIS(smw,i) = i

	    return
	}

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (axno, 3, TY_INT)
	call salloc (axval, 3, TY_INT)
	call aclri (Memi[axno], 3)

	# Determine the dimensions.
	mw = SMW_MW(smw,0)
	pdim = mw_stati (mw, MW_NDIM)
	ldim = IM_NDIM(im)
	call mw_gaxmap (mw, Memi[axno], Memi[axval], pdim)

	# Set the physical dispersion axis.
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    call salloc (r, pdim, TY_DOUBLE)
	    call salloc (w, pdim, TY_DOUBLE)
	    call salloc (cd, pdim*pdim, TY_DOUBLE)

	    # Check for a transposed or rotated 2D image.
	    SMW_TRANS(smw) = NO
	    if (pdim == 2) {
		call mw_gltermd (mw, Memd[cd], Memd[w], pdim)
		if (Memd[cd] == 0D0 && Memd[cd+3] == 0D0) {
		    Memd[cd] = Memd[cd+1]
		    Memd[cd+1] = 0.
		    Memd[cd+3] = Memd[cd+2]
		    Memd[cd+2] = 0.
		    call mw_sltermd (mw, Memd[cd], Memd[w], pdim)
		    paxis = SMW_PAXIS(smw,1)
		    if (paxis == 1)
			SMW_PAXIS(smw,1) = 2
		    else
			SMW_PAXIS(smw,1) = 1
		    SMW_TRANS(smw) = YES
		} else if (Memd[cd+1] != 0D0 || Memd[cd+2] != 0D0) {
		    Memd[w] = 0
		    Memd[w+1] = 0
		    Memd[cd] = 1
		    Memd[cd+1] = 0
		    Memd[cd+2] = 0
		    Memd[cd+3] = 1
		    call mw_sltermd (mw, Memd[cd], Memd[w], pdim)
		}
	    }

	    # If the dispersion axis is of length 1 or has been excluded find
	    # the first longer axis and print a warning.

	    paxis = SMW_PAXIS(smw,1)
	    i = max (1, min (pdim, paxis))
	    laxis = max (1, Memi[axno+i-1])
	    if (IM_LEN(im,laxis) == 1)
		do laxis = 1, ldim
		    if (IM_LEN(im,laxis) != 1)
			break

	    # Determine the number of spectra.
	    nspec = 1
	    do i = 1, ldim
		if (i != laxis)
		    nspec = nspec * IM_LEN(im,i)
	    SMW_NSPEC(smw) = nspec
	    SMW_NBANDS(smw) = 1

	    i = paxis
	    do paxis = 1, pdim
		if (Memi[axno+paxis-1] == laxis)
		    break

	    if (i != paxis && nspec > 1) {
		call eprintf (
		    "WARNING: Dispersion axis %d not found. Using axis %d.\n")
		call pargi (i)
		call pargi (paxis)
	    }

	    # Set the dispersion system.
	    call mw_gwtermd (mw, Memd[r], Memd[w], Memd[cd], pdim)
	    if (SMW_TRANS(smw) == YES) {
		Memd[cd] = Memd[cd+1]
		Memd[cd+1] = 0.
		Memd[cd+3] = Memd[cd+2]
		Memd[cd+2] = 0.
	    }
	    if (pdim == 2 && (Memd[cd+1] != 0D0 || Memd[cd+2] != 0D0)) {
		iferr (dtype = imgeti (im, "DC-FLAG"))
		    dtype = DCNO
		if (dtype != DCNO) {
		    call sfree (sp)
		    call error (1,
		    "Rotated, dispersion calibrated spectra are not allowed")
		}
		Memd[r] = 0
		Memd[r+1] = 0
		Memd[w] = 0
		Memd[w+1] = 0
		Memd[cd] = 1
		Memd[cd+1] = 0
		Memd[cd+2] = 0
		Memd[cd+3] = 1
	    }
	    do i = 0, pdim-1 {
		dw = Memd[cd+i*(pdim+1)]
		if (dw == 0D0)
		    Memd[cd+i*(pdim+1)] = 1D0
	    }
	    call mw_swtermd (mw, Memd[r], Memd[w], Memd[cd], pdim)

	    dw = Memd[cd+(paxis-1)*(pdim+1)]
	    w1 = Memd[w+paxis-1] - (Memd[r+paxis-1] - 1) * dw
	    nw = IM_LEN(im,laxis)

	    i = 2 ** (paxis - 1)
	    ct = smw_sctran (smw, "logical", "physical", i)
	    nw = max (smw_c1tranr (ct, 0.5), smw_c1tranr (ct, nw+0.5))
	    call smw_ctfree (ct)

	    iferr (dtype = imgeti (im, "DC-FLAG")) {
		iferr (call mw_gwattrs (mw,paxis,"axtype",Memc[str],SZ_LINE))
		    Memc[str] = EOS
		if (streq (Memc[str], "ra") || streq (Memc[str], "dec"))
		    dtype = DCNO
		else if (fp_equald (1D0, w1) || fp_equald (1D0, dw))
		    dtype = DCNO
		else
		    dtype = DCLINEAR
	    }
	    if (dtype==DCLOG) {
		if (abs(w1)>20. || abs(w1+(nw-1)*dw)>20.)
		    dtype = DCLINEAR
		else {
		    w1 = 10D0 ** w1
		    dw = w1 * (10D0 ** ((nw-1)*dw) - 1) / (nw - 1)
		}
	    }

	    if (dtype != DCNO) {
		
		
		iferr (call mw_gwattrs (mw,paxis,"label",Memc[str],SZ_LINE)) {
		iferr (call mw_gwattrs(mw,paxis,"units",Memc[str],SZ_LINE)) {
			call mw_swattrs (mw, paxis, "units", "angstroms")
			call mw_swattrs (mw, paxis, "label", "Wavelength")
		    }
		}
	    }

	    SMW_DTYPE(smw) = INDEFI
	    call smw_swattrs (smw, 1, 1, INDEFI, INDEFI,
		dtype, w1, dw, nw, 0D0, INDEFR, INDEFR, "")
	case SMW_ES, SMW_MS:
	    paxis = 1
	    i = Memi[axno+1]
	    if (i == 0)
		SMW_NSPEC(smw) = 1
	    else
		SMW_NSPEC(smw) = IM_LEN(im,i)
	    i = Memi[axno+2]
	    if (i == 0)
		SMW_NBANDS(smw) = 1
	    else
		SMW_NBANDS(smw) = IM_LEN(im,i)
	}

	# Check and set the physical and logical dispersion axes.
	laxis = Memi[axno+paxis-1]
	if (laxis == 0) {
	    if (Memi[axval+paxis-1] == 0)
		laxis = paxis
	    else
		call error (1, "No dispersion axis")
	}

	SMW_PDIM(smw) = pdim
	SMW_LDIM(smw) = ldim
	SMW_PAXIS(smw,1) = paxis
	SMW_LAXIS(smw,1) = laxis
	SMW_LLEN(smw,1) = IM_LEN(im,laxis)
	SMW_LLEN(smw,2) = 1
	SMW_LLEN(smw,3) = 1

	# Set the spatial axes.
	i = 2
	do laxis = 1, ldim {
	    if (laxis != SMW_LAXIS(smw,1)) {
		do paxis = 1, pdim
		    if (Memi[axno+paxis-1] == laxis)
			break
		SMW_PAXIS(smw,i) = paxis
		SMW_LAXIS(smw,i) = laxis
		SMW_LLEN(smw,i) = IM_LEN(im,laxis)
		i = i + 1
	    }
	}

	# Set the default title.
	call smw_sapid (smw, 0, 1, IM_TITLE(im))

	call sfree (sp)
end
