include	<mwset.h>
include	<smw.h>


# SMW_OLDMS -- Convert old multispec format into MULTISPEC SMW.

procedure smw_oldms (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U Input MWCS pointer, output SMW pointer

int	i, j, k, nchar, ap, beam, dtype, nw, axes[2]
double	w1, dw, z
real	aplow[2], aphigh[2]
pointer	sp, key, val, lterm, mw, mw_open()
int	imgeti(), mw_stati(), ctoi(), ctor(), ctod(), imofnlu(), imgnfn()
errchk	imgstr, mw_gltermd, mw_sltermd
data	axes/1,2/

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, SZ_LINE, TY_CHAR)
	call salloc (lterm, 12, TY_DOUBLE)

	# Set the basic multispec MWCS
	i = mw_stati (smw, MW_NDIM)
	j = max (2, i)
	mw = mw_open (NULL, j)
	call mw_newsystem (mw, "multispec", j)
	call mw_swtype (mw, axes, 2, "multispec", "")
	if (j > 2)
	    call mw_swtype (mw, 3, 1, "linear", "")
	call mw_gltermd (smw, Memd[lterm+j], Memd[lterm], i)
	if (i == 1) {
	    Memd[lterm+1] = 0.
	    Memd[lterm+3] = 0.
	    Memd[lterm+4] = 0.
	    Memd[lterm+5] = 1.
	}
	call mw_sltermd (mw, Memd[lterm+j], Memd[lterm], j)

	iferr (dtype = imgeti (im, "DC-FLAG"))
	    dtype = -1
	else {
	    call mw_swattrs (mw, 1, "label", "Wavelength")
	    call mw_swattrs (mw, 1, "units", "Angstroms")
	}

	call mw_close (smw)
	smw = mw

	# Set the SMW data structure.
	call smw_open (smw, NULL, im)
	do i = 1, SMW_NSPEC(smw) {
	    call smw_mw (smw, i, 1, mw, j, k)
	    call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
		call pargi (j)
	    call imgstr (im, Memc[key], Memc[val], SZ_LINE)
	    call imdelf (im, Memc[key])

	    k = 1
	    nchar = ctoi (Memc[val], k, ap)
	    nchar = ctoi (Memc[val], k, beam)
	    nchar = ctod (Memc[val], k, w1)
	    nchar = ctod (Memc[val], k, dw)
	    nchar = ctoi (Memc[val], k, nw)
	    if (ctor (Memc[val], k, aplow[1]) == 0)
		aplow[1] = INDEF
	    if (ctor (Memc[val], k, aphigh[1]) == 0)
		aphigh[1] = INDEF
	    z = 0.

	    k = dtype
	    if (k==1 && (abs(w1)>20. || abs(w1+(nw-1)*dw)>20.))
		k = 0
	    call smw_swattrs (smw, i, 1, ap, beam, k, w1, dw, nw, z,
		aplow, aphigh, "")

	    call sprintf (Memc[key], SZ_FNAME, "APID%d")
		call pargi (j)
	    ifnoerr (call imgstr (im, Memc[key], Memc[val], SZ_LINE)) {
		call smw_sapid (smw, i, 1, Memc[val])
		call imdelf (im, Memc[key])
	    }
	}

	# Delete old parameters
	i = imofnlu (im,
	    "DISPAXIS,APFORMAT,BEAM-NUM,DC-FLAG,W0,WPC,NP1,NP2")
	while (imgnfn (i, Memc[key], SZ_FNAME) != EOF) {
	    iferr (call imdelf (im, Memc[key]))
		;
	}
	call imcfnl (i)

	# Update MWCS
	call smw_saveim (smw, im)

	call sfree (sp)
end
