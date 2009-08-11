include	<mwset.h>
include	<smw.h>


# SMW_OLDMS -- Convert old multispec format into MULTISPEC SMW.

procedure smw_oldms (im, smw)

pointer	im			#I IMIO pointer
pointer	smw			#U Input MWCS pointer, output SMW pointer

size_t	sz_val
long	ap, nw, i, j, k, c_1
int	nchar, beam, dtype, ii, ij, ik, axes[2]
double	w1, dw, z
real	aplow[2], aphigh[2]
pointer	sp, key, val, lterm, mw, pp
pointer	mw_open(), imofnlu()
int	imgeti(), mw_stati(), ctoi(), ctol(), ctor(), ctod(), imgnfn()
errchk	imgstr, mw_gltermd, mw_sltermd
data	axes/1,2/

include	<nullptr.inc>

begin
	c_1 = 1

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (key, sz_val, TY_CHAR)
	sz_val = SZ_LINE
	call salloc (val, sz_val, TY_CHAR)
	sz_val = 12
	call salloc (lterm, sz_val, TY_DOUBLE)

	# Set the basic multispec MWCS
	ii = mw_stati (smw, MW_NDIM)
	ij = max (2, ii)
	mw = mw_open (NULLPTR, ij)
	call mw_newsystem (mw, "multispec", ij)
	call mw_swtype (mw, axes, 2, "multispec", "")
	if (ij > 2)
	    call mw_swtype (mw, 3, 1, "linear", "")
	call mw_gltermd (smw, Memd[lterm+ij], Memd[lterm], ii)
	if (ii == 1) {
	    Memd[lterm+1] = 0.
	    Memd[lterm+3] = 0.
	    Memd[lterm+4] = 0.
	    Memd[lterm+5] = 1.
	}
	call mw_sltermd (mw, Memd[lterm+ij], Memd[lterm], ij)

	iferr (dtype = imgeti (im, "DC-FLAG"))
	    dtype = -1
	else {
	    call mw_swattrs (mw, 1, "label", "Wavelength")
	    call mw_swattrs (mw, 1, "units", "Angstroms")
	}

	call mw_close (smw)
	smw = mw

	# Set the SMW data structure.
	call smw_open (smw, NULLPTR, im)
	do i = 1, SMW_NSPEC(smw) {
	    call smw_mw (smw, i, c_1, mw, j, k)
	    call sprintf (Memc[key], SZ_FNAME, "APNUM%d")
		call pargl (j)
	    call imgstr (im, Memc[key], Memc[val], SZ_LINE)
	    call imdelf (im, Memc[key])

	    ik = 1
	    nchar = ctol (Memc[val], ik, ap)
	    nchar = ctoi (Memc[val], ik, beam)
	    nchar = ctod (Memc[val], ik, w1)
	    nchar = ctod (Memc[val], ik, dw)
	    nchar = ctol (Memc[val], ik, nw)
	    if (ctor (Memc[val], ik, aplow[1]) == 0)
		aplow[1] = INDEF
	    if (ctor (Memc[val], ik, aphigh[1]) == 0)
		aphigh[1] = INDEF
	    z = 0.

	    ik = dtype
	    if (ik==1 && (dabs(w1)>20. || dabs(w1+(nw-1)*dw)>20.))
		ik = 0
	    call smw_swattrs (smw, i, c_1, ap, beam, ik, w1, dw, nw, z,
		aplow, aphigh, "")

	    call sprintf (Memc[key], SZ_FNAME, "APID%d")
		call pargl (j)
	    ifnoerr (call imgstr (im, Memc[key], Memc[val], SZ_LINE)) {
		call smw_sapid (smw, i, c_1, Memc[val])
		call imdelf (im, Memc[key])
	    }
	}

	# Delete old parameters
	pp = imofnlu (im,
	    "DISPAXIS,APFORMAT,BEAM-NUM,DC-FLAG,W0,WPC,NP1,NP2")
	while (imgnfn (pp, Memc[key], SZ_FNAME) != EOF) {
	    iferr (call imdelf (im, Memc[key]))
		;
	}
	call imcfnl (pp)

	# Update MWCS
	call smw_saveim (smw, im)

	call sfree (sp)
end
