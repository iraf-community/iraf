include	<error.h>
include	<smw.h>


# SMW_SCTRAN -- Set the SMW coordinate system transformation.
# This routine sets up the SMW_CT structure which may consist of multiple
# pointers if the MWCS is split.  If the MWCS is not split then the MWCS
# transformation routine is used directly.  However if the MWCS is split then
# there may need to be an intermediate mapping from the input coordinate to
# the physical coordinate in which the split MWCS is defined as well as a
# transformation for each WCS piece.

pointer procedure smw_sctran (smw, system1, system2, axbits)

pointer	smw		#I SMW pointer
char	system1[ARB]	#I Input coordinate system
char	system2[ARB]	#I Output coordinate system
int	axbits		#I Bitmap defining axes to be transformed
pointer	ct		#O SMW CT pointer

int	i, cttype, nct, axes[3], naxes, strdic()
pointer	mw_sctran()
errchk	mw_sctran

begin
	# Determine the coordinate transformation type and setup the structure.
	cttype = 10 * (strdic (system1, system1, ARB, SMW_CTTYPES) + 1) +
	    strdic (system2, system2, ARB, SMW_CTTYPES) + 1

	nct = SMW_NMW(smw)
	if (cttype == SMW_LP || cttype == SMW_PL)
	    nct = 1

	call calloc (ct, SMW_CTLEN(nct), TY_STRUCT)
	SMW_SMW(ct) = smw
	SMW_CTTYPE(ct) = cttype
	SMW_NCT(ct) = nct

	# Determine dispersion and aperture axes.
	call mw_gaxlist (SMW_MW(smw,0), axbits, axes, naxes)
	do i = 1, naxes {
	    if (axes[i] == SMW_PAXIS(smw,1))
		SMW_DAXIS(ct) = i
	    if (axes[i] == SMW_PAXIS(smw,2))
		SMW_AAXIS(ct) = i
	}

	# If the MWCS is not split use the MWCS transformation directly.
	if (nct == 1) {
	    iferr (SMW_CT(ct,0) = mw_sctran (SMW_MW(smw,0), system1, system2,
		axbits)) {
		switch (cttype) {
		case SMW_WL, SMW_WP:
	            SMW_CT(ct,0) = mw_sctran (SMW_MW(smw,0), "physical",
			system2, axbits)
		case SMW_LW, SMW_PW:
	            SMW_CT(ct,0) = mw_sctran (SMW_MW(smw,0), system1,
			"physical", axbits)
		default:
		    call erract (EA_ERROR)
		}
	    }
	    return(ct)
	}

	# If there is a split MWCS then setup the intermediary transformation.
	switch (cttype) {
	case SMW_LW:
	    SMW_CTL(ct) = mw_sctran (SMW_MW(smw,0), system1, "physical",
		axbits)
	    do i = 0, nct-1 {
		iferr (SMW_CT(ct,i) = mw_sctran (SMW_MW(smw,i), "physical",
		    system2, axbits))
		    SMW_CT(ct,i) = mw_sctran (SMW_MW(smw,i), "physical",
			"physical", axbits)
	    }
	case SMW_WL:
	    do i = 0, nct-1 {
		iferr (SMW_CT(ct,i) = mw_sctran (SMW_MW(smw,i), system1,
		    "physical", axbits))
		    SMW_CT(ct,i) = mw_sctran (SMW_MW(smw,i), "physical",
			"physical", axbits)
	    }
	    SMW_CTL(ct) = mw_sctran (SMW_MW(smw,0), "physical", system2,
		axbits)
	case SMW_PW, SMW_WP:
	    do i = 0, nct-1 {
		iferr (SMW_CT(ct,i) = mw_sctran (SMW_MW(smw,i), system1,
		    system2, axbits))
		    SMW_CT(ct,i) = mw_sctran (SMW_MW(smw,i), "physical",
			system2, axbits)
	    }
	}

	return (ct)
end
