include "../lib/daophotdef.h"

# DP_VFUNCTION -- Verify the analytic psf function.

procedure dp_vfunction (dao)

pointer	dao		# pointer to the daophot structure.

int	len
pointer	sp, str, pstr
int	scan(), nscan(), strlen(), dp_fctdecode(), dp_strwrd()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call salloc (pstr, SZ_FNAME, TY_CHAR)

	# Print the current function list.
	len = strlen (DP_FUNCLIST(dao))
	call strcpy (DP_FUNCLIST(dao), Memc[pstr], len - 1)
	call printf ("Analytic psf function(s) (%s): ")
	    call pargstr (Memc[pstr+1])
	call flush (STDOUT)

	# Confirm the PSF function type.
	if (scan() == EOF)
	    ;
	else {
	    call gargstr (Memc[str], SZ_FNAME)
	    if (nscan () != 1)
	        ;
	    else if (dp_fctdecode (Memc[str], Memc[pstr], SZ_FNAME) <= 0)
	        ;
	    else
	        call strcpy (Memc[pstr], DP_FUNCLIST(dao), SZ_FNAME)
	}

	# Print the confirmed function list.
	len = strlen (DP_FUNCLIST(dao))
	call strcpy (DP_FUNCLIST(dao), Memc[pstr], len - 1)
	call printf ( "\tAnalytic psf function(s): %d\n")
	    call pargstr (Memc[pstr+1])

	# Set the function type.
	if (dp_strwrd (1, Memc[str], SZ_FNAME, DP_FUNCLIST(dao)) <= 0)
	    call strcpy ("gauss", DP_FUNCTION(dao), SZ_FNAME)
	else
	    call strcpy (Memc[str], DP_FUNCTION(dao), SZ_FNAME)

	call sfree (sp)
end


# DP_VVARORDER -- Verify the order of variability of the psf function. 

procedure dp_vvarorder (dao)

pointer	dao		# pointer to the daophot structure.

int	varorder
int	scan(), nscan()

begin
	# Confirm that the psf is variable.
	call printf ("Order of variable psf (%d): ")
	    call pargi (DP_VARORDER(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    varorder = DP_VARORDER(dao)
	else {
	    call gargi (varorder)
	    if (nscan () != 1)
	        varorder = DP_VARORDER(dao)
	    else if (varorder < -1 || varorder > 2)
	        varorder = DP_VARORDER(dao)
	}
	DP_VARORDER(dao) = varorder
	call printf ( "\tOrder of variable psf: %d\n")
	    call pargi (varorder)
end


# DP_VFEXPAND -- Verify whether or not to expand the analytics function.

procedure dp_vfexpand (dao)

pointer	dao		# pointer to the daophot structure.

bool	fexpand
bool	itob()
int	scan(), nscan(), btoi()

begin
	# Confirm whether of not to expand the analytic function.
	call printf ("Expand the analytic function (%b): ")
	    call pargb (itob (DP_FEXPAND(dao)))
	call flush (STDOUT)
	if (scan() == EOF)
	    fexpand = itob (DP_FEXPAND(dao))
	else {
	    call gargb (fexpand)
	    if (nscan () != 1)
	        fexpand = itob (DP_FEXPAND(dao))
	}
	DP_FEXPAND(dao) = btoi (fexpand)
	call printf ( "\tExpand analytic fucntion: %b\n")
	    call pargb (fexpand)
end


# DP_VSATURATED -- Verify whether or not to use saturated stars in the
# psf computation.

procedure dp_vsaturated (dao)

pointer	dao		# pointer to the daophot structure.

bool	saturated
bool	itob()
int	scan(), nscan(), btoi()

begin
	# Confirm whether of not to use saturated psf stars.
	call printf ("Use saturated psf stars (%b): ")
	    call pargb (itob (DP_SATURATED(dao)))
	call flush (STDOUT)
	if (scan() == EOF)
	    saturated = itob (DP_SATURATED(dao))
	else {
	    call gargb (saturated)
	    if (nscan () != 1)
	        saturated = itob (DP_SATURATED(dao))
	}
	DP_SATURATED(dao) = btoi (saturated)
	call printf ( "\tUse saturated psf stars: %b\n")
	    call pargb (saturated)
end


# DP_VFWHMPSF -- Confirm the fwhm of the psf.

procedure dp_vfwhmpsf (dao)

pointer	dao		# pointer to the daophot structure

real	sfwhmpsf, fwhmpsf
int	scan(), nscan()

begin
	# Confirm the psf radius.
	call printf ( "Fwhm of psf in scale units (%g): ")
	    call pargr (DP_SFWHMPSF(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    sfwhmpsf = DP_SFWHMPSF(dao)
	else {
	    call gargr (sfwhmpsf)
	    if (nscan () != 1)
	        sfwhmpsf = DP_SFWHMPSF(dao)
	}
	fwhmpsf = sfwhmpsf / DP_SCALE(dao)

	DP_SFWHMPSF(dao) = sfwhmpsf
	DP_FWHMPSF(dao) = fwhmpsf

	call printf ( "\tNew fwhm: %g scale units %g pixels\n")
	    call pargr (sfwhmpsf)
	    call pargr (fwhmpsf)
end


# DP_VPSFRAD -- Confirm the psf radius.

procedure dp_vpsfrad (dao)

pointer	dao		# pointer to the daophot structure

real	rpsfrad, psfrad
int	scan(), nscan()

begin
	# Confirm the psf radius.
	call printf ( "Psf radius in scale units (%g): ")
	    call pargr (DP_RPSFRAD(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    rpsfrad = DP_RPSFRAD(dao)
	else {
	    call gargr (rpsfrad)
	    if (nscan () != 1)
	        rpsfrad = DP_RPSFRAD(dao)
	}
	psfrad = rpsfrad / DP_SCALE(dao)

	DP_RPSFRAD(dao) = rpsfrad
	DP_SPSFRAD(dao) = rpsfrad
	DP_PSFRAD(dao) = psfrad

	call printf ( "\tNew psf radius: %g scale units %g pixels\n")
	    call pargr (rpsfrad)
	    call pargr (psfrad)
end


# DP_VFITRAD -- Confirm the fitting radius.

procedure dp_vfitrad (dao)

pointer	dao		# pointer to the daophot structures

real	sfitrad, fitrad
int	scan(), nscan()

begin
	# Confirm the fitting radius.
	call printf ( "Fitting radius in scale units (%g): ")
	    call pargr (DP_SFITRAD(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    sfitrad = DP_SFITRAD(dao)
	else {
	    call gargr (sfitrad)
	    if (nscan () != 1)
	        sfitrad = DP_SFITRAD(dao)
	}
	fitrad = sfitrad / DP_SCALE(dao)

	DP_SFITRAD(dao) = sfitrad
	DP_FITRAD(dao) = fitrad

	call printf ( "\tNew fitting radius: %g scale units %g pixels\n")
	    call pargr (sfitrad)
	    call pargr (fitrad)
end


# DP_VMATCHRAD -- Confirm the matching radius.

procedure dp_vmatchrad (dao)

pointer	dao		# pointer to the daophot structure

real	smatchrad, matchrad
int	scan(), nscan()

begin
	# Confirm the matching radius.
	call printf ( "Matching radius in scale units (%g): ")
	    call pargr (DP_SMATCHRAD(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    smatchrad = DP_SMATCHRAD(dao)
	else {
	    call gargr (smatchrad)
	    if (nscan () != 1)
	        smatchrad = DP_SMATCHRAD(dao)
	}
	matchrad = smatchrad / DP_SCALE(dao)

	DP_SMATCHRAD(dao) = smatchrad
	DP_MATCHRAD(dao) = matchrad

	call printf ( "\tNew matching radius: %g scale units %g pixels\n")
	    call pargr (smatchrad)
	    call pargr (matchrad)
end


# DP_VMERGERAD -- Confirm the merging radius.

procedure dp_vmergerad (dao)

pointer	dao		# pointer to the daophot structure

real	smergerad, mergerad
int	scan(), nscan()

begin
	# Confirm the merging radius.
	call printf ( "Merging radius in scale units (%g): ")
	    call pargr (DP_SMERGERAD(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    smergerad = DP_SMERGERAD(dao)
	else {
	    call gargr (smergerad)
	    if (nscan () != 1)
	        smergerad = DP_SMERGERAD(dao)
	}
	if (IS_INDEFR(smergerad))
	    mergerad = INDEFR
	else
	    mergerad = smergerad / DP_SCALE(dao)

	DP_SMERGERAD(dao) = smergerad
	DP_MERGERAD(dao) = mergerad

	call printf ( "\tNew merging radius: %g scale units %g pixels\n")
	    call pargr (smergerad)
	    call pargr (mergerad)
end


# DP_VFITRAD -- Confirm the fitting radius.


# DP_VDATAMIN-- Verify the minimum good data value.

procedure dp_vdatamin (dao)

pointer	dao		# pointer to the daophot structure

real	datamin
int	scan(), nscan()

begin
	# Confirm the threshold parameter.
	call printf ("Minimum good data value (%g) (CR or value): ")
	    call pargr (DP_MINGDATA(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    datamin = DP_MINGDATA(dao)
	else {
	    call gargr (datamin)
	    if (nscan () != 1)
	        datamin = DP_MINGDATA(dao)
	}
	DP_MINGDATA(dao) = datamin

	call printf ("\tNew minimum good data value: %g counts\n")
	    call pargr (datamin)
end


# DP_VDATAMAX-- Verify the maximum good data value.

procedure dp_vdatamax (dao)

pointer	dao		# pointer to the daophot structure

real	datamax
int	scan(), nscan()

begin
	# Confirm the threshold parameter.
	call printf ("Maximum good data value (%g) (CR or value): ")
	    call pargr (DP_MAXGDATA(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    datamax = DP_MAXGDATA(dao)
	else {
	    call gargr (datamax)
	    if (nscan () != 1)
	        datamax = DP_MAXGDATA(dao)
	}
	DP_MAXGDATA(dao) = datamax

	call printf ("\tNew maximum good data value: %g counts\n")
	    call pargr (datamax)
end


# DP_VMAXGROUP -- Verify the maximum group size.

procedure dp_vmaxgroup (dao)

pointer	dao		# pointer to the daophot strucuture

int	maxgroup
int	scan(), nscan()

begin
	call printf ( "Maximum group size in number of stars (%d): ")
	    call pargi (DP_MAXGROUP(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    maxgroup = DP_MAXGROUP(dao)
	else {
	    call gargi (maxgroup)
	    if (nscan () != 1)
	        maxgroup = DP_MAXGROUP(dao)
	}
	DP_MAXGROUP(dao) = maxgroup

	call printf ( "\tNew maximum group size: %d stars\n")
	    call pargi (maxgroup)
end


# DP_VCRITSNRATIO -- Verify the critical signal-to-noise ratio.

procedure dp_vcritnsratio (dao)

pointer	dao		# pointer to the daophot structure

real	critsnratio
int	scan(), nscan()

begin
	# Confirm the critical signal-to-noise ratio.
	call printf ( "Critical S/N ratio in stdevs per pixel (%g): ")
	    call pargr (DP_CRITSNRATIO(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    critsnratio = DP_CRITSNRATIO(dao)
	else {
	    call gargr (critsnratio)
	    if (nscan () != 1)
	        critsnratio = DP_CRITSNRATIO(dao)
	}
	DP_CRITSNRATIO(dao) = critsnratio

	call printf ( "\tNew critical S/N ratio: %g stdevs per pixel\n")
	    call pargr (critsnratio)
end


# DP_VRECENTER -- Verify whether or not to recenter the stars.

procedure dp_vrecenter (dao)

pointer	dao		# pointer to the daophot structure.

bool	recenter
bool	itob()
int	scan(), nscan(), btoi()

begin
	# Confirm whether of not to recenter the stars.
	call printf ("Recenter the stars (%b): ")
	    call pargb (itob (DP_RECENTER(dao)))
	call flush (STDOUT)
	if (scan() == EOF)
	    recenter = itob (DP_RECENTER(dao))
	else {
	    call gargb (recenter)
	    if (nscan () != 1)
	        recenter = itob (DP_RECENTER(dao))
	}
	DP_RECENTER(dao) = btoi (recenter)
	call printf ( "\tRecenter the stars: %b\n")
	    call pargb (recenter)
end


# DP_VFITSKY -- Verify whether or not to refit the sky value.

procedure dp_vfitsky (dao)

pointer	dao		# pointer to the daophot structure.

bool	fitsky
bool	itob()
int	scan(), nscan(), btoi()

begin
	# Confirm whether of not to refit the sky.
	call printf ("Refit the sky (%b): ")
	    call pargb (itob (DP_FITSKY(dao)))
	call flush (STDOUT)
	if (scan() == EOF)
	    fitsky = itob (DP_FITSKY(dao))
	else {
	    call gargb (fitsky)
	    if (nscan () != 1)
	        fitsky = itob (DP_FITSKY(dao))
	}
	DP_FITSKY(dao) = btoi (fitsky)
	call printf ( "\tRefit the sky: %b\n")
	    call pargb (fitsky)
end


# DP_VGROUPSKY -- Verify whether or not to fit group sky values.

procedure dp_vgroupsky (dao)

pointer	dao		# pointer to the daophot structure

bool	groupsky
bool	itob()
int	scan(), nscan(), btoi()

begin
	# Confirm whether of not to use group sky values.
	call printf ("Use group sky values (%b): ")
	    call pargb (itob (DP_GROUPSKY(dao)))
	call flush (STDOUT)
	if (scan() == EOF)
	    groupsky = itob (DP_GROUPSKY(dao))
	else {
	    call gargb (groupsky)
	    if (nscan () != 1)
	        groupsky = itob (DP_GROUPSKY(dao))
	}
	DP_GROUPSKY(dao) = btoi (groupsky)
	call printf ( "\tUse group sky values: %b\n")
	    call pargb (groupsky)
end


# DP_VSANNULUS -- Confirm the inner radius of the sky fitting annulus.

procedure dp_vsannulus (dao)

pointer	dao		# pointer to the daophot structure

real	sannulus, annulus
int	scan(), nscan()

begin
	# Confirm the inner radius of the sky annulus.
	call printf ( "Inner radius of sky annulus in scale units (%g): ")
	    call pargr (DP_SANNULUS(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    sannulus = DP_SANNULUS(dao)
	else {
	    call gargr (sannulus)
	    if (nscan () != 1)
	        sannulus = DP_SANNULUS(dao)
	}
	annulus = sannulus / DP_SCALE(dao)

	DP_SANNULUS(dao) = sannulus
	DP_ANNULUS(dao) = annulus

	call printf ( "\tNew inner radius: %g scale units %g pixels\n")
	    call pargr (sannulus)
	    call pargr (annulus)
end


# DP_VWSANNULUS -- Confirm the width of the sky fitting annulus.

procedure dp_vwsannulus (dao)

pointer	dao		# pointer to the daophot structure

real	sdannulus, dannulus
int	scan(), nscan()

begin
	# Confirm the inner radius of the sky annulus.
	call printf ( "Width of sky annulus in scale units (%g): ")
	    call pargr (DP_SDANNULUS(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    sdannulus = DP_SDANNULUS(dao)
	else {
	    call gargr (sdannulus)
	    if (nscan () != 1)
	        sdannulus = DP_SDANNULUS(dao)
	}
	dannulus = sdannulus / DP_SCALE(dao)

	DP_SDANNULUS(dao) = sdannulus
	DP_DANNULUS(dao) = dannulus

	call printf ( "\tNew annulus width: %g scale units %g pixels\n")
	    call pargr (sdannulus)
	    call pargr (dannulus)
end
