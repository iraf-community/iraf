include "../lib/daophotdef.h"

# DP_VVARPSF -- Verify that the psf is variable.

procedure dp_vvarpsf (dao)

pointer	dao		# pointer to the daophot structure.

bool	varpsf
bool	itob()
int	scan(), nscan(), btoi()

begin
	# Confirm that the psf is variable.
	call printf ("Variable psf function (%b): ")
	    call pargb (itob (DP_VARPSF(dao)))
	call flush (STDOUT)
	if (scan() == EOF)
	    varpsf = itob (DP_VARPSF(dao))
	else {
	    call gargb (varpsf)
	    if (nscan () != 1)
	        varpsf = itob (DP_VARPSF(dao))
	}
	DP_VARPSF(dao) = btoi (varpsf)
	call printf ( "\tVariable psf: %b\n")
	    call pargb (varpsf)
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


# DP_VCRITOVLAP -- Verify the critical overlap.

procedure dp_vcritovlap (dao)

pointer	dao		# pointer to the daophot structure

real	critovlap
int	scan(), nscan()

begin
	# Confirm the critical overlap.
	call printf ( "Critical overlap in stdevs per pixel (%g): ")
	    call pargr (DP_CRITOVLAP(dao))
	call flush (STDOUT)
	if (scan() == EOF)
	    critovlap = DP_CRITOVLAP(dao)
	else {
	    call gargr (critovlap)
	    if (nscan () != 1)
	        critovlap = DP_CRITOVLAP(dao)
	}
	DP_CRITOVLAP(dao) = critovlap

	call printf ( "\tNew critical overlap: %g stdevs per pixel\n")
	    call pargr (critovlap)
end
