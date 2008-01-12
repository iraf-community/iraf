include	"sensfunc.h"


# T_SENSFUNC -- Determine sensitivities and residual extinctions.
# The input is a file of standard star produced by the task STANDARD.
# The input data is read into an array of structures, one per standard
# star.  The stars common to the aperture to be fit are flagged
# and then the data is passed to the main routine SF_SENSFUNC.
# This routine determines a sensitivity curve for the aperture which
# is output by the procedure as well as some optional statistical
# information.  It returns an optional residual extinction curve
# for each aperture.  The residual extinctions curves are finally combined
# and output as a revised extinction table.

procedure t_sensfunc ()

pointer	standards		# Input standard star data filename
pointer	sensitivity		# Output root sensitivity function imagename
pointer	aps			# Aperture list
bool	ignoreaps		# Ignore apertures?	
pointer	logfile			# Output log for statistics
pointer	function		# Sensitivity function type
int	order			# Order of sensitivity function
int	interactive		# Interactive?

int	i, j, aperture, nstds, napertures, nextn, clgeti()
pointer	sp, str, stds, apertures, wextn, extn, ecvs, gp
bool	clgetb()
pointer	rng_open()
errchk	sf_sensfunc

begin
	call smark (sp)
	call salloc (standards, SZ_FNAME, TY_CHAR)
	call salloc (sensitivity, SZ_FNAME, TY_CHAR) 
	call salloc (str, SZ_LINE, TY_CHAR) 
	call salloc (logfile, SZ_FNAME, TY_CHAR) 
	call salloc (function, SZ_FNAME, TY_CHAR)

	# CL parameter input.
	call clgstr ("standards", Memc[standards], SZ_FNAME)
	call clgstr ("sensitivity", Memc[sensitivity], SZ_FNAME) 
	call clgstr ("apertures", Memc[str], SZ_LINE)
	ignoreaps = clgetb ("ignoreaps")
	call clgstr ("logfile", Memc[logfile], SZ_FNAME) 
	call clgstr ("function", Memc[function], SZ_FNAME)
	order = clgeti ("order")
	if (clgetb ("interactive"))
	    interactive = 2
	else
	    interactive = 3

	# Decode aperture list.
	iferr (aps = rng_open (Memc[str], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture list")

	# Get the standard star data, the aperture array, and the
	# extinction table, and allocate and initialize an array of
	# residual extinction curves for each aperture.

	call sf_stds (Memc[standards], aps, ignoreaps, stds, nstds)
	if (nstds == 0) {
	    call sfree (sp)
	    return
	}
	call sf_apertures (Memi[stds], nstds, apertures, napertures)
	call ext_load (wextn, extn, nextn)
	call salloc (ecvs, napertures, TY_INT)
	call amovki (NULL, Memi[ecvs], napertures)

	# For each aperture flag standard stars to be used and call sf_sensfunc.
	gp = NULL
	do j = 1, napertures {
	    aperture = Memi[apertures+j-1]
	    do i = 1, nstds - 2
		if (STD_BEAM(Memi[stds+i-1]) == aperture)
		    STD_FLAG(Memi[stds+i-1]) = SF_INCLUDE
		else
		    STD_FLAG(Memi[stds+i-1]) = SF_EXCLUDE
	    call sf_sensfunc (gp, Memi[stds], nstds, Memr[wextn], Memr[extn],
		nextn, Memc[sensitivity], Memc[logfile], Memi[ecvs+j-1],
		Memc[function], order, ignoreaps, interactive)
	}
	call sf_gfree (gp)

	# Output a revised extinction table by combining the residual
	# extinction curves for the apertures.  The table name is obtained
	# by this proceudre.

	call sf_eout (Memr[wextn], Memr[extn], nextn, Memi[ecvs], napertures)

	# Finish up.
	call sf_free (stds, nstds, apertures, napertures)
	call ext_free (wextn, extn)
	do j = 1, napertures
	    call cvfree (Memi[ecvs+j-1])
	call rng_close (aps)
	call sfree (sp)
end
