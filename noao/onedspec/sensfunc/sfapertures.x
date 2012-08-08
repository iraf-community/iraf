include "sensfunc.h"

# SF_APERTURES -- Determine the apertures in use.

procedure sf_apertures (stds, nstds, apertures, napertures)

pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	apertures		# Pointer to apertures (returned)
int	napertures		# Number of apertures (returned)

int	i, j, aperture

errchk	malloc, realloc

begin
	call malloc (apertures, nstds, TY_INT)
	napertures = 0
	do i = 1, nstds {
	    aperture = STD_BEAM(stds[i])
	    for (j=1; (j<=napertures)&&(aperture!=Memi[apertures+j-1]); j=j+1)
		;
	    napertures = max (napertures, j)
	    Memi[apertures+j-1] = aperture
	}
	call realloc (apertures, napertures, TY_INT)
end
