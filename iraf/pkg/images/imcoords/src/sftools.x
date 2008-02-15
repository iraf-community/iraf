include <mach.h>
include "starfind.h"

# SF_GPARS-- Read in the star finding parameters from the datapars file.

procedure sf_gpars (sf)

pointer sf                      #I pointer to the star finding structure

int	clgeti()
real	clgetr()

begin
	# Initialize the data structure.
	call sf_init (sf)

        # Fetch the star finding parameters.
	SF_HWHMPSF(sf) = clgetr ("hwhmpsf")
	SF_FRADIUS(sf) = clgetr ("fradius")
	SF_THRESHOLD(sf) = clgetr ("threshold")
	SF_DATAMIN(sf) = clgetr ("datamin")
	SF_DATAMAX(sf) = clgetr ("datamax")
	SF_SEPMIN(sf) = clgetr ("sepmin")
	SF_NPIXMIN(sf) = clgeti ("npixmin")
	SF_MAGLO(sf) = clgetr ("maglo")
	SF_MAGHI(sf) = clgetr ("maghi")
	SF_ROUNDLO(sf) = clgetr ("roundlo")
	SF_ROUNDHI(sf) = clgetr ("roundhi")
	SF_SHARPLO(sf) = clgetr ("sharplo")
	SF_SHARPHI(sf) = clgetr ("sharphi")
end


# SF_INIT -- Initialize the STARFIND task data structure and set the 
# star finding parameters to their default values.

procedure sf_init (sf)

pointer	sf			#U pointer to the star finding structure

begin
	call calloc (sf, LEN_STARFIND, TY_STRUCT)

	SF_HWHMPSF(sf) = DEF_HWHMPSF
	SF_FRADIUS(sf) = DEF_FRADIUS
	SF_THRESHOLD(sf) = DEF_THRESHOLD
	SF_DATAMIN(sf) = DEF_DATAMIN
	SF_DATAMAX(sf) = DEF_DATAMAX
	SF_SHARPLO(sf) = DEF_SHARPLO
	SF_SHARPHI(sf) = DEF_SHARPHI
	SF_ROUNDLO(sf) = DEF_ROUNDLO
	SF_ROUNDHI(sf) = DEF_ROUNDHI
	SF_MAGLO(sf) = DEF_MAGLO
	SF_MAGHI(sf) = DEF_MAGHI
	SF_SEPMIN(sf) = DEF_SEPMIN
	SF_NPIXMIN(sf) = DEF_NPIXMIN
end


# SF_FREE -- Free the STARFIND task data structure.

procedure sf_free (sf)

pointer	sf			#U pointer to the star finding structure

begin
	call mfree (sf, TY_STRUCT)
end
