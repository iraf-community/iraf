include <fset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"

# APBPHOT -- Procedure to compute the magnitudes for a list of objects
# interactively.

procedure apbphot (ap, im, cl, sd, out, id, ld, gd, mgd, gid, interactive)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
int	sd			# sky file descriptor
int	out			# output file descriptor
int	id, ld			# sequence and list numbers
pointer	gd			# pointer to stdgraph stream
pointer	mgd			# pointer to plot metacode file
pointer	gid			# pointer to image display stream
int	interactive		# interactive pr batch mode

int	stdin, ild, radius, cier, sier, pier
pointer	sp, str
real	wx, wy
int	fscan(), nscan(), apfitsky(), apfitcenter(), apmag(), strncmp()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call fstats (cl, F_FILENAME, Memc[str], SZ_FNAME)

	# Initialize
	ild = ld
	radius = 4 * (apstatr (ap, ANNULUS) + apstatr (ap, DANNULUS) + 1.0) *
	    apstatr (ap, SCALE)
	call ap_imbuf (ap, radius, YES)

	# Print query.
	if (strncmp ("STDIN", Memc[str], 5) == 0) {
	    stdin = YES
	    call printf ("Type object x and y coordinates (^D or ^Z to end): ")
	    call flush (STDOUT)
	} else
	    stdin = NO

	# Loop over the coordinate file.
	while (fscan (cl) != EOF) {

	    # Get and store the coordinates.
	    call gargr (wx)
	    call gargr (wy)
	    if (nscan () != 2) {
		if (stdin == YES) {
	    	    call printf (
		        "Type object x and y coordinates (^D or ^Z to end): ")
	    	    call flush (STDOUT)
		}
		next
	    }


            # Transform the input coordinates.
            switch (apstati(ap,WCSIN)) {
            case WCS_WORLD, WCS_PHYSICAL:
                call ap_itol (ap, wx, wy, wx, wy, 1)
            case WCS_TV:
                call ap_vtol (im, wx, wy, wx, wy, 1)
            default:
                ;
            }
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Center the coordinates, fit the sky and compute magnitudes.
	    cier = apfitcenter (ap, im, wx, wy)
	    sier = apfitsky (ap, im, apstatr (ap, XCENTER), apstatr (ap,
	        YCENTER), sd, gd)
	    pier = apmag (ap, im, apstatr (ap, XCENTER), apstatr (ap, YCENTER),
	        apstati (ap, POSITIVE), apstatr (ap, SKY_MODE),
		apstatr (ap, SKY_SIGMA), apstati (ap, NSKY))

	    # Print the results if interactive mode.
	    if (interactive == YES) {
		call ap_qpmag (ap, cier, sier, pier)
		if (gid != NULL)
		    call apmark (ap, gid, apstati (ap, MKCENTER), apstati (ap,
			MKSKY), apstati (ap, MKAPERT))
	    }

	    # Write the results.
	    if (id == 1)
	        call ap_param (ap, out, "phot")
	    call ap_pmag (ap, out, id, ild, cier, sier, pier)

	    # Make plots if mgd is enabled.
	    call ap_pplot (ap, im, id, mgd, YES)

	    # Prepare for the next object.
	    id = id + 1
	    ild = ild + 1
	    call apsetr (ap, WX, wx)
	    call apsetr (ap, WY, wy)
	    if (stdin == YES) {
		call printf (
		    "Type object x and y coordinates (^Z or ^D to end): ")
		call flush (STDOUT)
	    }
	}

	call ap_imbuf (ap, 0, YES)
	call sfree (sp)
end
