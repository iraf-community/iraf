include <fset.h>
include "../lib/apphot.h"
include "../lib/display.h"
include "../lib/center.h"
include "../lib/fitsky.h"

# AP_BRADPROF -- Procedure to fit the radial profiles of a list of objects
# in a list of images.

procedure ap_bradprof (ap, im, cl, gid, gd, mgd, out, id, ld, interactive)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
pointer	gid			# display device stream
pointer	gd			# pointer to graphics stream
pointer	mgd			# pointer to metacode file
int	out			# output file descriptor
int	id, ld			# sequence and list numbers
int	interactive		# interactive pr batch mode

int	stdin, ild, cier, sier, pier, rier
pointer	sp, str
real	wx, wy
int	fscan(), nscan(), apfitsky(), apfitcenter(), ap_frprof(), apstati()
int	strncmp()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME + 1, TY_CHAR)
	call fstats (cl, F_FILENAME, Memc[str], SZ_FNAME)

	# Initialize.
	ild = ld

	# Print query if coords is equal to STDIN.
	if (strncmp ("STDIN", Memc[str], 5) == 0) {
	    stdin = YES
	    call printf (
	    "Type x and y coordinates of object (^D or ^Z to end): ")
	    call flush (STDOUT)
	} else
	    stdin = NO

	# Loop over the coordinate file.
	while (fscan (cl) != EOF) {

	    # Get the coordinates.
	    call gargr (wx)
	    call gargr (wy)
	    if (nscan () != 2) {
		if (stdin == YES) {
	    	    call printf (
		    "Type x and y coordinates of object (^D or ^Z to end): ")
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

	    # Set the current cursor position.
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Fit the center, sky and radial profile.
	    cier = apfitcenter (ap, im, wx, wy)
	    sier = apfitsky (ap, im, apstatr (ap, XCENTER), apstatr (ap,
	        YCENTER), NULL, gd)
	    rier = ap_frprof (ap, im, apstatr (ap, XCENTER), apstatr (ap,
		YCENTER), pier)

	    # Output the results.
	    if (interactive == YES) {
		call ap_qprprof (ap, cier, sier, pier, rier)
		if (id != NULL)
		    call aprmark (ap, gid, apstati (ap, MKCENTER),
		        apstati (ap, MKSKY), apstati (ap, MKAPERT))
	    }
	    if (id == 1)
	        call ap_param (ap, out, "radprof")
	    call ap_prprof (ap, out, id, ild, cier, sier, pier, rier)
	    call ap_rpplot (ap, id, mgd, YES)

	    # Setup for the next object.
	    id = id + 1
	    ild = ild + 1
	    call apsetr (ap, WX, wx)
	    call apsetr (ap, WY, wy)
	    if (stdin == YES) {
	        call printf (
		"Type x and y coordinates of object (^D or ^Z to end): ")
	        call flush (STDOUT)
	    }
	}

	call sfree (sp)
end
