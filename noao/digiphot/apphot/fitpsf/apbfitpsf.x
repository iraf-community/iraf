include <fset.h>
include "../lib/apphot.h"
include "../lib/display.h"

# APBFITPSF -- Procedure to fit a functional form to the PSF for a list of
# objects.

procedure apbfitpsf (ap, im, cl, out, gid, id, ld, interactive)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
int	out			# output file descriptor
pointer	gid			# image display stream
int	id			# initial id
int	ld			# list number
int	interactive		# interactive mode

int	ier, ild, stdin
pointer	sp, str
real	wx, wy
int	fscan, nscan(), strncmp(), apsffit(), apstati()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call fstats (cl, F_FILENAME, Memc[str], SZ_FNAME)

	# Initialize.
	ild = ld

	# Print query if input is STDIN.
	if (strncmp ("STDIN", Memc[str], 5) == 0) {
	    stdin = YES
	    call printf ("Type object x and y coordinates (^Z or ^D to end): ")
	    call flush (STDOUT)
	} else
	    stdin = NO

	# Loop over the interesting objects.
	while (fscan(cl) != EOF) {

	    # Decode the centers.
	    call gargr (wx)
	    call gargr (wy)
	    if (nscan () != 2) {
		if (stdin == YES) {
	            call printf (
		        "Type object x and y coordinates (^Z or ^D to end): ")
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

	    # Store the current cursor coordinates.
	    call apsetr (ap, CWX, wx)
	    call apsetr (ap, CWY, wy)

	    # Fit the psf.
	    ier = apsffit (ap, im, wx, wy)

	    # Output the results.
	    if (interactive == YES) {
		call ap_qppsf (ap, ier)
		if (gid != NULL)
		    call appfmark (ap, gid, apstati (ap, MKPSFBOX))
	    }
	    if (id == 1)
	        call ap_param (ap, out, "fitpsf")
	    call ap_ppsf (ap, out, id, ild, ier)
	    
	    # Prepare for the next object.
	    id = id + 1
	    ild = ild + 1
	    call apsetr (ap, WX, wx)
	    call apsetr (ap, WY, wy)
	    if (stdin == YES) {
		call printf (
		    "Type object x and y coordinates (^D or ^Z to end): ")
		call flush (STDOUT)
	    } 

	}
	call sfree (sp)
end
