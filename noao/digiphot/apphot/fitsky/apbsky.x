include <fset.h>
include "../lib/apphot.h"
include "../lib/display.h"

# APBSKY -- Procedure to determine the sky statistics for a list of objects
# in batch mode using a simple coordinate list.

procedure apbsky (ap, im, cl, sd, out, id, ld, gd, mgd, gid, interactive)

pointer ap			# pointer to apphot structure
pointer	im			# pointer to IRAF image
int	cl			# starlist file descriptor
int	sd			# sky file descriptor
int	out			# output file descriptor
int	id, ld			# sequence and list numbers
int	gd			# pointer to stdgraph stream
pointer	mgd			# pointer to graphics metacode file
pointer	gid			# pointer to image display stream
int	interactive		# interactive mode

int	stdin, ier, ild
pointer	sp, str
real	wx, wy
int	apfitsky(), fscan(), nscan(), strncmp(), apstati()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call fstats (cl, F_FILENAME, Memc[str], SZ_FNAME)

	# Initialize.
	ild = ld

	# Print the query.
	if (strncmp ("STDIN", Memc[str], 5) == 0) {
	    stdin = YES
	    call printf ("Type object x and y coordinates (^D or ^Z to end): ")
	    call flush (STDOUT)
	} else
	    stdin = NO

	# Loop over the coordinate file.
	while (fscan (cl) != EOF) {

	    # Fetch and store the coordinates.
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

	    # Fit the sky value.
	    ier = apfitsky (ap, im, wx, wy, sd, gd)

	    # Print the sky values.
	    if (interactive == YES) {
		call ap_qspsky (ap, ier)
		if (gid != NULL)
		    call apmark (ap, gid, NO, apstati (ap, MKSKY), NO)
	    }
	    call ap_splot(ap, id, mgd, YES)
	    if (id == 1)
	        call ap_param (ap, out, "fitsky")
	    call ap_pssky (ap, out, id, ild, ier)

	    # Set up for the next object.
	    id = id + 1
	    ild = ild + 1
	    call apsetr (ap, WX, wx)
	    call apsetr (ap, WY, wy)

	    # print query
	    if (stdin == YES) {
		call printf (
		    "Type object x and y coordinates (^D or ^Z to end): ")
		call flush (STDOUT)
	    } 
	}

	call sfree (sp)
end
