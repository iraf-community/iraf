include <fset.h>
include "../lib/apphot.h"
include "../lib/display.h"

# APBCENTER -- Procedure to determine accurate centers for a list of objects
# in batch mode using a simple text file list.

procedure apbcenter (ap, im, cl, out, id, ld, mgd, gid, interactive)

pointer ap			# pointer to the apphot structure
pointer	im			# pointer to the IRAF image
int	cl			# the coordinate file descriptor
int	out			# output file descriptor
int	id			# output file sequence number
int	ld			# the coordinate file list number
pointer	mgd			# pointer to the metacode file stream
pointer	gid			# pointer to the image display stream
int	interactive		# mode of use

int	stdin, ier, ild
pointer	sp, str
real	wx, wy
int	apfitcenter(), fscan(), nscan(), strncmp(), apstati()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call fstats (cl, F_FILENAME, Memc[str], SZ_FNAME)

	# Initialize.
	ild = ld

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

	    # Fit the center.
	    ier = apfitcenter (ap, im, wx, wy)

	    # Print and/or plot the results.
	    if (interactive == YES) {
		call ap_qcenter (ap, ier)
		if (gid != NULL)
		    call apmark (ap, gid, apstati (ap, MKCENTER), NO, NO)
	    }
	    call ap_cplot (ap, id, mgd, YES)
	    if (id == 1)
	        call ap_param (ap, out, "center")
	    call ap_pcenter (ap, out, id, ild, ier)

	    # Setup for the next object.
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
