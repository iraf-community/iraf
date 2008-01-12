include "quadgeom.h"

# QUADDELETE --  Delete subimages, one for each readout.

procedure quaddelete (qg, rootname)

pointer	qg		#I Pointer to open quadgeom structure
char	rootname[ARB]	#I Root name for subimages.

int	amp
pointer	fullname

pointer	sp
int	imaccess()

begin
	call smark (sp)
	call salloc (fullname, SZ_LINE, TY_CHAR)

	# Loop over active readouts
	do amp = 1, QG_NAMPS(qg) {

	    # The sub-section image will only exist if this is not a phantom
	    if (QG_PHANTOM (qg, amp) == NO) {

		# Make sub-image name
		call sprintf (Memc[fullname], SZ_LINE, "%s.%s")
		    call pargstr (rootname)
		    call pargstr (Memc[QG_AMPID(qg, amp)])

		# Delete the sub-image (if it exists)
		if (imaccess (Memc[fullname], READ_ONLY) == YES) {
		    call imdelete (Memc[fullname])
		}
	    }
	}
	
	call sfree (sp)
end
