include <error.h>
include <imhdr.h>
include	"r2df.h"

# R2DFRD -- Convert a 2D-FRUTTI file into an IRAF imagefile.
# An EOT is signalled by returning EOF.

int procedure r2dfrd (camfile, iraffile)

char	camfile[ARB]
char	iraffile[ARB]

int	stat
pointer	cam_fd, im
int	r2dfrhdr()
pointer mtopen(), immap()
errchk	salloc, r2dfrhdr, mtopen, close, immap, delete
errchk	r2dfrim
include	"r2df.com"

begin
	# Open input 2D-FRUTTI file.  If an error occurs on open file
	# is at EOT.

	cam_fd = mtopen (camfile, READ_ONLY, 0)

	# Print long or short header.

	if (long_header == YES || short_header == YES) {
	    if (make_image == YES) {
	        call printf ("File: %s  ")
		    call pargstr (iraffile)
	    } else {
		call printf ("File: %s  ")
		    call pargstr (camfile)
	    }
	    if (long_header == YES)
		call printf ("\n")
	 }

	 # Create IRAF image header.  If only a header listing is desired
	 # then a temporary image header is created and later deleted.

	 if (make_image == NO)
	     call strcpy ("dev$null", iraffile, SZ_FNAME)
	 im = immap (iraffile, NEW_IMAGE, LEN_USER_AREA)

	 # Read the header. EOT is signalled by an EOF status from fits_read_
	 # header. Create an IRAF image if desired

	 iferr {
	    stat = r2dfrhdr (cam_fd, im)
	    if (stat == EOF)
		call printf ("End of data\n")
	    else {
	        if (make_image == YES)
	            call r2dfrim (cam_fd, im)
	    }
	} then
	    call erract (EA_WARN)

	# Close files and clean up.

	call imunmap (im)
	if (stat == EOF || make_image == NO)
	    call imdelete (iraffile)
	if (long_header == YES)
	    call printf ("\n")
	call close (cam_fd)

	return (stat)
end
