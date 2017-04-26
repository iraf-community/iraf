
include <error.h>
include <imhdr.h>
include	"rpds.h"

# PDS_READ -- Convert a PDS file
# An EOT is signalled by returning EOF.

int procedure pds_read (pdsfile, iraffile)

char	pdsfile[ARB], iraffile[ARB]
int	pds_fd
int	stat
long	parameters[LEN_PAR_ARRAY]
pointer	im

int	pds_read_header(), mtopen()
pointer	immap()

errchk	salloc, pds_read_header, pds_read_image, mtopen, immap, imdelete

include	"rpds.com"

begin
	# Open input PDS data
	pds_fd = mtopen (pdsfile, READ_ONLY, 0)

	if (long_header == YES || short_header == YES) {
	    if (make_image == YES) {
	        call printf ("File: %s  ")
		    call pargstr (iraffile)
	    } else {
		call printf ("File: %s  ")
		    call pargstr (pdsfile)
	    }
	    if (long_header == YES)
		call printf ("\n")
	}

	# Create IRAF image header.  If only a header listing is desired
	# then a temporary image header is created and later deleted.

	if (make_image == NO)
	    call strcpy ("dev$null", iraffile, SZ_FNAME)

	im = immap (iraffile, NEW_IMAGE, 0)

	# Read header.  EOT is signalled by an EOF status from pds_read_header.
	iferr {
	    stat = pds_read_header (pds_fd, im, parameters)
	    if (stat == EOF)
	        call printf ("End of data\n")
	    else {
	        # Create an IRAF image if desired
	        if (make_image == YES)
	            call pds_read_image (pds_fd, im, parameters)
	    }
	} then
	    call erract (EA_WARN)

	if (long_header == YES)
	    call printf ("\n")

	# Close files and clean up
	call imunmap (im)
	if (stat == EOF || make_image == NO)
	    call imdelete (iraffile)
	call close (pds_fd)

	return (stat)
end
