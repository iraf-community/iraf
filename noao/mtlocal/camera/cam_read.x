include <error.h>
include <imhdr.h>
include	"rcamera.h"

# CAM_READ -- Convert a CAMERA file into an IRAF imagefile.
# An EOT is signalled by returning EOF.

int procedure cam_read (camfile, iraffile, image_ranges, nimages)

char	camfile[ARB]
char	iraffile[ARB]
int	image_ranges[ARB]
int	nimages

char	irafname[SZ_FNAME]
int	cam_fd,	image_number, image_count, stat, nrecs
long	loffset
pointer	im

int	cam_read_header(), mtopen(), immap(), get_next_number()
int	mt_skip_record()
int	strlen()
long	note()

errchk	salloc, cam_read_header, mtopen, close, immap, imdelete
errchk	mt_skip_record, cam_read_image

include	"rcamera.com"

begin
	# Open input CAMERA file. If an error occurs on open file is at EOT
	cam_fd = mtopen (camfile, READ_ONLY, 0)

	image_count = 1
	image_number = 0

	# loop over the image list
	while (get_next_number (image_ranges, image_number) != EOF) {

	    # Read header. An EOF status from cam_read_header will cause READ_
	    # CAMERA to skip to the next tape file.

	    while (image_count <= image_number) {

		# An EOF mean end of file. If the image number is not
		# in the image list the appropriate number of records
		# are skipped. READ_HEADER returns the the number of
		# data records to be skipped if the input is from tape
		# or the number of chars to be skipped in the disk file
		# to read the next header.

	        stat = cam_read_header (cam_fd)

	        if (stat == EOF)

		    break

		else if (image_number != image_count) {

		    if (tape == YES)
		        nrecs = mt_skip_record (cam_fd, stat)
		    else {
			loffset = note (cam_fd)
			call seek (cam_fd, loffset + stat)
		    }

	        } else {

		    # add image number to output file name
		    call strcpy (iraffile, irafname, SZ_FNAME)
		    if (nimages > 1) {
			call sprintf (irafname[strlen(irafname)+1], SZ_FNAME,
				    ".%03d")
			    call pargi (image_number)
		    }

	            # Print long or short header
	            if (long_header == YES || short_header == YES) {
	                if (make_image == YES) {
	                    call printf ("File: %s  ")
		                call pargstr (irafname)
	                } else {
		            call printf ("File: %s  ")
		                call pargstr (camfile)
	                }
	                if (long_header == YES)
		            call printf ("\n")
	            }

	            # Create IRAF image header.  If only a header listing is
		    # desired then a temporary image header is created and
		    # later deleted.
	            if (make_image == NO)
	        	call strcpy ("dev$null", irafname, SZ_FNAME)
	    	    im = immap (irafname, NEW_IMAGE, LEN_USER_AREA)

		    # Decode the image header
		    call cam_rparams (im)

	            # Create an IRAF image if desired
	            if (make_image == YES)
	                call cam_read_image (cam_fd, im)

	            if (long_header == YES)
	                call printf ("\n")

	            # Close files and clean up
	            call imunmap (im)
	            if (make_image == NO)
	                call imdelete (irafname)

		    # If headers only skip data records
		    if (make_image == NO) {
			if (tape == YES)
		            nrecs = mt_skip_record (cam_fd, stat)
			else {
			    loffset = note (cam_fd)
			    call seek (cam_fd, loffset + stat)
			}
		    }

	        }

		image_count = image_count + 1
	    }

	    if (stat == EOF)
		break
	}

	# Close tape file
	call close (cam_fd)

	# Return status
	if (image_count == 1)
	    return (EOF)
	else
	    return (OK)
end
