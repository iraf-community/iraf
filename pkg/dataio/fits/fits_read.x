# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <imset.h>
include	"rfits.h"

# RFT_READ_FITZ -- Convert a FITS file. An EOT is signalled by returning EOF.

int procedure rft_read_fitz (fitsfile, iraffile)

char	fitsfile[ARB]		# FITS file name
char	iraffile[ARB]		# IRAF file name

int	fits_fd, stat, min_lenuserarea, ip
pointer	im, sp, fits, envstr
int	rft_read_header(), mtopen(), immap(), strlen(), envfind(), ctoi()
errchk	smark, sfree, salloc, rft_read_header, rft_read_image, mtopen, immap
errchk	imdelete, close, imunmap

include	"rfits.com"

begin
	# Open input FITS data.
	fits_fd = mtopen (fitsfile, READ_ONLY, 0)

	# Allocate memory for program data structure.
	call smark (sp)
	call salloc (fits, LEN_FITS, TY_STRUCT)
	call salloc (envstr, SZ_FNAME, TY_CHAR)

	# Set up for printing a long or a short header.
	if (long_header == YES || short_header == YES) {
	    if (make_image == YES) {
	        call printf ("File: %s  ")
		    call pargstr (iraffile)
	    } else {
		call printf ("File: %s  ")
		    call pargstr (fitsfile)
	    }
	    if (long_header == YES)
		call printf ("\n")
	}
	call flush (STDOUT)

	# Create the IRAF image header. If only a header listing is desired
	# then map the scratch image onto DEV$NULL (faster than a real file).

	if (make_image == NO)
	    call strcpy ("dev$null", iraffile, SZ_FNAME)
	if (envfind ("min_lenuserarea", Memc[envstr], SZ_FNAME) > 0) {
	    ip = 1
	    if (ctoi (Memc[envstr], ip, min_lenuserarea) <= 0)
		min_lenuserarea = LEN_USERAREA
	    else
		min_lenuserarea = max (LEN_USERAREA, min_lenuserarea)
	} else
	    min_lenuserarea = LEN_USERAREA
	im = immap (iraffile, NEW_IMAGE, min_lenuserarea)

	# Read header.  EOT is signalled by an EOF status from fits_read_header.
	# Create an IRAF image if desired.

	iferr {
	    IRAFNAME(fits) = EOS
	    stat = rft_read_header (fits_fd, fits, im)
	    if (stat == EOF)
	        call printf ("End of data\n")
	    else {
	        if (make_image == YES)
	            call rft_read_image (fits_fd, fits, im)
	    }
	} then {
	    call flush (STDOUT)
	    call erract (EA_WARN)
	}

	# Close files and clean up.
	call imunmap (im)

	# Optionally restore the old IRAF name.
	if (stat == EOF || make_image == NO) {
	    call imdelete (iraffile)
	} else if (old_name == YES && strlen (IRAFNAME(fits)) != 0) {
	    call imrename (iraffile, IRAFNAME(fits))
	    call printf ("    File: %s restored to IRAF File: %s\n")
		call pargstr (iraffile)
		call pargstr (IRAFNAME(fits))
	}

	if (long_header == YES)
	    call printf ("\n")

	call close (fits_fd)
	call sfree (sp)

	return (stat)
end
