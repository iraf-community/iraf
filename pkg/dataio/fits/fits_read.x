# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <imhdr.h>
include <fset.h>
include	"rfits.h"

# RFT_READ_FITZ -- Convert a FITS file. An EOT is signalled by returning EOF.

int procedure rft_read_fitz (fitsfile, iraffile)

char	fitsfile[ARB]		# FITS file name
char	iraffile[ARB]		# IRAF file name

int	fits_fd, stat, min_lenuserarea, ip
pointer	im, sp, fits, envstr
int	rft_read_header(), mtopen(), immap(), strlen(), envfind(), ctoi()
errchk	smark, sfree, salloc, rft_read_header, rft_read_image, rft_find_eof()
errchk	rft_scan_file, mtopen, immap, imdelete, close, imunmap

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
	        if (make_image == YES) {
	            call rft_read_image (fits_fd, fits, im)
		    if (fe > 0.0)
			call rft_find_eof (fits_fd)
		} else if (fe > 0.0)
		    call rft_scan_file (fits_fd, fits, im, fe)
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
	    iferr {
		call imgimage (IRAFNAME(fits), IRAFNAME(fits), SZ_FNAME)
	        call imrename (iraffile, IRAFNAME(fits))
	    } then {
		call printf ("   Cannot rename image %s to %s\n")
		    call pargstr (iraffile)
		    call pargstr (IRAFNAME(fits))
	        call flush (STDOUT)
	        call erract (EA_WARN)
	    } else {
	        call printf ("    File: %s restored to IRAF File: %s\n")
		    call pargstr (iraffile)
		    call pargstr (IRAFNAME(fits))
	    }
	}

	if (long_header == YES)
	    call printf ("\n")

	call close (fits_fd)
	call sfree (sp)

	return (stat)
end


# RFT_FIND_EOF -- Read the FITS data file until EOF is reached.

procedure rft_find_eof (fd)

int	fd			# the FITS file descriptor

int	szbuf
pointer	sp, buf
int	fstati(), read()
errchk	read

begin
	# Scan through the file.
	szbuf = fstati (fd, F_BUFSIZE)
	call smark (sp)
	call salloc (buf, szbuf, TY_CHAR)
	while (read (fd, Memc[buf], szbuf) != EOF)
	    ;
	call sfree (sp)
end


# RFT_SCAN_FILE -- Determine whether it is more efficient to read the
# entire file or to skip forward to the next file if the parameter
# make_image was set to no.

procedure  rft_scan_file (fd, fits, im, fe)

int	fd			# the FITS file descriptor
pointer	fits			# pointer to the FITS descriptor
pointer	im			# pointer to the output image
real	fe			# maximum file size in Kb for scan mode

int	i, szbuf
pointer	sp, buf
real	file_size
int	fstati(), read()
errchk	read

begin
	# Compute the file size in Kb and return if it is bigger than fe.
	file_size = 1.0
	do i = 1, IM_NDIM(im)
	    file_size = file_size * IM_LEN(im,i)
	if (IM_NDIM(im) <= 0)
	    file_size = 0.0
	else
	    file_size = file_size * abs (BITPIX(fits)) / FITS_BYTE / 1.0e3
	if (file_size >= fe)
	    return

	# Scan through the file.
	szbuf = fstati (fd, F_BUFSIZE)
	call smark (sp)
	call salloc (buf, szbuf, TY_CHAR)
	while (read (fd, Memc[buf], szbuf) != EOF)
	    ;
	call sfree (sp)
end
