# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fset.h>
include	<error.h>
include <mach.h>
include <imhdr.h>
include "wfits.h"

# WFT_WRITE_FITZ -- Procedure to convert a single IRAF file to a FITS file.
# If the make_image switch is set the header and pixel files are output
# to the output destination. If the make_image switch is off the header
# is printed to the standard output.

procedure wft_write_fitz (iraf_file, fits_file, image_number, nimages)

char	iraf_file[ARB]		# IRAF file name
char	fits_file[ARB]		# FITS file name
int	image_number		# the current image number
int	nimages			# the number of images

int	fits_fd, chars_rec, nchars, ip, min_lenuserarea
pointer	im, sp, fits, envstr

int	mtfile(), mtopen(), open(), fnldir(), envfind(), ctoi()
pointer	immap()
errchk	immap, imunmap, open, mtopen, close, smark, salloc, sfree
errchk	delete, wft_write_header, wft_write_image, wft_data_limits

include "wfits.com"

begin
	# Open the output file. Check whether the output file is a magtape
	# device or a binary file. If the output file is magtape check
	# for a legal blocking factor.

	if (image_number == 1 || wextensions == NO) {
	    if (make_image == NO)
	        call strcpy ("dev$null", fits_file, SZ_FNAME)
	    if (mtfile (fits_file) == YES) {
	        chars_rec = (blkfac * len_record * FITS_BYTE) / (SZB_CHAR *
	            NBITS_BYTE)
	        fits_fd = mtopen (fits_file, WRITE_ONLY, chars_rec)
	    } else
	        fits_fd = open (fits_file, NEW_FILE, BINARY_FILE)
	}

	# Allocate memory for program data structure.
	call smark (sp)
	call salloc (fits, LEN_FITS, TY_STRUCT)
	call salloc (envstr, SZ_FNAME, TY_CHAR)

	# Set up the minimum length of the user area.
	if (envfind ("min_lenuserarea", Memc[envstr], SZ_FNAME) > 0) {
	    ip = 1
	    if (ctoi (Memc[envstr], ip, min_lenuserarea) <= 0)
		min_lenuserarea = LEN_USERAREA
	    else
		min_lenuserarea = max (LEN_USERAREA, min_lenuserarea)
	} else
	    min_lenuserarea = LEN_USERAREA

	# Write the global header.
	if (image_number == 1 && gheader == YES) {

	    XTENSION(fits) = EXT_PRIMARY

	    # Open a dummy image.
	    im = immap ("dev$null", NEW_IMAGE, 0)
	    NAXIS(im) = 0
	    PIXTYPE(im) = TY_SHORT
	    OBJECT(im) = EOS
	    IRAFNAME(fits) = EOS

            if (long_header == YES || short_header == YES) {
                call printf ("Global header")
		if (make_image == YES) {
		    call printf (" -> %s[0] ")
		        call pargstr (fits_file)
		}
		if (long_header == YES)
		    call printf ("\n")
		else if (short_header == YES)
		    call printf (" ")
            }
	    call flush (STDOUT)

	    iferr {
		call wft_write_header (im, fits, fits_fd)
	        if (make_image == YES)
		    call wft_write_image (im, fits, fits_fd)
	    } then {

	        # Print the error message.
	        call flush (STDOUT)
	        call erract (EA_WARN)

	        # Close files and cleanup.
	        call imunmap (im)
	        #if (image_number == nimages || wextensions == NO)
	            call close (fits_fd)
	        if (make_image == NO)
	            call delete (fits_file)
	        call sfree (sp)

	        # Assert an error.
	        call erract (EA_ERROR)

	    } else {
		call imunmap (im)
	    }

	    if (long_header == YES)
	        call printf ("\n")
	}

	# Map the input image. Construct the old iraf name by removing
	# the directory specification.
	# Print the id string.
        if (long_header == YES || short_header == YES) {
            call printf ("Image %d: %s")
                call pargi (image_number)
                call pargstr (iraf_file)
        }

	# Define whether the image to be written is to be the FITS primary
	# data image file or a FITS image extension file.
	if (image_number == 1) {
	    if (wextensions == YES && gheader == YES)
	        XTENSION(fits) = EXT_IMAGE
	    else 
	        XTENSION(fits) = EXT_PRIMARY
	} else {
	    if (wextensions == YES)
	        XTENSION(fits) = EXT_IMAGE
	    else
	        XTENSION(fits) = EXT_PRIMARY
	}

	im = immap (iraf_file, READ_ONLY, min_lenuserarea)
	call imgcluster (iraf_file, IRAFNAME(fits), SZ_FNAME)
	nchars = fnldir (IRAFNAME(fits), IRAFNAME(fits), SZ_FNAME)
	call strcpy (iraf_file[nchars+1], IRAFNAME(fits), SZ_FNAME)

	# Write header and image.
	iferr {

	    if (short_header == YES || long_header == YES) {
	        if (make_image == YES) {
		    if (wextensions == YES && nimages > 1) {
		        call printf (" -> %s[%d] ")
		            call pargstr (fits_file)
			    if (gheader == YES)
			        call pargi (image_number)
			    else
			        call pargi (image_number - 1)
		    } else {
		        call printf (" -> %s ")
		            call pargstr (fits_file)
		    }
	        }
		if (long_header == YES)
		    call printf ("\n")
		else if (short_header == YES)
		    call printf (" ")
	    }
	    call flush (STDOUT)

	    call wft_write_header (im, fits, fits_fd)
	    if (make_image == YES)
		call wft_write_image (im, fits, fits_fd)

	    if (long_header == YES)
	        call printf ("\n")

	} then {

	    # Print the error message.
	    call flush (STDOUT)
	    call erract (EA_WARN)

	    # Close files and cleanup.
	    call imunmap (im)
	    #if (image_number == nimages || wextensions == NO)
	        call close (fits_fd)
	    if (make_image == NO)
	        call delete (fits_file)
	    call sfree (sp)

	    # Assert an error.
	    call erract (EA_ERROR)

	} else {

	    # Close files and cleanup.
	    call imunmap (im)
	    if (image_number == nimages || wextensions == NO)
	        call close (fits_fd)
	    if (make_image == NO)
	        call delete (fits_file)
	    call sfree (sp)
	}

end


# WFT_DATA_LIMITS -- Procedure to calculate the maximum and minimum data values
# in an IRAF image. Values are only calculated if the max and min are unknown
# or the image has been modified since the last values were calculated.

procedure wft_data_limits (im, irafmin, irafmax)

pointer	im		# image pointer
real	irafmin		# minimum picture value
real	irafmax		# maximum picture value

int	npix
long	v[IM_MAXDIM]
pointer	buf
real	maxval, minval
int	imgnlr()
errchk	imgnlr

begin
	# Compute the data minimum and maximum if the image values
	# are undefined out-of-date.

	if (LIMTIME(im) < MTIME(im) && NAXIS(im) > 0) {

	    irafmax = -MAX_REAL
	    irafmin = MAX_REAL
	    npix = NAXISN(im,1)

	    call amovkl (long(1), v, IM_MAXDIM)
	    while (imgnlr (im, buf, v) != EOF) {
	        call alimr (Memr[buf], npix, minval, maxval)
	        irafmin = min (irafmin, minval)
	        irafmax = max (irafmax, maxval)
	    }

	} else {

	    irafmax = IM_MAX(im)
	    irafmin = IM_MIN(im)

	}
end
