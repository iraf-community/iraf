# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <error.h>
include <fset.h>
include "wfits.h"

# T_WFITS -- This procedure converts a series of IRAF image files to
# FITS image files.

procedure t_wfits ()

bool	newtape
char	iraf_files[SZ_FNAME], fits_files[SZ_FNAME], in_fname[SZ_FNAME]
char	out_fname[SZ_FNAME]
int	list, nfiles, file_number, fits_record

bool	clgetb()
double	clgetd()
int	imtopen(), imtlen (), strlen(), wft_get_bitpix(), clgeti(), imtgetim()
int	mtfile(), strmatch(), stridxs(), btoi(), fstati()
data	fits_record/2880/
include "wfits.com"

begin
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Open iraf_files template and determine number of files in list
	call clgstr ("iraf_files", iraf_files, SZ_FNAME)
	list = imtopen (iraf_files)
	nfiles = imtlen (list)

	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	make_image = btoi (clgetb ("make_image"))
	bitpix = wft_get_bitpix (clgeti ("bitpix"))
	if (bitpix != ERR) {
	    call printf ("WARNING: Default bitpix overridden.\n")
	    call printf ("\tBitpix set to: %d\n")
		call pargi (bitpix)
	    call printf ("\tLoss of precision may result.\n\n")
	}

	# Get length of record in FITS bytes
	len_record = fits_record
	if (len_record != fits_record)
	    call printf ("Warning: Record length is not FITS standard\n")
	blkfac = clgeti ("blocking_factor")
	if (blkfac > 10 && mod (blkfac, SZB_CHAR) != 0)
	    call error (0, "Block size must be an integral number of chars.")
	if (blkfac > 1 && blkfac <= 10) {
	    call printf ("WARNING: FITS tape blocking factor is %d\n\n")
		call pargi (blkfac)
	}
	if (blkfac > 10) {
	    call printf ("WARNING: Blocking factor %d is not legal fits\n")
		call pargi (blkfac)
	}

	# Get scaling parameters
	scale = btoi (clgetb ("scale"))
	if (scale == YES) {
	    autoscale = btoi (clgetb ("autoscale"))
	    if (autoscale == NO) {
		bscale = clgetd ("bscale")
		bzero = clgetd ("bzero")
	    }
	} else {
	    autoscale = NO
	    bscale = 1.0d0
	    bzero = 0.0d0
	}
	if (autoscale == NO) {
	    call printf ("WARNING: Autoscaling has been turned off.\n")
	    call printf ("\tBscale set to: %g  Bzero set to: %g\n")
		call pargd (bscale)
		call pargd (bzero)
	    call printf ("\tLoss of precision may result.\n\n")
	}

	# Get output file name. If no tape file number is given for output,
	# the user is asked if the tape is blank or contains data.
	# If the tape is blank output begins at BOT, otherwise at EOT

	if (make_image == YES) {
	    call clgstr ("fits_files", fits_files, SZ_FNAME)
	    if (mtfile (fits_files) == YES) {
	        if (fits_files[strlen(fits_files)] != ']') {
	            newtape = clgetb ("newtape")
		    if (newtape) {
		        call sprintf (fits_files[strlen(fits_files) + 1],
			    SZ_FNAME, "%s")
			    call pargstr ("[1]")
		    } else {
		        call sprintf (fits_files[strlen(fits_files) + 1],
			    SZ_FNAME, "%s")
			    call pargstr ("[EOT]")
		    }
	        } else
		    newtape = false
	    }
	} else
	    fits_files[1] = EOS

	# Loop through the list of output files.

	file_number = 1
	while (imtgetim (list, in_fname, SZ_FNAME) != EOF) {

	    # print id string
	    if (long_header == YES || short_header == YES) {
		call printf ("File %d: %s")
		    call pargi (file_number)
		    call pargstr (in_fname)
	    }

	    # Get output filename. If single file output to disk, use name
	    # fits_file. If multiple file output to disk, the file number
	    # is added to the output file name.

	    if (make_image == YES) {
	        if (mtfile (fits_files) == YES) {
		    if (file_number == 2 && strmatch(fits_files,"[EOT]") == 0) {
		        call sprintf (fits_files[stridxs("[", fits_files)],
			    SZ_FNAME, "%s")
			    call pargstr ("[EOT]")
		    }
		    call strcpy (fits_files, out_fname, SZ_FNAME)
	        } else {
		    call strcpy (fits_files, out_fname, SZ_FNAME)
		    if (nfiles > 1) {
		        call sprintf (out_fname[strlen(fits_files)+1], SZ_FNAME,
			    "%03d")
			    call pargi (file_number)
		    }
	        }
	    }

	    # write each output file
	    iferr (call wft_write_fitz (in_fname, out_fname)) {
		call printf ("Error writing file: %s\n")
		    call pargstr (out_fname)
		call erract (EA_WARN)
		break
	    } else
		file_number = file_number + 1
	}

	call clpcls (list)
end


# WFT_GET_BITPIX -- This procedure fetches the user determined bitpix or ERR if
# the bitpix is not one of the permitted FITS types.

int procedure wft_get_bitpix (bitpix)

int	bitpix

begin
	switch (bitpix) {
	case FITS_BYTE, FITS_SHORT, FITS_LONG:
	    return (bitpix)
	default:
	    return (ERR)
	}
end
