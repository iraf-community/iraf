# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include <error.h>
include <fset.h>
include "wfits.h"

# T_WFITS -- This procedure converts a series of IRAF image files to
# FITS image files.

procedure t_wfits ()

char	iraf_files[SZ_FNAME]	# list of IRAF images
char	fits_files[SZ_FNAME]	# list of FITS files
bool	newtape			# new or used tape ?
char	in_fname[SZ_FNAME]	# input file name
char	out_fname[SZ_FNAME]	# output file name

int	imlist, flist, nimages, nfiles, file_number
bool	clgetb()
double	clgetd()
int	imtopen(), imtlen (), strlen(), wft_get_bitpix(), clgeti(), imtgetim()
int	mtfile(), strmatch(), stridxs(), btoi(), fstati(), fntlenb(), fntgfnb()
pointer	fntopnb()

include "wfits.com"

begin
	# Flush on a newline if STDOUT has not been redirected.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Open iraf_files template and determine number of files in list.
	call clgstr ("iraf_files", iraf_files, SZ_FNAME)
	imlist = imtopen (iraf_files)
	nimages = imtlen (imlist)

	# Get the wfits parameters.
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	make_image = btoi (clgetb ("make_image"))

	# Get the FITS bits per pixel.
	bitpix = wft_get_bitpix (clgeti ("bitpix"))
	if (bitpix != ERR) {
	    call printf ("WARNING: Default bitpix overridden.\n")
	    call printf ("\tBitpix set to: %d\n")
		call pargi (bitpix)
	}

	# Get length of record in FITS bytes.
	len_record = FITS_RECORD
	blkfac = clgeti ("blocking_factor")
	if (blkfac > MAX_BLKFAC && mod (blkfac, SZB_CHAR) != 0)
	    call error (0, "Block size must be an integral number of chars.")
	if (blkfac > 1 && blkfac <= MAX_FITS_BLKFAC) {
	    call printf ("WARNING: FITS tape blocking factor is %d\n")
		call pargi (blkfac)
	}
	if (blkfac > MAX_FITS_BLKFAC) {
	    call printf ("WARNING: Blocking factor %d is not legal fits\n")
		call pargi (blkfac)
	}

	# Get scaling parameters.
	scale = btoi (clgetb ("scale"))
	if (scale == YES) {
	    if (clgetb ("autoscale"))
		autoscale = YES
	    else {
	        bscale = clgetd ("bscale")
	        bzero = clgetd ("bzero")
		autoscale = NO
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
	}

	# Get output file name. If no tape file number is given for output,
	# the user is asked if the tape is blank or contains data.
	# If the tape is blank output begins at BOT, otherwise at EOT

	if (make_image == YES) {
	    call clgstr ("fits_files", fits_files, SZ_FNAME)
	    if (mtfile (fits_files) == YES) {
		flist = NULL
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
	    } else {
		flist = fntopnb (fits_files, NO)
		nfiles = fntlenb (flist)
		if ((nfiles > 1) && (nfiles != nimages))
		    call error (0,
		    "T_WFITS: Input and output lists are not the same length")
	    }
	} else {
	    fits_files[1] = EOS
	    flist = NULL
	}

	# Loop through the list of input images files.

	file_number = 1
	while (imtgetim (imlist, in_fname, SZ_FNAME) != EOF) {

	    # Print id string.
	    if (long_header == YES || short_header == YES) {
		call printf ("File %d: %s")
		    call pargi (file_number)
		    call pargstr (in_fname)
	    }

	    # Get output filename. If single file output to disk, use name
	    # fits_file. If multiple file output to disk, the file number
	    # is added to the output file name, if no output name list is
	    # supplied. If an output name list is supplied then the names
	    # are extracted one by one from that list.

	    if (make_image == YES) {
	        if (mtfile (fits_files) == YES) {
		    if (file_number == 2 && strmatch(fits_files,"[EOT]") == 0) {
		        call sprintf (fits_files[stridxs("[", fits_files)],
			    SZ_FNAME, "%s")
			    call pargstr ("[EOT]")
		    }
		    call strcpy (fits_files, out_fname, SZ_FNAME)
	        } else if (nfiles > 1) {
		    if (fntgfnb (flist, out_fname, SZ_FNAME) == EOF)
			 call error (0, "Error reading output file name")
		} else {
		    call strcpy (fits_files, out_fname, SZ_FNAME)
		    if (nimages > 1) {
		        call sprintf (out_fname[strlen(fits_files)+1], SZ_FNAME,
			    "%03d")
			    call pargi (file_number)
		    }
	        }
	    }

	    # Write each output file.
	    iferr (call wft_write_fitz (in_fname, out_fname)) {
		call printf ("Error writing file: %s\n")
		    call pargstr (out_fname)
		call erract (EA_WARN)
		break
	    } else
		file_number = file_number + 1
	}

	# Close up the input and output lists.
	call clpcls (imlist)
	if (flist != NULL)
	    call fntclsb (flist)
end


# WFT_GET_BITPIX -- This procedure fetches the user determined bitpix or ERR if
# the bitpix is not one of the permitted FITS types.

int procedure wft_get_bitpix (bitpix)

int	bitpix

begin
	switch (bitpix) {
	case FITS_BYTE, FITS_SHORT, FITS_LONG, FITS_REAL, FITS_DOUBLE:
	    return (bitpix)
	default:
	    return (ERR)
	}
end
