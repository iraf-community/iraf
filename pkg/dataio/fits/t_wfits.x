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
char	fextn[SZ_FNAME]		# the fits extension

char	ch
int	imlist, flist, nimages, nfiles, file_number, addext, index
bool	clgetb(), streq()
double	clgetd()
int	imtopen(), imtlen (), wft_get_bitpix(), clgeti(), imtgetim()
int	mtfile(), btoi(), fstati(), fntlenb(), fntgfnb(), mtneedfileno()
int	wft_blkfac(), fntrfnb(), strlen(), strldx()
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
	if (nimages == 1)
	    wextensions = NO
	else
	    wextensions = btoi (clgetb ("extensions"))
	if (wextensions == NO)
	    gheader = NO
	else
	    gheader = btoi (clgetb ("global_hdr"))
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	make_image = btoi (clgetb ("make_image"))

	# Get the FITS bits per pixel and the FITS logical record size.
	bitpix = wft_get_bitpix (clgeti ("bitpix"))
	len_record = FITS_RECORD

	# Get the scaling parameters.
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

	# Get the output file name and type (tape or disk). If no tape file
	# number is given for output, the user is asked if the tape is blank
	# or contains data. If the tape is blank output begins at BOT,
	# otherwise at EOT.

	call clgstr ("fextn", fextn, SZ_FNAME)
	ch = '.'
	if (make_image == YES) {
	    call clgstr ("fits_files", fits_files, SZ_FNAME)
	    if (mtfile (fits_files) == YES) {
		flist = NULL
		if (mtneedfileno (fits_files) == YES) {
	            newtape = clgetb ("newtape")
		    if (newtape)
			call mtfname (fits_files, 1, out_fname, SZ_FNAME)
		    else
			call mtfname (fits_files, EOT, out_fname, SZ_FNAME)
	        } else {
		    call strcpy (fits_files, out_fname, SZ_FNAME)
		    newtape = false
		}
	    } else {
		flist = fntopnb (fits_files, NO)
		nfiles = fntlenb (flist)
		if (wextensions == YES && nfiles > 1)
		    call error (0,
			"Only one output FITS extensions file can be written")
		if ((nfiles > 1) && (nfiles != nimages))
		    call error (0,
		    "T_WFITS: Input and output lists are not the same length")
	    }
	} else {
	    fits_files[1] = EOS
	    flist = NULL
	}

	# Get the fits file blocking factor.
	blkfac = wft_blkfac (fits_files, clgeti ("blocking_factor"))

	# Loop through the list of input images files.

	file_number = 1
	while (imtgetim (imlist, in_fname, SZ_FNAME) != EOF) {

	    # Get the output file name. If single file output to disk, use
	    # name fits_file. If multiple file output to disk, the file number
	    # is added to the output file name, if no output name list is
	    # supplied. If an output name list is supplied then the names
	    # are extracted one by one from that list.

	    if (make_image == YES) {
	        if (mtfile (fits_files) == YES) {
		    if (wextensions == NO && file_number == 2)
			call mtfname (out_fname, EOT, out_fname, SZ_FNAME)
	        } else if (nfiles > 1) {
		    if (fntgfnb (flist, out_fname, SZ_FNAME) == EOF)
			 call error (0, "Error reading output file name")
		    if (fextn[1] != EOS) {
			addext = OK
			index = strldx (ch, out_fname)
			if (index > 0) {
			    if (streq (fextn, out_fname[index+1]))
				addext = ERR
			    else
				addext = OK
			}
			if (addext == OK){
		            call strcat (".", out_fname, SZ_FNAME)
		            call strcat (fextn, out_fname, SZ_FNAME)
			}
		    }
		} else {
		    if (fntrfnb (flist, 1, out_fname, SZ_FNAME) == EOF)
		        call strcpy (fits_files, out_fname, SZ_FNAME)
		    if (nimages > 1 && wextensions == NO) {
		        call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME,
			    "%04d")
			    call pargi (file_number)
		    }
		    if (fextn[1] != EOS) {
			addext = OK
			index = strldx (ch, out_fname)
			if (index > 0) {
			    if (streq (fextn, out_fname[index+1]))
				addext = ERR
			    else
				addext = OK
			}
			if (addext == OK){
		            call strcat (".", out_fname, SZ_FNAME)
		            call strcat (fextn, out_fname, SZ_FNAME)
			}
		    }
	        }
	    }

	    # Write each output file.
	    iferr (call wft_write_fitz (in_fname, out_fname, file_number,
		nimages)) {
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


# WFT_BLKFAC -- Get the fits tape blocking factor.

int procedure wft_blkfac (file, ublkfac)

char	file[ARB]		# the input file name
int	ublkfac			# the user supplied blocking factor

int	bs, fb, blkfac
pointer	gty
int	mtfile(), mtcap(), gtygeti()
errchk	mtcap(), gtygeti()

begin
	# Return a blocking factor of 1 if the file is a disk file.
	if (mtfile (file) == NO)
	    return (0)

	# Open the tapecap device entry for the given device, and get
	# the device block size and default FITS blocking factor
	# parameters.

	iferr (gty = mtcap (file))
	    return (max (ublkfac,1))
	iferr (bs = gtygeti (gty, "bs")) {
	    call gtyclose (gty)
	    return (max (ublkfac,1))
	}
	iferr (fb = max (gtygeti (gty, "fb"), 1))
	    fb = 1

	# Determine whether the device is a fixed or variable blocked
	# device. Set the fits blocking factor to the value of the fb
	# parameter if device is fixed block or if the user has
	# requested the default blocking factor. Set the blocking factor
	# to the user requested value if the device supports variable
	# blocking factors.

	if (bs == 0) {
	    if (ublkfac <= 0)
	        blkfac = fb
	    else
		blkfac = ublkfac
	} else
	    blkfac = fb

	call gtyclose (gty)

	return (blkfac)
end
