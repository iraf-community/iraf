# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<fset.h>
include	<error.h>
include	<mach.h>
include	<imhdr.h>
include	"imtext.h"

define	SZ_FORMAT	20


# WTEXTIMAGE -- Write a text file from an IRAF image.  Image header information
# is written in the "keyword = value / comment" format of FITS.  Pixel values
# follow the header.  The resulting text file can be read as a FITS image.  The
# header cards include "NAXIS = 0", indicating no binary data matrix is written.
# The encoded pixel values can be read as special records following the null
# data matrix.

procedure t_wtextimage ()

bool	header
bool	pixels
pointer	im
char	output[SZ_FNAME], format[SZ_FORMAT], imlist[SZ_LINE]
char	image[SZ_FNAME], out_fname[SZ_FNAME]
int	maxll, file_num, out, input, nfiles

pointer	immap()
bool	clgetb(), strne()
int	clgeti(), imtgetim(), open(), imtopen(), fstati(), imtlen()

begin
	# Open template of input image filenames.
	call clgstr ("input", imlist, SZ_LINE)
	input = imtopen (imlist)
	nfiles = imtlen (input)

	# See if STDOUT has been redirected and get output filename.
	if (fstati (STDOUT, F_REDIR) == YES) {
	    # Output has been redirected, set output filename to STDOUT
	    call strcpy ("STDOUT", output, SZ_FNAME)
	} else {
	    # Get output filename from cl
	    call clgstr ("output", output, SZ_FNAME)
	}
	
	# Get other parameters from cl.
	header = clgetb ("header")
	pixels = clgetb ("pixels")
	maxll = min (MAX_LENTEXT, clgeti ("maxlinelen"))
	if (maxll <= 0)
	    call error (1, "Illegal maximum line length:  must be > 0")

	call clgstr ("format", format, SZ_FORMAT)
	call strlwr (format)

	file_num = 0

	while (imtgetim (input, image, SZ_FNAME) != EOF) {
	    file_num = file_num + 1

	    # Open image.
	    iferr (im = immap (image, READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    if (nfiles > 1 && strne (output, "STDOUT")) {
		# Generate unique output file name
		call sprintf (out_fname, SZ_FNAME, "%s.%03d")
		    call pargstr (output)
		    call pargi (file_num)
	    } else
		call strcpy (output, out_fname, SZ_FNAME)

	    # Open output file. 
	    iferr (out = open (out_fname, APPEND, TEXT_FILE)) {
		call imunmap (im)
		call erract (EA_WARN)
		next
	    }

	    iferr (call wti_convert_image (im,image,out,header,pixels,
		maxll,format))
		call erract (EA_WARN)

	    call imunmap (im)
	    call close (out)
	}

	call imtclose (input)
end


# WTI_CONVERT_IMAGE -- called once for each image to be converted.  This 
# procedure determines the output pixel format and then directs the processing
# depending on user request.

procedure wti_convert_image (im, image, out, header, pixels, maxll, user_format)

pointer	im			# input image
char	image[ARB]		# image name
int	out			# output text file descriptor
bool	header			# convert header information (y/n)?
bool	pixels			# convert pixels (y/n)?
int	maxll			# maximum line length of text file
char	user_format[ARB] 	# output format for single pixel entered by user

int	width, decpl, fmtchar
pointer	sp, out_format, ftn_format, spp_format, ep
errchk	wti_determine_fmt, wti_write_header
errchk	wti_putint, wti_putreal, wti_putcomplex

begin
	call smark (sp)
	call salloc (out_format, SZ_FORMAT, TY_CHAR)
	call salloc (spp_format, SZ_FORMAT, TY_CHAR)
	call salloc (ftn_format, SZ_FORMAT, TY_CHAR)
	call salloc (ep, SZ_LINE, TY_CHAR)

	# Clear the format variables.
	call aclrc (Memc[out_format], SZ_FORMAT)
	call aclrc (Memc[spp_format], SZ_FORMAT)
	call aclrc (Memc[ftn_format], SZ_FORMAT)
	call aclrc (Memc[ep], SZ_LINE)
	fmtchar = ' '

	# Determine the output format.

	if (user_format[1] == EOS) {
	    # Format has not been set by user.  Set appropriate defaults.
	    switch (IM_PIXTYPE(im)) {
	    case TY_USHORT:
		call strcpy ("6d",     Memc[spp_format], SZ_FORMAT)
	    case TY_SHORT:
		call strcpy ("7d",     Memc[spp_format], SZ_FORMAT)
	    case TY_INT:
		call strcpy ("12d",    Memc[spp_format], SZ_FORMAT)
	    case TY_LONG:
	        call strcpy ("12d",    Memc[spp_format], SZ_FORMAT)
	    case TY_REAL:
		call strcpy ("14.7g",  Memc[spp_format], SZ_FORMAT)
	    case TY_DOUBLE:
		call strcpy ("22.15g", Memc[spp_format], SZ_FORMAT)
	    case TY_COMPLEX:
		call strcpy ("21.7z",  Memc[spp_format], SZ_FORMAT)
	    }
	} else
	    call strcpy (user_format, Memc[spp_format], SZ_FORMAT)

	call wti_determine_fmt (Memc[spp_format], Memc[ftn_format],
	    decpl, fmtchar, width)

	# Write the header.
	if (header) {
	    if (width > 0) {
		if ((maxll / width) < 1) {
		    call sprintf (Memc[ep], SZ_LINE, 
	                "%s: output maxlinelen=%d is too short for format %s")
			call pargstr (image)
			call pargi (maxll)
			call pargstr (Memc[ftn_format])
		    call error (2, Memc[ep])
		}

	        call sprintf (Memc[out_format], SZ_FORMAT, "%d%s")
		    call pargi (maxll / width)
		    call pargstr (Memc[ftn_format])
	    } else
		call strcpy ("*", Memc[out_format], SZ_FORMAT)

	    call wti_write_header (im, image, out, Memc[out_format])
	}

	# Write out the pixels in text form.
	if (pixels) {
	    switch (fmtchar) {
	    case 'd':
		 call wti_putint (im, out, maxll, width)
	    case 'e', 'f', 'g':
		 call wti_putreal (im, out, maxll, decpl, fmtchar, width)
	    case 'z':
		 call wti_putcomplex (im, out, maxll, decpl, 'e', width)
	    }
	}

	call sfree (sp)
end


# WTI_DETERMINE_FMT -- Extract field width from input format string and 
# generate a fortran format equivalent to the input spp format.  The input
# format may be either a Fortran sytle format or an SPP format.

procedure wti_determine_fmt (spp_format, ftn_format, decpl, fmtchar, width)

char	spp_format[ARB] 	# SPP format of each pixel
char	ftn_format[ARB]		# equivalent Fortran format (output)
int	decpl			# number of decimal places of precision (output)
int	fmtchar			# format character (output)
int	width			# field width (output)

int	ip
bool	fortran_format
int	ctoi()

begin
	# Parse either an SPP format "W.Dc" or a Fortran format "cW.D" to
	# determine the field width, number of decimal places or precision,
	# and the format char.  If the field width is missing or zero we set
	# width=0 to flag that free format output is desired.

	for (ip=1;  IS_WHITE (spp_format[ip]);  ip=ip+1)
	    ;
	fortran_format = IS_ALPHA (spp_format[ip])
	if (fortran_format) {
	    if (spp_format[ip] == 'i')
		fmtchar = 'd'
	    ip = ip + 1
	}

	# Extract W and D fields.
	if (ctoi (spp_format, ip, width) == 0)
	    width = 0
	if (spp_format[ip] == '.') {
	    ip = ip + 1
	    if (ctoi (spp_format, ip, decpl) == 0)
		decpl = 0
	} else
	    decpl = 0

	if (!fortran_format && spp_format[ip] != EOS) {
	    fmtchar = spp_format[ip]
	    ip = ip + 1
	}

	if (spp_format[ip] != EOS)
	    call error (3, "unacceptable numeric format")

	# Construct the FTN version of the spp_format.  This will be
	# output in the header.

	switch (fmtchar) {
	case 'd':
	    call sprintf (ftn_format, SZ_FORMAT, "I%d")
		call pargi (width)
	case 'e', 'f', 'g':
	    call sprintf (ftn_format, SZ_FORMAT, "%c%d.%d")
		call pargi (TO_UPPER (fmtchar))
		call pargi (width)
		call pargi (decpl)
	case 'z':
	    # Tell Fortran to use a list directed read to read complex data.
	    call strcpy ("*", ftn_format, SZ_FORMAT)
	    width = 0

	default:
	    call error (4, "Improper format.  Must be chosen from [defgz].")
	}
end
