include <imhdr.h>
include <mach.h>
include <fset.h>
include	"rcamera.h"

# CAM_READ_HEADER -- Read a CAMERA header.
# If EOF is reached the routine returns EOF, otherwise it returns the
# number of data records in the CAMERA image file.

int procedure cam_read_header (cam_fd)

int	cam_fd

char	text[LEN_CAM_TEXT], header[LEN_HEADER * SZ_SHORT]
int	i, sz_rec, nbytes
pointer	im
short	parameters[LEN_CAM_PARAMETERS]
int	read(), cam_roundup(), fstati()

# entry points
int	cam_rparams()
errchk	cam_decode_header, read, chrupk, bswaps
include "rcamera.com"

begin
	# Read in header record.
	sz_rec = cam_roundup (SZB_CHAR * LEN_HEADER, SZB_CHAR) / SZB_CHAR
	i = read (cam_fd, header, sz_rec)
	if (i == EOF)
	    return (EOF)
	else if (i != sz_rec)
	    call error (1, "Error reading CAMERA header")
	if (tape == YES) {
	    nbytes = fstati (cam_fd, F_SZBBLK)
	    if ((nbytes != SZB_CHAR * LEN_HEADER) && (nbytes != SZB_CHAR *
		(LEN_HEADER+ 1)))
		call error (0, "Not a camera file")
	}

	# If the least significant byte is first byteswap the camera
	# parameters otherwise byteswap the header text. Return the
	# number of data records which is stored in parameters[4].

	if (lsbf == YES)
	    call bswap2 (header, 2 * LEN_CAM_PARAMETERS + 1, header,
			 2 * LEN_CAM_PARAMETERS + 1, LEN_CAM_TEXT)
	if (lsbf != BYTE_SWAP2)
	    call bswap2 (header, 1, header, 1, 2 * LEN_CAM_PARAMETERS)
	call bytmov (header, 1,  parameters, 1, LEN_CAM_PARAMETERS * 2)

	# If tape return the number of tape data records to skip, otherwise
	# return the number of chars to skip. An error will occur if the
	# number of tape bytes to skip is not an integral number of chars.

	if (tape == YES)
	    return (int (parameters[4]))
	else
	    return (int (parameters[4]) * int (parameters[25]) * 2 / SZB_CHAR)

# Decode the header parameters.
entry cam_rparams (im)

	# Extract and trim the header text.
	call bytmov (header, 2 * LEN_CAM_PARAMETERS + 1, text, 1, LEN_CAM_TEXT)
	call chrupk (text, 1, text, 1, LEN_CAM_TEXT)
	for (i = LEN_CAM_TEXT; text[i] == ' ' && i >= 1; i = i - 1)
	    ;
	text[i+1] = EOS

	# Put the CAMERA parameters in the IRAF image header.
	call cam_decode_header (im, parameters, text)
	call cam_print_header (parameters, text)

	return (OK)
end


# CAM_DECODE_HEADER -- Decode a CAMERA header record.

procedure cam_decode_header (im, parameters, text)

pointer	im
short	parameters[ARB]
char	text[ARB]

include	"rcamera.com"

begin
	# Determine the length of the record in short integers.
	len_record = REC_LEN(parameters)

	# Set IRAF image parameters. Send extra keywords to the user area.
	if (make_image == YES) {
	    NAXIS(im) = 2
	    PARAM5(im) = NAXIS1(parameters)
	    PARAM6(im) = NAXIS2(parameters)
	    call strcpy (text, TITLE(im), LEN_TITLE)
	    call cam_store_keywords (parameters, im)
	}
end


# CAM_PRINT_HEADER -- Print the CAMERA header

procedure cam_print_header (parameters, text)

short	parameters[ARB]
char	text[ARB]

include	"rcamera.com"

begin
	if (long_header == YES)
	    call cam_long_header (parameters, text)
	if (short_header == YES && long_header == NO) {
	    call printf ("ID: %.30s  ")
	        call pargstr (text)
	    call printf ("Size = %d x %d\n")
		call pargs (NAXIS1(parameters))
		call pargs (NAXIS2(parameters))
	}
end


# CAM_ROUNDUP -- Procedure to round an integer to the next highest number
# divisible by base.

int procedure cam_roundup (number, base)

int	number, base

int	value

begin
	if (mod(number, base) == 0)
	    return (number)
	else {
	    value = (number/base + 1) * base
	    return (value)
	}
end
