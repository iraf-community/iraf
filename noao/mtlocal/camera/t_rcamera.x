include <error.h>
include <fset.h>

# T_RCAMERA -- Read CAMERA format data.  Further documentation given
# in rcamera.hlp

define	MAX_RANGES	100

procedure t_rcamera()

char	infile[SZ_FNAME]		# input file name list
char	outfile[SZ_FNAME]		# output image name list
char	file_list[SZ_LINE]		# input file number list
char	image_list[SZ_LINE]		# output image number list
int	offset				# output file number offset

char	in_fname[SZ_FNAME], out_fname[SZ_FNAME]
int	range[MAX_RANGES * 2 + 1], image_ranges[MAX_RANGES * 2 + 1]
int	nimages, nfiles, file_number, stat, lenlist, junk
pointer	list

bool	clgetb()
char	clgetc()
int	fstati(), mtfile(), mtneedfileno(), clgeti(), btoi(), fntlenb()
int	fntgfnb(), cam_read(), decode_ranges(), get_next_number()
int	cam_get_image_type()
pointer	fntopnb()

include	"rcamera.com"

begin
	# Flush the standard output.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get the parameters.
	call clgstr ("camera_file", infile, SZ_FNAME)
	if (mtfile (infile) == YES) {
	    list = NULL
	    tape = YES
	    if (mtneedfileno (infile) == YES)
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	} else {
	    tape = NO
	    list = fntopnb (infile, NO)
	    lenlist = fntlenb (list)
	    call sprintf (file_list, SZ_LINE, "1-%d")
		call pargi (lenlist)
	}

	# Decode the ranges string.
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (1, "Illegal file number list")

	# Set up the output options.
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	lsbf = btoi (clgetb ("standard_format"))
	make_image = btoi (clgetb ("make_image"))
	offset = clgeti ("offset")

	# Get the image pixel type.
	if (make_image == YES) {
	    data_type = cam_get_image_type (clgetc ("datatype"))
	    call clgstr ("iraf_file", outfile, SZ_FNAME)
	} else
	    outfile[1] = EOS

	# Get the list of images per file.
	call clgstr ("image_list", image_list, SZ_LINE)
	if (decode_ranges (image_list, image_ranges, MAX_RANGES, nimages)
	    == ERR)
	    call error (1, "Illegal image number list")

	# Read successive CAMERA files, convert and write into a numbered
	# succession of output IRAF files.

	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    # Get the input file name.
	    if (tape == NO)
		junk = fntgfnb (list, in_fname, SZ_FNAME)
	    else {
		if (mtneedfileno (infile) == YES)
		    call mtfname (infile, file_number, in_fname, SZ_FNAME)
	        else
	            call strcpy (infile, in_fname, SZ_FNAME)
	    }

	    # Get the output file name.
	    if (nfiles > 1) {
		call sprintf (out_fname[1], SZ_FNAME, "%s%03d")
		    call pargstr (outfile)
		    call pargi (file_number + offset)
	    } else
	        call strcpy (outfile, out_fname, SZ_FNAME)

	    # Convert CAMERA file to the output IRAF file. If EOT is reached
	    # then exit. If an error is detected that is not trapped lower down
	    # in the code terminate execution.

	    iferr (stat = cam_read (in_fname, out_fname, image_ranges,
	    	    nimages))
		call erract (EA_FATAL)
	    if (stat == EOF) 	# EOT found
		break
	}

	if (list != NULL)
	    call fntclsb (list)
end


# CAM_GET_IMAGE_TYPE -- Convert a character to an IRAF image type.

define	NTYPES	7

int procedure cam_get_image_type (c)

char	c			# the input data type

int	i, type_codes[NTYPES]
int	stridx()
string	types "usilrdx"
data	type_codes /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
		    TY_DOUBLE, TY_COMPLEX/

begin
	i = stridx (c, types)
	if (i == 0)
	    return (ERR)
	else
	    return (type_codes[stridx (c, types)])
end
