include <error.h>
include <fset.h>

# T_PDSREAD -- Read PDS format data.  Further documentation given
# in pds.hlp

define	MAX_RANGES	100

procedure t_pdsread()

char	infile[SZ_FNAME]		# the input file name list
char	outfile[SZ_FNAME]		# the output file name list
char	file_list[SZ_FNAME]		# the input file number list
int	offset				# the output file name offset

char	in_fname[SZ_FNAME], out_fname[SZ_FNAME]
int	range[MAX_RANGES*2+1], nfiles, file_number, stat, junk
int	lenlist
pointer	list

bool	clgetb()
char	clgetc()
int	fstati(), btoi(), clgeti(), fntlenb(), fntgfnb(), mtfile()
int	mtneedfileno()
int	pds_read(), decode_ranges(), get_next_number(), pds_get_image_type()
pointer	fntopnb()

include	"rpds.com"

begin
	# Set up the standard output to flush on a newline.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get the input file name(s).
	call clgstr ("pds_file", infile, SZ_FNAME)
	if (mtfile (infile) == YES) {
	    list = NULL
	    if (mtneedfileno (infile) == YES)
	        call clgstr ("file_list", file_list, SZ_LINE)
	    else
	        call strcpy ("1", file_list, SZ_LINE)
	} else {
	    list = fntopnb (infile, YES)
	    lenlist = fntlenb (list)
	    call sprintf (file_list, SZ_LINE, "1-%d")
		call pargi (lenlist)
	}

	# Decode the ranges.
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (1, "Illegal file number list")

	# Setup the output options.
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	make_image = btoi (clgetb ("make_image"))
	tenbit = btoi (clgetb ("tenbit"))
	ninetrack = btoi (clgetb ("ninetrack"))
	offset = clgeti ("offset")

	# Set the output image data type.
	if (make_image == YES) {
	    data_type = pds_get_image_type (clgetc ("datatype"))
	    call clgstr ("iraf_file", outfile, SZ_FNAME)
	} else
	    outfile[1] = EOS

	# Read successive PDS files, convert and write into a numbered
	# succession of output IRAF files.

	file_number = 0
	while (get_next_number (range, file_number) != EOF) {

	    # Get the input file name.
	    if (list != NULL)
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

	    # Convert PDS file to the output IRAF file. If EOT is reached
	    # then exit. If an error is detected then print a warning and
	    # continue with the next file.

	    iferr (stat = pds_read (in_fname, out_fname))
		call erract (EA_FATAL)
	    if (stat == EOF)	# EOT found
		break
	}

	if (list != NULL)
	    call fntclsb (list)
end

# GET_IMAGE_TYPE -- Convert a character to an IRAF image type.

define	NTYPES	7

int procedure pds_get_image_type(c)

char	c
int	i, type_codes[NTYPES]
int	stridx()

string	types "usilrds"			# supported image data types
data	type_codes /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL,
		    TY_DOUBLE, TY_COMPLEX/

begin
	i = stridx (c, types)
	if (i == 0)
	    return (ERR)
	else
	    return (type_codes[stridx (c, types)])
end
