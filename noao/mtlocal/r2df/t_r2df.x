include <error.h>
include <fset.h>

define	MAX_RANGES	100
define	NTYPES		7


# R2DF -- CL callable task to read 2D-FRUTTI format data.  Further
# documentation given in 2df.hlp.

procedure t_r2df()

char	infile[SZ_FNAME+1], outfile[SZ_FNAME+1]
char	in_fname[SZ_FNAME+1], out_fname[SZ_FNAME+1]
char	file_list[SZ_LINE+1]
int	range[MAX_RANGES*2+2]
int	file_number, stat, offset, nfiles

bool	clgetb()
char	clgetc()
int	strlen(), clgeti(), btoi(), mtfile()
int	r2dfread(), decode_ranges(), get_next_number(), r2dfget_type()
include	"r2df.com"

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get parameters.
	call clgstr ("r2df_file", infile, SZ_FNAME)
	tape = NO
	if (mtfile (infile) == YES)
	    tape = YES

	# Request the input files to be converted if the input file is a
	# tape device.  Otherwise, convert only the first file.

	if (tape == YES && infile[strlen(infile)] != ']')
	    call clgstr ("file_list", file_list, SZ_LINE)
	else
	    call strcpy ("1", file_list, SZ_LINE)
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (1, "Illegal file number list")
	
	# Set options.
	long_header = btoi (clgetb ("long_header"))
	short_header = btoi (clgetb ("short_header"))
	lsbf = btoi (clgetb ("standard_format"))
	make_image = btoi (clgetb ("make_image"))
	offset = clgeti ("offset")

	if (make_image == YES) {
	    data_type = r2dfget_type (clgetc ("datatype"))
	    call clgstr ("iraf_file", outfile, SZ_FNAME)
	} else
	    outfile[1] = EOS

	# Read successive 2D-FRUTTI files, convert and write into a numbered
	# succession of output IRAF files.

	file_number = 0
        while (get_next_number (range, file_number) != EOF) {
	    # Get names of input and output files.
	    call strcpy (infile, in_fname, SZ_FNAME)
	    call strcpy (outfile, out_fname, SZ_FNAME)

	    if (tape == YES && infile[strlen(infile)] != ']') {
		call sprintf (in_fname[strlen(in_fname)+1], SZ_FNAME, "[%d]")
		    call pargi (file_number)
	    }
	    if (nfiles > 1) {
		call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME, "%03d")
		    call pargi (file_number + offset)
	    }

	    # Convert 2D-FRUTTI file to the output IRAF file.
	    # If EOT is reached then exit.
	    # If an error is detected then print a warning and continue with
	    # the next file.

	    iferr (stat = r2dfread (in_fname,out_fname))
		call erract (EA_FATAL)
	    if (stat == EOF) 	# EOT found
		return
	}
end


# R2DFGET_TYPE -- Convert a character to an IRAF image type.

int procedure r2dfget_type (c)

char	c
int	type_codes[NTYPES], i
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
