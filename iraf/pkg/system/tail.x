# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

# TAIL -- Print the tail (last few lines) of each of the named text files on the
# standard output.  If more than one file is to be printed, a brief header is
# printed for each file.
# 
# Params:
# 	input_files		File matching template
# 	nlines [h,12]		Number of lines at tail of file to be printed
#
# If NLINES is negative, its absolute value is the number of lines to skip at
# head of file.

procedure t_tail()

char	fname[SZ_FNAME]
bool	multiple_files
int	nlines, list
int	clpopni(), clplen(), clgfil(), clgeti()

begin
	list = clpopni ("input_files")
	nlines = clgeti ("nlines")

	multiple_files = (clplen (list) > 1)
	
	while (clgfil (list, fname, SZ_FNAME) != EOF)
	    iferr (call print_tail (STDOUT, fname, nlines, multiple_files))
		call erract (EA_WARN)

	call clpcls (list)
end


# PRINT_TAIL -- Print the last few lines of a file on the output stream
# given as the first argument, optionally plus a header.

procedure print_tail (out, fname, nlines, print_file_name)

char	fname[ARB]
int	out, nlines
bool	print_file_name

pointer	sp, offsets, line
int	in, linenum, index, noffsets, nlines_in_file
int	open(), getline()
long	note()
errchk	open, getline, putline, note

begin
	if (nlines == 0)
	    return
	else
	    noffsets = abs (nlines) + 1

	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (offsets, noffsets, TY_LONG)

	# Open the file.
	in = open (fname, READ_ONLY, TEXT_FILE)

	if (print_file_name) {
	    call fprintf (out, "\n\n===> %s <===\n")
		call pargstr (fname)
	}

	# If nlines is negative, skip nlines lines at head of file and
	# print rest of file.  If nlines is positive, print nlines lines
	# at tail of file.

	linenum = 0
	repeat {
	    linenum = linenum + 1
	    if (nlines < 0 && linenum > -nlines)
		break
	    else if (nlines > 0) {
		index = mod (linenum - 1, noffsets)
		Meml[offsets+index] = note (in)
	    }
	} until (getline (in, Memc[line]) == EOF)

	nlines_in_file = linenum - 1

	# Seek back to offset of desired line, if not skipping head of file.
	if (nlines > 0)
	    if (nlines_in_file <= nlines)
		call seek (in, BOFL)
	    else {
		index = mod (nlines_in_file - nlines, noffsets)
		call seek (in, Meml[offsets+index])
	    }

	# If nlines is positive, print nlines lines, otherwise print rest
	# of file.
	linenum = 1
	while ((linenum <= nlines || nlines < 0) &&
	    (getline (in, Memc[line]) != EOF)) {
		call putline (out, Memc[line])
		linenum = linenum + 1
	}

	call sfree (sp)
	call close (in)
end
