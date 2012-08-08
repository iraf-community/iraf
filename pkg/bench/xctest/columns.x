# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include <chars.h>
include <error.h>

define	MAX_FILES	12

.help columns
.nf___________________________________________________________________
COLUMNS -- convert a multicolumn file into a multifile column.
	One file `sdastemp.n' is produced with each column in a 
	Separate file.

usage: COLUMNS  number_of_columns  File_name
.endhelp______________________________________________________________


# COLUMNS.X --  SDAS support utility
#
# This routine allows SDAS to treat multicolumn tables
# as simple CL lists.  Each column in the table is referenced in
# SDAS by a different parameter, pointing in the .par file to 
# a different list.  This routine is a preprocessor which takes
# a multicolumn file and generates a multifile column.
#
# To allow for column headers in the multicolumn file,
# any line which begins with a `#' will be ignored.
# All data is transferred as text.

procedure t_columns()

char	fname[SZ_FNAME], outfile[SZ_FNAME], outroot[SZ_FNAME]
char	line[SZ_LINE], word[SZ_LINE], filenum[SZ_FNAME]
int	numcols, infile
int	outnum[MAX_FILES]
int	nchar, nfile, ip
int	clgeti(), open(), getline(), itoc(), ctowrd()
errchk  open, getline

begin

	# Get the number of columns and the input file name
	call clgstr ("filename", fname, SZ_FNAME)
	numcols = clgeti ("numcols")
	call clgstr ("outroot", outroot, SZ_FNAME)

	# Open all the files
	infile = open (fname, READ_ONLY, TEXT_FILE)
	for (nfile=1;  nfile <= numcols;  nfile=nfile+1)  {
	    nchar   = itoc (nfile, filenum, 2)
	    call strcpy ( outroot, outfile, SZ_FNAME)
	    call strcat ( filenum, outfile, SZ_FNAME)
	    outnum[nfile] = open (outfile, NEW_FILE, TEXT_FILE)
	}

	# Separate each line of the input file
	while (getline(infile, line) != EOF) {
	    if ((line[1] != '#') && (line[1] != '\n'))  {
		ip = 1
		for (nfile=1; nfile <= numcols; nfile=nfile+1)  {
		    nchar = ctowrd (line, ip, word, SZ_LINE)
		    call strcat ('\n',word, SZ_LINE)
		    call putline (outnum[nfile], word)
		}
	    }
	}

	# close the files
	call close(infile)
	for (nfile=1; nfile <= numcols; nfile=nfile+1) {
	    call close(outnum[nfile])
	}
end
