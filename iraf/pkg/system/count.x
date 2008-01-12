# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>
include	<error.h>

define	SZ_LUT		256		# "whitespace" lookup table
define	CC_WHITE	1		# "white" character class
define	CC_NONWHITE	0		# "word" character

# COUNT -- Count the number of lines, words, and characters in the named text
# files, or in the standard input.  Print the results on the standard output.
# 
# output format (single file):
# 
# 	nlines nwords nchars
# 
# output format (several files):
# 
# 	nlines nwords nchars filename1
# 	nlines nwords nchars filename2
# 	nlines nwords nchars Total

procedure t_count()

char	fname[SZ_FNAME]
long	nlines, nwords, nchars
long	totlines, totwords, totchars
int	nfiles, list
int	clpopni(), clgfil()

begin
	totlines = 0
	totwords = 0
	totchars = 0
	nfiles = 0

	list = clpopni ("files")

	while (clgfil (list, fname, SZ_FNAME) != EOF) {
	    iferr (call count_file (fname, nlines, nwords, nchars))
		call erract (EA_WARN)
	    else {
		call print_stats (STDOUT, nlines, nwords, nchars, fname)
		call flush (STDOUT)
	    }

	    totlines = totlines + nlines
	    totwords = totwords + nwords
	    totchars = totchars + nchars
	    nfiles = nfiles + 1
	}

	call clpcls (list)

	if (nfiles > 1)
	    call print_stats (STDOUT, totlines, totwords, totchars, "Total")
end


# COUNT_FILE -- Open a file and count the number of lines, words, and
# characters in the file.

procedure count_file (fname, nlines, nwords, nchars)

char	fname[SZ_FNAME], lbuf[SZ_LINE], ch, class[SZ_LUT]
long	nlines, nwords, nchars
int	first_time, state, fd, ip, open(), getline()
errchk	open, getline
data	first_time /YES/

begin
	# Initialize the lookup table, used to count words.  In this case,
	# NEWLINE is considered to be whitespace.

	if (first_time == YES) {
	    do ip = 1, SZ_LUT
		class[ip] = CC_NONWHITE

	    class[BLANK] = CC_WHITE
	    class[TAB] = CC_WHITE
	    class[NEWLINE] = CC_WHITE

	    first_time = NO
	}

	fd = open (fname, READ_ONLY, TEXT_FILE)

	nwords = 0
	nchars = 0
	state = CC_WHITE

	# Increment word count at the beginning of every word.  A "word"
	# is defined as a sequence of characters delimited by whitespace.
	# COUNT does not know anything about quoted strings.

	for (nlines=0;  getline (fd, lbuf) != EOF;  ) {
	    do ip = 1, SZ_LINE {
		ch = lbuf[ip]
		if (ch == EOS)
		    break
		else if (class[ch] != state) {
		    nwords = nwords + state
		    state = class[ch]
		}
	    }
	    nchars = nchars + ip - 1
	    if (lbuf[ip-1] == '\n')
		nlines = nlines + 1
	}   

	call close (fd)
end


# PRINT_STATS -- Format the COUNT statistics summary line and print it on
# the named file.

procedure print_stats (fd, nlines, nwords, nchars, fname)

int	fd
long	nlines, nwords, nchars
char	fname[ARB]

begin
	call fprintf (fd, "%7d %7d %7d %s\n")
	    call pargl (nlines)
	    call pargl (nwords)
	    call pargl (nchars)
	    call pargstr (fname)
end
