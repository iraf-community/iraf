# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pattern.h>
include	<ctype.h>

# MATCH -- Search for the indicated pattern in each line of a file or files,
# passing or stopping only those lines which contain the pattern.

procedure t_match()

bool	stop_matched_lines		# pass or stop matched lines?
char	chset[2]
int	fd, list
pointer	sp, ip, line, fname, patbuf, pattern
bool	line_matches, encode_pattern, print_file_names, match_pattern
bool	clgetb()
int	strmatch(), strsearch(), patmatch(), patmake(), stridxs()
int	open(), getline(), clpopni(), clplen(), clgfil()

begin
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (patbuf, SZ_LINE, TY_CHAR)
	call salloc (pattern, SZ_FNAME, TY_CHAR)

	# Get pattern.  If it contains either * or [, general pattern
	# matching with an encoded pattern must be used.  If no metacharacters
	# at all are used, or if metacharacters are disabled, use STRSEARCH
	# for maximum efficiency.

	call clgstr ("pattern", Memc[pattern], SZ_FNAME)
	if (clgetb ("metacharacters")) {

	    # Check for the tough pattern matching metachars.
	    chset[1] = CH_CLOSURE
	    chset[2] = CH_CCL
	    chset[3] = EOS
	    encode_pattern = (stridxs (chset, Memc[pattern]) > 0)

	    # If don't have to use patmatch, see if we have a simple alphanum
	    # string so that we can use strsearch.

	    if (!encode_pattern) {
		for (ip=pattern;  IS_ALNUM(Memc[ip]);  ip=ip+1)
		    ;
		match_pattern = (Memc[ip] != EOS)
	    }

	} else {
	    encode_pattern = false
	    match_pattern = false
	}

	stop_matched_lines = clgetb ("stop")
	list = clpopni ("files")
	print_file_names = (clplen(list) > 1) && (clgetb("print_file_names"))

	if (encode_pattern)
	    if (patmake (Memc[pattern], Memc[patbuf], SZ_LINE) == ERR)
		call error (1, "Pattern is too complex")

	# Search each file in the list, passing a line on to the output
	# only if the line matches and stop is false, or if the line does
	# not match and stop is true.

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)

	    while (getline (fd, Memc[line]) != EOF) {
		if (encode_pattern)
		    line_matches = (patmatch (Memc[line], Memc[patbuf]) > 0)
		else if (match_pattern)
		    line_matches = (strmatch (Memc[line], Memc[pattern]) > 0)
		else
		    line_matches = (strsearch (Memc[line], Memc[pattern]) > 0)

		if ((line_matches && !stop_matched_lines) ||
		    (!line_matches && stop_matched_lines)) {

		    if (print_file_names) {
			call printf ("%s:")
			    call pargstr (Memc[fname])
		    }

		    call putline (STDOUT, Memc[line])
		    call flush (STDOUT)
		}
	    }

	    call close (fd)
	}

	call sfree (sp)
	call clpcls (list)
end
