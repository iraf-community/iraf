# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<ctotok.h>

define	SZ_LBUF		1024
define	MAX_TAGS	8192
define	SZ_SBUF		256000
define	TAGSFILE	"tags"


# MKTAGS -- Make a "tags" database for the VI editor.  Each line of this
# database contains the following fields:
#
#	tag		name of a procedure or macro define
#	file		source file in which the tag is found
#	pattern		VI search pattern for opening file to pattern
#
# Mktags takes as input a list of files and produces [1] the "tags" database
# file as output, [2] a tags listing on the standard output, or both [1],[2].
# VI uses the tags database to rapidly move to the source for the named tag,
# using the ":ta tag" command.  This is very useful when editing a large
# package, especially when it is not clear what file the tagged procedure
# is in.

procedure t_mktags()

char	fname[SZ_FNAME], lbuf[SZ_LBUF], tag[SZ_FNAME]
bool	listing, mktags
int	fd, list, ip, ip1, ip2, op, linenum, i, j, out

bool	clgetb()
int	tg_compare()
extern	tg_compare()
int	clpopni(), clgfil()
int	open(), tg_getlongline(), gstrmatch(), stridxs()
pointer	tg_putstr()

int	ntags
pointer	tg_op
pointer	tg_sbuf
pointer	tg_tag[MAX_TAGS]
pointer	tg_file[MAX_TAGS]
pointer	tg_lnum[MAX_TAGS]
pointer	tg_lbuf[MAX_TAGS]
int	tg_sort[MAX_TAGS]

common	/tagcom/ ntags, tg_op, tg_sbuf, tg_tag, tg_file, tg_lnum, tg_lbuf,
	tg_sort

begin
	call malloc (tg_sbuf, SZ_SBUF, TY_CHAR)
	list  = clpopni ("files")
	tg_op = tg_sbuf
	ntags = 0

	listing = clgetb ("listing")
	mktags  = clgetb ("tags")

	# Process each file in the list.
	while (clgfil (list, fname, SZ_FNAME) != EOF) {
	    iferr (fd = open (fname, READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		next
	    } else
		linenum = 0

	    # Examine each line in the file until a procedure statement is
	    # found.  Add a tag record for each such item found.

	    while (tg_getlongline (fd, lbuf, SZ_LBUF, linenum) != EOF) {
		if (gstrmatch (lbuf, "procedure", ip1, ip2) > 0) {
		    # Ignore keyword if found in argument list or string
		    # constant.

		    ip = stridxs ("(\"#,;", lbuf)
		    if (ip < ip1)
			next

		    # Extract tag name.
		    for (ip=ip2+1;  IS_WHITE (lbuf[ip]);  ip=ip+1)
			;
		    for (op=1;  lbuf[ip] != EOS;  ip=ip+1)
		        if (IS_ALNUM(lbuf[ip]) || lbuf[ip] == '_') {
			    tag[op] = lbuf[ip]
			    op = op + 1
			} else
			    break
		    tag[op] = EOS

		    # Delete newline.
		    while (lbuf[ip] != EOS)
			ip = ip + 1
		    if (lbuf[ip-1] == '\n')
			lbuf[ip-1] = EOS

		    # Add tag to tag list.
		    ntags = ntags + 1
		    if (ntags > MAX_TAGS)
			call error (1, "too many tags")

		    tg_file[ntags] = tg_putstr (fname)
		    tg_lnum[ntags] = linenum
		    tg_tag [ntags] = tg_putstr (tag)
		    tg_lbuf[ntags] = tg_putstr (lbuf)
		    tg_sort[ntags] = ntags
		}
	    }

	    call close (fd)
	    call flush (STDOUT)
	}

	call clpcls (list)

	# Sort the tags list.
	if (ntags > 1)
	    call qsort (tg_sort, ntags, tg_compare)

	# Print the tags database file.  Any existing tags file will be
	# overwritten.

	if (mktags) {
	    iferr (call delete (TAGSFILE))
		;
	    out = open (TAGSFILE, NEW_FILE, TEXT_FILE)

	    do i = 1, ntags {
		j = tg_sort[i]
		call fprintf (out, "%s\t%s\t/^%s$/\n")
		    call pargstr (Memc[tg_tag[j]])
		    call pargstr (Memc[tg_file[j]])
		    call pargstr (Memc[tg_lbuf[j]])
	    }

	    call close (out)
	}

	# Print the tags listing if desired.
	if (listing) {
	    do i = 1, ntags {
		j = tg_sort[i]

		# Process spooled line, compressing whitespace to a single
		# blank and skipping leading whitespace.
			
		op = 1
		for (ip=tg_lbuf[j];  IS_WHITE (Memc[ip]);  ip=ip+1)
		    ;
		for (;  Memc[ip] != EOS;  ip=ip+1)
		    if (IS_WHITE (Memc[ip])) {
			lbuf[op] = ' '
			op = op + 1
			while (IS_WHITE (Memc[ip]))
			    ip = ip + 1
			ip = ip - 1
		    } else {
			lbuf[op] = Memc[ip]
			op = op + 1
		    }
		lbuf[op] = EOS

		call printf ("%-18s%3d %-18s %s\n")
		    call pargstr (Memc[tg_tag[j]])
		    call pargi   (tg_lnum[j])
		    call pargstr (Memc[tg_file[j]])
		    call pargstr (lbuf)
	    }
	}

	call mfree (tg_sbuf, TY_CHAR)
end


# TG_COMPARE -- String compare of two tags.

int procedure tg_compare (s1, s2)

int	s1			# t_sort index of string 1
int	s2			# t_sort index of string 2

int	ntags
pointer	tg_op
pointer	tg_sbuf
pointer	tg_tag[MAX_TAGS]
pointer	tg_file[MAX_TAGS]
pointer	tg_lnum[MAX_TAGS]
pointer	tg_lbuf[MAX_TAGS]
int	tg_sort[MAX_TAGS]

common	/tagcom/ ntags, tg_op, tg_sbuf, tg_tag, tg_file, tg_lnum, tg_lbuf,
	tg_sort

int	strncmp()

begin
	return (strncmp (Memc[tg_tag[s1]], Memc[tg_tag[s2]], ARB))
end


# TG_PUTSTR -- Add a string to the string buffer and return a pointer to
# the beginning of the string.

pointer procedure tg_putstr (str)

char	str[ARB]		# string to be appended

int	nchars
pointer	newstr
int	strlen()

int	ntags
pointer	tg_op
pointer	tg_sbuf
pointer	tg_tag[MAX_TAGS]
pointer	tg_file[MAX_TAGS]
pointer	tg_lnum[MAX_TAGS]
pointer	tg_lbuf[MAX_TAGS]
int	tg_sort[MAX_TAGS]

common	/tagcom/ ntags, tg_op, tg_sbuf, tg_tag, tg_file, tg_lnum, tg_lbuf,
	tg_sort

begin
	nchars = strlen (str)
	newstr = tg_op

	if (tg_op - tg_sbuf + nchars >= SZ_SBUF)
	    call error (2, "out of string buffer space")

	call strcpy (str, Memc[newstr], nchars)
	tg_op = tg_op + nchars + 1

	return (newstr)
end


# TG_GETLONGLINE -- Get a long line, i.e., a logical line possibly spanning
# several physical lines with the newlines escaped at the ends.  Skip
# comment lines and .help sections.

int procedure tg_getlongline (fd, obuf, maxch, linenum)

int	fd			# input file
char	obuf[ARB]		# output buffer
int	maxch
int	linenum

int	op, status
int	getline(), strncmp()

begin
	op = 1

	while (maxch - op + 1 >= SZ_LINE) {
	    # Get next non-comment line.
	    repeat {
		status = getline (fd, obuf[op])
		linenum = linenum + 1

		if (status == EOF) {
		    break
		} else if (obuf[op] == '#') {
		    next
		} else if (obuf[op] == '.') {
		    # Skip help sections.
		    if (strncmp (obuf[op], ".help", 5) == 0) {
			repeat {
			    status = getline (fd, obuf[op])
			    linenum = linenum + 1
			    if (status == EOF)
				break
			    if (strncmp (obuf[op], ".endhelp", 8) == 0)
				break
			}
		    } else
			break
		} else
		    break
	    }

	    if (status == EOF) {
		if (op == 1)
		    return (EOF)
		else
		    return (op - 1)
	    } else
		op = op + status

	    if (obuf[op-2] == '\\' && obuf[op-1] == '\n')
		op = op - 2
	    else
		break
	}

	return (op - 1)
end
