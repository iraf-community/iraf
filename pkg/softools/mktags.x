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
