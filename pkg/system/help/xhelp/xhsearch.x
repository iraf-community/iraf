# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>
include <ctype.h>
include	"xhelp.h"


# XH_SEARCH -- Given a search pattern generate a list of tasks matching the
# pattern.

procedure xh_search (xh, exact_match, pattern)

pointer	xh					#i task struct pointer
int	exact_match				#i require an exact match?
char	pattern[ARB]				#i search pattern

pointer	sp, lfile, line, helpstr, item
pointer	tp, lp, pat
char	task[SZ_FNAME], pkg[SZ_FNAME], desc[SZ_FNAME]
int	i, ip, fd, fdl
long	fsize

long	fstatl()
int	open(), xh_match(), getline(), gstrcpy()
errchk	open, xh_updt_quickref

begin
	call smark (sp)
	call salloc (lfile, SZ_FNAME, TY_CHAR)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (item, SZ_LINE, TY_CHAR)
	call salloc (pat, SZ_FNAME, TY_CHAR)

	# Open the quick reference file.
	iferr (fd = open (QUICKREF(xh), READ_ONLY, TEXT_FILE))
	    call error (0, "Cannot open quick-reference file.")

	# Open the temp file for the matched lines.
        call mktemp ("tmp$xhelpq", Memc[lfile], SZ_FNAME)
        fdl = open (Memc[lfile], NEW_FILE, TEXT_FILE)

	# Allocate space for the results string.
	fsize = fstatl (fd, F_FILESIZE)

	# Check pattern for multi-word searches.
	call aclrc (Memc[pat], SZ_FNAME)
	for (ip=1; IS_WHITE(pattern[ip]); ip=ip+1)
	    ;
	if (pattern[ip] == '{') {
	    ip = ip + 1
	    for (i=0; pattern[ip] != '}'; ip=ip+1) {
		Memc[pat+i] = pattern[ip]
		i = i + 1
	    }
	} else
	    call strcpy (pattern[ip], Memc[pat], SZ_FNAME)

	# Loop over the lines in the file and match the pattern.
	while (getline(fd,Memc[line]) != EOF) {
	    if (xh_match (Memc[line], Memc[pat], exact_match) != 0)
		call putline (fdl, Memc[line])	# save it to the file
	}
	call close (fdl)
	call close (fd)

	# Now read back the file to see if we matched anything, and so we
	# can parse the file for task/package names.
        fdl = open (Memc[lfile], READ_ONLY, TEXT_FILE)
	fsize = fstatl (fdl, F_FILESIZE)

	# See whether we got anything, if not return.
	if (fsize == 0) {
	    call sprintf (Memc[line], SZ_LINE, 
		"No help available for\npattern `%s'.")
	            call pargstr (Memc[pat])
	    call gmsg (XH_GP(xh), "alert", Memc[line])

	    call close (fdl)
	    call delete (Memc[lfile])
	    call sfree (sp)
	    return
	}
	call calloc (helpstr, 5*fsize, TY_CHAR)

	# Read back the sorted list, separate the task name, package, and
	# descriptions and format it for the GUI.
	tp = helpstr
	tp = tp + gstrcpy ("<HTML><BODY><PRE>", Memc[tp], ARB)
	while (getline(fdl, Memc[line]) != EOF) {
	    Memc[item] = EOS
	    lp = line

	    # Get the task name.
	    for (i=1; !IS_WHITE(Memc[lp]); i=i+1) {
		task[i] = Memc[lp]
		lp = lp + 1
		if (Memc[lp] == EOS || i >= SZ_FNAME)
		    break 
	    }
	    task[i] = EOS

	    # Skip delimiter.
	    while (IS_WHITE(Memc[lp]) || Memc[lp] == '-') 
		lp = lp + 1

	    # Get the description up to the '[' package name.
	    for (i=1; Memc[lp] != '['; i=i+1) {
		desc[i] = Memc[lp]
		lp = lp + 1
		if (Memc[lp] == EOS || i >= SZ_FNAME)
		    break 
	    }
	    desc[i] = EOS

	    # Get the package name up to the ']' delimiter.
	    if (Memc[lp] != EOS) {
		lp = lp + 1
	        for (i=1; Memc[lp] != ']'; i=i+1) {
		    pkg[i] = Memc[lp]
		    lp = lp + 1
		    if (Memc[lp] == '\n' || Memc[lp] == EOS || i >= SZ_FNAME)
		        break 
	        }
	    }
	    pkg[i] = EOS

	    call sprintf(Memc[item], SZ_LINE, "<A HREF=%s.%s>%10.10s</A> ")
		call pargstr (pkg)
		call pargstr (task)
		call pargstr (task)
	    tp = tp + gstrcpy (Memc[item], Memc[tp], ARB)
	    call sprintf(Memc[item], SZ_LINE, "<A HREF=%s.%s>%10.10s</A> ")
		call pargstr (pkg)
		call pargstr (pkg)
		call pargstr (pkg)
	    tp = tp + gstrcpy (Memc[item], Memc[tp], ARB)
	    call sprintf(Memc[item], SZ_LINE, "  %s\n")
		call pargstr (desc)
	    tp = tp + gstrcpy (Memc[item], Memc[tp], ARB)
	}
	call strcat ("</PRE></BODY></HTML>\n", Memc[tp], SZ_LINE)
	Memc[tp] = EOS
	call close (fdl)

	# Send it to the GUI.
	call gmsg (XH_GP(xh), "apropos", Memc[helpstr])

	# Clean up.
	call delete (Memc[lfile])
	call mfree (helpstr, TY_CHAR)
	call sfree (sp)
end


# XH_MATCH -- Match any or all words in a pattern against the given line.
# We can either look for an exact match or just the occurence of one word
# in the pattern.

int procedure xh_match (line, pattern, exact_match)

char	line[ARB]				#i line to be matched
char	pattern[ARB]				#i pattern words
int	exact_match

char	word[SZ_FNAME]
int	i, j
int	strsearch()

begin
	if (exact_match == YES) {
	    return (strsearch(line, pattern))
	} else {
	    # See if any word in the pattern matches in the line.
	    for (i=1; pattern[i] != EOS; i=i+1) {
		for (j=1; pattern[i] != EOS && pattern[i] != ' '; j=j+1) {
		    word[j] = pattern[i]
		    i = i + 1
		}
		word[j] = EOS
	        if (strsearch(line, word) != 0)
		    return (YES)
	    }
	    return (NO)
	}
end
