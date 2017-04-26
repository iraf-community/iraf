# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# WORDS -- Break the input up into a series of words or strings.  A word
# is a sequence of characters delimited by whitespace or newline.  A string
# is delimited by single or double quotes, and may not span more than a single
# line.

procedure t_words()

int	fd, list, last_nscan
pointer	sp, fname, word
int	clpopni(), clgfil(), fscan(), nscan(), open()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (word, SZ_LINE, TY_CHAR)

	list = clpopni ("files")

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)

	    # We do not know how may "words" there are on a line; get words
	    # until no more.
	    while (fscan (fd) != EOF)
		repeat {
		    # When nscan() does not increment after a call to gargwrd(),
		    # we are all done.
		    last_nscan = nscan()
		    call gargwrd (Memc[word], SZ_LINE)
		    if (nscan() > last_nscan) {
			call printf ("%s\n")
			    call pargstr (Memc[word])
		    } else
			break
		}

	    call close (fd)
	}

	call clpcls (list)
	call sfree (sp)
end
