include	<ctype.h>

define	NALLOC	512	# Allocation increment
define	MAXRECS	1000	#  Maximum number of records


# ODR_OPENP -- Open onedspec record image pattern

procedure odr_openp (list, records)

int	list			# Image list
char	records[ARB]		# Record string

int	i, n, nalloc, rec
int	decode_ranges(), imtgetim(), strlen(), get_next_number()
pointer	sp, fname, image, recs, images, imtopen()

begin
	call smark (sp)
	call salloc (fname, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_LINE, TY_CHAR)

	# Check for empty string.
	for (i=1; IS_WHITE(records[i]); i=i+1)
	    ;
	if (records[i] == EOS) {
	    call sfree (sp)
	    return
	}

	# Decode record string.
	call salloc (recs, 300, TY_INT)
	if (decode_ranges (records, Memi[recs], 100, i) == ERR)
	    call error (1, "Bad record specification")
	if (i > MAXRECS)
	    call error (1, "Too many records")

	n = 0
	nalloc = NALLOC
	call malloc (images, nalloc, TY_CHAR)
	Memc[images] = EOS

	rec = -1
	repeat {
	    repeat {
		if (rec < 0) {
		    i = imtgetim (list, Memc[fname], SZ_LINE)
		    if (i == EOF)
			break

		    # Strip sections and extensions
		    call imgimage (Memc[fname], Memc[fname], SZ_LINE)
		    i = strlen (Memc[fname])
		    switch (Memc[fname+i-1]) {
		    case 'h':
			if (i > 3 && Memc[fname+i-4] == '.')
			    Memc[fname+i-4] = EOS
		    case 'l':
			if (i > 2 && Memc[fname+i-3] == '.')
			    Memc[fname+i-3] = EOS
		    }
		}

		i = get_next_number (Memi[recs], rec)
		if (i != EOF) {
		    call sprintf (Memc[image], SZ_LINE, "%s.%04d")
			call pargstr (Memc[fname])
			call pargi (rec)
		    break
		}
		rec = -1
	    }

	    if (i == EOF)
		break

	    n = n + strlen (Memc[image]) + 1
	    if (n > nalloc) {
		nalloc = n + NALLOC
		call realloc (images, nalloc, TY_CHAR)
	    }
	    if (Memc[images] != EOS)
		call strcat (",", Memc[images], nalloc)
	    call strcat (Memc[image], Memc[images], nalloc)
	}

	call imtclose (list)
	list = imtopen (Memc[images])

	call mfree (images, TY_CHAR)
	call sfree (sp)
end
