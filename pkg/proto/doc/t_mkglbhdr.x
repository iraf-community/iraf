include	<error.h>
include	<imhdr.h>

# T_MKGLBHDR -- Make a global header from common image and reference keywords.

procedure t_mkglbhdr ()

pointer	input			# Input image list
char	oname[SZ_FNAME]		# Output global image
char	rname[SZ_FNAME]		# Reference image
pointer	exclude			# Exclusion keyword list

int	i
char	iname[SZ_FNAME], key[8]
pointer	im, refim, recs, ptr, ptr1

bool	matchcard()
int	clpopnu(), clgfil(), imtgetim(), strncmp(), nowhite()
pointer	imtopenp(), immap()

errchk	immap

begin
	# Get parameters.
	input = imtopenp ("input")
	call clgstr ("output", oname, SZ_FNAME)
	call clgstr ("reference", rname, SZ_FNAME)
	exclude = clpopnu ("exclude")

	# Use the first image in the input list if no reference is specified.
	if (nowhite (rname, rname, SZ_FNAME) == 0)
	    i = imtgetim (input, rname, SZ_FNAME)

	iferr {
	    im = NULL; refim = NULL; recs = NULL

	    # Make list of reference cards.
	    ptr = immap (rname, READ_ONLY, 0); refim = ptr
	    ptr = IM_USERAREA(refim)
	    do i = 0, ARB {
		if (Memc[ptr] == EOS)
		    break
		if (i == 0)
		    call malloc (recs, 1000, TY_POINTER)
		else if (mod (i, 1000) == 0)
		    call realloc (recs, i+1000, TY_POINTER)
		#Memc[ptr+80] = EOS
	        Memi[recs+i] = ptr
	        ptr = ptr + 81
	    }
	    Memi[recs+i] = EOF

	    # Exclude specified keywords.
	    while (clgfil (exclude, iname, SZ_FNAME) != EOF) {
	        call sprintf (key, 8, "%-8.8s")
		    call pargstr (iname)
		call strupr (key)
		for (i=0; Memi[recs+i]!=EOF; i=i+1) {
		    ptr = Memi[recs+i]
		    if (ptr == NULL)
		        next
		    if (Memc[ptr] == ' ')
		        next
		    if (strncmp (key, Memc[ptr], 8) == 0)
		        Memi[recs+i] = NULL
		}
	    }
	        
	    # Loop through input images eliminating reference cards.
	    while (imtgetim (input, iname, SZ_FNAME) != EOF) {
	        ptr = immap (iname, READ_ONLY, 0); im = ptr
		ptr = IM_USERAREA(im)
		for (i=0; Memi[recs+i]!=EOF; i=i+1) {
		    ptr = Memi[recs+i]
		    if (ptr == NULL)
		        next
		    if (Memc[ptr] == ' ')
		        next
		    if (!matchcard (Memc[IM_USERAREA(im)], Memc[ptr]))
		        Memi[recs+i] = NULL
		}
		call imunmap (im)
	    }

	    # Eliminate multiple blank lines.
	    for (i=0; Memi[recs+i]!=EOF; i=i+1) {
		ptr1 = Memi[recs+i]
	        if (ptr == NULL)
		    next
		if (Memc[ptr] != ' ')
		    break
		Memi[recs+i] = NULL
	    }
	    ptr1 = ptr
	    for (; Memi[recs+i]!=EOF; i=i+1) {
		ptr = Memi[recs+i]
	        if (ptr == NULL)
		    next
		if (Memc[ptr] == ' ' && Memc[ptr1] == ' ')
		    Memi[recs+i] = NULL
		else
		    ptr1 = ptr
	    }

	    # Write the output global header.
	    ptr = immap (oname, NEW_COPY, refim); im = ptr
	    IM_PIXTYPE(im) = TY_SHORT
	    IM_NDIM(im) = 0
	    ptr1 = IM_USERAREA(im)
	    for (i=0; Memi[recs+i]!=EOF; i=i+1) {
		ptr = Memi[recs+i]
		if (ptr == NULL)
		    next
		call strcpy (Memc[ptr], Memc[ptr1], 81)
		ptr1 = ptr1 + 81
	    }
	    Memc[ptr1] = EOS
	    call imunmap (im)

	} then
	    call erract (EA_WARN)


	# Finish up.
	if (im != NULL)
	    call imunmap (im)
	if (refim != NULL)
	    call imunmap (refim)
	call mfree (recs, TY_POINTER)

	call clpcls (exclude)
	call imtclose (input)
end


# MATCHCARD -- Match a card given by pat to a string which is a user area.
# This is a simple version of gstrmatch.

bool procedure matchcard (str, pat)

char	str[ARB]		# String to search
char	pat[ARB]		# String to match
char	ch, pch
int	i, ip, pp

begin
	do ip = 1, ARB {
	    if (str[ip] == EOS)
		break

	    i = ip
	    for (pp=1;  pp < 81;  pp=pp+1) {
		pch = pat[pp]
		ch = str[i]
		i = i + 1
		if (pch != ch)
		    break
	    }

	    if (pp == 81)
		return (true)
	    else if (str[i] == EOS)
		break
	}

	return (false)
end
