include	<imhdr.h>


# T_TEXT2MASK -- Create a pixel mask from a text file.
# The text file consists of rectangular regions.  The mask values may
# be set to identify rectangles which are square, narrower along lines,
# and narrower along columns.

procedure t_text2mask ()

pointer	text			# Text file
pointer	mask			# Pixel mask
int	nc			# Number of columns
int	nl			# Number of lines
short	linterp			# Mask value for narrow line rectangles 
short	cinterp			# Mask value for narrow column rectangles 
short	square			# Mask value for squares
short	pixel			# Mask value for single pixel

short	val
int	i, fd, nc1, nl1, c1, c2, l1, l2
pointer	sp, pm

pointer	immap(), impl2s(), imps2s()
int	clgeti(), nowhite(), strmatch(), open(), fscan(), nscan()
errchk	open, immap

begin
	call smark (sp)
	call salloc (text, SZ_FNAME, TY_CHAR)
	call salloc (mask, SZ_FNAME, TY_CHAR)

	# Get task parameters.
	call clgstr ("mask", Memc[mask], SZ_FNAME)
	call clgstr ("text", Memc[text], SZ_FNAME)
	nc = clgeti ("ncols")
	nl = clgeti ("nlines")
	linterp = clgeti ("linterp")
	cinterp = clgeti ("cinterp")
	square = clgeti ("square")
	pixel = clgeti ("pixel")

	# Force a pixel text format and extension.
	i = nowhite (Memc[mask], Memc[mask], SZ_FNAME)
	if (!strmatch (Memc[mask], ".pl$") > 0)
	    call strcat (".pl", Memc[mask], SZ_FNAME)

	# Open the files and abort on an error.
	fd = open (Memc[text], READ_ONLY, TEXT_FILE)
	pm = immap (Memc[mask], NEW_IMAGE, 0)

	# Set the output image.
	IM_LEN(pm,1) = nc
	IM_LEN(pm,2) = nl
	call sprintf (IM_TITLE(pm), SZ_IMTITLE, "Pixel mask from file %s")
	    call pargstr (Memc[text])

	# Set the good pixel values.
	val = 0
	do i = 1, nl
	    call amovks (val, Mems[impl2s(pm,i)], nc)

	# Set the bad pixel values.
	while (fscan (fd) != EOF) {
	    call gargi (c1)
	    call gargi (c2)
	    call gargi (l1)
	    call gargi (l2)
	    if (nscan() != 4) {
	        if (nscan() == 2) {
		    l1 = c2
		    c2 = c1
		    l2 = l1
		} else
		    next
	    }

	    c1 = max (1, c1)
	    c2 = min (nc, c2)
	    l1 = max (1, l1)
	    l2 = min (nl, l2)
	    nc1 = c2 - c1 + 1
	    nl1 = l2 - l1 + 1
	    if (nc1 < 1 || nl1 < 1)
		next

	    # Select mask value based on shape of rectangle.
	    if (nc1 < nl1)
		val = linterp
	    else if (nc1 > nl1)
		val = cinterp
	    else if (nc1 == 1)
		val = pixel
	    else
		val = square
	    call amovks (val, Mems[imps2s(pm,c1,c2,l1,l2)], nc1*nl1)
	}

	# Finish up.
	call imunmap (pm)
	call close (fd)
end
