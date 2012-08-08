include	<imhdr.h>

# T_BADPIXIMAGE -- Create a bad pixel image mask from a bad pixel file.

procedure t_badpiximage ()

pointer	bpfile			# Bad pixel file
pointer	bpimage			# Bad pixel image
pointer	template		# Template image
short	goodval, badval		# Good and bad values

int	i, nc, nl, c1, c2, l1, l2, fd, x1, x2, xstep, y1, y2, ystep
pointer	sp, str, im, im1

short	clgets()
bool	ccdflag()
pointer	immap(), impl2s(), imps2s()
int	open(), fscan(), nscan(), stridxs(), strmatch()
errchk	open, immap

begin
	call smark (sp)
	call salloc (bpfile, SZ_FNAME, TY_CHAR)
	call salloc (bpimage, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get task parameters.
	call clgstr ("fixfile", Memc[bpfile], SZ_FNAME)
	call clgstr ("template", Memc[template], SZ_FNAME)
	call clgstr ("image", Memc[bpimage], SZ_FNAME)
	goodval = clgets ("goodvalue")
	badval = clgets ("badvalue")

	# Open the files and abort on an error.
	fd = open (Memc[bpfile], READ_ONLY, TEXT_FILE)
	im1 = immap (Memc[template], READ_ONLY, 0)
	im = immap (Memc[bpimage], NEW_COPY, im1)

	# Set the output image.
	IM_PIXTYPE(im) = TY_SHORT
	call sprintf (IM_TITLE(im), SZ_IMTITLE,
	    "Bad pixel image from bad pixel file %s")
	    call pargstr (Memc[bpfile])

	# Set the good pixel values.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	do i = 1, nl
	    call amovks (goodval, Mems[impl2s(im,i)], nc)

	# Set the bad pixel values.  By default the bad pixel coordinates
	# refer to the image directly but if the word "untrimmed" appears
	# in a comment then the coordinates refer to the untrimmed image.
	# This is the same algorithm as used in SETFIXPIX for CCDPROC.

	x1 = 1
	xstep = 1
	y1 = 1
	ystep = 1
	while (fscan (fd) != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    if (Memc[str] == '#') {
		call gargstr (Memc[str], SZ_LINE)
		if (strmatch (Memc[str], "{untrimmed}") != 0) {
		    if (ccdflag (im, "trim")) {
	    		call hdmgstr (im, "trim", Memc[str], SZ_LINE)
	    		x2 = stridxs ("[", Memc[str])
	    		if (x2 != 0) {
			    x1 = 1
			    x2 = IM_LEN(im,1)
			    xstep = 1
			    y1 = 1
			    y2 = IM_LEN(im,2)
			    ystep = 1
			    call ccd_section (Memc[str+x2-1], x1, x2, xstep,
				y1, y2, ystep)
			}
		    }
		}
	        next
	    }
		    
	    call reset_scan()
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

	    c1 = max (1, (c1 - x1 + xstep - 1) / xstep + 1)
	    c2 = min (nc, (c2 - x1) / xstep + 1)
	    l1 = max (1, (l1 - y1 + ystep - 1) / ystep + 1)
	    l2 = min (nl, (l2 - y1) / ystep + 1)

	    if ((c1 > c2) || (l1 > l2))
		next

	    i = (c2 - c1 + 1) * (l2 - l1 + 1)
	    call amovks (badval, Mems[imps2s(im,c1,c2,l1,l2)], i)
	}

	# Finish up.
	call imunmap (im)
	call imunmap (im1)
	call close (fd)
end
