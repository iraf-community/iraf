# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<mwset.h>


# T_RIMCURSOR -- Read the image cursor list until EOF is seen on the list,
# transforming the coordinates to the desired system and printing the
# transformed cursor reads on the standard output.

procedure t_rimcursor()

double	px, py, wx, wy
pointer sp, gp, ct, mw, fmt, rest
int	axis, frame, newframe, ntokens
pointer	gopen(), rim_getctran()
errchk	gopen, rim_getctran
int	clscan(), nscan()

begin
	call smark (sp)
	call salloc (fmt, SZ_FNAME, TY_CHAR)
	call salloc (rest, SZ_LINE, TY_CHAR)

	# Open graphics context (doesn't work currently for stdimage).
	iferr (gp = gopen ("stdimage", APPEND, STDIMAGE))
	    gp = NULL

	frame = 1
	mw = NULL
	ct = NULL

	# Read the cursor repeatedly until EOF is seen.
	while (clscan ("cursor") != EOF) {
	    # Get cursor value.
	    call gargd (px)
	    call gargd (py)
	    call gargi (newframe)
	    newframe = newframe / 100
	    call gargstr (Memc[rest], SZ_LINE)

	    # Get coordinate transformation.
	    ntokens = nscan()
	    if (ntokens < 2)
		call error (1, "bad cursor read")
	    else if (mw == NULL || (ntokens >= 3 && newframe != frame)) {
		if (mw != NULL)
		    call mw_close (mw)
		ct = rim_getctran (newframe, mw)
		frame = newframe
	    }

	    # Transform coordinates.
	    if (ct != NULL)
		call mw_c2trand (ct, px,py, wx,wy)
	    else {
		wx = px
		wy = py
	    }

	    # Always output transformed coordinates.
	    do axis = 1, 2 {
		# Use format given in MWCS, if there is one.
		Memc[fmt] = EOS
		if (mw != NULL) {
		    iferr (call mw_gwattrs(mw,axis,"format",Memc[fmt],SZ_FNAME))
			Memc[fmt] = EOS
		}
		if (Memc[fmt] == EOS)
		    call strcpy ("%0.15g", Memc[fmt], SZ_FNAME)

		# Output the transformed value.
		call fprintf (STDOUT, Memc[fmt])
		    if (axis == 1)
			call pargd (wx)
		    else
			call pargd (wy)

		call putci (STDOUT, ' ')
	    }

	    # Output WCS field if present in input.
	    if (ntokens > 2) {
		call fprintf (STDOUT, "%d01")
		    call pargi (frame)
	    }

	    # Output rest of cursor value if present in input.
	    if (ntokens > 3)
		call putline (STDOUT, Memc[rest])

	    call putci (STDOUT, '\n')
	}

	# Shutdown.
	if (mw != NULL)
	    call mw_close (mw)
	if (gp != NULL)
	    call gclose (gp)
	call sfree (sp)
end


# RIM_GETCTRAN -- Get the WCS context to be used for coordinate transforms.
# Determine the reference image, load the WCS, and compile a transform from
# logical image coordinates to the desired coordinate system.

pointer procedure rim_getctran (frame, mw)

int	frame			#I image frame, if display is used
pointer	mw			#O MWCS descriptor

int	status
bool	use_display
pointer	sp, wcs, imname, ds, iw, im, ct
pointer	imd_mapframe(), iw_open(), immap(), mw_sctran(), mw_openim()
errchk	imd_mapframe, iw_open, immap, mw_sctran, mw_openim
int	envfind(), clgeti()
bool	streq()

begin
	call smark (sp)
	call salloc (imname, SZ_LINE, TY_CHAR)
	call salloc (wcs, SZ_FNAME, TY_CHAR)

	# Access image display to get name of reference image?
	if (clgeti ("$nargs") > 0)
	    use_display = false
	else if (envfind ("stdimcur", Memc[imname], SZ_LINE) > 0)
	    use_display = streq (Memc[imname], "stdimage")
	else
	    use_display = false

	# Get the name of the reference image.
	if (use_display) {
	    ds = imd_mapframe (frame, READ_ONLY, NO)
	    iw = iw_open (ds, frame, Memc[imname], SZ_LINE, status)
	    call iw_close (iw)
	    call imunmap (ds)
	} else
	    call clgstr ("image", Memc[imname], SZ_LINE)

	# Map the image if one was specified.
	if (Memc[imname] == EOS)
	    im = NULL
	else iferr (im = immap (Memc[imname], READ_ONLY, 0))
	    im = NULL

	# Get WCS if image was given.
	if (im == NULL) {
	    mw = NULL
	    ct = NULL
	} else {
	    ifnoerr (mw = mw_openim (im)) {
		call clgstr ("wcs", Memc[wcs], SZ_FNAME)
		if (Memc[wcs] == EOS)
		    call strcpy ("logical", Memc[wcs], SZ_FNAME)
		ct = mw_sctran (mw, "logical", Memc[wcs], 03B)
	    } else {
		mw = NULL
		ct = NULL
	    }

	    # We don't need the image once the WCS is loaded.
	    call imunmap (im)
	    if (mw != NULL)
		call mw_seti (mw, MW_REFIM, NULL)
	}

	call sfree (sp)
	return (ct)
end
