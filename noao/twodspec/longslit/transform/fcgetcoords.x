include	<imio.h>
include	<pkg/dttext.h>
include	<pkg/igsfit.h>

# FC_GETCOORDS -- Get feature coordinates for the specified axis and list
# of images.  Determine the image dimensions.

procedure fc_getcoords (database, list, axis, xmin, xmax, ymin, ymax,
    coords, ncoords, labels)

char	database[ARB]			# Database
int	list				# List of images
int	axis				# Image axis
real	xmin, xmax			# Image X limits
real	ymin, ymax			# Image Y limits
pointer	coords				# Coordinate data pointer
pointer	ncoords				# Number of coordinate points
char	labels[SZ_LINE,IGSPARAMS]	# Axis labels

char	image1[SZ_FNAME], image2[SZ_FNAME], root[SZ_FNAME]
int	i, j, rec, index, offset, nfeatures
real	value
pointer	dt, im, x, y, user

int	fc_getim(), dtgeti(), dtscan()
bool	strne()
pointer	dtmap1(), immap()

errchk	dtgstr

begin
	x = NULL
	ncoords = 0
	axis = 0

	while (fc_getim (list, image1, SZ_FNAME) != EOF) {
	    call sprintf (root, SZ_FNAME, "id%s")
		call pargstr (image1)
	    dt = dtmap1 (database, root, READ_ONLY)
	    do rec = 1, DT_NRECS(dt) {

		iferr (call dtgstr (dt, rec, "task", image2, SZ_FNAME))
		    next
		if (strne ("identify", image2))
		    next

	        call dtgstr (dt, rec, "image", image2, SZ_FNAME)
		call get_root (image2, root, SZ_FNAME)
		if (strne (image1, root))
		    next

		# Map the 1D image section and determine the axis, the
		# line or column in the 2D image, and the 2D image size.

		im = immap (image2, READ_ONLY, 0)
		j = IM_VMAP(im, 1)
		switch (j) {
		case 1:
		    index = IM_VOFF (im, 2) + 1
		case 2:
		    index = IM_VOFF (im, 1) + 1
		}

		xmin = 1.
		xmax = IM_SVLEN (im, 1)
		ymin = 1.
		ymax = IM_SVLEN (im, 2)
		call imunmap (im)

		if (axis == 0)
		    axis = j

		if (j != axis)
		    next

		# Allocate memory for the feature information and read
		# the database.

		nfeatures = dtgeti (dt, rec, "features")
		if (x == NULL) {
		    ncoords = nfeatures
		    call malloc (x, ncoords, TY_REAL)
		    call malloc (y, ncoords, TY_REAL)
		    call malloc (user, ncoords, TY_REAL)
		} else {
		    ncoords = ncoords + nfeatures
		    call realloc (x, ncoords, TY_REAL)
		    call realloc (y, ncoords, TY_REAL)
		    call realloc (user, ncoords, TY_REAL)
		}

		do i = 1, nfeatures {
		    offset = ncoords - nfeatures + i - 1
		    j = dtscan (dt)
		    switch (axis) {
		    case 1:
		        call gargr (Memr[x+offset])
			Memr[y+offset] = index
		    case 2:
			Memr[x+offset] = index
		        call gargr (Memr[y+offset])
		    }
		    call gargr (Memr[user+offset])
		    call gargr (value)
		    if (!IS_INDEF (value))
			Memr[user+offset] = value
		}
	    }

	    # Unmmap the database.
	    call dtunmap (dt)
	}

	# If no features were found return.

	if (ncoords == 0)
	    return

	call xt_sort3 (Memr[user], Memr[x], Memr[y], ncoords)
	call malloc (coords, ncoords*IGSPARAMS, TY_REAL)
	call amovr (Memr[x], Memr[coords+(X-1)*ncoords], ncoords)
	call amovr (Memr[y], Memr[coords+(Y-1)*ncoords], ncoords)
	call amovr (Memr[user], Memr[coords+(Z-1)*ncoords], ncoords)
	call amovkr (1., Memr[coords+(W-1)*ncoords], ncoords)

	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)
	call mfree (user, TY_REAL)

	call fc_setfeatures (Memr[coords], Memr[coords+(Z-1)*ncoords], ncoords)

	call strcpy ("X (pixels)", labels[1,X], SZ_LINE)
	call strcpy ("Y (pixels)", labels[1,Y], SZ_LINE)
	call strcpy ("User", labels[1,Z], SZ_LINE)
	call strcpy ("Surface", labels[1,S], SZ_LINE)
	call strcpy ("Residuals", labels[1,R], SZ_LINE)
end


# FC_SETFEATURES -- Set the feature numbers.

procedure fc_setfeatures (features, user, npts)

real	features[npts]			# Feature numbers
real	user[npts]			# User coordinates
int	npts				# Number of points

int	i

begin
	features[1] = 1
	do i = 2, npts {
	    features[i] = features[i-1]
	    if (user[i] != user[i-1])
		features[i] = features[i] + 1
	}
end
