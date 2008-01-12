include	<imio.h>
include	<mach.h>
include	<mwset.h>
include	<pkg/dttext.h>
include	<pkg/igsfit.h>

# FC_GETCOORDS -- Get feature coordinates for the specified axis and list
# of images.  Determine the image dimensions.

procedure fc_getcoords (database, list, axis, xmin, xmax, ymin, ymax,
    coords, ncoords, labels, un)

char	database[ARB]			# Database
int	list				# List of images
int	axis				# Image axis
real	xmin, xmax			# Image X limits
real	ymin, ymax			# Image Y limits
pointer	coords				# Coordinate data pointer
pointer	ncoords				# Number of coordinate points
char	labels[SZ_LINE,IGSPARAMS]	# Axis labels
pointer	un				# Units pointer

char	image1[SZ_FNAME], image2[SZ_FNAME], root[SZ_FNAME], units[SZ_FNAME]
int	i, j, rec, index, imin, imax, nfeatures, ntotal
real	value, wt, ltm[2,2], ltv[2]
pointer	dt, im, mw, ct, x, y, user

int	fc_getim(), dtgeti(), dtscan(), mw_stati()
real	mw_c1tranr()
bool	strne()
pointer	dtmap1(), immap(), mw_openim(), mw_sctran(), un_open()

errchk	dtmap1, dtgstr, immap

begin
	x = NULL
	ncoords = 0
	ntotal = 0
	axis = 0
	imin = MAX_INT
	imax = -MAX_INT
	un = NULL

	while (fc_getim (list, image1, SZ_FNAME) != EOF) {
	    call strcpy ("id", root, SZ_FNAME)
	    call imgcluster (image1, root[3], SZ_FNAME-2)
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
		imin = min (imin, index)
		imax = max (imax, index)

		xmin = 1.
		xmax = IM_SVLEN (im, 1)
		ymin = 1.
		ymax = IM_SVLEN (im, 2)

		if (axis == 0)
		    axis = j

		if (j != axis) {
		    call imunmap (im)
		    call eprintf (
       "Warning: Fit axes don't agree for combine option.  Ignoring %s.\n")
		       call pargstr (image1)
		    break
		}

		# Set the WCS to convert the feature positions from
		# IDENTIFY/REIDENTIFY which are in "physical" coordinates
		# to "logical" coordinates currently used by TRANSFORM.

		mw = mw_openim (im)
		call mw_seti (mw, MW_USEAXMAP, NO)
		i = mw_stati (mw, MW_NPHYSDIM)
		call mw_gltermr (mw, ltm, ltv, i)
		if (ltm[1,1] == 0. && ltm[2,2] == 0.) {
		    ltm[1,1] = ltm[2,1]
		    ltm[2,1] = 0.
		    ltm[2,2] = ltm[1,2]
		    ltm[1,2] = 0.
		    call mw_sltermr (mw, ltm, ltv, i)
		} else if (ltm[1,2] != 0. || ltm[2,1] != 0.) {
		    ltv[1] = 0.
		    ltv[2] = 0.
		    ltm[1,1] = 1.
		    ltm[2,1] = 0.
		    ltm[2,2] = 1.
		    ltm[1,2] = 0.
		    call mw_sltermr (mw, ltm, ltv, i)
		}
		call mw_seti (mw, MW_USEAXMAP, YES)
		ct = mw_sctran (mw, "physical", "logical", 1)

		# Allocate memory for the feature information and read
		# the database.

		ifnoerr (call dtgstr (dt, rec, "units", units, SZ_FNAME))
		    un = un_open (units)
		nfeatures = dtgeti (dt, rec, "features")
		if (x == NULL) {
		    call malloc (x, nfeatures, TY_REAL)
		    call malloc (y, nfeatures, TY_REAL)
		    call malloc (user, nfeatures, TY_REAL)
		} else {
		    call realloc (x, ncoords+nfeatures, TY_REAL)
		    call realloc (y, ncoords+nfeatures, TY_REAL)
		    call realloc (user, ncoords+nfeatures, TY_REAL)
		}

		do i = 1, nfeatures {
		    j = dtscan (dt)
		    call gargr (value)
		    switch (axis) {
		    case 1:
			Memr[x+ncoords] = mw_c1tranr (ct, value)
			Memr[y+ncoords] = index
		    case 2:
			Memr[x+ncoords] = index
			Memr[y+ncoords] = mw_c1tranr (ct, value)
		    }
		    call gargr (value)
		    call gargr (value)
		    call gargr (wt)
		    call gargr (wt)
		    call gargr (wt)
		    if (!IS_INDEF (value) && wt > 0.) {
			Memr[user+ncoords] = value
			ncoords = ncoords + 1
		    }
		    ntotal = ntotal + 1
		}
		call mw_close (mw)
		call imunmap (im)
	    }

	    # Finish up
	    call dtunmap (dt)
	}

	# Set coordinates.  Take error action if no features are found.

	if (ncoords > 0) {
	    call xt_sort3 (Memr[user], Memr[x], Memr[y], ncoords)
	    call malloc (coords, ncoords*IGSPARAMS, TY_REAL)
	    call amovr (Memr[x], Memr[coords+(X-1)*ncoords], ncoords)
	    call amovr (Memr[y], Memr[coords+(Y-1)*ncoords], ncoords)
	    call amovr (Memr[user], Memr[coords+(Z-1)*ncoords], ncoords)
	    call amovkr (1., Memr[coords+(W-1)*ncoords], ncoords)

	    call fc_setfeatures (Memr[coords], Memr[coords+(Z-1)*ncoords],
		ncoords)

	    call strcpy ("X (pixels)", labels[1,X], SZ_LINE)
	    call strcpy ("Y (pixels)", labels[1,Y], SZ_LINE)
	    call strcpy ("User", labels[1,Z], SZ_LINE)
	    call strcpy ("Surface", labels[1,S], SZ_LINE)
	    call strcpy ("Residuals", labels[1,R], SZ_LINE)
	}

	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)
	call mfree (user, TY_REAL)

	if (ncoords == 0) {
	    if (ntotal == 0)
		call error (1, "No coordinates found in database")
	    else
		call error (1, "Only INDEF coordinates found in database")
	}
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
