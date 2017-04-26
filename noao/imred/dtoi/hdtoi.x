include	<imhdr.h>
include	<mach.h>
include	<math/curfit.h>
include	<error.h>
include	"hdicfit/hdicfit.h"

# T_HDTOI -- transform an image from density to intensity, according
# to an hd curve described in an input database.  A look up table of
# all possible values is generated with the curfit package, and
# then the image is transformed line by line.  A fog value is subtracted
# from the image prior to transformation, and it can be entered as either
# a number or a list of fog images from which the fog value is calculated.
# If a fog value has not been entered by the user, it is read from the database.

procedure t_hdtoi ()

pointer	sp, cv, fog, db, im_in, lut, im_out, imageout, imagein, option
bool	verbose
int	minval, maxval, in_list, rec, ip, out_list, fog_list, ngpix
int	datatype, nluv, updatedb, nfpix
real	sigma, floor, scale, fog_val, sdev

char	clgetc()
bool	streq(), clgetb()
pointer	ddb_map(), immap()
int	imtopenp(), ddb_locate(), ctor(), imtlen(), imtgetim()
int	get_data_type(), imtopen()
real	clgetr(), ddb_getr()

begin
	call smark (sp)
	call salloc (cv,       SZ_FNAME, TY_CHAR)
	call salloc (fog,      SZ_LINE,  TY_CHAR)
	call salloc (imageout, SZ_FNAME, TY_CHAR)
	call salloc (imagein,  SZ_FNAME, TY_CHAR)

	# Get cl parameters
	in_list = imtopenp ("input")
	out_list = imtopenp ("output")
	call clgstr ("database", Memc[cv], SZ_FNAME)
	call clgstr ("fog", Memc[fog], SZ_LINE)
	sigma = clgetr ("sigma")
	floor = clgetr ("floor")
	verbose = clgetb ("verbose")
	updatedb = NO

	datatype = get_data_type (clgetc ("datatype"))
	if (datatype == ERR)
	    call eprintf ("Using input pixel datatype for output\n")

	db = ddb_map (Memc[cv], READ_ONLY)
	rec = ddb_locate (db, "common")
	scale = ddb_getr (db, rec, "scale")

	# If not specified by user, get fog value from database.  User can
	# specify fog as a real number or a list of fog file names.

	if (streq (Memc[fog], "")) {
	    rec = ddb_locate (db, "fog")
	    fog_val = ddb_getr (db, rec, "density")
	} else {
	    ip = 1
	    if (ctor (Memc[fog], ip, fog_val) == 0) {
		if (verbose)
		    call eprintf ("Calculating fog value ...\n")
		fog_list = imtopen (Memc[fog])
		call salloc (option, SZ_FNAME, TY_CHAR)
		call clgstr ("option", Memc[option], SZ_FNAME)
	        call hd_fogcalc (fog_list, fog_val, sdev, ngpix, scale, sigma,
		    Memc[option], nfpix)

		call eprintf ("Fog density = %f, sdev = %f, ngpix = %d\n")
		    call pargr (fog_val)
		    call pargr (sdev)
		    call pargi (ngpix)

		updatedb = YES
	    }
	}

	# Generate look up table.  First, the range of input values to
	# calculate output values for must be determined.  Arguments
	# minval and maxval are integers because we assume all input
	# images are short integers.

	call hd_glimits (in_list, minval, maxval)
	nluv = (maxval - minval) + 1
	call salloc (lut, nluv, TY_REAL)

	if (verbose)
	    call eprintf ("Generating look up table ...\n")
	call hd_wlut (db, Memr[lut], minval, maxval, fog_val, floor)

	# Loop through input images, applying transform
	if (imtlen (in_list) != imtlen (out_list)) {
	    call imtclose (in_list)
	    call imtclose (out_list)
	    call error (0, "Number of input and output images not the same")
	}

	while ((imtgetim (in_list, Memc[imagein], SZ_FNAME) != EOF) &&
	    (imtgetim (out_list, Memc[imageout], SZ_FNAME) != EOF)) {

	    iferr (im_in = immap (Memc[imagein], READ_ONLY, 0)) {
		call erract (EA_WARN)
		next 
	    }

	    iferr (im_out = immap (Memc[imageout], NEW_COPY, im_in)) {
		call imunmap (im_in)
		call erract (EA_WARN)
		next
	    }

	    if (verbose) {
	        call eprintf ("Density to Intensity Transform: %s ===> %s\n")
    		    call pargstr (Memc[imagein])
    		    call pargstr (Memc[imageout])
	    }

	    call hd_transform (im_in, im_out, Memr[lut], nluv, minval, datatype)

	    call imunmap (im_in)
	    call imunmap (im_out)
	}

	call ddb_unmap (db)
	call imtclose (in_list)
	call imtclose (out_list)

	if (updatedb == YES) {
	    db = ddb_map (Memc[cv], APPEND)
	    # Write fog information to database as single record
	    call ddb_prec (db, "fog")
	    call ddb_putr (db, "density", fog_val)
	    call ddb_putr (db, "sdev", sdev)
	    call ddb_puti (db, "ngpix", ngpix)
	    call ddb_pstr (db, "option", Memc[option])
	    call ddb_unmap (db)
	}

	call sfree (sp)
end


# HD_TRANSFORM -- Apply transformation to image.

procedure hd_transform (im, im_out, lu_table, nvals, minval, datatype)

pointer	im		# Input image header pointer
pointer	im_out		# Transformed image header pointer
real	lu_table[ARB]	# Array of intensity values
int	nvals		# Number of values in the lut
int	minval		# Offset to first value in look up table
int	datatype	# Pixel type on output

int	j, ncols
pointer	ptr_in, ptr_out, sp, luti
pointer	impl2r(), imgl2i(), impl2i()

begin
	if (datatype == ERR)
	    IM_PIXTYPE(im_out) = IM_PIXTYPE(im)
	else
	    IM_PIXTYPE(im_out) = datatype

	ncols = IM_LEN(im,1)

	switch (datatype) {
	case TY_REAL, TY_DOUBLE:
	    # Loop over input image rows.  The look up table is left as
	    # a real array and a floating point image is written out.

	    do j = 1, IM_LEN(im,2) {
	        ptr_in = imgl2i (im, j)
	        ptr_out = impl2r (im_out, j)
		call asubki (Memi[ptr_in], minval, Memi[ptr_in], ncols)
		call alutr (Memi[ptr_in], Memr[ptr_out], ncols, lu_table)
	    }

	default:
	    # Loop over input image rows. The look up table is truncated
	    # to type integer.

	    call smark (sp)
	    call salloc (luti, nvals, TY_INT)
	    call achtri (lu_table, Memi[luti], nvals)

	    do j = 1, IM_LEN(im,2) {
	        ptr_in = imgl2i (im, j)
	        ptr_out = impl2i (im_out, j)
		call asubki (Memi[ptr_in], minval, Memi[ptr_in], ncols)
		call aluti (Memi[ptr_in], Memi[ptr_out], ncols, Memi[luti])
	    }

	    call sfree (sp)
	}
end


# HD_WLUT -- write look up table, such that intensity = lut [a/d output].
# An entry is made in the look up table for every possible input value,
# from minval to maxval.

procedure hd_wlut (db, lut, minval, maxval, fog_val, floor)

pointer	db		# Pointer to database file
real 	lut[ARB]	# Pointer to look up table, which gets filled here
int	minval		# Minimum value to transform
int	maxval		# Maximum value to transform
real	fog_val		# Fog value to be subtracted from densities
real	floor		# Value assigned to densities below fog

bool	zerofloor
pointer	sp, trans, fcn, save, cv, dens, ind_var, value
int	rec, nsave, i, function, nneg, npos, nvalues
real	scale, maxcvval, factor, maxexp, maxden, maxdenaf

bool	fp_equalr()
int	strncmp(), ddb_locate(), ddb_geti(), cvstati()
real	ddb_getr(), clgetr(), cveval()
extern	hd_powerr()

begin
	call smark (sp)
	call salloc (trans, SZ_FNAME, TY_CHAR)
	call salloc (fcn, SZ_FNAME, TY_CHAR)

	nvalues = (maxval - minval) + 1
	call salloc (ind_var, nvalues, TY_REAL)
	call salloc (dens,    nvalues, TY_REAL)
	call salloc (value,   nvalues, TY_REAL)

	rec = ddb_locate (db, "common")
	scale = ddb_getr (db, rec, "scale")
	maxden = ddb_getr (db, rec, "maxden")

	rec = ddb_locate (db, "cv")
	nsave = ddb_geti (db, rec, "save")
	call salloc (save, nsave, TY_REAL)
	call ddb_gar (db, rec, "save", Memr[save], nsave, nsave)
	call ddb_gstr (db, rec, "transformation", Memc[trans], SZ_LINE)

	call cvrestore (cv, Memr[save])
	function = cvstati (cv, CVTYPE)

	if (function == USERFNC) {
	    # Need to restablish choice of user function
	    call ddb_gstr (db, rec, "function", Memc[fcn], SZ_FNAME)
	    if (strncmp (Memc[fcn], "power", 1) == 0) 
		call cvuserfnc (cv, hd_powerr)
	    else
		call error (0, "Unknown user function in database")
	}

	maxdenaf = maxden - fog_val
	call hd_aptrans (maxdenaf, maxcvval, 1, Memc[trans])
	maxcvval = 10.0 ** (cveval (cv, maxcvval))
	factor = clgetr ("ceiling") / maxcvval
	maxexp = real (MAX_EXPONENT) - (log10 (factor) + 1.0)

	zerofloor = false
	if (fp_equalr (0.0, floor))
	    zerofloor = true

	do i = 1, nvalues
	    Memr[value+i-1] = real (minval + i - 1)

	# Scale all posible voltage values to density above fog
	call altmr (Memr[value], Memr[dens], nvalues, scale, -fog_val)

	# Find index of first value greater than MIN_DEN.  Values less than 
	# this must be handled as the user specified with the floor parameter.

	for (nneg=0; Memr[dens+nneg] < MIN_DEN; nneg=nneg+1)
	    ;
	npos = nvalues - nneg
	
	# Generate independent variable vector and then lut values.  The
	# logic is different if there are values below fog.  Evaluating 
	# the polynomial fit with cvvector yields the log exposure.  This
	# is then converted to intensity and scaled by a user supplied factor.

	if (nneg > 0) {
	    if (zerofloor) {
		call amovkr (0.0, lut, nneg)
		call hd_aptrans (Memr[dens+nneg],Memr[ind_var],npos,Memc[trans])
		call cvvector (cv, Memr[ind_var], lut[nneg+1], npos)
		call argtr (lut[nneg+1], npos, maxexp, maxexp)
		do i = nneg+1, nvalues
		    lut[i] = (10. ** lut[i]) * factor

	    } else {
		call amulkr (Memr[dens], -1.0, Memr[dens], nneg)

		# Care must be taken so that no density of value 0.0 is 
		# passed to hd_aptrans.  This would cause an overflow.

		do i = 1, nneg {
		    if (fp_equalr (Memr[dens], 0.0))
			Memr[dens] = MIN_DEN
		}

		call hd_aptrans (Memr[dens],Memr[ind_var], nvalues, Memc[trans])
		call cvvector (cv, Memr[ind_var], lut, nvalues)
		call argtr (lut, nvalues, maxexp, maxexp)
	        do i = 1, nvalues
		    lut[i] = (10.0 ** lut[i]) * factor
		call amulkr (lut, -1.0, lut, nneg)
	    }
		
	} else {
	    call hd_aptrans (Memr[dens], Memr[ind_var], nvalues, Memc[trans])
	    call cvvector (cv, Memr[ind_var], lut, nvalues)
	    call argtr (lut, nvalues, maxexp, maxexp)
	    do i = 1, nvalues
		lut[i] = (10.0 ** lut[i]) * factor
	}

	call cvfree (cv)
	call sfree (sp)
end


# HD_APTRANS -- Apply transformation, generating a vector of independent
# variables from a density vector.  It is assummed all values in the
# input density vector are valid and will not cause arithmetic errors.
# No checking for out of bounds values is performed.

procedure hd_aptrans (density, ind_var, nvalues, transform)

real	density[nvalues]	# Density vector - input
real	ind_var[nvalues]	# Ind variable vector - filled on output
int	nvalues			# Length of vectors
char	transform[ARB]		# String containing transformation type

int	i
int	strncmp()

begin
	if (strncmp (transform, "logopacitance", 1) == 0) {
	    do i = 1, nvalues
	        ind_var[i] = log10 ((10. ** density[i]) - 1.0)

	} else if (strncmp (transform, "k75", 2) == 0) {
	    do i = 1, nvalues
	        ind_var[i] = density[i] + .75 * log10(1. - 10. ** (-density[i]))

	} else if (strncmp (transform, "k50", 2) == 0) {
	    do i = 1, nvalues
	        ind_var[i] = density[i] + .50 * log10(1. - 10. ** (-density[i]))

	} else if (strncmp (transform, "none", 1) == 0) {
	    do i = 1, nvalues
	        ind_var[i] = density[i]

	} else 
	    call error (0, "Unrecognized transformation in database file")
end


# HD_GLIMITS -- Determinine the range of max and min values for a list
# of images.

procedure hd_glimits (in_list, minval, maxval)

int	in_list		# File descriptor for list of images
int	minval		# Smallest pixel value - returned
int	maxval		# Largest  pixel value - returned

pointer	im
char	image[SZ_FNAME]
real	current_min, current_max, min, max
pointer	immap()
int	imtgetim()
errchk	imtgetim, im_minmax, imtrew

begin
	current_min = MAX_REAL
	current_max = EPSILONR

	while (imtgetim (in_list, image, SZ_FNAME) != EOF) {
	    iferr (im = immap (image, READ_ONLY, 0))
		# Just ignore it, warning will be printed by t_hdtoi
		next

	    # Update min max values if necessary
	    if (IM_LIMTIME(im) < IM_MTIME(im))
	        call im_minmax (im, IM_MIN(im), IM_MAX(im))

	    min = IM_MIN(im)
	    max = IM_MAX(im)

	    if (min < current_min)
		current_min = min

	    if (max > current_max)
		current_max = max
	    
	    call imunmap (im)
	}

	minval = int (current_min)
	maxval = int (current_max)

	call imtrew (in_list)
end
