include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pkg/gtools.h>
include	<pkg/xtanswer.h>
include	"apertures.h"
 
define	MAXBUF	(512*100)		# Buffer size for column access
 
# T_APSCATTER -- Subtract scattered light.
#
# A one dimensional function is fit to the scattered light across the
# dispersion taken from between the defined apertures.  The fitting uses
# an interative rejection algorithm to further exclude signal from the
# apertures.  Since the scattered light is expected to be smooth the
# individual one dimensional fits across dispersion are then smoothed
# by fitting a one dimensional function along the dispersion.  The latter
# is optional.  The output consists of the scattered light subtracted
# input image.  The output image may be the same as the input image.
# Additionally the scattered light surface may be output.  The fitting
# may be done interactively or noninteractively.
 
procedure t_apscatter ()
 
int	listin			# List of input images to be correctd
int	listout			# List of output images (may be empty)
int	listscat		# List of scattered light images (may be empty)
int	listrefs		# List of reference images (may be empty)
 
int	recenter		# Recenter reference apertures?
int	find			# Find apertures automatically?
int	trace			# Trace apertures?
int	subtract		# Fit and subtract scattered light?
int	smooth			# Smooth scattered light?
int	dbwrite			# Write aperture data to database?
 
int	interactive		# Run interactively?
int	edit			# Edit apertures?
int	fittrace		# Fit traced points interactively?
int	fitscatter		# Fit scattered light interactively?
int	fitsmooth		# Smooth scattered light interactively?
 
pointer	aps			# Pointer to array of aperture pointers
int	naps			# Number of apertures
 
int	i, line, nsum, trline
pointer	sp, image, scatter, reference, str, ic1, gt1, ic2, gt2
 
bool	clgetb(), strne()
int	ap_answer(), ap_getim(), btoi(), clgeti(), imtopenp()
 
errchk	tr_trace, ap_flat, ap_find, ap_edit, ap_getaps, ap_recenter, ap_dbread
 
begin
	call smark (sp)
	call salloc (aps, AP_MAXAPS, TY_INT)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (scatter, SZ_FNAME, TY_CHAR)
	call salloc (reference, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Get the image lists.
	listin = imtopenp ("input")
	listout = imtopenp ("output")
	listscat = imtopenp ("scatter")
	listrefs = imtopenp ("references")
 
	# Set the option parameters.
	interactive = btoi (clgetb ("interactive"))
	recenter = ap_answer ("recenter", interactive)
	find = ap_answer ("find", interactive)
	trace = ap_answer ("trace", interactive)
	subtract = ap_answer ("subtract", interactive)
	smooth = ap_answer ("smooth", interactive)
	dbwrite = ap_answer ("apio.dbwrite", interactive)
 
	# If not interactive turn off all the interactive flags.
	edit = ap_answer ("edit", interactive)
	fittrace = ap_answer ("aptrace.fittrace", interactive)
	fitscatter = ap_answer ("fitscatter", interactive)
	fitsmooth = ap_answer ("fitsmooth", interactive)
	if (interactive == NO) {
	    edit = ALWAYSNO
	    fittrace = ALWAYSNO
	    fitscatter = ALWAYSNO
	    fitsmooth = ALWAYSNO
	}
 
	# Initialize
	naps = 0
	Memc[reference] = EOS
 
	line = clgeti ("apedit.line")
	nsum = clgeti ("apedit.nsum")
	trline = clgeti ("aptrace.line")
 
	# Initialize the trace and scattered light fitting parameters.
	call tr_init ()
	call scat_open (ic1, gt1, ic2, gt2)
 
	# Process each input image.
	while (ap_getim (listin, Memc[image], SZ_FNAME) != EOF) {
	    if (ap_getim (listrefs, Memc[str], SZ_LINE) != EOF)
		call strcpy (Memc[str], Memc[reference], SZ_FNAME)
 
	    iferr {
		if (clgetb ("apio.verbose"))
		    call printf ("Searching aperture database ...\n")
 
		# Get the reference apertures.
		iferr (call ap_dbread (Memc[reference], Memi[aps], naps))
		    call error (0, "Reference image apertures not found")
		if ((naps != 0) && strne (Memc[reference], Memc[image]))
		    call ap_recenter (Memc[image], line, nsum,
			recenter, dbwrite, Memi[aps], naps)
 
		# If no apertures then get the image apertures.
		if (naps == 0)
	    	    iferr (call ap_dbread (Memc[image], Memi[aps], naps))
			;
 
		# Find apertures.
	        call ap_find (Memc[image], line, nsum, find, dbwrite, 
		    Memi[aps], naps)
 
		# Edit apertures.
		call ap_edit (Memc[image], line, nsum, edit, dbwrite,
		    Memi[aps], naps)
 
		# Trace the apertures.
		call tr_trace (Memc[image], trline, trace, fittrace,
		    dbwrite, Memi[aps], naps)
 
		# Make an aperture plot.
		call ap_plot (Memc[image], line, nsum, Memi[aps], naps)
 
		# Record last apertures.
		if ((dbwrite == YES) || (dbwrite == ALWAYSYES))
	            call ap_dbwrite ("last", ALWAYSYES, Memi[aps], naps)
 
		# Substract the scattered light.
		if (ap_getim (listscat, Memc[scatter], SZ_LINE) == EOF)
		    Memc[scatter] = EOS
		if (ap_getim (listout, Memc[str], SZ_LINE) == EOF) {
		    call mktemp ("tmp", Memc[str], SZ_LINE)
	            call ap_scatter (Memc[image], Memc[str], Memc[scatter],
			Memi[aps], naps, ic1, gt1, ic2, gt2, line, subtract,
			fitscatter, smooth, fitsmooth)
		    call imdelete (Memc[image])
		    call imrename (Memc[str], Memc[image])
		} else
	            call ap_scatter (Memc[image], Memc[str], Memc[scatter],
			Memi[aps], naps, ic1, gt1, ic2, gt2, line, subtract,
			fitscatter, smooth, fitsmooth)
 
		# Free memory.
		for (i = 1; i <= naps; i = i + 1)
		    call ap_free (Memi[aps+i-1])
		naps = 0
	    } then
		call erract (EA_WARN)
	}
 
	# Free memory and finish up.
	call ap_gclose ()
	call tr_done ()
	call scat_close (ic1, gt1, ic2, gt2)
	call imtclose (listin)
	call imtclose (listout)
	call imtclose (listscat)
	call imtclose (listrefs)
	call sfree (sp)
end
 
 
# AP_SCATTER -- Fit and subtract the scattered light from between the apertures.
#
# Each line of the input image across the dispersion is read.  The points to
# be fit are selected from between the apertures (which includes a buffer
# distance).  The fitting is done using the ICFIT package.  If not smoothing
# along the dispersion write the scattered light subtracted output directly
# thus minimizing I/O.  If smoothing write the fits to the output.  During
# the smoothing process this image is converted to the scattered light
# subtracted image.  The scattered light image is only created after the
# output image by subtracting the input from the output.  Things are done
# in this order since the output is always produced but the scattered light
# image is often not produced.
 
procedure ap_scatter (input, output, scatter, aps, naps, ic1, gt1, ic2, gt2,
	line, subtract, fitscatter, smooth, fitsmooth)
 
char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image
char	scatter[SZ_FNAME]	# Scattered light image
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures
pointer	ic1, ic2		# ICFIT pointers
pointer	gt1, gt2		# GTOOLS pointers
int	line			# Line to be edited
int	subtract		# Fit and subtract scattered light?
int	fitscatter		# Fit scattered light interactively?
int	smooth			# Smooth scattered light?
int	fitsmooth		# Smooth scattered light interactively?
 
int	i, aaxis, daxis, npts, nlines, nscatter, outmode
pointer	sp, str, in, out, scat, cv, gp, indata, outdata, col, x, y, w 
int	ap_gline(), ap_gdata()
bool	clgetb()
pointer	immap(), imgl2r(), impl2r()
 
begin
	if (naps < 1)
	    return
 
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Query the user if needed for this image.
	call sprintf (Memc[str], SZ_LINE, "Subtract scattered light in %s?")
	    call pargstr (input)
	call xt_answer (Memc[str], subtract)
	if ((subtract == NO) || (subtract == ALWAYSNO)) {
	    call sfree (sp)
	    return
	}
 
	call sprintf (Memc[str], SZ_LINE,
	    "Fit scattered light for %s interactively?")
	    call pargstr (input)
	call xt_answer (Memc[str], fitscatter)
 
	call sprintf (Memc[str], SZ_LINE, "Smooth the scattered light in %s?")
	    call pargstr (input)
	call xt_answer (Memc[str], smooth)
	if ((smooth == YES) || (smooth == ALWAYSYES)) {
	    call sprintf (Memc[str], SZ_LINE,
	        "Smooth the scattered light for %s interactively?")
	        call pargstr (input)
	    call xt_answer (Memc[str], fitsmooth)
	}
 
	# Map the input and output images.  Warn and return on an error.
	iferr (in = immap (input, READ_ONLY, 0)) {
	    call sfree (sp)
	    call erract (EA_WARN)
	    return
	}
	outmode = NEW_COPY
	iferr (out = immap (output, outmode, in)) {
	    call imunmap (in)
	    call sfree (sp)
	    call erract (EA_WARN)
	    return
	}
 
	# Allocate memory for curve fitting.
	call ap_sort (i, aps, naps, 2)
	aaxis = AP_AXIS(aps[1])
	daxis = mod (aaxis, 2) + 1
	npts = IM_LEN (in, aaxis)
	nlines = IM_LEN (in, daxis)
	call salloc (col, npts, TY_REAL)
	call salloc (x, npts, TY_REAL)
	call salloc (y, npts, TY_REAL)
	call salloc (w, npts, TY_REAL)
 
	do i = 1, npts
	    Memr[col+i-1] = i
	call ic_putr (ic1, "xmin", Memr[col])
	call ic_putr (ic1, "xmax", Memr[col+npts-1])
 
	# If the interactive flag is set then use icg_fit to set the
	# fitting parameters.  AP_GLINE returns EOF when the user
	# is done.
 
	if (fitscatter == YES || fitscatter == ALWAYSYES) {
	    call ap_gopen (gp)
 
	    if (IS_INDEFI (line))
		i = nlines / 2
	    else
		i = line
	    indata = NULL
	    while (ap_gline (ic1, gt1, in, aaxis, i, indata) != EOF) {
	        call ap_gscatter1 (aps, naps, i, Memr[indata], npts,
		    Memr[x], Memr[y], Memr[w], nscatter)
	        call icg_fit (ic1, gp, "apio.cursor", gt1, cv, Memr[x], Memr[y],
		    Memr[w], nscatter)
	    }
	}
 
	# Loop through the input image and create an output image.
	# To minimize I/O if not smoothing write the final image
	# directly otherwise write the fit.  AP_SMOOTH will then
	# smooth along the dispersion and compute the scattered
	# light subtracted image.
 
	if (clgetb ("apio.verbose")) {
	    call printf (
		"Fitting the scattered light across the dispersion ...\n")
	    call flush (STDOUT)
	}
 
	i = 0
	while (ap_gdata (in, out, outmode, aaxis, MAXBUF, i, indata,
	    outdata) != EOF) {
	    call ap_gscatter1 (aps, naps, i, Memr[indata], npts, Memr[x],
		Memr[y], Memr[w], nscatter)
	    call ic_fit (ic1, cv, Memr[x], Memr[y], Memr[w], nscatter,
		YES, YES, YES, YES)
	    call cvvector (cv, Memr[col], Memr[outdata], npts)
	    if (smooth == NO || smooth == ALWAYSNO)
	        call asubr (Memr[indata], Memr[outdata], Memr[outdata], npts)
	}
 
	call imastr (out, "apscatter", "Scattered light subtracted")
	call imunmap (in)
	call imunmap (out)
	call cvfree (cv)
 
	# Smooth along the dispersion.
	call ap_smooth (input, output, aps, naps, ic2, gt2, smooth, fitsmooth)
 
	# If a scattered light image is desired compute it from the difference
	# of the input and output images.
 
	if (scatter[1] != EOS) {
	    in = immap (input, READ_ONLY, 0)
	    out = immap (output, READ_ONLY, 0)
	    ifnoerr (scat = immap (scatter, NEW_COPY, in)) {
		npts = IM_LEN(in,1)
		nlines = IM_LEN(in,2)
		do i = 1, nlines
		    call asubr (Memr[imgl2r(in,i)], Memr[imgl2r(out,i)],
			Memr[impl2r(scat,i)], npts)
		call imunmap (scat)
	    } else
		call erract (EA_WARN)
	    call imunmap (in)
	    call imunmap (out)
	}
 
	# Make a log.
	call sprintf (Memc[str], SZ_LINE,
	    "APSCATTER  - Scattered light subtracted from %s.")
	    call pargstr (input)
	call ap_log (Memc[str])
 
	call sfree (sp)
end
 
 
# AP_SMOOTH -- Smooth the scattered light by fitting one dimensional functions.
#
# The output image consists of smooth one dimensional fits across the
# dispersion.  This routine reads each line along the dispersion and fits
# a function to smooth the fits made across the dispersion.  The output
# image is used both as input of the cross dispersion fits and as output
# of the scattered light subtracted image.
 
procedure ap_smooth (input, output, aps, naps, ic, gt, smooth, fitsmooth)
 
char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures
pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
int	smooth			# Smooth the scattered light?
int	fitsmooth		# Smooth the  scattered light interactively?
 
int	i, aaxis, daxis, npts, nlines, outmode, new
pointer	in, out, cv, gp, indata, outdata, x, w 
int	ap_gline(), ap_gdata()
bool	clgetb()
pointer	immap()
 
begin
	if (smooth == NO || smooth == ALWAYSNO)
	    return
 
	# Map the images.  There is no reason to expect errors.
	# The output image is both read and written.
 
	in = immap (input, READ_ONLY, 0)
	outmode = READ_WRITE
	out = immap (output, outmode, 0)
 
	# Allocate memory for curve fitting.
	aaxis = AP_AXIS(aps[1])
	daxis = mod (aaxis, 2) + 1
	npts = IM_LEN (in, daxis)
	nlines = IM_LEN (in, aaxis)
	call salloc (x, npts, TY_REAL)
	call salloc (w, npts, TY_REAL)
 
	do i = 1, npts
	    Memr[x+i-1] = i
	call amovkr (1., Memr[w], npts)
	call ic_putr (ic, "xmin", Memr[x])
	call ic_putr (ic, "xmax", Memr[x+npts-1])
 
	# If the interactive flag is set then use icg_fit to set the
	# fitting parameters.  AP_GLINE returns EOF when the user
	# is done.
 
	if (fitsmooth == YES || fitsmooth == ALWAYSYES) {
	    call ap_gopen (gp)
 
	    i = nlines / 2
	    outdata = NULL
	    while (ap_gline (ic, gt, out, daxis, i, outdata) != EOF) {
	        call icg_fit (ic, gp, "apio.cursor", gt, cv, Memr[x],
		    Memr[outdata], Memr[w], npts)
		call amovkr (1., Memr[w], npts)
	    }
	}
 
	# Loop through the input image and create an output image.
	if (clgetb ("apio.verbose")) {
	    call printf ("Smoothing scattered light along the dispersion ...\n")
	    call flush (STDOUT)
	}
 
	# Use the new flag to optimize the fitting.
	new = YES
	i = 0
	while (ap_gdata (in, out, outmode, daxis, MAXBUF, i, indata,
	    outdata) != EOF) {
	    call ic_fit (ic, cv, Memr[x], Memr[outdata], Memr[w], npts,
		new, YES, new, new)
	    call cvvector (cv, Memr[x], Memr[outdata], npts)
	    call asubr (Memr[indata], Memr[outdata], Memr[outdata], npts)
	    new = NO
	}
 
	call imunmap (in)
	call imunmap (out)
	call cvfree (cv)
end
 
 
# AP_GSCATTER -- Get scattered light pixels.
#
# The pixels outside the apertures extended by the specified buffer
# distance are selected.  The x and weight arrays are also set.
# The apertures must be sorted by position.
 
procedure ap_gscatter1 (aps, naps, line, data, npts, x, y, w, nscatter)
 
pointer	aps[naps]		# Apertures
int	naps			# Number of apertures
int	line			# Line
real	data[npts]		# Image data
int	npts			# Number of points
real	x[npts]			# Scattered light positions
real	y[npts]			# Image data
real	w[npts]			# Weights
int	nscatter		# Number of scattered light pixels
 
real	buf			# Aperture buffer
 
int	i, j, axis
int	low, high
real	center, cveval(), clgetr()
 
begin
	buf = clgetr ("buffer") + 0.5
 
	axis = AP_AXIS(aps[1])
	nscatter = 0
	low = 1
	do i = 1, naps {
	    center = AP_CEN(aps[i],axis) + cveval (AP_CV(aps[i]), real (line))
	    high = min (npts, int (center + AP_LOW(aps[i],axis) - buf))
	    do j = low, high {
		nscatter = nscatter + 1
		x[nscatter] = j
		y[nscatter] = data[j]
		w[nscatter] = 1.
	    }
	    low = max (1, int (center + AP_HIGH(aps[i],axis) + buf))
	}
	high = npts
	do j = low, high {
	    nscatter = nscatter + 1
	    x[nscatter] = j
	    y[nscatter] = data[j]
	    w[nscatter] = 1.
	}
end
 
 
# SCAT_OPEN -- Set the fitting parameters and graphics for ICFIT.
 
procedure scat_open (ic1, gt1, ic2, gt2)
 
pointer	ic1, ic2	# ICFIT pointer
pointer	gt1, gt2	# GTOOLS pointer
 
int	clgeti()
real	clgetr()
pointer	gt_init()
pointer	sp, str
 
begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	call ic_open (ic1)
	call clgstr ("apscat1.function", Memc[str], SZ_LINE)
	call ic_pstr (ic1, "function", Memc[str])
	call ic_puti (ic1, "order", clgeti ("apscat1.order"))
	call clgstr ("apscat1.sample", Memc[str], SZ_LINE)
	call ic_pstr (ic1, "sample", Memc[str])
	call ic_puti (ic1, "naverage", clgeti ("apscat1.naverage"))
	call ic_puti (ic1, "niterate", clgeti ("apscat1.niterate"))
	call ic_putr (ic1, "low", clgetr ("apscat1.low_reject"))
	call ic_putr (ic1, "high", clgetr ("apscat1.high_reject"))
	call ic_putr (ic1, "grow", clgetr ("apscat1.grow"))
	call ic_pstr (ic1, "ylabel", "")
	gt1 = gt_init()
	call gt_sets (gt1, GTTYPE, "line")
 
	call ic_open (ic2)
	call clgstr ("apscat2.function", Memc[str], SZ_LINE)
	call ic_pstr (ic2, "function", Memc[str])
	call ic_puti (ic2, "order", clgeti ("apscat2.order"))
	call clgstr ("apscat2.sample", Memc[str], SZ_LINE)
	call ic_pstr (ic2, "sample", Memc[str])
	call ic_puti (ic2, "naverage", clgeti ("apscat2.naverage"))
	call ic_puti (ic2, "niterate", clgeti ("apscat2.niterate"))
	call ic_putr (ic2, "low", clgetr ("apscat2.low_reject"))
	call ic_putr (ic2, "high", clgetr ("apscat2.high_reject"))
	call ic_putr (ic2, "grow", clgetr ("apscat2.grow"))
	call ic_pstr (ic2, "ylabel", "")
	gt2 = gt_init()
	call gt_sets (gt2, GTTYPE, "line")
 
	call sfree (sp)
end
 
 
# SCAT_CLOSE -- Update the fitting parameters and free the structures.
 
procedure scat_close (ic1, gt1, ic2, gt2)
 
pointer	ic1, ic2		# ICFIT pointer
pointer	gt1, gt2		# GTOOLS pointer
 
pointer	sp, str
int	ic_geti()
real	ic_getr()
 
begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	call ic_gstr (ic1, "function", Memc[str], SZ_LINE)
	call clpstr ("apscat1.function", Memc[str])
	call ic_gstr (ic1, "sample", Memc[str], SZ_LINE)
	call clpstr ("apscat1.sample", Memc[str])
	call clputi ("apscat1.order", ic_geti (ic1, "order"))
	call clputi ("apscat1.naverage", ic_geti (ic1, "naverage"))
	call clputi ("apscat1.niterate", ic_geti (ic1, "niterate"))
	call clputr ("apscat1.low", ic_getr (ic1, "low"))
	call clputr ("apscat1.high", ic_getr (ic1, "high"))
	call clputr ("apscat1.grow", ic_getr (ic1, "grow"))
 
	call ic_gstr (ic2, "function", Memc[str], SZ_LINE)
	call clpstr ("apscat2.function", Memc[str])
	call ic_gstr (ic2, "sample", Memc[str], SZ_LINE)
	call clpstr ("apscat2.sample", Memc[str])
	call clputi ("apscat2.order", ic_geti (ic2, "order"))
	call clputi ("apscat2.naverage", ic_geti (ic2, "naverage"))
	call clputi ("apscat2.niterate", ic_geti (ic2, "niterate"))
	call clputr ("apscat2.low", ic_getr (ic2, "low"))
	call clputr ("apscat2.high", ic_getr (ic2, "high"))
	call clputr ("apscat2.grow", ic_getr (ic2, "grow"))
 
	call ic_closer (ic1)
	call gt_free (gt1)
	call ic_closer (ic2)
	call gt_free (gt2)
	call sfree (sp)
end
 
 
# AP_GDATA -- Get the next line of image data.  Return EOF at end.
# This task optimizes column access if needed.  It assumes sequential access.
 
int procedure ap_gdata (in, out, outmode, axis, maxbuf, index, indata, outdata)
 
pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
int	outmode			# Mode of output image (NEW_COPY|READ_WRITE)
int	axis			# Image axis
int	maxbuf			# Maximum buffer size for column axis
int	index			# Last line (input), current line (returned)
pointer	indata			# Input data pointer
pointer	outdata			# Output data pointer
 
int	i, last_index, col1, col2, nc, ncols, nlines, ncols_block
pointer	inbuf, outbuf, buf, ptr
pointer	imgl2r(), impl2r(), imgs2r(), imps2r()
 
begin
	# Increment to the next image vector.
	index = index + 1
 
	# Initialize for the first vector.
	if (index == 1) {
	    ncols = IM_LEN (in, 1)
	    if (IM_NDIM (in) == 1)
		nlines = 1
	    else
		nlines = IM_LEN (in, 2)
 
	    switch (axis) {
	    case 1:
		last_index = nlines
	    case 2:
		last_index = ncols
	        ncols_block = max (1, min (ncols, maxbuf / nlines))
		col2 = 0
 
	        call malloc (indata, nlines, TY_REAL)
	        call malloc (outdata, nlines, TY_REAL)
	    }
	}
 
	# Finish up if the last vector has been done.
	if (index > last_index) {
	    if (axis == 2) {
	        ptr = outbuf + index - 1 - col1
	        do i = 1, nlines {
		    Memr[ptr] = Memr[outdata+i-1]
		    ptr = ptr + nc
	        }
 
	        call mfree (indata, TY_REAL)
	        call mfree (outdata, TY_REAL)
	    }
 
	    index = 0
	    return (EOF)
	}
 
	# Get the next image vector.
	switch (axis) {
	case 1:
	    indata = imgl2r (in, index)
	    outdata = impl2r (out, index)
	    if (outmode == READ_WRITE)
	        call amovr (Memr[imgl2r(out,index)], Memr[outdata], ncols)
	case 2:
	    if (index > 1) {
		ptr = outbuf + index - 1 - col1
		do i = 1, nlines {
		    Memr[ptr] = Memr[outdata+i-1]
		    ptr = ptr + nc
		}
	    }
 
	    if (index > col2) {
		col1 = col2 + 1
		col2 = min (ncols, col1 + ncols_block - 1)
		nc = col2 - col1 + 1
		inbuf = imgs2r (in, col1, col2, 1, nlines)
		outbuf = imps2r (out, col1, col2, 1, nlines)
		if (outmode == READ_WRITE)
		    buf = imgs2r (out, col1, col2, 1, nlines)
	    }
 
	    ptr = inbuf + index - col1
	    do i = 1, nlines {
		Memr[indata+i-1] = Memr[ptr]
		ptr = ptr + nc
	    }
	    if (outmode == READ_WRITE) {
		ptr = buf + index - col1
	        do i = 1, nlines {
		    Memr[outdata+i-1] = Memr[ptr]
		    ptr = ptr + nc
		}
	    }
	}
 
	return (index)
end
 
 
define	CMDS	"|quit|line|buffer|"
define	QUIT	1		# Quit
define	LINE	2		# Line to examine
define	BUFFER	3		# Buffer distance
 
# AP_GLINE -- Get image data to be fit interactively.  Return EOF
# when the user enters EOF or CR.  The out of bounds
# requests are silently limited to the nearest edge.
 
int procedure ap_gline (ic, gt, im, axis, line, data)
 
pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
pointer	im			# IMIO pointer
int	axis			# Image axis
int	line			# Line to get
pointer	data			# Image data
 
real	rval, clgetr()
int	stat, cmd, ival, strdic(), scan(), nscan()
pointer	sp, name, str, imgl2r(), imgs2r()
 
begin
	call smark (sp)
	call salloc (name, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	stat = OK
	if (data != NULL) {
	    cmd = 0
	    repeat {
	        switch (cmd) {
	        case QUIT:
		    stat = EOF
		    break
	        case LINE:
		    call gargi (ival)
		    if (nscan() == 1) {
		        if (axis == 1)
		            call printf ("line %d - ")
		        else
		            call printf ("column %d - ")
		        call pargi (line)
		    } else {
		        line = max (1, min (IM_LEN(im,axis), ival))
		        break
		    }
	        case BUFFER:
		    call gargr (rval)
		    if (nscan() == 1) {
		        call printf ("buffer %g - ")
			    call pargr (clgetr ("buffer"))
		    } else {
		        call clputr ("buffer", rval)
		        break
		    }
	        }
 
	        call printf ("Command (quit, buffer <value>, line <value>): ")
	        call flush (STDOUT)
	        stat = scan ()
	        if (stat == EOF)
		    break
	        call gargwrd (Memc[str], SZ_LINE)
	        cmd = strdic (Memc[str], Memc[str], SZ_LINE, CMDS)
	    }
	}
 
	if (stat != EOF) {
	    call imstats (im, IM_IMAGENAME, Memc[name], SZ_FNAME)
	    switch (axis) {
	    case 1:
	        call sprintf (Memc[str], SZ_LINE, "%s: Fit line %d\n%s")
	            call pargstr (Memc[name])
	            call pargi (line)
	            call pargstr (IM_TITLE(im))
	        call gt_sets (gt, GTTITLE, Memc[str])
	        call ic_pstr (ic, "xlabel", "Column")
	        data = imgl2r (im, line)
	    case 2:
	        call sprintf (Memc[str], SZ_LINE, "%s: Fit column %d\n%s")
	            call pargstr (Memc[name])
	            call pargi (line)
	            call pargstr (IM_TITLE(im))
	        call gt_sets (gt, GTTITLE, Memc[str])
	        call ic_pstr (ic, "xlabel", "Line")
	        data = imgs2r (im, line, line, 1, IM_LEN(im,2))
	    }
	}
 
	call sfree (sp)
	return (stat)
end
