include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<pkg/gtools.h>
include	"apertures.h"
 
define	MAXBUF	500000		# Buffer size (number of reals) for col access
 
 
# AP_SCATTER -- Fit and subtract the scattered light from between the apertures.
#
# Each line of the input image across the dispersion is read.  The points to
# be fit are selected from between the apertures (which includes a buffer
# distance).  The fitting is done using the ICFIT package.  If not smoothing
# along the dispersion write the scattered light subtracted output directly
# thus minimizing I/O.  If smoothing save the fits in memory.  During the
# smoothing process the fits are evaluated at each point along the dispersion
# and then fit to the create the scattered light subtracted output image.  A
# scattered light image is only created after the output image by subtracting
# the input from the output.

procedure ap_scatter (input, output, scatter, aps, naps, line)
 
char	input[SZ_FNAME]		# Input image
char	output[SZ_FNAME]	# Output image
char	scatter[SZ_FNAME]	# Scattered light image
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures
int	line			# Line to be edited
 
bool	smooth
int	i, aaxis, daxis, npts, nlines, nscatter, nscatter1, new
pointer	sp, str, in, out, scat, cv, cvs, gp, indata, outdata, col, x, y, w 
pointer	ic1, ic2, ic3, gt1, gt2
data	ic3/NULL/

real	clgetr()
int	clgeti(), ap_gline(), ap_gdata()
bool	clgetb(), ap_answer(), apgansb()
pointer	gt_init(), immap(), ap_immap(), imgl2r(), impl2r()

common	/aps_com/ ic1, ic2, gt1, gt2
 
begin
	if (naps < 1)
	    return
 
	# Query the user.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call sprintf (Memc[str], SZ_LINE, "Subtract scattered light in %s?")
	        call pargstr (input)
	if (!ap_answer ("ansscat", Memc[str])) {
	    call sfree (sp)
	    return
	}

	call sprintf (Memc[str], SZ_LINE,
	    "Fit scattered light for %s interactively?")
	    call pargstr (input)
	if (ap_answer ("ansfitscatter", Memc[str]))
	    ;

	call sprintf (Memc[str], SZ_LINE, "Smooth the scattered light in %s?")
	    call pargstr (input)
	if (ap_answer ("anssmooth", Memc[str])) {
	    call sprintf (Memc[str], SZ_LINE,
	        "Smooth the scattered light for %s interactively?")
	        call pargstr (input)
	    if (ap_answer ("ansfitsmooth", Memc[str]))
		;
	}
	smooth = apgansb ("anssmooth")

	# Initialize the ICFIT pointers.
	if (ic1 == NULL || ic3 == NULL) {
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

	    ic3 = ic1
	}
 
	# Map the input and output images.  Warn and return on an error.
	iferr (in = ap_immap (input, aaxis, daxis)) {
	    call sfree (sp)
	    call erract (EA_WARN)
	    return
	}
	iferr (out = immap (output, NEW_COPY, in)) {
	    call imunmap (in)
	    call sfree (sp)
	    call erract (EA_WARN)
	    return
	}
	if (IM_PIXTYPE(out) != TY_DOUBLE)
	    IM_PIXTYPE(out) = TY_REAL
 
	# Allocate memory for curve fitting.
	call ap_sort (i, aps, naps, 1)
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
 
	if (apgansb ("ansfitscatter")) {
	    call ap_gopen (gp)
 
	    if (IS_INDEFI (line))
		i = nlines / 2
	    else
		i = line
	    indata = NULL
	    while (ap_gline (ic1, gt1, NULL, in, aaxis, aaxis, i, indata) !=
		EOF) {
	        call ap_gscatter1 (aps, naps, i, Memr[indata], npts,
		    Memr[x], Memr[y], Memr[w], nscatter)
	        call icg_fit (ic1, gp, "gcur", gt1, cv, Memr[x], Memr[y],
		    Memr[w], nscatter)
	    }
	    call cvfree (cv)
	}
 
	# Loop through the input image and create an output image.
	# To minimize I/O if not smoothing write the final image
	# directly otherwise save the fit.  AP_SMOOTH will then
	# smooth along the dispersion and compute the scattered
	# light subtracted image.
 
	if (clgetb ("verbose")) {
	    call printf (
		"Fitting the scattered light across the dispersion ...\n")
	    call flush (STDOUT)
	}
 
	if (!smooth) {
	    nscatter = 0
	    i = 0
	    while (ap_gdata (in, out, NULL, aaxis, MAXBUF, i,
		indata, outdata) != EOF) {
		call ap_gscatter1 (aps, naps, i, Memr[indata], npts, Memr[x],
		    Memr[y], Memr[w], nscatter1)
		if (nscatter != nscatter1)
		    new = YES
		else
		    new = NO
		nscatter = nscatter1
		call ic_fit (ic1, cv, Memr[x], Memr[y], Memr[w], nscatter,
		    new, YES, new, new)
		call cvvector (cv, Memr[col], Memr[outdata], npts)
		call asubr (Memr[indata], Memr[outdata], Memr[outdata], npts)
	    }
	    call cvfree (cv)
	} else {
	    call salloc (cvs, nlines, TY_POINTER)
	    call amovki (NULL, Memi[cvs], nlines)

	    new = YES
	    i = 0
	    while (ap_gdata (in, NULL, NULL, aaxis, MAXBUF, i,
		indata, outdata) != EOF) {
		call ap_gscatter1 (aps, naps, i, Memr[indata], npts, Memr[x],
		    Memr[y], Memr[w], nscatter)
		call ic_fit (ic1, Memi[cvs+i-1], Memr[x], Memr[y], Memr[w],
		    nscatter, new, YES, new, new)
	    }
     
	    # Smooth and subtract along the dispersion.
	    call ap_smooth (in, out, aaxis, daxis, aps, naps, ic2, gt2, cvs)
	    do i = 1, nlines
		call cvfree (Memi[cvs+i-1])
	}

	call imastr (out, "apscatter", "Scattered light subtracted")
	call imunmap (out)
	call imunmap (in)
 
	# If a scattered light image is desired compute it from the difference
	# of the input and output images.
 
	if (scatter[1] != EOS) {
	    in = immap (input, READ_ONLY, 0)
	    out = immap (output, READ_ONLY, 0)
	    ifnoerr (scat = immap (scatter, NEW_COPY, in)) {
		if (IM_PIXTYPE(scat) != TY_DOUBLE)
		    IM_PIXTYPE(scat) = TY_REAL
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
	    "SCATTER - Scattered light subtracted from %s")
	    call pargstr (input)
	call ap_log (Memc[str], YES, YES, NO)
 
	call sfree (sp)
end


# SCAT_FREE -- Free scattered light memory.

procedure scat_free ()

pointer	ic1, ic2, gt1, gt2
pointer	sp, str

int	ic_geti()
real	ic_getr()

common	/aps_com/ ic1, ic2, gt1, gt2

begin
	if (ic1 != NULL) {
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
	}
end
 
 
# AP_SMOOTH -- Smooth the scattered light by fitting one dimensional functions.
#
# The output image consists of smooth one dimensional fits across the
# dispersion.  This routine reads each line along the dispersion and fits
# a function to smooth the fits made across the dispersion.  The output
# image is used both as input of the cross dispersion fits and as output
# of the scattered light subtracted image.
 
procedure ap_smooth (in, out, aaxis, daxis, aps, naps, ic, gt, cvs)
 
pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
int	aaxis, daxis		# Aperture and dispersion axes
pointer	aps[ARB]		# Apertures
int	naps			# Number of apertures
pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
pointer	cvs			# CURFIT pointers
 
int	i, npts, nlines, new
pointer	cv, gp, indata, outdata, x, w 

int	ap_gline(), ap_gdata()
bool	clgetb(), apgansb()
 
begin
	if (!apgansb ("anssmooth"))
	    return
 
	# Allocate memory for curve fitting.
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
 
	if (apgansb ("ansfitsmooth")) {
	    call ap_gopen (gp)
 
	    i = nlines / 2
	    outdata = NULL
	    while (ap_gline (ic, gt, cvs, out, daxis, aaxis, i, outdata) !=
		EOF) {
	        call icg_fit (ic, gp, "gcur", gt, cv, Memr[x],
		    Memr[outdata], Memr[w], npts)
		call amovkr (1., Memr[w], npts)
	    }
	    call mfree (outdata, TY_REAL)
	}
 
	# Loop through the input image and create an output image.
	if (clgetb ("verbose")) {
	    call printf ("Smoothing scattered light along the dispersion ...\n")
	    call flush (STDOUT)
	}
 
	# Use the new flag to optimize the fitting.
	new = YES
	i = 0
	while (ap_gdata (in, out, cvs, daxis, MAXBUF, i,
	    indata, outdata) != EOF) {
	    call ic_fit (ic, cv, Memr[x], Memr[outdata], Memr[w], npts,
		new, YES, new, new)
	    call cvvector (cv, Memr[x], Memr[outdata], npts)
	    call asubr (Memr[indata], Memr[outdata], Memr[outdata], npts)
	    new = NO
	}
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
real	center, ap_cveval(), clgetr()
 
begin
	buf = clgetr ("buffer") + 0.5
	call aclrr (x, npts)
 
	axis = AP_AXIS(aps[1])
	do i = 1, naps {
	    center = AP_CEN(aps[i],axis) + ap_cveval (AP_CV(aps[i]), real(line))
	    low = max (1, int (center + AP_LOW(aps[i],axis) - buf))
	    high = min (npts, int (center + AP_HIGH(aps[i],axis) + buf))
	    do j = low, high
		x[j] = 1
	}

	nscatter = 0
	do i = 1, npts {
	    if (x[i] == 0.) {
		nscatter = nscatter + 1
		x[nscatter] = i
		y[nscatter] = data[i]
		w[nscatter] = 1.
	    }
	}
end
 
 
# AP_GDATA -- Get the next line of image data.  Return EOF at end.
# This task optimizes column access if needed.  It assumes sequential access.
 
int procedure ap_gdata (in, out, cvs, axis, maxbuf, index, indata, outdata)
 
pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer (NULL if no output)
pointer	cvs			# CURFIT pointers
int	axis			# Image axis
int	maxbuf			# Maximum buffer size chars for column axis
int	index			# Last line (input), current line (returned)
pointer	indata			# Input data pointer
pointer	outdata			# Output data pointer
 
real	val, ap_cveval()
int	i, last_index, col1, col2, nc, nd, ncols, nlines, ncols_block
pointer	inbuf, outbuf, ptr, imgl2r(), impl2r(), imgs2r(), imps2r()
 
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
		nd = ncols
		last_index = nlines
	    case 2:
		nd = nlines
		last_index = ncols
	        ncols_block =
		    max (1, min (ncols, maxbuf / nlines))
		col2 = 0
 
	        call malloc (indata, nlines, TY_REAL)
		if (out != NULL)
		    call malloc (outdata, nlines, TY_REAL)
	    }
	}
 
	# Finish up if the last vector has been done.
	if (index > last_index) {
	    if (axis == 2) {
	        call mfree (indata, TY_REAL)
		if (out != NULL) {
		    ptr = outbuf + index - 1 - col1
		    do i = 1, nlines {
			Memr[ptr] = Memr[outdata+i-1]
			ptr = ptr + nc
		    }
		    call mfree (outdata, TY_REAL)
		}
	    }
	    index = 0
	    return (EOF)
	}
 
	# Get the next image vector.
	switch (axis) {
	case 1:
	    indata = imgl2r (in, index)
	    if (out != NULL)
		outdata = impl2r (out, index)
	case 2:
	    if (out != NULL)
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
		if (out != NULL)
		    outbuf = imps2r (out, col1, col2, 1, nlines)
	    }
 
	    ptr = inbuf + index - col1
	    do i = 1, nlines {
		Memr[indata+i-1] = Memr[ptr]
		ptr = ptr + nc
	    }
	}
	if (cvs != NULL) {
	    val = index
	    do i = 1, nd
		Memr[outdata+i-1] = ap_cveval (Memi[cvs+i-1], val)
	}
 
	return (index)
end
 
 
define	CMDS	"|quit|line|column|buffer|"
define	QUIT	1		# Quit
define	LINE	2		# Line to examine
define	COLUMN	3		# Column to examine
define	BUFFER	4		# Buffer distance
 
# AP_GLINE -- Get image data to be fit interactively.  Return EOF
# when the user enters EOF or CR.  The out of bounds
# requests are silently limited to the nearest edge.
 
int procedure ap_gline (ic, gt, cvs, im, axis, aaxis, line, data)
 
pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
pointer	cvs			# CURFIT pointers
pointer	im			# IMIO pointer
int	axis			# Image axis
int	aaxis			# Aperture axis
int	line			# Line to get
pointer	data			# Image data
 
real	rval, clgetr(), ap_cveval()
int	i, stat, cmd, ival, strdic(), scan(), nscan()
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
		    if (axis == 2 || nscan() == 1) {
		        call printf ("line %d - ")
		        call pargi (line)
		    } else {
		        line = max (1, min (IM_LEN(im,2), ival))
		        break
		    }
	        case COLUMN:
		    call gargi (ival)
		    if (axis == 1 || nscan() == 1) {
		        call printf ("column %d - ")
		        call pargi (line)
		    } else {
		        line = max (1, min (IM_LEN(im,1), ival))
		        break
		    }
	        case BUFFER:
		    if (axis == aaxis) {
			call gargr (rval)
			if (nscan() == 1) {
			    call printf ("buffer %g - ")
				call pargr (clgetr ("buffer"))
			} else {
			    call clputr ("buffer", rval)
			    break
			}
		    }
	        }
 
		if (axis == aaxis) {
		    if (axis == 1)
			call printf (
			    "Command (quit, buffer <value>, line <value>): ")
		    else
			call printf (
			    "Command (quit, buffer <value>, column <value>): ")
		} else {
		    if (axis == 1)
			call printf (
			    "Command (quit, line <value>): ")
		    else
			call printf (
			    "Command (quit, column <value>): ")
		}
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
		if (axis == aaxis)
		    data = imgl2r (im, line)
		else {
		    if (data == NULL)
			call malloc (data, IM_LEN(im,1), TY_REAL)
		    rval = line
		    do i = 1, IM_LEN(im,1)
			Memr[data+i-1] = ap_cveval (Memi[cvs+i-1], rval)
		}
	    case 2:
		call sprintf (Memc[str], SZ_LINE, "%s: Fit column %d\n%s")
		    call pargstr (Memc[name])
		    call pargi (line)
		    call pargstr (IM_TITLE(im))
		call gt_sets (gt, GTTITLE, Memc[str])
		call ic_pstr (ic, "xlabel", "Line")
		if (axis == aaxis)
		    data = imgs2r (im, line, line, 1, IM_LEN(im,2))
		else {
		    if (data == NULL)
			call malloc (data, IM_LEN(im,2), TY_REAL)
		    rval = line
		    do i = 1, IM_LEN(im,2)
			Memr[data+i-1] = ap_cveval (Memi[cvs+i-1], rval)
		}
	    }
	}
 
	call sfree (sp)
	return (stat)
end
