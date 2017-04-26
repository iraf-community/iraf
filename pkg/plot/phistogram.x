include	<mach.h>
include	<imhdr.h>
include	<gset.h>

define	SZ_HISTBUF	512
define	SZ_CHOICE	18

define	HIST_TYPES	"|normal|cumulative|difference|second_difference|"
define	NORMAL		1
define	CUMULATIVE	2
define	DIFFERENCE	3
define	SECOND_DIFF	4

define	PLOT_TYPES	"|line|box|fullbox|"
define	LINE		1
define	BOX		2
define	FULLBOX		3

define	PATTERN_TYPES	"|solid|dashed|dotted|dotdash|"
define	SOLID		1
define	DASHED		2
define	DOTTED		3
define	DOTDASH		4

define	DEF_TITLE	"imtitle"	# define the default title
define	SZ_TITLE	512		# plot title buffer

# T_PHISTOGRAM -- Compute and plot the histogram of an image.

procedure t_phistogram()

int	isimage, npix, nbins, nbins1, nlevels, nwide, z1i, z2i, i, hist_type
pointer im, tx, sp, hgm, hgmr, buf, input, str, v
real	z1, z2, dz, z1temp, z2temp, zstart

bool	streq(), clgetb(), fp_equalr()
int	clgeti(), clgwrd(), open(), ph_gdata(), imgnlr(), imgnli()
pointer	immap()
real	clgetr()
errchk	immap()

begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_CHOICE, TY_CHAR)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Get the image name.
	call clgstr ("input", Memc[input], SZ_LINE)
	if (streq (Memc[input], "STDIN")) {
	    isimage = NO
	    tx = open (Memc[input], READ_ONLY, TEXT_FILE)
	    npix = ph_gdata (tx, buf, SZ_HISTBUF)
	} else {
	    iferr {
	        im = immap (Memc[input], READ_ONLY, 0)
	    } then {
	        isimage = NO
	        tx = open (Memc[input], READ_ONLY, TEXT_FILE)
	        npix = ph_gdata (tx, buf, SZ_HISTBUF)
	    } else {
	        isimage = YES
	        npix = IM_LEN(im,1)
	        call amovkl (long(1), Meml[v], IM_MAXDIM)
	    }
	}

	# Get histogram range.
	z1 = clgetr ("z1")
	z2 = clgetr ("z2")

	if (IS_INDEFR(z1) || IS_INDEFR(z2)) {
	    if (isimage == NO) {
		call alimr (Memr[buf], npix, z1temp, z2temp)
	    } else if (IM_LIMTIME(im) >= IM_MTIME(im)) {
		z1temp = IM_MIN(im)
		z2temp = IM_MAX(im)
	    } else {
		call ph_imminmax (im, z1temp, z2temp)
	    }
	    if (IS_INDEFR(z1))
		z1 = z1temp
	    if (IS_INDEFR(z2))
		z2 = z2temp
	}

	if (z1 > z2) {
	    dz = z1;  z1 = z2;  z2 = dz
	}

	# Get the default histogram resolution.
	dz = clgetr ("binwidth")
	if (IS_INDEFR(dz)) {
	    nbins = clgeti ("nbins")
	} else {
	    nbins = nint ((z2 - z1) / dz)
	    if ((z1 + nbins * dz) < z2)
		nbins = nbins + 1
	    z2 = z1 + nbins * dz
	}

	# Set the integer defaults.
	if (isimage == YES) {
	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	        z1i = nint (z1)
	        z2i = nint (z2)
	        z1 = real (z1i)
	        z2 = real (z2i)
	    }
	}


	# Adjust the resolution of the histogram and/or the data range
	# so that an integral number of data values map into each
	# histogram bin (to avoid aliasing effects).

	if (isimage == YES && clgetb ("autoscale"))
	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
		nlevels = z2i - z1i
		nwide = max (1, nint (real (nlevels) / real (nbins)))
		nbins = max (1, nint (real (nlevels) / real (nwide)))
		z2i = z1i + nbins * nwide
		z2 = real (z2i)
	    }

	# The extra bin counts the pixels that equal z2 and shifts the
	# remaining bins to evenly cover the interval [z1,z2].
	# Real numbers could be handled better - perhaps adjust z2
	# upward by ~ EPSILONR (in ahgm itself).

	nbins1 = nbins + 1

	# Initialize the histogram buffer and image line vector.
	call salloc (hgm,  nbins1, TY_INT)
	call aclri  (Memi[hgm], nbins1)

	# Read successive lines of the image and accumulate the histogram.

	if (isimage == NO) {

	    # Test for NULL data range.
	    if (fp_equalr (z1, z2)) {
	        call eprintf ("Warning: File `%s' has no data range.\n")
		    call pargstr (Memc[input])
		call mfree (buf, TY_REAL)
	        call sfree (sp)
	        call close (tx)
	        return
	    }

	    call ahgmr (Memr[buf], npix, Memi[hgm], nbins1, z1, z2)

	} else {
	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:

	        # Test for constant valued image.
	        if (z1i == z2i) {
	            call eprintf ("Warning: Image `%s' has no data range.\n")
		        call pargstr (Memc[input])
	            call sfree (sp)
	            call imunmap (im)
	            return
	        }

	        while (imgnli (im, buf, Meml[v]) != EOF) 
		    call ahgmi (Memi[buf], npix, Memi[hgm], nbins1, z1i, z2i)

	    default:

	        # Test for constant valued image.
	        if (fp_equalr (z1, z2)) {
	            call eprintf ("Warning: Image `%s' has no data range.\n")
		        call pargstr (Memc[input])
	            call sfree (sp)
	            call imunmap (im)
	            return
	        }

	        while (imgnlr (im, buf, Meml[v]) != EOF)
		    call ahgmr (Memr[buf], npix, Memi[hgm], nbins1, z1, z2)
	    }
	}

	# "Correct" the topmost bin for pixels that equal z2.  Each
	# histogram bin really wants to be half open.

	if (clgetb ("top_closed"))
	    Memi[hgm+nbins-1] = Memi[hgm+nbins-1] + Memi[hgm+nbins1-1]
	dz = (z2 - z1) / real (nbins)

	hist_type = clgwrd ("hist_type", Memc[str], SZ_CHOICE, HIST_TYPES)
	switch (hist_type) {
	case NORMAL:
	    # do nothing
	case CUMULATIVE:
	    call ph_acumi (Memi[hgm], Memi[hgm], nbins)
	case DIFFERENCE:
	    call ph_amrgi (Memi[hgm], Memi[hgm], nbins)
	    z1 = z1 + dz / 2.
	    z2 = z2 - dz / 2.
	    nbins = nbins - 1
	case SECOND_DIFF:
	    call ph_amrgi (Memi[hgm], Memi[hgm], nbins)
	    call ph_amrgi (Memi[hgm], Memi[hgm], nbins-1)
	    z1 = z1 + dz
	    z2 = z2 - dz
	    nbins = nbins - 2
	default:
	    call error (0, "Unknown histogram type")
	}

	# List or plot the histogram.  In list format, the bin value is the
	# z value of the left side (start) of the bin.

	if (clgetb ("listout")) {

	    zstart = z1 + dz / 2.0
	    do i = 1, nbins {
		call printf ("%g %d\n")
		    call pargr (zstart)
		    call pargi (Memi[hgm+i-1])
		    zstart = zstart + dz
	    }

	} else {

	    # Convert the histogram to the correct data type for plotting
	    # and do the plot.

	    call salloc (hgmr, nbins, TY_REAL)
	    call achtir (Memi[hgm], Memr[hgmr], nbins)
	    if (isimage == YES)
	        call ph_plot (Memr[hgmr], nbins, z1, z2, dz, hist_type,
		    Memc[input], IM_TITLE(im))
	    else
	        call ph_plot (Memr[hgmr], nbins, z1, z2, dz, hist_type,
		    Memc[input], "")
	}

	if (isimage == YES) {
	    call imunmap (im)
	} else {
	    call mfree (buf, TY_REAL)
	    call close (tx)
	}
	call sfree (sp)
end


# PH_GDATA -- Read the data from a text file.

int procedure ph_gdata (fd, data, sz_bufincr)

int	fd		# input text file descriptor
pointer	data		# pointer to the ouput data array
int	sz_bufincr	# increment for data buffer size

int	szbuf, ndata
int	fscan(), nscan()

begin
	# Get some buffer space.
	call malloc (data, sz_bufincr, TY_REAL)
	szbuf = sz_bufincr

	# Read the data.
	ndata = 0
	while (fscan (fd) != EOF) {
	    call gargr (Memr[data+ndata])
	    if (nscan() != 1)
		next
	    ndata = ndata + 1
	    if (ndata < szbuf)
		next
	    szbuf = szbuf + sz_bufincr
	    call realloc (data, szbuf, TY_REAL)
	}

	# Fit the buffer size to the data.
	if (ndata > 0)
	    call realloc (data, ndata, TY_REAL)

	return (ndata)
end


# PH_PLOT -- Plot the histogram.

procedure ph_plot (hgmr, nbins, z1, z2, dz, hist_type, hsource, hid)

real	hgmr[ARB]		# the histogram values
int	nbins			# the number of bins in the histogram
real	z1			# the lower limit of the histogram
real	z2			# the upper limit of the histogram
real	dz			# the bin width of the histogram
int	hist_type		# the histogram type
char	hsource[ARB]		# source of the histogram data
char	hid[ARB]		# the id of the histogram

pointer	sp, title, xlabel, ylabel, device, str, gp
real	hmin, hmax, wx1, wx2, wy1, wy2, vx1, vx2, vy1, vy2
bool	clgetb(), streq()
int	clgwrd(), btoi(), clgeti()
pointer	gopen()
real	clgetr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (str, max (SZ_CHOICE, SZ_TITLE), TY_CHAR)

	call clgstr ("device", Memc[device], SZ_FNAME)
	if (! clgetb ("append")) {

	    # Open the graphics device.
	    gp = gopen (Memc[device], NEW_FILE, STDGRAPH)

	    # Get the world coordinate system of the plot.
	    wx1 = clgetr ("wx1")
	    wx2 = clgetr ("wx2")
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")
	    if (IS_INDEFR(wx1))
		wx1 = z1
	    if (IS_INDEFR(wx2))
		wx2 = z2
	    if (IS_INDEFR(wy1) || IS_INDEFR(wy2)) {
		call alimr (hgmr, nbins, hmin, hmax)
		if (IS_INDEFR(wy1))
		    wy1 = 0.0
		if (IS_INDEFR(wy2))
		    wy2 = hmax
	    }
	    call gswind (gp, wx1, wx2, wy1, wy2)
	    call gseti (gp, G_ROUND, btoi (clgetb ("round")))
	    if (clgetb ("fill"))
		call gsetr (gp, G_ASPECT, 0.0)
	    else
		call gsetr (gp, G_ASPECT, 1.0)

	    if (clgetb ("logx"))
	        call gseti (gp, G_XTRAN, GW_LOG)
	    else
	        call gseti (gp, G_XTRAN, GW_LINEAR)
	    if (clgetb ("logy"))
	        call gseti (gp, G_YTRAN, GW_LOG)
	    else
	        call gseti (gp, G_YTRAN, GW_LINEAR)

	    # Set the view port.
	    vx1 = clgetr ("vx1")
	    vx2 = clgetr ("vx2")
	    vy1 = clgetr ("vy1")
	    vy2 = clgetr ("vy2")
	    call gsview (gp, vx1, vx2, vy1, vy2)

	    # Draw the box around the plot and label the tick marks.
	    if (clgetb ("box")) {

		# Label the tick marks.
		call gseti (gp, G_LABELTICKS, btoi (clgetb ("ticklabels")))

		# Get the number of tick marks.
		call gseti (gp, G_XNMAJOR, clgeti ("majrx"))
		call gseti (gp, G_XNMINOR, clgeti ("minrx"))
		call gseti (gp, G_YNMAJOR, clgeti ("majry"))
		call gseti (gp, G_YNMINOR, clgeti ("minry"))

	        # Allocate space for the labels and title.
	        call salloc (title, SZ_TITLE, TY_CHAR)
	        call salloc (xlabel, SZ_FNAME, TY_CHAR)
	        call salloc (ylabel, SZ_FNAME, TY_CHAR)

	        # Format the x and y axis labels.
	        call clgstr ("xlabel", Memc[xlabel], SZ_FNAME)
	        call clgstr ("ylabel", Memc[ylabel], SZ_FNAME)

	        # Format the plot title, starting with the system banner.
	        call clgstr ("title", Memc[title], SZ_TITLE) 
	        if (streq (Memc[title], DEF_TITLE)) {
	            call sysid (Memc[str], SZ_TITLE)
	            call sprintf (Memc[title], SZ_TITLE,
	            "%s\n%s of %s  %s\nFrom z1=%g to z2=%g, nbins=%d, width=%g")
		    call pargstr (Memc[str])
	            switch (hist_type) {
	            case NORMAL:
	                call pargstr ("Histogram")
	            case CUMULATIVE:
	                call pargstr ("Cumulative histogram")
	            case DIFFERENCE:
	                call pargstr ("Difference histogram")
	            case SECOND_DIFF:
	                call pargstr ("Second difference histogram")
	            default:
	                call error (0, "Unknown histogram type")
	            }
	            call pargstr (hsource)
	            call pargstr (hid)
	            call pargr (z1)
	            call pargr (z2)
	            call pargi (nbins)
	            call pargr (dz)
	        }

	        call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	    }

	} else
	    gp = gopen (Memc[device], APPEND, STDGRAPH)


	# Set the vector pattern.
	switch (clgwrd ("pattern", Memc[str], SZ_LINE, PATTERN_TYPES)) {
	case SOLID:
	    call gseti (gp, G_PLTYPE, GL_SOLID)
	case DASHED:
	    call gseti (gp, G_PLTYPE, GL_DASHED)
	case DOTTED:
	    call gseti (gp, G_PLTYPE, GL_DOTTED)
	case DOTDASH:
	    call gseti (gp, G_PLTYPE, GL_DOTDASH)
	}

	# Draw the plot.  Center the bins for plot_type=line.
	switch (clgwrd ("plot_type", Memc[str], SZ_LINE, PLOT_TYPES)) {
	case LINE:
	    call gvline (gp, hgmr, nbins, z1 + dz/2., z2 - dz/2.)
	case BOX:
	    call ph_hgline (gp, hgmr, nbins, z1, z2)
	case FULLBOX:
	    call ph_fhgline (gp, hgmr, nbins, z1, z2)
	default:
	    call error (0, "Unknown histogram plot type")
	}

	call gclose (gp)
	call sfree (sp)
end


# PH_HGLINE -- Draw a stepped curve of the histogram data.

procedure ph_hgline (gp, ydata, npts, x1, x2)

pointer	gp		# Graphics descriptor
real	ydata[ARB]	# Y coordinates of the line endpoints
int	npts		# Number of line endpoints
real	x1, x2

int	pixel
real	left, right, top, bottom, x, y, dx

begin
	call ggwind (gp, left, right, bottom, top)

	dx = (x2 - x1) / npts

	# Do the first vertical line.
	call gamove (gp, x1, bottom)
	call gadraw (gp, x1, ydata[1])

	# Do the first horizontal line.
	call gadraw (gp, x1 + dx, ydata[1])

	# Draw the remaining horizontal lines.
	do pixel = 2, npts {
	    x = x1 + dx * (pixel - 1)
	    y = ydata[pixel]
	    call gadraw (gp, x, y)
	    call gadraw (gp, x + dx, y)
	}

	# Draw the last vertical line.
	call gadraw (gp, x + dx, bottom)
end


# PH_FHGLINE -- Draw a stepped curve of the histogram data.

procedure ph_fhgline (gp, ydata, npts, x1, x2)

pointer	gp		# Graphics descriptor
real	ydata[ARB]	# Y coordinates of the line endpoints
int	npts		# Number of line endpoints
real	x1, x2

int	pixel
real	left, right, top, bottom, x, y, dx

begin
	call ggwind (gp, left, right, bottom, top)

	dx = (x2 - x1) / npts

	# Do the first vertical line.
	call gamove (gp, x1, bottom)
	call gadraw (gp, x1, ydata[1])

	# Do the first horizontal line.
	call gadraw (gp, x1 + dx, ydata[1])

	# Draw the remaining horizontal lines.
	do pixel = 2, npts {
	    x = x1 + dx * (pixel - 1)
	    y = ydata[pixel]
	    call gadraw (gp, x, y)
	    call gamove (gp, x, bottom)
	    call gadraw (gp, x, y)
	    call gadraw (gp, x + dx, y)
	}

	# Draw the last vertical line.
	call gadraw (gp, x + dx, bottom)
end


# These two routines are intended to be generic vops routines.  Only
# the integer versions are included since that's all that's used here.

# <NOT IMPLEMENTED!>  The operation is carried out in such a way that
# the result is the same whether or not the output vector overlaps
# (partially) the input vector.  The routines WILL work in place!

# PH_ACUMI -- Compute a cumulative vector (generic).  Should b[1] be zero?

procedure ph_acumi (a, b, npix)

int	a[ARB], b[ARB]
int	npix, i

# int	npix, i, a_first, b_first

begin
#	call zlocva (a, a_first)
#	call zlocva (b, b_first)
#
#	if (b_first <= a_first) {
	    # Shouldn't use output arguments internally,
	    # but no reason to use this routine unsafely.
	    b[1] = a[1]
	    do i = 2, npix
		b[i] = b[i-1] + a[i]
#	} else {
	    # overlapping solution not implemented yet!
#	}
end


# PH_AMRG -- Compute a marginal (forward difference) vector (generic).

procedure ph_amrgi (a, b, npix)

int	a[ARB], b[ARB]
int	npix, i

# int	npix, i, a_first, b_first

begin
#	call zlocva (a, a_first)
#	call zlocva (b, b_first)
#
#	if (b_first <= a_first) {
	    do i = 1, npix-1
		b[i] = a[i+1] - a[i]
	    b[npix] = 0
#	} else {
	    # overlapping solution not implemented yet!
#	}
end
