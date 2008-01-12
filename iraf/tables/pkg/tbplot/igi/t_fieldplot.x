define	DATA_SIZE	1024
define	HEAD		1
define	CENTER		0
define	TAIL		-1
define	MARGIN_FRAC	0.035

include	<mach.h>
include	<math.h>
include <gset.h>

procedure t_fieldplot ()

#  This task draws a plot of a vector field;  an arrow represents the
#  magnitude and direction of the vector at any number of positions within
#  a two dimensional field.  Data consist of four values:  x and y position
#  and magnitude and direction.  Alternately, the vector may be specified
#  by projected magnitude along x and y. 
#  
#  If the boolean task parameter `rtheta' is true, the vectors are assumed
#  to be specified by magnitude (r) and direction (theta).  The magnitude
#  is assumed to be in the same units and scale as the x and y coordinate
#  data.  The direction angle is specified counterclockwise from the
#  horizontal (positive x). If the boolean task parameter `degrees' is
#  true, the directions are assumed to be in degrees, otherwise, they are
#  in radians. If `rtheta' is false, the projected magnitudes are assumed
#  to be in the same units and scale as the x and y coordinate data. 
#  
#  The size of the arrows may be adjusted using the floating point task
#  parameter `magscale'.  This specifies a factor applied to the vector
#  magnitudes in transforming them to the plot scale.  For example,
#  magscale=2.0 would plot arrows twice the size of the normal plot scale. 
#  The size of the heads drawn on the arrows may be adjusted by the
#  floating point task parameter arrowsize.  The size is specified in
#  normalized device coordinates (NDC).  A marker may be drawn at the tail
#  of the vector (at the field coordinates).  The style of marker is
#  specified by the integer task parameter `tailmark'.

#  4/12/91 Added device and viewport parameters,
#  implemented through pset devpar.  ZGL
#  5/3/91 Added zeroplot parameter to optionally ignore zero-size marks.  ZGL
#  1/11/93 Change append+ mode to avoid rescaling the plot and
#  redrawing the axes.  ZGL
#  Change parameter 'indata' to 'input'.

pointer	gp
pointer	xcoord, ycoord		# Coordinates of field value
pointer	xmag, ymag		# Magnitude of field
int	npts			# Number of points
pointer	sp
pointer	device
pointer	intitle
real	magscale
int	position
real	vl, vr, vb, vt		# Viewport edges
bool	margin			# Add margin to avoid truncating?
real	hsize			# Arrow head size (NDC)
pointer	indata			# Input data file name
pointer	fd			# Data

pointer	open(), gopen()
real	clgetr()
int	clgeti(), markpos()
bool	streq(), clgetb()

begin
	call smark (sp)
	call salloc (device, SZ_LINE,  TY_CHAR)
	call salloc (indata, SZ_FNAME, TY_CHAR)

	call clgstr ("input", Memc[indata], SZ_LINE)
	if (streq (Memc[indata], "STDIN"))
	    fd = STDIN
	else
	    fd = open (Memc[indata], READ_ONLY, TEXT_FILE)

	# Read the data
	call getfield (fd, clgetb ("rtheta"), 
	    xcoord, ycoord, xmag, ymag, npts)

	magscale = clgetr ("magscale")

	# Adjust the size of the symbols
	call scalemag (Memr[xmag], Memr[ymag], npts, magscale)

	call clgstr ("device", Memc[device], SZ_LINE)

	if (clgetb ("append"))
	    # Append to existing plot;  use old scale and axes
	    gp = gopen (Memc[device], APPEND, STDGRAPH)

	else {
	    # Open graphics
	    gp = gopen (Memc[device], NEW_FILE, STDGRAPH)

	    # Viewport edges
	    vl = clgetr ("left");    vr = clgetr ("right")
	    vb = clgetr ("bottom");  vt = clgetr ("top")
	    call gsview (gp, vl, vr, vb, vt)

	    position = markpos ()
	    margin = clgetb ("margin")

	    # Scale the plot
	    call fldscl (gp, Memr[xcoord], Memr[ycoord], 
		Memr[xmag], Memr[ymag], npts, position, margin)

	    if (clgetb ("axes")) {
		# Draw the axes and title
		call salloc (intitle, SZ_LINE, TY_CHAR)
		call clgstr ("title", Memc[intitle], SZ_LINE)
		call fldaxis (gp, Memc[intitle], magscale)
	    }
	}

	call sfree  (sp)

	if (clgetb ("head"))
	    hsize = clgetr ("headsize")
	else
	    hsize = 0.0

	# Do the field vectors
	call pfield (gp, Memr[xcoord], Memr[ycoord], 
	    Memr[xmag], Memr[ymag], npts, 
	    position, hsize, 
	    clgeti ("psnmark"), clgetr ("marksize"), clgetb ("zeroplot"))

	call gclose (gp)
	call sfree (sp)
end


procedure getfield (fd, rtheta, xcoord, ycoord, xmag, ymag, npts)

pointer	fd			# Data source
bool	rtheta			# Magnitude and direction? (instead of dx, dy)
pointer	xcoord, ycoord		# Coordinates of field value
pointer	xmag, ymag		# Magnitude of field
int	npts			# Number of points

int	row
pointer	sp, inline

int	sscan(), nscan(), getline()
bool	clgetb()

begin
	npts = DATA_SIZE
	call malloc (xcoord, npts, TY_REAL)
	call malloc (ycoord, npts, TY_REAL)
	call malloc (xmag, npts, TY_REAL)
	call malloc (ymag, npts, TY_REAL)

	call smark (sp)
	call salloc (inline, SZ_LINE, TY_CHAR)

	row = 0
	while (getline (fd, Memc[inline]) != EOF) {
	    # Read an input file row
	    if (Memc[inline] == '#' || Memc[inline] == '\n')
		# Ignore comments and blank lines
		next

	    row = row + 1
	    if (row > npts) {
		# Ran out of room;  Allocate more scratch space
		npts = npts + DATA_SIZE
		call realloc (xcoord, npts, TY_REAL)
		call realloc (ycoord, npts, TY_REAL)
		call realloc (xmag, npts, TY_REAL)
		call realloc (ymag, npts, TY_REAL)
	    }

	    # Pop the cell into the data arrays
	    if (sscan (Memc[inline]) == EOF) 
		# End of data
		row = row - 1
	    else {
		call gargr (Memr[xcoord+row-1])
		call gargr (Memr[ycoord+row-1])
		call gargr (Memr[xmag+row-1])
		call gargr (Memr[ymag+row-1])

		if (nscan () < 4)
		    #  Not enough fields;  skip this line
		    row = row - 1
	    }
	}

	call sfree (sp)

	if (row == 0)
	    call error (0, "No data read")

	npts = row
	# Resize the data buffer
	call realloc (xcoord, npts, TY_REAL)
	call realloc (ycoord, npts, TY_REAL)
	call realloc (xmag, npts, TY_REAL)
	call realloc (ymag, npts, TY_REAL)

	if (rtheta)
	    # Input is direction and magnitude 
	    call rth_dxy (Memr[xmag], Memr[ymag], npts, clgetb ("degrees"))
end



procedure rth_dxy (xmag, ymag, npts, degrees)

# Convert input from direction and magnitude to projections in X and Y
# Direction is assumed to be from +X counterclockwise

real	xmag[ARB], ymag[ARB]	# Magnitude of field
int	npts			# Number of points
bool	degrees			# Position angle in degrees?

int 	i
real	r, theta

begin
	if (degrees)
	    # Convert degrees to radians
	    call adivkr (ymag, RADIAN, ymag, npts)

	do i = 1, npts {
	    r = xmag[i]
	    theta = ymag[i]
	    xmag[i] = r * cos (theta)
	    ymag[i] = r * sin (theta)
	}
end



procedure scalemag (xmag, ymag, npts, magscale)

real	xmag[ARB], ymag[ARB]
int	npts
real	magscale

begin
	if (!IS_INDEF(magscale) && magscale != 1.0) {
	    call amulkr (xmag, magscale, xmag, npts)
	    call amulkr (ymag, magscale, ymag, npts)
	}
end



procedure fldscl (gp, xcoord, ycoord, xmag, ymag, npts, position, margin)

pointer	gp
real	xcoord[ARB], ycoord[ARB]
real	xmag[ARB], ymag[ARB]
int	npts
int	position
bool	margin			# Add margin to avoid truncating?

real	left, right, bottom, top

begin
	if (margin) {
	    #  Include a margin to plot entirety of symbolss
	    call extlim (xcoord, xmag, npts, position, left, right)
	    call extlim (ycoord, ymag, npts, position, bottom, top)
	    call gswind (gp, left, right, bottom, top)

	} else {
	    # No margin;  permit overlaying on image
	    call gascale (gp, xcoord, npts, 1)
	    call gascale (gp, ycoord, npts, 2)
	}
end



procedure extlim (coord, mag, npts, position, minax, maxax)

#  extlim -- Adjust the plot WCS to include space for that portion of
# arrows potentially extending beyond the viewport as defined by the
# point coordinates.

real	coord[ARB]
real	mag[ARB]
int	npts
real	minax, maxax
int	position

real	min1, max1
real	min2, max2
pointer	sp, vect
real	margin

begin
	call smark  (sp)
	call salloc (vect, npts, TY_REAL)

	switch (position) {
	case TAIL:
	    call alimr (coord, npts, min1, max1)
	    call aaddr (coord, mag, Memr[vect], npts)

	case CENTER:
	    call amulkr (mag, 0.5, Memr[vect], npts)
	    call aaddr  (coord, Memr[vect], Memr[vect], npts)
	    call alimr  (Memr[vect], npts, min1, max1)
	    call asubr  (Memr[vect], mag, Memr[vect], npts)

	case HEAD:
	    call alimr (coord, npts, min1, max1)
	    call asubr (coord, mag, Memr[vect], npts)
	}

	call alimr (Memr[vect], npts, min2, max2)
	minax = min (min1, min2)
	maxax = max (max1, max2)

	margin = MARGIN_FRAC * (maxax - minax)
	minax  = minax - margin
	maxax  = maxax + margin

	call sfree (sp)
end



int procedure markpos ()

int	position
pointer	sp, posdic, mpword, mpfind

int	strdic()

begin
	call smark  (sp)

	call salloc (posdic, SZ_LINE, TY_CHAR)
	call clgstr ("crdpsn.p_min", Memc[posdic], SZ_LINE)
	call salloc (mpword, SZ_LINE, TY_CHAR)
	call clgstr ("crdpsn", Memc[mpword], SZ_LINE)
	call strlwr (Memc[mpword])
	call salloc (mpfind, SZ_LINE, TY_CHAR)

	position = strdic (Memc[mpword], Memc[mpfind], SZ_LINE, Memc[posdic])

	if (position == 0)
	    position = TAIL
	else
	    position = position - 2

	call sfree  (sp)

	return (position)
end



procedure pfield (gp, xcoord, ycoord, xmag, ymag, npts, 
	position, arrowsize, tailmark, marksize, zeroplot)

pointer	gp
real	xcoord[ARB], ycoord[ARB]
real	xmag[ARB], ymag[ARB]
int	npts
int	position
real	arrowsize
int	tailmark
real	marksize
bool	zeroplot		# Plot zero-sized markers?

int	i
real	x1, y1, x2, y2
real	xs, ys, ar
real	dist

real	ggetr()

begin
	ar = ggetr (gp, "ys") / ggetr (gp, "xs")

	if (ar < 1.0) {
	    xs = ar
	    ys = 1.0

	} else if (ar > 1.0) {
	    xs = 1.0
	    ys = 1.0 / ar
	}

	xs = 2.0 * marksize * xs
	ys = 2.0 * marksize * ys

	do i = 1, npts {
	    x1 = xcoord[i]
	    y1 = ycoord[i]
	    x2 = xcoord[i] + xmag[i]
	    y2 = ycoord[i] + ymag[i]

	    if (!zeroplot) {
		#  Find the absolute marker size
		dist = sqrt ((x2 - x1)**2 + (y2 - y1)**2)
		
		if (dist <= EPSILONR)
	    	    # Zero size;  don't plot this one
	    	    next
	    }

	    if (!IS_INDEFI(tailmark) && tailmark != 0 && marksize > 0.0)
		call gmark (gp, x1, y1, tailmark, xs, ys)

	    call draw_arrow (gp, x1, y1, xmag[i], ymag[i], 
		position, arrowsize)
	}
end



procedure draw_arrow (gp, x, y, dx, dy, position, arrowsize)

pointer	gp
real	x, y
real	dx, dy
int	position
real	arrowsize

real	xa, ya, xb, yb, xc, yc
real	x1, y1, x2, y2
real	dhx, dhy
real	alpha

begin
	switch (position) {
	case TAIL:
	    x1 = x;  y1 = y
	    x2 = x + dx;  y2 = y + dy

	case CENTER:
	    dx = dx / 2.0;  dy = dy / 2.0
	    x1 = x - dx;  y1 = y - dy
	    x2 = x + dx;  y2 = y + dy

	case HEAD:
	    x1 = x - dx;  y1 = y - dy
	    x2 = x;  y2 = y
	}

	call gline (gp, x1, y1, x2, y2)

	if (arrowsize <= 0.0 || IS_INDEF(arrowsize) || 
	    (dx == 0.0 && dy == 0.0))
	    return

	call wcs_dev (gp, x1, y1, xa, ya)
	call wcs_dev (gp, x2, y2, xb, yb)

	dhx = xb - xa;  dhy = yb - ya
	alpha = atan2 (dhy, dhx)

	call ndc_dev (gp, arrowsize, arrowsize, dhx, dhy)

	xa = -2.0 * dhx;  ya = -1.0 * dhx
	call rotate (xa, ya, alpha, xa, ya)
	xc = xa + xb;  yc = ya + yb
	call dev_wcs (gp, xc, yc, x1, y1)
	call gline (gp, x1, y1, x2, y2)

	xa = -2.0 * dhx;  ya = +1.0 * dhx
	call rotate (xa, ya, alpha, xa, ya)
	xc = xa + xb;  yc = ya + yb
	call dev_wcs (gp, xc, yc, x1, y1)
	call gline (gp, x1, y1, x2, y2)
end



procedure dev_wcs (gp, dx, dy, wx, wy)

pointer	gp
real	dx, dy		# Device coordinates
real	wx, wy		# WCS coordinates

real	nx, ny		# NDC coordinates
real	xs, ys
int	wcs

int	gstati()
real	ggetr()

begin
	xs = ggetr (gp, "xs")
	ys = ggetr (gp, "ys")
	wcs = gstati (gp, G_WCS)

	nx = dx / xs
	ny = dy / ys

	call gctran (gp, nx, ny, wx, wy, 0, wcs)
end



procedure wcs_dev (gp, wx, wy, dx, dy)

pointer	gp
real	wx, wy		# WCS coordinates
real	dx, dy		# Device coordinates

real	nx, ny		# NDC coordinates
real	xs, ys
int	wcs

int	gstati()
real	ggetr()

begin
	xs = ggetr (gp, "xs")
	ys = ggetr (gp, "ys")
	wcs = gstati (gp, G_WCS)

	call gctran (gp, wx, wy, nx, ny, wcs, 0)
	dx = nx * xs
	dy = ny * ys
end



procedure ndc_dev (gp, nx, ny, dx, dy)

pointer	gp
real	nx, ny		# NDC coordinates
real	dx, dy		# Device coordinates

real	xs, ys
int	wcs

int	gstati()
real	ggetr()

begin
	xs = ggetr (gp, "xs")
	ys = ggetr (gp, "ys")
	wcs = gstati (gp, G_WCS)

	dx = nx * xs
	dy = ny * ys
end


procedure rotate (x1, y1, alpha, x2, y2)

real	x1, y1
real	alpha		# Rotation in degrees
real	x2, y2

real	x, y
real	ca, sa

begin
	ca = cos (alpha);  sa = sin (alpha)
	x = x1 * ca - y1 * sa
	y = x1 * sa + y1 * ca
	x2 = x
	y2 = y
end


procedure fldaxis (gp, intitle, magscale)

pointer	gp
char	intitle[ARB]
real	magscale

pointer	sp, title
int	ntitle

bool	fp_equalr()

begin
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)

	# Build the title
	ntitle = 1
	call sysid  (Memc[title], SZ_LINE)

	if (intitle[1] != EOS) {
	    call sprintf (Memc[title], SZ_LINE, "%s\n%s")
		call pargstr (Memc[title])
		call pargstr (intitle)
	    ntitle = ntitle + 1
	}

	if (!fp_equalr (magscale, 1.0)) {
	    call sprintf (Memc[title], SZ_LINE, "%s\nVector scale: %.3gX")
		call pargstr (Memc[title])
		call pargr (magscale)
	    ntitle = ntitle + 1
	}

	# Draw the axes
	call gseti  (gp, G_NTITLELINES, ntitle)
	call glabax (gp, Memc[title], EOS, EOS)

	call sfree (sp)
end
