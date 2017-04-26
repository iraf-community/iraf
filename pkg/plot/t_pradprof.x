include <imhdr.h>
include <gset.h>
include	<math.h>

# T_ PRADPROF -- Compute a radial profile using user specified coordinates
# and plot or list the result. 

procedure t_pradprof()

int	images 			# the list of images
real	xinit, yinit		# the initial guess for the profile center
real	pradius			# the plotting radius
real	paz1, paz2		# azimuth limits
bool	center			# center the object before computing profile
int	cboxsize		# the centering box width
bool	list			# list output instead of plot output

int	rboxsize, npts
pointer	sp, imname, im, radius, azimuth, intensity
real	xcntr, ycntr

bool	clgetb()
int	imtopenp(), imtgetim(), clgeti(), rp_radius()
pointer	immap()
real	clgetr()

begin
	# Allocate stack space.
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	# Get the radial profiling parameters. The width of the extraction
	# box mut be odd.
	images = imtopenp ("input")
	xinit = clgetr ("xinit")
	yinit = clgetr ("yinit")
	pradius = clgetr ("radius")
	paz1 = clgetr ("az1")
	paz2 = clgetr ("az2")
	rboxsize = 2 * (nint (pradius + 1.0)) + 1

	# Get the centering parameters. The centering box must be odd.
	center = clgetb ("center")
	if (center) {
	   cboxsize = clgeti ("cboxsize")
	   if (mod (cboxsize, 2) == 0)
	        cboxsize = cboxsize + 1
	}

	# List the radial profile instead of plotting it?
 	list = clgetb ("list")

	# Allocate memory for vectors. 
	call malloc (radius, rboxsize * rboxsize, TY_REAL)
	call malloc (azimuth, rboxsize * rboxsize, TY_REAL)
	call malloc (intensity, rboxsize * rboxsize, TY_REAL)

	# Loop over all images
	while (imtgetim (images, Memc[imname], SZ_FNAME) != EOF) {

	    # Open the image.
	    iferr (im = immap (Memc[imname], READ_ONLY, 0)) {
		call eprintf ("Image %s not found\n")
		call pargstr (Memc[imname])
		next
	    }

	    # Find the star center, if center=yes.
            if (center)
	       call rp_cntr (im, xinit, yinit, cboxsize, xcntr, ycntr)
	    else {
               xcntr = xinit
	       ycntr = yinit
	    }

	    # Get the radius and intensity vectors.
	    npts = rp_radius (im, xcntr, ycntr, rboxsize, pradius, paz1, paz2,
	        Memr[radius], Memr[azimuth], Memr[intensity])

	    # Make list of the radial profile if list=yes or plot if list=no.
   	    if (list) 
		call rp_rlist (Memc[imname], xcntr, ycntr, paz1, paz2,
		    Memr[radius], Memr[azimuth], Memr[intensity], npts)
	    else 
		call rp_rplot (Memc[imname], xcntr, ycntr, paz1, paz2,
		    Memr[radius], Memr[azimuth], Memr[intensity], npts)

	    call imunmap (im)
	}

	call mfree (radius, TY_REAL)
	call mfree (intensity, TY_REAL)
	call imtclose (images)
	call sfree (sp)
end


# RP_CNTR -- Compute the star center using a simple 1D centroiding algorithm
# on the x and y marginals, after thresholding at the mean. 

procedure rp_cntr (im, xstart, ystart, boxsize, xcntr, ycntr)

pointer	im			# pointer to the input image
real	xstart, ystart		# starting coordinates
int	boxsize			# width of the centering box
real	xcntr, ycntr		# centered coordinates

int	half_box, x1, x2, y1, y2
int	ncols, nrows, nx, ny, try
pointer	bufptr, sp, x_vect, y_vect
real	xinit, yinit
pointer	imgs2r()

begin
	# Initialize.
	half_box = (boxsize - 1) / 2
	xinit = xstart
	yinit = ystart
	ncols = IM_LEN (im, 1)
	nrows = IM_LEN (im, 2)

	try = 0
	repeat {

	    # Compute the extraction region.
	    x1 = max (xinit - half_box, 1.0) + 0.5
	    x2 = min (xinit + half_box, real (ncols)) + 0.5
	    y1 = max (yinit - half_box, 1.0) + 0.5
	    y2 = min (yinit + half_box, real (nrows)) + 0.5
	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    # Get the data.
	    bufptr = imgs2r (im, x1, x2, y1, y2)

	    call smark (sp)
	    call salloc (x_vect, nx, TY_REAL)
	    call salloc (y_vect, ny, TY_REAL)

	    # Compute the marginals.
	    call aclrr (Memr[x_vect], nx)
	    call aclrr (Memr[y_vect], ny)
	    call rp_rowsum (Memr[bufptr], Memr[x_vect], nx, ny)
	    call rp_colsum (Memr[bufptr], Memr[y_vect], nx, ny)

	    # Compute the centers.
	    call rp_getcenter (Memr[x_vect], nx, xcntr)
	    call rp_getcenter (Memr[y_vect], ny, ycntr)

	    # Add in offsets to image coordinate system.
	    xcntr = xcntr + x1
	    ycntr = ycntr + y1

	    call sfree (sp)

	    # If the shifts are greater than a pixel to 1 more iteration.
	    try = try + 1
	    if (try == 1) {
		if ((abs (xcntr-xinit) > 1.0) || (abs (ycntr-yinit) > 1.0)) {
		    xinit = xcntr
		    yinit = ycntr
		}
	    } else
		break
	}
end


# RP_RADIUS  -- Get the data and compute the radius and intensity vectors.

int procedure rp_radius (im, xcntr, ycntr, rboxsize, pradius, paz1, paz2,
	radius, azimuth, intensity)

pointer	im			# pointer to the input image
real	xcntr, ycntr		# the center of the extraction box
int	rboxsize		# the width of the extraction box
real	pradius			# the plotting radius
real	paz1, paz2		# the azimuth limits
real	radius[ARB]		# the output radius vector
real	azimuth[ARB]		# the output azimuth vector
real	intensity[ARB]		# the output intensity vector

int	half_box, ncols, nrows, x1, x2, y1, y2, nx, ny, npts
pointer	bufptr	
real	xinit, yinit
int	rp_vectors()
pointer	imgs2r()

begin
	# Initialize.
	half_box = (rboxsize - 1) / 2
	xinit = xcntr
	yinit = ycntr
	ncols = IM_LEN(im,1)
	nrows = IM_LEN(im,2)

	# Get the data.
	x1 = max (xinit - half_box, 1.0) + 0.5
	x2 = min (xinit + half_box, real (ncols)) + 0.5
	y1 = max (yinit - half_box, 1.0) + 0.5
	y2 = min (yinit + half_box, real (nrows)) + 0.5
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	bufptr = imgs2r (im, x1, x2, y1, y2)

	# Compute the radius and intensity vectors.
	npts = rp_vectors (Memr[bufptr], nx, ny, x1, y1, xcntr, ycntr, 
	    pradius, paz1, paz2, radius, azimuth, intensity) 

	return (npts)
end


# RP_RLIST -- Print the intensity as a function of radial distance on the
# standard output.

procedure rp_rlist (imname,  xcntr, ycntr, paz1, paz2, radius, azimuth,
	intensity, npts)

char	imname[ARB]		# the name of the input image
real	xcntr, ycntr		# the center of the radial profile
real	paz1, paz2		# the azimuth limits
real	radius[npts]		# the radius vector
real	azimuth[npts]		# the azimuth vector
real	intensity[npts]		# the intensity vector
int	npts			# the number of points

int	i

begin
	call printf ("# [%s]   xcntr:%7.2f   ycntr:%7.2f\n")
	    call pargstr (imname)
	    call pargr (xcntr)
	    call pargr (ycntr)
	call printf ("# az1:%7.2f   az2:%7.2f\n")
	    call pargr (min(paz1,paz2))
	    call pargr (max(paz1,paz2))

	do i = 1, npts {
	    call printf ("%7.2f   %g\n")
		call pargr (radius[i])
		call pargr (intensity[i])
	}
end


# RP_RPLOT -- Plot intensity as a function of radial distance.

procedure rp_rplot (imname, xcntr, ycntr, az1, az2,
	radius, azimuth, intensity, npts)

char	imname[ARB]
int	npts
real	xcntr, ycntr, az1, az2
real	radius[npts], azimuth[npts], intensity[npts]

char	device[SZ_LINE]
int	mode
pointer	gp	

bool	clgetb()
pointer	gopen()

begin
	call clgstr ("graphics", device, SZ_LINE)

	if (clgetb("append"))
	   mode = APPEND
	else
	   mode = NEW_FILE
 
	gp = gopen (device, mode, STDGRAPH)
	call rp_graph (gp, imname, xcntr, ycntr, az1, az2,
	    mode, radius, intensity, npts) 
	call gclose (gp)
end


# RP_VECTORS -- Compute the radius and intensity vectors.

int procedure rp_vectors (a, nx, ny, x1, y1, xcntr, ycntr, pradius, paz1, paz2,
	radius, azimuth, intensity)
								
real	a[nx,ny]		# the input data array
int	nx, ny			# dimensions of the input array
int	x1, y1			# lower left corner of input array
real	xcntr, ycntr		# coordinates of center pixel
real	pradius			# the plotting radius
real	paz1, paz2		# the azimuth limits
real	radius[ARB]		# the output radius vector
real	azimuth[ARB]		# the output azimuth vector
real	intensity[ARB]		# the output intensity vector

int	i, j, npts
real	dx, dy, az, pr2, az1, az2, r2, dy2

begin
	az1 = DEGTORAD (min (paz1, paz2))
	az2 = DEGTORAD (max (paz1, paz2))
	while (az1 < 0.) {
	    az1 = az1 + TWOPI
	    az2 = az2 + TWOPI
	}
	while (az1 > TWOPI) {
	    az1 = az1 - TWOPI
	    az2 = az2 - TWOPI
	}
	pr2 = pradius * pradius

	npts = 0
	do i = 1, ny {
	    dy = (ycntr - y1 + 1 - i)
	    dy2 = dy ** 2 
	    do j = 1, nx {
		dx = (xcntr - x1 + 1 - j)
		r2 = dx ** 2 + dy2 
		if (r2 > pr2)
		    next
		az = atan2 (dy, dx)
		if (az < 0.)
		    az = az + TWOPI
		if (az < az1 || az > az2)
		    next
		npts = npts + 1
		radius[npts] = sqrt (r2)
		azimuth[npts] = RADTODEG (az)
		intensity[npts] = a[j,i]
	    }
        }

	return (npts)
end


# RP_ROWSUM -- Sum all the rows in a raster.

procedure rp_rowsum (v, row, nx, ny)

real	v[nx,ny]		# the input subraster
real	row[ARB]		# the output summed row
int	nx, ny			# the dimensions of the input subraster

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		row[j] = row[j] + v[j,i]
end


# RP_COLSUM -- Sum all the columns in a raster.

procedure rp_colsum (v, col, nx, ny)

real	v[nx,ny]		# the input subraster
real	col[ARB]		# the output summed column
int	nx, ny			# the dimensions of the input subraster

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		col[j] = col[j] + v[i,j]
end


# RP_GETCENTER -- Compute the centroid of an array.

procedure rp_getcenter (v, nv, vc)

real	v[ARB]			# the input array
int	nv			# length of the input array
real	vc			# the output centroid

int	i
real	sum1, sum2, sigma, cont

begin
	sum1 = 0.0
	sum2 = 0.0

	call aavgr (v, nv, cont, sigma)
	do i = 1, nv
	    if (v[i] > cont) {
	        sum1 = sum1 + (i-1) * (v[i] - cont)
	        sum2 = sum2 + (v[i] - cont)
	    }

	vc = sum1 / sum2
end


define	MTYPES	"|point|box|plus|cross|circle|hebar|vebar|hline|vline|diamond|"
define	RP_GBUF	0.10	
define	RP_SZTITLE	512
define	DEF_IMTITLE	"imtitle"

# RP_GRAPH -- Graph the radial profile.

procedure rp_graph (gp, imname, xcntr, ycntr, az1, az2, mode, x, y, npts)

pointer	gp		# GIO pointer
char	imname[ARB]	# image name
real	xcntr		# starting x coordinate 
real	ycntr		# starting y coordinate 
real	az1, az2	# azimuth limits
int	mode		# Mode
real	x[npts]		# X data
real	y[npts]		# Y data
int	npts		# Number of points

int	i, marks[10], linepattern, patterns[4], clgeti(), btoi(), strdic()
pointer	sp, marker, title, xlabel, ylabel
real	x1, x2, y1, y2, wx1, wx2, wy1, wy2, vx1, vx2, vy1,vy2, temp, 
	szmarker, clgetr()
bool	clgetb(), streq()

data	patterns/GL_SOLID, GL_DASHED, GL_DOTTED, GL_DOTDASH/
data	marks/GM_POINT, GM_BOX, GM_PLUS, GM_CROSS, GM_CIRCLE, GM_HEBAR,
	GM_VEBAR, GM_HLINE, GM_VLINE, GM_DIAMOND/

begin
	call smark (sp)
	call salloc (marker, SZ_LINE, TY_CHAR)

	# If a new graph setup all the axes and labeling options and then
	# make the graph.

	if (mode == NEW_FILE) {

	    call gclear (gp)

	    linepattern = 0

	    x1 = clgetr ("wx1")
	    x2 = clgetr ("wx2")
	    y1 = clgetr ("wy1")
	    y2 = clgetr ("wy2")

	    if (IS_INDEF (x1) || IS_INDEF (x2))
	        call gascale (gp, x, npts, 1)
	    if (IS_INDEF (y1) || IS_INDEF (y2))
	        call gascale (gp, y, npts, 2)

	    call gswind (gp, x1, x2, y1, y2)
	    call ggwind (gp, wx1, wx2, wy1, wy2)

	    temp = wx2 - wx1
	    if (IS_INDEF (x1))
	        wx1 = wx1 - RP_GBUF * temp
	    if (IS_INDEF (x2))
	        wx2 = wx2 + RP_GBUF * temp

	    temp = wy2 - wy1
	    if (IS_INDEF (y1))
	        wy1 = wy1 - RP_GBUF * temp
	    if (IS_INDEF (y2))
	        wy2 = wy2 + RP_GBUF * temp

	    call gswind (gp, wx1, wx2, wy1, wy2)
	    call gsetr (gp, G_ASPECT, 0.)
	    call gseti (gp, G_ROUND, btoi (clgetb ("round")))

	    if (clgetb("fill"))
		call gsetr (gp, G_ASPECT, 0.0)
	    else
		call gsetr (gp, G_ASPECT, 1.0)

	    i = GW_LINEAR
	    if (clgetb ("logx"))
		i = GW_LOG
	    call gseti (gp, G_XTRAN, i)
	    i = GW_LINEAR
	    if (clgetb ("logy"))
		i = GW_LOG
	    call gseti (gp, G_YTRAN, i)

	    # Set the view port
	    vx1 = clgetr ("vx1")
	    vx2 = clgetr ("vx2")
	    vy1 = clgetr ("vy1")
	    vy2 = clgetr ("vy2")
	    call gsview (gp, vx1, vx2, vy1, vy2)

	    if (clgetb ("box")) {

	        # Get number of major and minor tick marks.
	        call gseti (gp, G_XNMAJOR, clgeti ("majrx"))
	        call gseti (gp, G_XNMINOR, clgeti ("minrx"))
	        call gseti (gp, G_YNMAJOR, clgeti ("majry"))
	        call gseti (gp, G_YNMINOR, clgeti ("minry"))

	        # Label tick marks on axes?
	        call gseti (gp, G_LABELTICKS,
		    btoi (clgetb ("ticklabels")))

	        # Fetch labels and plot title string. 
		call salloc (title, RP_SZTITLE, TY_CHAR)
		call salloc (xlabel, SZ_LINE, TY_CHAR)
		call salloc (ylabel, SZ_LINE, TY_CHAR)

		# Build system info string
		call sysid (Memc[title], SZ_LINE)
		call strcat ("\n", Memc[title], RP_SZTITLE)

		# Build the title string
		call clgstr ("title", Memc[marker], SZ_LINE)
		if (streq (Memc[marker], DEF_IMTITLE)) {
		   call sprintf (Memc[marker], SZ_LINE, 
		       "Radial Plot of %s at [%0.2f,%0.2f] az=[%.1f,%.1f]")
		       call pargstr (imname)
		       call pargr (xcntr)
		       call pargr (ycntr)
		       call pargr (az1)
		       call pargr (az2)
		}
		call strcat (Memc[marker], Memc[title], RP_SZTITLE)

		call clgstr ("xlabel", Memc[xlabel], SZ_LINE)
		call clgstr ("ylabel", Memc[ylabel], SZ_LINE)

	        call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	    }
	}

	# Draw the data.
	if (clgetb ("pointmode")) {
	    call clgstr ("marker", Memc[marker], SZ_LINE)
	    i = strdic (Memc[marker], Memc[marker], SZ_LINE, MTYPES)
	    if (i == 0)
		i = 2
	    if (marks[i] == GM_POINT)
		szmarker = 0.0
	    else
		szmarker = clgetr ("szmarker")
	    call gpmark (gp, x, y, npts, marks[i], szmarker, szmarker)
	}
	else {
	    linepattern = min (4, linepattern + 1)
	    call gseti (gp, G_PLTYPE, patterns[linepattern])
	    call gpline (gp, x, y, npts)
	}
	call gflush (gp)

	call sfree (sp)
end
