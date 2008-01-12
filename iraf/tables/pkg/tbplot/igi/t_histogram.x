include	<mach.h>
include	<gset.h>
include	<imhdr.h>
include	<tbset.h>
include	<foc.h>

define THIST_TOL	1	# tolerance for test for last bin

# Default viewport edges.
define  EDGEL  0.1
define  EDGER  0.9
define  EDGEB  0.12
define  EDGET  0.85

# GHISTOGRAM -- Compute and plot or list the histogram of a table column, list
# or image
#
# D. Giaretta STScI, 01-Mar-1988	Original code
# D. Giaretta STScI, 15-Jun-1988	Add fulline and cumulative parameters
# Phil Hodge, 12-Sep-1988		Don't include tbtables.h
# Z. G. Levay, STScI, 6 Feb 1990	Move to stplot package
#  and change the name to hisogram
## 12 June 1992  Permit z1 > z2.  Modified tahgmr() accordingly.  ZGL
## 20 July 1993  Added viewport, append parameters
#--

procedure t_histogram()

char	input[SZ_FNAME]
real	z1, z2, minval, maxval
bool	listout, fulline, cumulative
char	title[SZ_LINE], xlab[SZ_LINE], ylab[SZ_LINE], name[SZ_LINE]
int	npix, nbins, i, coord_getx()
pointer gp, sp, hgm, hgmr, device, val
real	left, right, bottom, top
real	vl, vr, vb, vt
int	mode

bool	clgetb(), fp_equalr(), logplot
int	clgeti()
real	clgetr()
pointer	gopen()

begin
	call smark (sp)
	call salloc (device,  SZ_FNAME, TY_CHAR)

	# Get the image name.
	call clgstr ("input", input, SZ_LINE)
	npix = coord_getx( input, "colname", val, name, SZ_LINE)

	# Get histogram length and allocate buffer.
	nbins = clgeti ("nbins")
	call salloc (hgm,  nbins+2, TY_INT)
	call salloc (hgmr, nbins+2, TY_REAL)
	z1 = clgetr ("z1")
	z2 = clgetr ("z2")

	# Output can be either a list or a plot.
	listout = clgetb ("listout")
	fulline = clgetb ("fulline")
	cumulative = clgetb("cumulative")

	if (!listout) {
	    call clgstr( "title", title, SZ_LINE)
	    # add file name ot title
	    call strcat( name, title, SZ_LINE)
	    call clgstr( "xlabel", xlab, SZ_LINE)
	    call clgstr( "ylabel", ylab, SZ_LINE)
	    call clgstr ("device", Memc[device], SZ_FNAME)

	    if (clgetb ("append"))
		mode = APPEND
	    else
	 	mode = NEW_FILE

	    logplot = clgetb("logplot")

	    left = clgetr ("left")
	    right = clgetr ("right")
	    bottom = clgetr ("bottom")
	    top = clgetr ("top")

	    if (IS_INDEF(left))
		vl = EDGEL
	    else
		vl = left

	    if (IS_INDEF(right))
		vr = EDGER
	    else
		vr = right

	    if (IS_INDEF(bottom))
		vb = EDGEB
	    else
		vb = bottom

	    if (IS_INDEF(top))
		vt = EDGET
	    else
		vt = top
	}

	call alimr( Memr[val], npix, minval, maxval)

	# Get histogram range.
	if (IS_INDEFR (z1)) 
	    z1 = minval
	if (IS_INDEFR (z2)) 
	    z2 = maxval

	if ( IS_INDEFI (nbins) )
	    nbins = max( 1, int(min( real(MAX_INT-1), abs(z2-z1))) +1 ) 

	# Initialize histogram 
	call aclri (Memi[hgm], nbins)

	# accumulate the histogram.

	# Test for constant valued data, which causes zero divide in ahgm.
	if (fp_equalr (z1, z2)) {
	    call eprintf ("Warning: Constant valued input has no data range.\n")
	    call sfree (sp)
	    return
	}

	call tahgmr (Memr[val], npix, Memi[hgm], nbins,  z1,  z2)

	# if cumulative histogram required add bins
	if ( cumulative ) 
	    do i = 2, nbins
		Memi[hgm+i-1] = Memi[hgm+i-2] + Memi[hgm+i-1]

	# List or plot the histogram.
	if (listout) {
	    do i = 1, nbins {
		call printf ("%g %d\n")
		    call pargr (z1+(i-0.5)*(z2-z1)/nbins)
		    call pargi (Memi[hgm+i-1])
	    }
	} else {
	    gp = gopen (Memc[device], mode, STDGRAPH)

	    call gsview (gp, vl, vr, vb, vt)

	    if ( logplot)
		call gseti( gp, G_YTRAN, GW_LOG)

	    for (i=1; i<=nbins; i=i+1)
		Memr[hgmr+i-1] = Memi[hgm+i-1]
	    call draw_histx( gp, nbins, Memr[hgmr], z1, z2, title, xlab, ylab,
					fulline )
	    call gclose (gp)
	}

	# Shutdown.
	call sfree (sp)
end


# COORD_GETX -- given a file name, figure out if it is a text file or
# an image or a table and then read the data array

int procedure coord_getx( input, tcol, val, name, lenname)

char	input[ARB]		# i: name of file with coordinates
char	tcol[ARB]		# i: keyword for col in table
pointer	val			# o: pointer to array
char	name[ARB]		# o: name of file used
int	lenname			# i: max length of name
#--

pointer	tbtopn(), immap(), pt, imgnlr(), buf
pointer	txcoord, tcoord, xnull, colptr
long	npts, i, v[IM_MAXDIM]
int	tbpsta(), nrows, gtextcoord(), line, naxis1
char	col[SZ_LINE]

errchk	tbtopn, tbcfnd, tbcgtr, gtextcoord, immap, imgnlr

begin

	pt   = NULL
	npts = 0

	# is this a TABLE?
	iferr ( pt = tbtopn( input, READ_ONLY, 0) )
	    pt = NULL

	if ( pt != NULL) {
	    # get the column names to read from ordinary table
	    call clgstr( tcol, col, SZ_COLNAME)
	    nrows = tbpsta( pt, TBL_NROWS)
	    call salloc ( txcoord, nrows, TY_REAL)
	    call salloc ( xnull , nrows, TY_BOOL)
	    call tbcfnd( pt, col, colptr, 1)
	    call tbcgtr( pt, colptr, Memr[txcoord], Memb[xnull], 1, nrows)
	    npts = 0
	    call salloc ( val, nrows, TY_REAL)
	    for ( i=0; i<=nrows-1; i=i+1) {
		if ( !Memb[xnull+i] ) {
		    Memr[val+npts] = Memr[txcoord+i]
		    npts = npts + 1
		}
	    }
	    # Append default extension if absent
	    call tbtext (input, name, lenname)
	    call tbtclo( pt)
	    return ( npts)
	}


	# is this an image?
	iferr ( pt = immap( input, READ_ONLY, 0 ) )
	    pt = NULL
	if ( pt != NULL) {
	    naxis1 = IM_LEN( pt, 1)
	    npts = 1
	    do i = 1, IM_NDIM(pt)
	        npts = npts*IM_LEN(pt, i)

	    call salloc( val, npts, TY_REAL )
	    call amovkl( long(1), v, IM_MAXDIM)
	    line = 0
	    while( imgnlr( pt, buf, v) != EOF) {
		call amovr( Memr[buf], Memr[val+line*naxis1], naxis1)
		line = line + 1
	    }
	    call strcpy( IM_TITLE(pt), name, lenname)
	    call imunmap( pt)
	    return ( npts)
	}


	# otherwise  TEXT - see if we can access it 
	if ( pt == NULL ) {
	    # read from text file if we can
	    iferr( nrows = gtextcoord( input, tcoord) ) 
		call error ( 0, "input file is neither table, image nor text")
	    	
	    call salloc ( val, nrows, TY_REAL )
	    for (i = 0 ; i <= nrows - 1 ; i=i+1){
		if ( !IS_INDEF( Memr[tcoord+i] ) ) {
		    Memr[val + npts ] = Memr[tcoord+i] 
		    npts = npts + 1
		}
	    }
	    call mfree( tcoord, TY_REAL )
	    call strcpy( input, name, lenname)
	}

	return (npts)		
end
# GTEXTCOORD -- read text file with data 
#               pointer must be mfree'd in calling routine

int procedure gtextcoord( input, data )

char	input[SZ_FNAME]	# i: input name
pointer	data		# o: pointers to real array
#--

pointer	open()
pointer	fdcoord			# text file pointer
char	line[SZ_LINE]
long	i_in_line, ip
real	dval
int	ctor()
long	init_size
int	getline()

errchk	getline, ctor, open

begin

	init_size = INIT_COORD_SIZE

	# read in the text data into buffer - guess initial size, and
	# expand buffer if required later
	call malloc( data, init_size, TY_REAL)

	i_in_line = 1
	ip = 0
	fdcoord = open(input, READ_ONLY, TEXT_FILE)
	while ( getline(fdcoord, line) != EOF ) {
	    i_in_line = 1
	    while ( ctor(line, i_in_line, dval) >= 1 && line[1] != '#' ) {
		if (ip > init_size) {
		    call realloc( data, init_size + INC_COORD_SIZE, TY_REAL)
		    init_size = init_size + INC_COORD_SIZE
		}
		Memr[data + ip ] = dval
		ip = ip + 1
	    }
	}
	call close(fdcoord)

	return ( ip )
end
# XR_HISTCRV -- plot data as histogram

procedure draw_histx( gp, nbins, hist, z1, z2, title, xlab, ylab, fulline)

pointer	gp		# i: Graphics descriptor
int	nbins		# i: no. of bins
real	hist[ARB]	# i: histogram data
real	z1, z2		# i: data limits
char	title[ARB]
char	xlab[ARB]
char	ylab[ARB]
bool	fulline
#--

real	min, max

errchk	gswind, glabax, xhstcrv

begin

	call alimr( hist, nbins, min, max)
	call gswind( gp, z1, z2, 0.0, max)
	call glabax  (gp, title, xlab, ylab)
	call xhstcrv  (gp, hist, nbins, z1, z2, GF_HOLLOW, fulline)

end

# XHSTCRV -- Draws a histogram style curve (bar graph).

procedure xhstcrv (gp, v, npts, xmin, xmax, style, fulline)

pointer	gp		# i: Graphics descriptor
real	v[ARB]		# i: Vector to be plotted (Y values)
int	npts		# i: Number of data values
real	xmin, xmax	# i: Range of vector in X
int	style		# i: Fill area style
bool	fulline
#--

int	bin
real	x
real	xbin[4], ybin[4]		# Fill polyline
real	dx, bw
real	left, right, bottom, top	# Data window

errchk	ggwind, gfill

begin
	dx = (xmax - xmin) / real(npts)

	call ggwind (gp, left, right, bottom, top)

	# Bin width is in world coordinates
	bw = -dx/2.0
	if ( fulline ) {
	    do bin = 1, npts {
	        if (!IS_INDEF(v[bin])) {
		    x = xmin + dx*real (bin-0.5)
		    xbin[1] = x - bw
		    ybin[1] = bottom
		    xbin[2] = xbin[1]
		    ybin[2] = v[bin]
		    xbin[3] = x + bw
		    ybin[3] = v[bin]
		    xbin[4] = xbin[3]
		    ybin[4] = bottom
		    call gfill (gp, xbin, ybin, 4, style)
		}
	    }
	} else {
	    dx = ( xmax-xmin)/npts
	    call gamove( gp, xmin, bottom)
	    do bin = 1, npts {
		call gadraw( gp, xmin+dx*real(bin-1), v[bin] )
		call gadraw( gp, xmin+dx*real(bin  ), v[bin] )
	    }
	    call gadraw( gp, xmax, bottom)
	}
end
# TAHGMR -- Accumulate the histogram of the input vector. 
# Note that z1, z2 are taken as the extreme edges of the first and last bins,
# NOT the mid-position of those bins.
# The output HGM should be cleared prior to the first call

procedure tahgmr (data, npix, hgm, nbins,  z1,  z2)

real	data[ARB]
int	npix
int	hgm[ARB]
int	nbins
real	z1, z2
#--

real	dz, z2upp, z1upp
int	bin, i
bool	flip

begin

	if ( nbins < 1 )
	    return
	
	dz	= real( nbins)/(z2-z1)
	flip	= dz < 0.0

	z2upp	= z2 + THIST_TOL*EPSILONR
	z1upp	= z1 + THIST_TOL*EPSILONR

	do i = 1, npix {
	    bin = min( int( (data[i] - z1) * dz ) + 1, nbins )

	    if (flip) {
		if ( data[i] < z2 ||  data[i] > z1upp )
		    next

	    } else {
		if ( data[i] < z1 ||  data[i] > z2upp )
		    next
	    }

	    hgm[bin] = hgm[bin] + 1
	}
end
