include	<error.h>
include	<imhdr.h>
include	<pmset.h>
include	"ace.h"
include	"cat.h"
include	"objs.h"
include	"evaluate.h"


# EVALUATE -- Evaluate object parameters.

procedure evaluate (evl, cat, im, om, skymap, sigmap, gainmap, expmap, logfd)

pointer	evl			#I Parameters
pointer	cat			#I Catalog structure
pointer	im			#I Image pointer
pointer	om			#I Object mask pointer
pointer	skymap			#I Sky map
pointer	sigmap			#I Sigma map
pointer	gainmap			#I Gain map
pointer	expmap			#I Exposure map
int	logfd			#I Logfile

int	i, n, c, l, nc, nl, c1, c2, nummax, num, nobjsap
real	x, x2, y, y2, s, s2, f, f2, val, sky, ssig, s2x, s2y
pointer	objs, obj, rlptr
pointer	data, skydata, ssigdata, gaindata, expdata, sigdata
pointer	sp, v, rl, sum_s2x, sum_s2y

int	andi(), ori(), ctor()
real	imgetr()
bool	pm_linenotempty()
errchk	salloc, calloc, malloc, evgdata

begin
	call smark (sp)
	call salloc (v, PM_MAXDIM, TY_LONG)
	call salloc (rl, 3+3*IM_LEN(im,1), TY_INT)

	if (logfd != NULL)
	    call fprintf (logfd, "  Evaluate objects:\n")

	objs = CAT_OBJS(cat)
	nummax = CAT_NUMMAX(cat)

	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)

	# Allocate work arrays.
	call salloc (sigdata, nc, TY_REAL)
	call salloc (sum_s2x, nummax, TY_REAL)
	call salloc (sum_s2y, nummax, TY_REAL)
	call aclrr (Memr[sum_s2x], nummax)
	call aclrr (Memr[sum_s2y], nummax)

	# Initialize isophotal quantities.
	do i = NUMSTART-1, nummax-1 {
	    obj = Memi[objs+i]
	    if (obj == NULL)
		next
	    OBJ_NPIX(obj) = 0
	    OBJ_SKY(obj) = 0.
	    OBJ_PEAK(obj) = 0.
	    OBJ_FLUX(obj) = 0.
	    OBJ_X1(obj) = 0.
	    OBJ_Y1(obj) = 0.
	    OBJ_X2(obj) = 0.
	    OBJ_Y2(obj) = 0.
	    OBJ_XY(obj) = 0.
	    OBJ_SIG(obj) = 0.
	    OBJ_ISIGAVG(obj) = 0.
	    OBJ_ISIGAVG2(obj) = INDEFR
	    OBJ_FLUXVAR(obj) = 0.
	    OBJ_XVAR(obj) = 0.
	    OBJ_YVAR(obj) = 0.
	    OBJ_XYCOV(obj) = 0.
	}

	# Initialize aperture photometry.
	call evapinit (cat, nobjsap)

	# Get magnitude zero.
	if (EVL_MAGZERO(evl,1) == '!') {
	    iferr (CAT_MAGZERO(cat) = imgetr (im, EVL_MAGZERO(evl,2))) {
		call erract (EA_WARN)
		CAT_MAGZERO(cat) = INDEFR
	    }
	} else {
	    i = 1
	    if (ctor (EVL_MAGZERO(evl,1), i, CAT_MAGZERO(cat)) == 0)
		CAT_MAGZERO(cat) = INDEFR
	}
	call catputr (cat, "magzero", CAT_MAGZERO(cat)) 

	# Go through the lines of the image accumulating the image data
	# into the parameters.  The data is read the first time it is
	# required.
	Memi[v] = 1
	do l = 1, nl {
	    Memi[v+1] = l
	    data = NULL

	    # Do circular aperture photometry.  Check nobjsap to avoid
	    # subroutine call.
	    if (nobjsap > 0)
		call evapeval (l, im, skymap, sigmap, gainmap, expmap,
		    data, skydata, ssigdata, gaindata, expdata, sigdata)

	    # Accumulate object region quantities if there are object
	    # regions in the current line.
	    if (!pm_linenotempty (om, Memi[v]))
		next
	    call pmglri (om, Memi[v], Memi[rl], 0, nc, 0)

	    # Go through each object region.
	    rlptr = rl
	    do i = 2, Memi[rl] {
		rlptr = rlptr + 3
		c1 = Memi[rlptr]
		c2 = c1 + Memi[rlptr+1] - 1
		num = MNUM(Memi[rlptr+2])

		# Do all unevaluated objects and their parents.
		while (num >= NUMSTART) {
		    if (data == NULL)
			call evgdata (l, im, skymap, sigmap, gainmap, expmap,
			    data, skydata, ssigdata, gaindata, expdata, sigdata)

		    obj = Memi[objs+num-1]
		    if (obj == NULL)
			break

		    if (OBJ_NPIX(obj) == 0) {
			val = Memr[data+c1-1]
			sky = Memr[skydata+c1-1]
			ssig = Memr[ssigdata+c1-1]

			OBJ_XMIN(obj) = c1
			OBJ_XMAX(obj) = c1
			OBJ_YMIN(obj) = l
			OBJ_YMAX(obj) = l
			OBJ_ISIGMAX(obj) =  (val - sky) / ssig
		    }

		    s2x = Memr[sum_s2x+num-1]
		    s2y = Memr[sum_s2y+num-1]
		    do c = c1, c2 {
			val = Memr[data+c-1]
			sky = Memr[skydata+c-1]
			ssig = Memr[ssigdata+c-1]
			s = Memr[sigdata+c-1]

			x = c - OBJ_XMIN(obj)
			y = l - OBJ_YMIN(obj)
			x2 = x * x
			y2 = y * y
			s2 = s * s

			OBJ_NPIX(obj) = OBJ_NPIX(obj) + 1
			OBJ_SKY(obj) = OBJ_SKY(obj) + sky
			OBJ_SIG(obj) = OBJ_SIG(obj) + ssig
			val = val - sky
			if (val > OBJ_PEAK(obj))
			    OBJ_PEAK(obj) = val
			OBJ_FLUX(obj) = OBJ_FLUX(obj) + val
			OBJ_FLUXVAR(obj) = OBJ_FLUXVAR(obj) + s2

			OBJ_XMIN(obj) = min (OBJ_XMIN(obj), c)
			OBJ_XMAX(obj) = max (OBJ_XMAX(obj), c)
			OBJ_X1(obj) = OBJ_X1(obj) + x * val
			OBJ_X2(obj) = OBJ_X2(obj) + x2 * val
			OBJ_XVAR(obj) = OBJ_XVAR(obj) + x2 * s2
			s2x = s2x + x * s2

			OBJ_YMIN(obj) = min (OBJ_YMIN(obj), l)
			OBJ_YMAX(obj) = max (OBJ_YMAX(obj), l)
			OBJ_Y1(obj) = OBJ_Y1(obj) + y * val
			OBJ_Y2(obj) = OBJ_Y2(obj) + y2 * val
			OBJ_YVAR(obj) = OBJ_YVAR(obj) + y2 * s2
			s2y = s2y + y * s2

			OBJ_XY(obj) = OBJ_XY(obj) + x * y * val
			OBJ_XYCOV(obj) = OBJ_XYCOV(obj) + x * y * s2

			val = val / ssig
			OBJ_ISIGAVG(obj) = OBJ_ISIGAVG(obj) + val
			OBJ_ISIGMAX(obj) = max (OBJ_ISIGMAX(obj), val)

		    }
		    Memr[sum_s2x+num-1] = s2x
		    Memr[sum_s2y+num-1] = s2y

		    num = OBJ_PNUM(obj)
		}
	    }
	}

	# Finish up the evaluations.
	do i = NUMSTART-1, nummax-1 {
	    obj = Memi[objs+i]
	    if (obj == NULL)
		next
	    n = OBJ_NPIX(obj)
	    if (n > 0) {
		OBJ_SKY(obj) = OBJ_SKY(obj) / n
		f = OBJ_FLUX(obj)
		if (f > 0.) {
		    f2 = f * f
		    x = OBJ_X1(obj) / f
		    s2x = Memr[sum_s2x+i]
		    s2y = Memr[sum_s2y+i]

		    OBJ_X1(obj) = x + OBJ_XMIN(obj)
		    OBJ_X2(obj) = OBJ_X2(obj) / f - x * x
		    OBJ_XVAR(obj) = (OBJ_XVAR(obj) - 2 * x * s2x + 
			x * x * OBJ_FLUXVAR(obj)) / f2

		    y = OBJ_Y1(obj) / f
		    OBJ_Y1(obj) = y + OBJ_YMIN(obj)
		    OBJ_Y2(obj) = OBJ_Y2(obj) / f - y * y
		    OBJ_YVAR(obj) = (OBJ_YVAR(obj) - 2 * y * s2y + 
			y * y * OBJ_FLUXVAR(obj)) / f2

		    OBJ_XY(obj) = OBJ_XY(obj) / f - x * y
		    OBJ_XYCOV(obj) = (OBJ_XYCOV(obj) - x * s2x -
			y * s2y + x * y * OBJ_FLUXVAR(obj)) / f2

		    if (IS_INDEFR(OBJ_XAP(obj)))
			OBJ_XAP(obj) = OBJ_X1(obj)
		    if (IS_INDEFR(OBJ_YAP(obj)))
			OBJ_YAP(obj) = OBJ_Y1(obj)
		} else {
		    OBJ_X1(obj) = INDEFR
		    OBJ_Y1(obj) = INDEFR
		    OBJ_X2(obj) = INDEFR
		    OBJ_Y2(obj) = INDEFR
		    OBJ_XY(obj) = INDEFR
		    OBJ_XVAR(obj) = INDEFR
		    OBJ_YVAR(obj) = INDEFR
		    OBJ_XYCOV(obj) = INDEFR
		    OBJ_FLUXVAR(obj) = INDEFR
		}
		if (OBJ_PEAK(obj) == 0.)
		    OBJ_PEAK(obj) = INDEFR
		OBJ_SIG(obj) = OBJ_SIG(obj) / n
		OBJ_ISIGAVG(obj) = OBJ_ISIGAVG(obj) / sqrt(real(n))
	    }
	    SETFLAG (obj, OBJ_EVAL)
	}

	# Do aperture photometry if we had to wait for the aperture centers
	# to be defined.
	if (nobjsap == 0) {
	    call evapinit (cat, nobjsap)
	    if (nobjsap > 0) {
		Memi[v] = 1
		do l = 1, nl {
		    Memi[v+1] = l
		    data = NULL
		    call evapeval (l, im, skymap, sigmap, gainmap, expmap,
			data, skydata, ssigdata, gaindata, expdata, sigdata)
		}
	    }
	}
	call evapfree ()

	# Set apportioned fluxes.
	call evapportion (cat, Memr[sum_s2x])

	# Set WCS coordinates.
	call evalwcs (cat, im)

	call sfree (sp)
end


# EVAPINIT -- Initialize aperture photometry.  nobjsap will signal whether
# there are any objects to evaluate.

procedure evapinit (cat, nobjsap)

pointer	cat			#I Catalog
int	nobjsap			#O Number of objects for aperture evaluation

int	i, nummax
pointer	tbl, stp, sym, apflux, obj, sthead(), stnext()

int	ycompare()
extern	ycompare
errchk	calloc, malloc

int	nobjs			# Number of objects to evaluate
int	naps			# Number of apertures per object
real	rmax			# Maximum aperture radius
pointer	r2aps			# Array of aperture radii squared (ptr)
pointer	ysort			# Array of Y sorted object number indices (ptr)
int	ystart			# Index of first object to consider
pointer	objs			# Array of object structure (ptr)
common	/evapcom/ nobjs, naps, rmax, r2aps, ysort, ystart, objs

begin
	nobjsap = 0
	nobjs = 0
	naps = 0
	r2aps = NULL
	ysort = NULL

	tbl = CAT_OUTTBL(cat)
	if (tbl == NULL)
	    return
	stp = TBL_STP(tbl)

	# Determine number of apertures.
	naps = 0
	for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
	    if (ENTRY_ID(sym) != ID_APFLUX)
		next
	}
	if (naps == 0)
	    return

	objs = CAT_OBJS(cat)
	nummax = CAT_NUMMAX(cat)

	# Allocate memory.
	call calloc (CAT_APFLUX(cat), nummax*naps, TY_REAL)
	call malloc (r2aps, naps, TY_REAL)
	call malloc (ysort, nummax, TY_INT)

	# Get the maximum radius since that will define the line
	# limits needed for each object.  Compute array of radius squared
	# for the apertures.  Pixels are checked for being in the aperture
	# in r^2 to avoid square roots.
	rmax = 0.
	naps = 0
	for (sym=sthead(stp); sym!=NULL; sym=stnext(stp,sym)) {
	    if (ENTRY_ID(sym) != ID_APFLUX)
		next
	    rmax = max (ENTRY_RAP(sym), rmax)
	    Memr[r2aps+naps] = ENTRY_RAP(sym) ** 2
	    naps = naps + 1
	}

	# Allocate regions of the apflux array to objects with
	# defined aperture centers.  For the objects create a sorted
	# index array by YAP so that we can quickly find objects
	# which include a particular line in their apertures.

	apflux = CAT_APFLUX(cat)
	do i = NUMSTART-1, nummax-1 {
	    obj = Memi[objs+i]
	    if (obj == NULL)
		next
	    if (IS_INDEFR(OBJ_XAP(obj)) || IS_INDEFR(OBJ_YAP(obj)))
		next
	    OBJ_APFLUX(obj) = apflux
	    apflux = apflux + naps
	    Memi[ysort+nobjsap] = i
	    nobjsap = nobjsap + 1
	}

	if (nobjsap > 1)
	    call gqsort (Memi[ysort], nobjsap, ycompare, objs)

	if (nobjsap == 0) {
	    call mfree (CAT_APFLUX(cat), TY_REAL)
	    call evapfree ()
	}
end


# EVAPFREE -- Free aperture photometry memory.

procedure evapfree ()

int	nobjs			# Number of objects to evaluate
int	naps			# Number of apertures per object
real	rmax			# Maximum aperture radius
pointer	r2aps			# Array of aperture radii squared (ptr)
pointer	ysort			# Array of Y sorted object number indices (ptr)
int	ystart			# Index of first object to consider
pointer	objs			# Array of object structure (ptr)
common	/evapcom/ nobjs, naps, rmax, r2aps, ysort, ystart, objs

begin
	call mfree (r2aps, TY_REAL)
	call mfree (ysort, TY_INT)
end


# EVAPEVAL -- Do circular aperture photometry.  Maintain i1 as the
# first entry in the sorted index array to be considered.  All
# earlier entries will have all aperture lines less than the
# current line.  Break on the first object whose minimum aperture
# line is greater than the current line.

procedure evapeval (l, im, skymap, sigmap, gainmap, expmap, data, skydata,
	ssigdata, gaindata, expdata, sigdata)

int	l			#I Line
pointer	im			#I Image
pointer	skymap			#I Sky map
pointer	sigmap			#I Sigma map
pointer	gainmap			#I Gain map
pointer	expmap			#I Exposure map
pointer	data			#O Image data
pointer	skydata			#O Sky data
pointer	ssigdata		#O Sky sigma data
pointer	gaindata		#O Gain data
pointer	expdata			#O Exposure data
pointer	sigdata			#O Total sigma data

int	i, j, nc, c
real	x, y, l2, r2, val, sky
pointer	obj, apflux

int	nobjs			# Number of objects to evaluate
int	naps			# Number of apertures per object
real	rmax			# Maximum aperture radius
pointer	r2aps			# Array of aperture radii squared (ptr)
pointer	ysort			# Array of Y sorted object number indices (ptr)
int	ystart			# Index of first object to consider
pointer	objs			# Array of object structure (ptr)
common	/evapcom/ nobjs, naps, rmax, r2aps, ysort, ystart, objs

begin
	nc = IM_LEN(im,1)
	do i = ystart, nobjs {
	    obj = Memi[objs+Memi[ysort+i-1]]
	    y = OBJ_YAP(obj)
	    if (y - rmax > l)
		break
	    if (y + rmax < l) {
		ystart = ystart + 1
		next
	    }
	    x = OBJ_XAP(obj)
	    apflux = OBJ_APFLUX(obj)
	    if (data == NULL)
		call evgdata (l, im, skymap, sigmap, gainmap, expmap,
		    data, skydata, ssigdata, gaindata, expdata, sigdata)

	    # Accumulate data within in the apertures using the r^2
	    # values.  Currently partial pixels are not considered and
	    # errors are not evaluated.
	    # Note that bad pixels or object overlaps are not excluded
	    # in the apertures.
	    l2 = (l - y) ** 2
	    do c = max (0, int(x-rmax)), min (nc, int(x+rmax+1)) {
		r2 = (c - x) ** 2 + l2
		do j = 0, naps-1 {
		    if (r2 < Memr[r2aps+j]) {
			val = Memr[data+c-1]
			sky = Memr[skydata+c-1]
			Memr[apflux+j] = Memr[apflux+j] + (val - sky)
		    }
		}
	    }
	}
end


# EVAPPORTION -- Compute apportioned fluxes after the object isophotoal
# fluxes have been computed.

procedure evapportion (cat, sum_flux)

pointer	cat			#I Catalog
real	sum_flux[ARB]		#I Work array of size NUMMAX

int	nummax, num, pnum, nindef
pointer	objs, obj, pobj

begin
	objs = CAT_OBJS(cat)
	nummax = CAT_NUMMAX(cat)

	call aclrr (sum_flux, nummax)
	do num = NUMSTART, nummax {
	    obj = Memi[objs+num-1]
	    if (obj == NULL)
		next
	    pnum = OBJ_PNUM(obj)
	    if (pnum == 0) {
		OBJ_FRAC(obj) = 1.
		OBJ_FRACFLUX(obj) = OBJ_FLUX(obj)
		next
	    }

	    sum_flux[pnum] = sum_flux[pnum] + max (0., OBJ_FLUX(obj))
	    OBJ_FRACFLUX(obj) = INDEFR
	}

	nindef = 0
	do num = NUMSTART, nummax {
	    obj = Memi[objs+num-1]
	    if (obj == NULL)
		next
	    pnum = OBJ_PNUM(obj)
	    if (pnum == 0)
		next
	    pobj = Memi[objs+pnum-1]

	    if (sum_flux[pnum] > 0.) {
		OBJ_FRAC(obj) = max (0., OBJ_FLUX(obj)) / sum_flux[pnum]
		if (IS_INDEFR(OBJ_FRACFLUX(pobj)))
		    nindef = nindef + 1
		else
		    OBJ_FRACFLUX(obj) = OBJ_FRACFLUX(pobj) * OBJ_FRAC(obj)
	    } else {
		OBJ_FRAC(obj) = INDEFR
		OBJ_FRACFLUX(obj) = OBJ_FLUX(obj)
	    }
	}

	while (nindef > 0) {
	    nindef = 0
	    do num = NUMSTART, nummax {
		obj = Memi[objs+num-1]
		if (obj == NULL)
		    next
		pnum = OBJ_PNUM(obj)
		if (pnum == 0)
		    next

		pobj = Memi[objs+pnum-1]
		if (IS_INDEFR(OBJ_FRACFLUX(pobj)))
		    nindef = nindef + 1
		else {
		    if (IS_INDEFR(OBJ_FRAC(obj)))
			OBJ_FRACFLUX(obj) = OBJ_FLUX(obj)
		    else
			OBJ_FRACFLUX(obj) = OBJ_FRACFLUX(pobj) * OBJ_FRAC(obj)
		}
	    }
	}
end


# EVALWCS -- Set WCS coordinates.

procedure evalwcs (cat, im)

pointer	cat			#I Catalog structure
pointer	im			#I IMIO pointer

int	i
pointer	mw, ct, objs, obj, mw_openim(), mw_sctran()
errchk	mw_openim

begin
	mw = mw_openim (im)
	ct = mw_sctran (mw, "logical", "world", 03B)

	objs = CAT_OBJS(cat)
	do i = NUMSTART-1, CAT_NUMMAX(cat)-1 {
	    obj = Memi[objs+i]
	    if (obj == NULL)
		next
	    if (IS_INDEFR(OBJ_XAP(obj)) || IS_INDEFR(OBJ_YAP(obj))) {
		OBJ_WX(obj) = INDEFD
		OBJ_WY(obj) = INDEFD
	    } else
		call mw_c2trand (ct, double(OBJ_XAP(obj)),
		    double(OBJ_YAP(obj)), OBJ_WX(obj), OBJ_WY(obj))
	}

	call mw_ctfree (ct)
	call mw_close (mw)
end


# YCOMPARE -- Compare Y values of two objects for sorting.

int procedure ycompare (objs, i1, i2)

pointer	objs			#I Pointer to array of objects
int	i1			#I Index of first object to compare
int	i2			#I Index of second object to compare

real	y1, y2

begin
	y1 = OBJ_YAP(Memi[objs+i1])
	y2 = OBJ_YAP(Memi[objs+i2])
	if (y1 < y2)
	    return (-1)
	else if (y1 > y2)
	    return (1)
	else
	    return (0)
end


# EVGDATA -- Get evaluation data for an image line.

procedure evgdata (l, im, skymap, sigmap, gainmap, expmap, data, skydata,
	ssigdata, gaindata, expdata, sigdata)

int	l			#I Line
pointer	im			#I Image
pointer	skymap			#I Sky map
pointer	sigmap			#I Sigma map
pointer	gainmap			#I Gain map
pointer	expmap			#I Exposure map
pointer	data			#O Image data
pointer	skydata			#O Sky data
pointer	ssigdata		#O Sky sigma data
pointer	gaindata		#O Gain data
pointer	expdata			#O Exposure data
pointer	sigdata			#O Total sigma data

int	nc
pointer	imgl2r(), map_glr()
errchk	imgl2r, map_glr, noisemodel

begin
	nc = IM_LEN(im,1)
	data = imgl2r (im, l)
	skydata = map_glr (skymap, l, READ_ONLY)
	ssigdata = map_glr (sigmap, l, READ_ONLY)
	if (gainmap == NULL && expmap == NULL)
	    sigdata = ssigdata
	else if (expmap == NULL) {
	    gaindata = map_glr (gainmap, l, READ_ONLY)
	    call noisemodel (Memr[data], Memr[skydata],
		Memr[ssigdata], Memr[gaindata], INDEFR,
		Memr[sigdata], nc)
	} else if (gainmap == NULL) {
	    expdata = map_glr (expmap, l, READ_WRITE)
	    call noisemodel (Memr[data], Memr[skydata],
		Memr[ssigdata], INDEFR, Memr[expdata],
		Memr[sigdata], nc)
	} else {
	    gaindata = map_glr (gainmap, l, READ_ONLY)
	    expdata = map_glr (expmap, l, READ_WRITE)
	    call noisemodel (Memr[data], Memr[skydata],
		Memr[ssigdata], Memr[gaindata],
		Memr[expdata], Memr[sigdata], nc)
	}
end
