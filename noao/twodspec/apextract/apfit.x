include	<imhdr.h>
include	<imset.h>
include	<pkg/gtools.h>
include	"apertures.h"


# AP_FITSPEC -- Fit a spectrum by a smoothing function.

procedure ap_fitspec (ap, in, spec, ny)

pointer	ap			# Aperture (used for labels)
pointer	in			# Input image (used for labels)
real	spec[ny]		# spectrum
int	ny			# Number of points in spectra

int	i, fd, apaxis, clgeti()
real	clgetr()
pointer	sp, str, x, wts, cv, gp, gt, ic, ic1, gt_init()
bool	ap_answer()
data	ic1 /NULL/
errchk	icg_fit, ic_fit

common	/apn_com/ ic, gt

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, ny, TY_REAL)
	call salloc (wts, ny, TY_REAL)

	do i = 1, ny {
	    Memr[x+i-1] = i
	    Memr[wts+i-1] = 1
	}

	if (ic == NULL || ic1 == NULL) {
	    call ic_open (ic)
	    ic1 = ic
	    call clgstr ("function", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "function", Memc[str])
	    call ic_puti (ic, "order", clgeti ("order"))
	    call clgstr ("sample", Memc[str], SZ_LINE)
	    call ic_pstr (ic, "sample", Memc[str])
	    call ic_puti (ic, "naverage", clgeti ("naverage"))
	    call ic_puti (ic, "niterate", clgeti ("niterate"))
	    call ic_putr (ic, "low", clgetr ("low_reject"))
	    call ic_putr (ic, "high", clgetr ("high_reject"))
	    call ic_putr (ic, "grow", clgetr ("grow"))
	    call ic_pstr (ic, "ylabel", "")

	    gt = gt_init()
	}

	call ic_putr (ic, "xmin", 1.)
	call ic_putr (ic, "xmax", real (ny))
	apaxis = AP_AXIS(ap)
	switch (apaxis) {
	case 1:
	    call ic_pstr (ic, "xlabel", "Line")
	case 2:
	    call ic_pstr (ic, "xlabel", "Column")
	}
	call gt_sets (gt, GTTYPE, "line")

	# Fit spectrum by a smoothing function.
	call sprintf (Memc[str], SZ_LINE,
	    "%s: %s - Aperture %s")
	    call pargstr (IM_HDRFILE(in))
	    call pargstr (IM_TITLE(in))
	    call pargi (AP_ID(ap))
	call gt_sets (gt, GTTITLE, Memc[str])

	# Query the user to fit the spectrum interactively.
	call sprintf (Memc[str], SZ_LINE,
	    "Fit spectrum for aperture %d for %s interactively?")
	    call pargi (AP_ID(ap))
	    call pargstr (IM_HDRFILE(in))
	if (ap_answer ("ansfitspec1", Memc[str])) {
	    call ap_gopen (gp)
	    call icg_fit (ic, gp, "gcur", gt, cv, Memr[x], spec,
		Memr[wts], ny)
	    call amovkr (1., Memr[wts], ny)
	} else
	    call ic_fit (ic, cv, Memr[x], spec, Memr[wts], ny,
		YES, YES, YES, YES)

	# Make a graph to the plot log.
	call ap_popen (gp, fd, "fitspec")
	if (gp != NULL) {
	    call icg_graphr (ic, gp, gt, cv, Memr[x], spec, Memr[wts], ny)
	    call ap_pclose (gp, fd)
	}

	call cvvector (cv, Memr[x], spec, ny)
	call cvfree (cv)
end


procedure ap_fitfree ()

pointer	ic, gt
common	/apn_com/ ic, gt

begin
	call ic_closer (ic)
	call gt_free (gt)
end


# AP_LNORM -- Normalize the input line apertures by the norm spectra.

procedure ap_lnorm (ap, out, gain, dbuf, nc, nl, c1, l1, spec, ny, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
real	gain			# Gain
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Normalization spectrum
int	ny			# Size of profile array
int	ys			# Start of spectrum in image
int	init			# Fill between apertures with 1?

bool	clgetb()		# Center normalize?
real	threshold, clgetr()	# Division threshold

int	i, ncols, nlines, ix1, ix2, iy, nsum
real	cen, low, high, s, x1, x2, sum, ap_cveval(), asumr()
pointer	cv, datain, dataout, imps2r(), impl2r()

begin
	threshold = clgetr ("threshold")

	cen = AP_CEN(ap,1)
	low = AP_CEN(ap,1) + AP_LOW(ap,1)
	high = AP_CEN(ap,1) + AP_HIGH(ap,1)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	# Normalize by the aperture width and apply threshold.
	call adivkr (spec, high - low, spec, ny)
	if (clgetb ("cennorm")) {
	    sum = 0.
	    nsum = 0
	    do i = 1, nlines {
		iy = i - ys + 1
		if (iy < 1 || iy > ny)
		    next
		s = cen + ap_cveval (cv, real (i))
		ix1 = max (1, int (s))
		ix2 = min (ncols, int (s + 1))
		if (ix1 > ix2)
		    next
		datain = dbuf + (i - l1) * nc + ix1 - c1
		if (ix1 == ix2)
		    sum = sum + Memr[datain]
		else
		    sum = sum + (ix2-s)*Memr[datain] + (s-ix1)*Memr[datain+1]
		nsum = nsum + 1
	    }
	    if (nsum > 0) {
	        sum = (asumr (spec, ny) / ny) / (sum / nsum / gain)
		call adivkr (spec, sum, spec, ny)
	    }
	}
	if (!IS_INDEF (threshold))
	    call arltr (spec, ny, threshold, threshold)

	do i = 1, nlines {
	    if (init == YES) {
	        dataout = impl2r (out, i)
	        call amovkr (1., Memr[dataout], ncols)
	    }

	    iy = i - ys + 1
	    if (iy < 1 || iy > ny)
		next
	    s = ap_cveval (cv, real (i))
	    x1 = max (0.5, low + s) 
	    x2 = min (ncols + 0.49, high + s) 
	    if (x1 > x2)
		next

	    ix1 = nint (x1)
	    ix2 = nint (x2)

	    datain = dbuf + (i - l1) * nc + ix1 - c1
	    if (init == YES)
		dataout = dataout + ix1 - 1
	    else
	        dataout = imps2r (out, ix1, ix2, i, i)
	    call adivkr (Memr[datain], spec[iy] * gain, Memr[dataout],
		ix2-ix1+1)
	}

	call imaddr (out, "CCDMEAN", 1.)
end


# AP_CNORM -- Normalize the input column apertures by the norm spectra.

procedure ap_cnorm (ap, out, gain, dbuf, nc, nl, c1, l1, spec, ny, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
real	gain			# Gain
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Normalization spectrum
int	ny			# Size of profile array
int	ys			# Start of spectrum in image
int	init			# Fill between apertures with 1?

bool	clgetb()		# Center normalize?
real	threshold, clgetr()	# Division threshold

int	ncols, nlines, ix, iy, ix1, ix2, iy1, iy2, nsum
real	cen, low, high, s, sum, ap_cveval(), asumr()
pointer	sp, y1, y2, cv, datain, dataout, buf, imps2r(), impl2r()

begin
	threshold = clgetr ("threshold")

	call smark (sp)
	call salloc (y1, 2 * ny, TY_INT)
	y1 = y1 - ys
	y2 = y1 + ny

	cen = AP_CEN(ap,2)
	low = AP_CEN(ap,2) + AP_LOW(ap,2)
	high = AP_CEN(ap,2) + AP_HIGH(ap,2)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	# Normalize by the aperture width and apply threshold.
	call adivkr (spec, high - low, spec, ny)
	if (clgetb ("cennorm")) {
	    sum = 0.
	    nsum = 0
	    do ix = ys, ys+ny-1 {
		s = cen + ap_cveval (cv, real (ix))
		iy1 = max (1, int (s))
		iy2 = min (nlines, int (s + 1))
		if (iy1 > iy2)
		    next
		datain = dbuf + (ix - l1) * nc + iy1 - c1
		if (iy1 == iy2)
		    sum = sum + Memr[datain]
		else
		    sum = sum + (iy2-s)*Memr[datain] + (s-iy1)*Memr[datain+1]
		nsum = nsum + 1
	    }
	    if (nsum > 0) {
	        sum = (asumr (spec, ny) / ny) / (sum / nsum / gain)
		call adivkr (spec, sum, spec, ny)
	    }
	}
	if (!IS_INDEF (threshold))
	    call arltr (spec, ny, threshold, threshold)

	do ix = ys, ys+ny-1 {
	    s = ap_cveval (cv, real (ix))
	    Memi[y1+ix] = nint (low + s) 
	    Memi[y2+ix] = nint (high + s) 
	}
	call alimi (Memi[y1+ys], 2 * ny, iy1, iy2)

	do iy = 1, nlines {
	    if (init == YES) {
	        buf = impl2r (out, iy)
	        call amovkr (1., Memr[buf], ncols)
	    }

	    if (iy < iy1 || iy > iy2)
		next

	    for (ix1=ys; ix1<=ys+ny-1; ix1=ix1+1) {
		if (iy < Memi[y1+ix1] || iy > Memi[y2+ix1])
		    next
		for (ix2=ix1+1; ix2<=ys+ny-1; ix2=ix2+1)
		    if (iy < Memi[y1+ix2] || iy > Memi[y2+ix2])
			break
		ix2 = ix2 - 1

	        datain = dbuf + (ix1 - l1) * nc + iy - c1
	        if (init == YES)
		    dataout = buf + ix1 - 1
	        else
	            dataout = imps2r (out, ix1, ix2, iy, iy)
		do ix = ix1, ix2 {
		    Memr[dataout] = Memr[datain] / spec[ix-ys+1] / gain
		    datain = datain +  nc
		    dataout = dataout + 1
		}
		ix1 = ix2
	    }
	}

	call imaddr (out, "CCDMEAN", 1.)

	call sfree (sp)
end


# AP_LFLAT -- Flatten the input line apertures by the norm spectra.

procedure ap_lflat (ap, out, dbuf, nc, nl, c1, l1, spec, sbuf, profile, nx, ny,
	xs, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Normalization spectrum
pointer	sbuf			# Sky buffer
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Start of spectrum in image
int	init			# Fill between apertures with 1?

real	threshold, clgetr()	# Division threshold

int	i, ncols, nlines, ix, iy, ix1, ix2
real	low, high, s, x1, x2, ap_cveval()
pointer	cv, datain, dataout, sky, imps2r(), impl2r()

begin
	threshold = clgetr ("threshold")
	if (IS_INDEF(threshold))
	    threshold = 0.
	threshold = max (0., threshold)

	low = AP_CEN(ap,1) + AP_LOW(ap,1)
	high = AP_CEN(ap,1) + AP_HIGH(ap,1)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	do i = 1, nlines {
	    if (init == YES) {
	        dataout = impl2r (out, i)
	        call amovkr (1., Memr[dataout], ncols)
	    }

	    iy = i - ys + 1
	    if (iy < 1 || iy > ny)
		next
	    s = ap_cveval (cv, real (i))
	    x1 = max (0.5, low + s) 
	    x2 = min (ncols + 0.49, high + s) 
	    if (x1 > x2)
		next

	    ix1 = nint (x1)
	    ix2 = nint (x2)

	    datain = dbuf + (i - l1) * nc + ix1 - c1
	    if (init == YES)
		dataout = dataout + ix1 - 1
	    else
	        dataout = imps2r (out, ix1, ix2, i, i)
	    if (sbuf != NULL)
		sky = sbuf + (iy - 1) * nx - xs[iy]
	    do ix = ix1, ix2 {
		s = spec[iy] * profile[iy, ix-xs[iy]+1]
		if (sbuf != NULL)
		    s = s + Memr[sky+ix]
		if (s > threshold)
		    Memr[dataout] = Memr[datain] / s
		else
		    Memr[dataout] = 1.
		datain = datain + 1
		dataout = dataout + 1
	    }
	}

	call imaddr (out, "CCDMEAN", 1.)
end


# AP_CFLAT -- Flatten the input column apertures by the norm spectra.

procedure ap_cflat (ap, out, dbuf, nc, nl, c1, l1, spec, sbuf, profile, nx, ny,
    xs, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Normalization spectrum
pointer	sbuf			# Sky buffer
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Start of spectrum in image
int	init			# Fill between apertures with 1?

real	threshold, clgetr()	# Division threshold

int	ncols, nlines, ix, iy, ix1, ix2, iy1, iy2
real	low, high, s, ap_cveval()
pointer	sp, y1, y2, cv, datain, dataout, sky, buf, imps2r(), impl2r()

begin
	threshold = clgetr ("threshold")
	if (IS_INDEF(threshold))
	    threshold = 0.
	threshold = max (0., threshold)

	call smark (sp)
	call salloc (y1, 2 * ny, TY_INT)
	y1 = y1 - ys
	y2 = y1 + ny

	low = AP_CEN(ap,2) + AP_LOW(ap,2)
	high = AP_CEN(ap,2) + AP_HIGH(ap,2)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	do ix = ys, ys+ny-1 {
	    s = ap_cveval (cv, real (ix))
	    Memi[y1+ix] = nint (low + s) 
	    Memi[y2+ix] = nint (high + s) 
	}
	call alimi (Memi[y1+ys], 2 * ny, iy1, iy2)

	do iy = 1, nlines {
	    if (init == YES) {
	        buf = impl2r (out, iy)
	        call amovkr (1., Memr[buf], ncols)
	    }

	    if (iy < iy1 || iy > iy2)
		next

	    for (ix1=ys; ix1<=ys+ny-1; ix1=ix1+1) {
		if (iy < Memi[y1+ix1] || iy > Memi[y2+ix1])
		    next
		for (ix2=ix1+1; ix2<=ys+ny-1; ix2=ix2+1)
		    if (iy < Memi[y1+ix2] || iy > Memi[y2+ix2])
			break
		ix2 = ix2 - 1

	        datain = dbuf + (ix1 - l1) * nc + iy - c1
	        if (init == YES)
		    dataout = buf + ix1 - 1
	        else
	            dataout = imps2r (out, ix1, ix2, iy, iy)
	        if (sbuf != NULL)
		    sky = sbuf - ys * nx + iy - xs[iy]
	        do ix = ix1, ix2 {
		    s = spec[ix-ys+1] * profile[ix-ys+1, iy-xs[ix-ys+1]+1]
		    if (sbuf != NULL)
			s = s + Memr[sky+ix*nx]
		    if (s > threshold)
		        Memr[dataout] = Memr[datain] / s
		    else
		        Memr[dataout] = 1.
		    datain = datain + nc
		    dataout = dataout + 1
	        }
		ix1 = ix2
	    }
	}

	call imaddr (out, "CCDMEAN", 1.)

	call sfree (sp)
end


# AP_LDIFF -- Model residuals.

procedure ap_ldiff (ap, out, gain, dbuf, nc, nl, c1, l1, spec, profile, nx, ny,
	xs, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
real	gain			# Gain
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Normalization spectrum
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Start of spectrum in image
int	init			# Fill between apertures with 1?

int	i, ncols, nlines, ix, iy, ix1, ix2
real	low, high, s, x1, x2, ap_cveval()
pointer	cv, datain, dataout, imps2r(), impl2r()

begin
	low = AP_CEN(ap,1) + AP_LOW(ap,1)
	high = AP_CEN(ap,1) + AP_HIGH(ap,1)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	do i = 1, nlines {
	    if (init == YES) {
	        dataout = impl2r (out, i)
	        call aclrr (Memr[dataout], ncols)
	    }

	    iy = i - ys + 1
	    if (iy < 1 || iy > ny)
		next
	    s = ap_cveval (cv, real (i))
	    x1 = max (0.5, low + s) 
	    x2 = min (ncols + 0.49, high + s) 
	    if (x1 > x2)
		next

	    ix1 = nint (x1)
	    ix2 = nint (x2)

	    datain = dbuf + (i - l1) * nc + ix1 - c1
	    if (init == YES)
		dataout = dataout + ix1 - 1
	    else
	        dataout = imps2r (out, ix1, ix2, i, i)
	    do ix = ix1, ix2 {
		s = spec[iy] * profile[iy, ix-xs[iy]+1]
		Memr[dataout] = (Memr[datain] - s) / gain
		datain = datain + 1
		dataout = dataout + 1
	    }
	}
end


# AP_CDIFF -- Model residuals

procedure ap_cdiff (ap, out, gain, dbuf, nc, nl, c1, l1, spec, profile, nx, ny,
    xs, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
real	gain			# Gain
pointer	dbuf			# Data buffer
int	nc, nl			# Size of data buffer
int	c1, l1			# Origin of data buffer
real	spec[ny]		# Normalization spectrum
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Start of spectrum in image
int	init			# Fill between apertures with 1?

int	ncols, nlines, ix, iy, ix1, ix2, iy1, iy2
real	low, high, s, ap_cveval()
pointer	sp, y1, y2, cv, datain, dataout, buf, imps2r(), impl2r()

begin
	call smark (sp)
	call salloc (y1, 2 * ny, TY_INT)
	y1 = y1 - ys
	y2 = y1 + ny

	low = AP_CEN(ap,2) + AP_LOW(ap,2)
	high = AP_CEN(ap,2) + AP_HIGH(ap,2)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	do ix = ys, ys+ny-1 {
	    s = ap_cveval (cv, real (ix))
	    Memi[y1+ix] = nint (low + s) 
	    Memi[y2+ix] = nint (high + s) 
	}
	call alimi (Memi[y1+ys], 2 * ny, iy1, iy2)

	do iy = 1, nlines {
	    if (init == YES) {
	        buf = impl2r (out, iy)
	        call aclrr (Memr[buf], ncols)
	    }

	    if (iy < iy1 || iy > iy2)
		next

	    for (ix1=ys; ix1<=ys+ny-1; ix1=ix1+1) {
		if (iy < Memi[y1+ix1] || iy > Memi[y2+ix1])
		    next
		for (ix2=ix1+1; ix2<=ys+ny-1; ix2=ix2+1)
		    if (iy < Memi[y1+ix2] || iy > Memi[y2+ix2])
			break
		ix2 = ix2 - 1

	        datain = dbuf + (ix1 - l1) * nc + iy - c1
	        if (init == YES)
		    dataout = buf + ix1 - 1
	        else
	            dataout = imps2r (out, ix1, ix2, iy, iy)
	        do ix = ix1, ix2 {
		    s = spec[ix-ys+1] * profile[ix-ys+1, iy-xs[ix-ys+1]+1]
		    Memr[dataout] = (Memr[datain] - s) / gain
		    datain = datain + nc
		    dataout = dataout + 1
	        }
		ix1 = ix2
	    }
	}

	call sfree (sp)
end


# AP_LFIT -- Model fit

procedure ap_lfit (ap, out, gain, spec, profile, nx, ny, xs, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
real	gain			# Gain
real	spec[ny]		# Normalization spectrum
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Start of spectrum in image
int	init			# Fill between apertures with 1?

int	i, ncols, nlines, ix, iy, ix1, ix2
real	low, high, s, x1, x2, ap_cveval()
pointer	cv, dataout, imps2r(), impl2r()

begin
	low = AP_CEN(ap,1) + AP_LOW(ap,1)
	high = AP_CEN(ap,1) + AP_HIGH(ap,1)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	do i = 1, nlines {
	    if (init == YES) {
	        dataout = impl2r (out, i)
	        call aclrr (Memr[dataout], ncols)
	    }

	    iy = i - ys + 1
	    if (iy < 1 || iy > ny)
		next
	    s = ap_cveval (cv, real (i))
	    x1 = max (0.5, low + s) 
	    x2 = min (ncols + 0.49, high + s) 
	    if (x1 > x2)
		next

	    ix1 = nint (x1)
	    ix2 = nint (x2)

	    if (init == YES)
		dataout = dataout + ix1 - 1
	    else
	        dataout = imps2r (out, ix1, ix2, i, i)
	    do ix = ix1, ix2 {
		s = spec[iy] * profile[iy, ix-xs[iy]+1]
		Memr[dataout] = s / gain
		dataout = dataout + 1
	    }
	}
end


# AP_CFIT -- Model fit

procedure ap_cfit (ap, out, gain, spec, profile, nx, ny, xs, ys, init)

pointer	ap			# Aperture structure
pointer	out			# Output IMIO pointer
real	gain			# Gain
real	spec[ny]		# Normalization spectrum
real	profile[ny,nx]		# Profile
int	nx, ny			# Size of profile array
int	xs[ny], ys		# Start of spectrum in image
int	init			# Fill between apertures with 1?

int	ncols, nlines, ix, iy, ix1, ix2, iy1, iy2
real	low, high, s, ap_cveval()
pointer	sp, y1, y2, cv, dataout, buf, imps2r(), impl2r()

begin
	call smark (sp)
	call salloc (y1, 2 * ny, TY_INT)
	y1 = y1 - ys
	y2 = y1 + ny

	low = AP_CEN(ap,2) + AP_LOW(ap,2)
	high = AP_CEN(ap,2) + AP_HIGH(ap,2)
	cv = AP_CV(ap)
	ncols = IM_LEN(out, 1)
	nlines = IM_LEN(out, 2)

	do ix = ys, ys+ny-1 {
	    s = ap_cveval (cv, real (ix))
	    Memi[y1+ix] = nint (low + s) 
	    Memi[y2+ix] = nint (high + s) 
	}
	call alimi (Memi[y1+ys], 2 * ny, iy1, iy2)

	do iy = 1, nlines {
	    if (init == YES) {
	        buf = impl2r (out, iy)
	        call aclrr (Memr[buf], ncols)
	    }

	    if (iy < iy1 || iy > iy2)
		next

	    for (ix1=ys; ix1<=ys+ny-1; ix1=ix1+1) {
		if (iy < Memi[y1+ix1] || iy > Memi[y2+ix1])
		    next
		for (ix2=ix1+1; ix2<=ys+ny-1; ix2=ix2+1)
		    if (iy < Memi[y1+ix2] || iy > Memi[y2+ix2])
			break
		ix2 = ix2 - 1

	        if (init == YES)
		    dataout = buf + ix1 - 1
	        else
	            dataout = imps2r (out, ix1, ix2, iy, iy)
	        do ix = ix1, ix2 {
		    s = spec[ix-ys+1] * profile[ix-ys+1, iy-xs[ix-ys+1]+1]
		    Memr[dataout] = s / gain
		    dataout = dataout + 1
	        }
		ix1 = ix2
	    }
	}

	call sfree (sp)
end
