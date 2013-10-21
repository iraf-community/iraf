include	<gio.h>
include	<gset.h>
include	<mach.h>

define	A0	(-1.74743)
define	A1	(0.73)
define	A2	(-0.24)
define	A3	(0.035)
define	MAXDEN	6.0

# T_SELFTEST -- a test procedure for the DTOI package.  Two intensity vectors
# are calculated in different ways and compared.  A plot of the residuals is
# shown.  A plot showing the extent of truncation errors is also drawn.  Two
# standard ranges of data values are available: 12 bit, representing PDS
# format data and 15 bit, representing the FITS data format available on the
# PDS.  Any other choice results in a small test, ranging from 1 - 144.

procedure t_selftest

bool	verbose
char	device[SZ_FNAME]
pointer	sp, intk, intc, raw, den, gp
int	min_raw, max_raw, nvalues, i, nbits
real	scale, factor, ceiling

bool	clgetb()
pointer	gopen()
int	clgeti()
real	clgetr()

begin
	call smark (sp)

	nbits = clgeti ("nbits")

	switch (nbits) {
	case 12:
	    min_raw = 1
	    max_raw = 3072
	    scale   = 0.00151
	case 15:
	    min_raw = 1
	    max_raw = 24576
	    scale   = 4.65 / 24575. 
	case 0:
	    call eprintf ("Using test data range from 1 - 144\n")
	    min_raw = 1
	    max_raw = 144
	    scale   = 0.0325
	default:
	    call eprintf ("Unknown case: nbits = '%d', Please supply values:\n")
		call pargi (nbits)
	    min_raw = 1
	    max_raw = clgeti ("max_raw")
	    # max density = 6.0.  Density = raw value * scale.
	    call clputr ("scale.p_maximum", real (MAXDEN / max_raw))
	    call clputr ("scale.p_default", real (4.65 / (max_raw - 1)))
	    scale = clgetr ("scale")
	}

	call clgstr ("device", device, SZ_FNAME)
	verbose = clgetb ("verbose")
	ceiling = clgetr ("ceiling")

	gp = gopen (device, NEW_FILE, STDGRAPH)

	nvalues = max_raw - min_raw + 1
	call salloc (intk, nvalues, TY_REAL)
	call salloc (intc, nvalues, TY_REAL)
	call salloc (den,  nvalues, TY_REAL)
	call salloc (raw,  nvalues, TY_REAL)

	do i = 1, nvalues
	    Memr[raw+i-1] = min_raw + i - 1

	call amulkr (Memr[raw], scale, Memr[den], nvalues)

	call hd_known (min_raw, max_raw, scale, Memr[intk], nvalues)
	call hd_calc  (min_raw, max_raw, scale, Memr[intc], nvalues)
	 
	if (verbose) {
	    factor = ceiling / Memr[intc+nvalues-1]
	    call printf ("# %20tRaw Value %40tDensity %60tIntensity\n\n")
	    do i = 1, nvalues {
	        call printf ("%20t%d %40t%g %60t%g\n")
		    call pargi (i)
		    call pargr (Memr[den+i-1])
		    call pargr (Memr[intc+i-1] * factor)
	    }
	}

	call hd_plotit (gp, Memr[den], Memr[intk], Memr[intc], nvalues)

	call hd_trunc (gp, Memr[den], nvalues, ceiling, Memr[intc])

	call gclose (gp)
	call sfree (sp)
end


# HD_KNOWN -- Calculate vector of known intensity values.

procedure hd_known (min_raw, max_raw, scale, intk, nvalues)

int	min_raw			# Minimum raw data value
int	max_raw			# Maximum raw data value
real	scale			# Density = raw_value * scale
real	intk[nvalues]		# Known intensities - filled on return
int	nvalues			# Number of intensity values requested

int	i
real	density, logo
real	exp

begin
	do i = min_raw, max_raw {
	    density = max (EPSILONR, i * scale)
	    logo = log10 ((10. ** density) - 1.0)
	    exp  = A0 + A1 * logo + A2 * logo ** 2 + A3 * logo ** 3
	    intk[i] = 10 ** (exp)
	}
end


# HD_CALC -- Calcuate vector of intensity values as in HDTOI.

procedure hd_calc (min, max, scale, intc, nvalues)

int	min			# Minimum raw data value
int	max			# Maximum raw data value
real	scale			# Density = raw_value * scale
real	intc[nvalues]		# Calculated intensity values - filled on return
int	nvalues			# Number of intensity values requested

real	cfit[9]
pointer	sp, lut

begin
	call smark (sp)
	call salloc (lut, nvalues, TY_REAL)

	cfit[1] = 5.0
	cfit[2] = 4.0
	cfit[3] = -10.0
	cfit[4] = MAXDEN
	cfit[5] = 1.
	cfit[6] = A0
	cfit[7] = A1
	cfit[8] = A2
	cfit[9] = A3
	call st_wlut (Memr[lut], min, max, scale, cfit)
	call st_transform (min, max, Memr[lut], nvalues, intc)

	call sfree (sp)
end


# HD_TRUNC -- Investigate truncation errors for real versus int output image.

procedure hd_trunc (gp, density, nvalues, ceiling, intc)

pointer	gp			# Pointer to graphics stream
real	density[nvalues]	# Density array
int	nvalues			# Number of density, intensity values
real	ceiling			# Max intensity to output
real	intc[nvalues]		# Calculated intensity values

pointer	sp, yint, yreal
int	npvals
real	factor

begin
	call smark (sp)

	# Only the lowest 5% of the data values are plotted
	npvals = nvalues * 0.05

	call salloc (yint, npvals, TY_INT)
	call salloc (yreal, npvals, TY_REAL)

	# Scale intensity vector to ceiling 
	factor = ceiling / intc[nvalues]

	call amulkr (intc, factor, intc, npvals)
	call achtri (intc, Memi[yint], npvals)
	call achtir (Memi[yint], Memr[yreal], npvals)

	call gascale (gp, density,  npvals, 1)
	call gascale (gp, Memr[yreal], npvals, 2)
	call gsview  (gp, 0.2, 0.9, 0.1, 0.4)
	call gseti (gp, G_ROUND, YES)
	call glabax  (gp, 
	    "Expand to see Truncation Errors\n (real=SOLID, integer=DASHED)", 
	    "Density (Lowest 5% only)", "Intensity")

	call gseti  (gp, G_PLTYPE, GL_SOLID)
	call gpline (gp, density, intc, npvals)

	call gseti  (gp, G_PLTYPE, GL_DASHED)
	call gpline (gp, density, Memr[yreal], npvals)

	call sfree (sp)
end


# HD_PLOTIT -- Plot residuals of calculated versus known itensity.

procedure hd_plotit (gp, density, intk, intc, nvalues)

pointer	gp			# Pointer to graphics stream
real	density[nvalues]	# Density array
real	intk[nvalues]		# Array of known intensities
real	intc[nvalues]		# Array of calculated intensities
int	nvalues			# Number of density, intensity values

pointer	sp, resid

begin
	call smark (sp)
	call salloc (resid, nvalues, TY_REAL)

	call asubr (intk, intc, Memr[resid], nvalues)

	call gascale (gp, density, nvalues, 1)
	call gascale (gp, Memr[resid], nvalues, 2)
	call gsview  (gp, 0.2, 0.9, 0.6, 0.9)
	call gseti   (gp, G_ROUND, YES)

	call glabax  (gp, "Residual Intensity\n (Known - Calculated)", 
	    "Density", "")
	call gpline  (gp, density, Memr[resid], nvalues)

	call sfree (sp)
end


# ST_WLUT -- Generate look up table, using technique of HDTOI.

procedure st_wlut (lut, min, max, scale, cfit)

real	lut[ARB]	# Look up table of intensities
int	min		# Minimum raw data value
int	max		# Maximum raw data value
real	scale		# Density = raw_value * scale
real	cfit[ARB]	# Coefficient array for restoring curfit

pointer	cv, sp, den, indv, kv
int	nvalues, i
extern	hd_powerr()

begin
	call smark (sp)
	nvalues = max - min + 1
	call salloc (den, nvalues, TY_REAL)
	call salloc (indv, nvalues, TY_REAL)
	call salloc (kv, nvalues, TY_REAL)
	do i = 1, nvalues
	    Memr[kv+i-1] = real (i)

	call amulkr (Memr[kv], scale, Memr[den], nvalues)

	call cvrestore (cv, cfit)
	call cvuserfnc (cv, hd_powerr)

	call hd_aptrans (Memr[den], Memr[indv], nvalues, "logo")
	call cvvector (cv, Memr[indv], lut, nvalues)
	do i = 1, nvalues
	    lut[i] = 10.0 ** lut[i]

	call cvfree (cv)
	call sfree (sp)
end


# ST_TRANSFORM -- Apply transformation from look up table to input vector.

procedure st_transform (min, max, lut, nvalues, intc)

int	min		# Minimum raw data value
int	max		# Maximum raw data value
real	lut[ARB]	# Array of intensity values
int	nvalues		# Number of density, intensity values
real	intc[ARB]	# Calculated intensities - returned

int	i

begin
	do i = 1, nvalues
	    intc[i] = lut[i]
end
