include	<mach.h>
include	"../shdr.h"
include "icombine.h"


# IC_COMBINE -- Combine images.

procedure ic_combiner (sh, shout, d, id, n, m, lflag, scales, zeros, wts,
	nimages, npts)

pointer	sh[nimages]		# Input spectra
pointer	shout			# Output spectrum
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# Image index ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Line flags
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
real	wts[nimages]		# Combining weights
int	nimages			# Number of input images
int	npts			# Number of points per output line

int	i, ctor()
real	r
pointer	sp, rn, g
errchk	ic_scale

include	"icombine.com"

begin
	call smark (sp)

	# Rebin spectra and set mask arrays
	call scb_rebin (sh, shout, lflag, nimages, npts)

	# Set scale and weights and log
	call ic_scale (sh, shout, lflag, scales, zeros, wts, nimages)

	# Set combine parameters
	switch (combine) {
	case AVERAGE:
	    if (dowts)
		keepids = true
	    else
		keepids = false
	case MEDIAN:
	    dowts = false
	    keepids = false
	case SUM:
	    keepids = false
	    reject = NONE
	    grow = 0
	}
	docombine = true

	# Set rejection algorithm specific parameters
	switch (reject) {
	case CCDCLIP, CRREJECT:
	    call salloc (rn, nimages, TY_REAL)
	    call salloc (g, nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0)
		call amovkr (r, Memr[rn], nimages)
	    else {
		do i = 1, nimages
		    Memr[rn+i-1] = RA(sh[i])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		call amovkr (r, Memr[g], nimages)
		do i = 1, nimages
		    Memr[rn+i-1] = (Memr[rn+i-1] / r) ** 2
	    } else {
		do i = 1, nimages {
		    r = DEC(sh[i])
		    Memr[g+i-1] = r
		    Memr[rn+i-1] = (Memr[rn+i-1] / r) ** 2
		}
	    }
	    if (!keepids) {
		if (doscale1 || grow > 0)
		    keepids = true
		else {
		    do i = 2, nimages {
			if (Memr[rn+i-1]!=Memr[rn] || Memr[g+i-1]!=Memr[g]) {
			    keepids = true
			    break
			}
		    }
		}
	    }
	    if (reject == CRREJECT)
		lsigma = MAX_REAL
	case MINMAX:
	    mclip = false
	    if (grow > 0)
		keepids = true
	case PCLIP:
	    mclip = true
	    if (grow > 0)
		keepids = true
	case AVSIGCLIP, SIGCLIP:
	    if (doscale1 || grow > 0)
		keepids = true
	case NONE:
	    mclip = false
	    grow = 0
	}

	if (keepids) {
	    do i = 1, nimages
		call salloc (id[i], npts, TY_INT)
	}

	call ic_gdatar (sh, d, id, n, m, lflag, scales, zeros, nimages, npts)

	switch (reject) {
	case CCDCLIP, CRREJECT:
	    if (mclip)
		call ic_mccdclipr (d, id, n, scales, zeros, Memr[rn],
		    Memr[g], nimages, npts, Memr[SY(shout)])
	    else
		call ic_accdclipr (d, id, n, scales, zeros, Memr[rn],
		    Memr[g], npts, Memr[SY(shout)])
	case MINMAX:
	    call ic_mmr (d, id, n, npts)
	case PCLIP:
	    call ic_pclipr (d, id, n, nimages, npts, Memr[SY(shout)])
	case SIGCLIP:
	    if (mclip)
		call ic_msigclipr (d, id, n, scales, zeros, nimages, npts,
		    Memr[SY(shout)])
	    else
		call ic_asigclipr (d, id, n, scales, zeros, nimages, npts,
		    Memr[SY(shout)])
	case AVSIGCLIP:
	    if (mclip)
		call ic_mavsigclipr (d, id, n, scales, zeros, nimages,
		    npts, Memr[SY(shout)])
	    else
		call ic_aavsigclipr (d, id, n, scales, zeros, nimages,
		    npts, Memr[SY(shout)])
	}

	if (grow > 0)
	    call ic_growr (d, id, n, nimages, npts, Memr[SY(shout)])

	if (docombine) {
	    switch (combine) {
	    case AVERAGE:
		call ic_averager (d, id, n, wts, npts, Memr[SY(shout)])
	    case MEDIAN:
		call ic_medianr (d, n, npts, Memr[SY(shout)])
	    case SUM:
		call ic_sumr (d, n, npts, Memr[SY(shout)])
	    }
	}

	call sfree (sp)
end
