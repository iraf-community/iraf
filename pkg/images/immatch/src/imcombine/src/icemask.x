# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>


# IC_EMASK --  Create exposure mask.

procedure ic_emask (pm, v, id, nimages, n, wts, npts)

pointer	pm			#I Pixel mask
long	v[ARB]			#I Output vector
pointer	id[nimages]		#I Image id pointers
int	nimages			#I Number of images
int	n[npts]			#I Number of good pixels
real	wts[npts]		#I Weights
int	npts			#I Number of output pixels per line

int	i, j, k, impnli()
real	exp
pointer	buf
errchk	impnli

pointer	exps			# Exposure times
pointer	ev			# IMIO coordinate vector
real	ezero			# Integer to real zero
real	escale			# Integer to real scale
int	einit			# Initialization flag
common	/emask/ exps, ev, ezero, escale, einit

begin
	# Write scaling factors to the header.
	if (einit == NO) {
	    if (ezero != 0. || escale != 1.) {
		call imaddr (pm, "MASKZERO", ezero)
		call imaddr (pm, "MASKSCAL", escale)
	    }
	    einit = YES
	}

	call amovl (v, Meml[ev], IM_MAXDIM)
	i = impnli (pm, buf, Meml[ev])
	call aclri (Memi[buf], npts)
	do i = 1, npts {
	    exp = 0.
	    do j = 1, n[i] {
		k = Memi[id[j]+i-1]
		if (wts[k] > 0.)
		    exp = exp + Memr[exps+k-1]
	    }
	    Memi[buf] = nint((exp-ezero)/escale)
	    buf = buf + 1
	}
end


# IC_EINIT --  Initialize exposure mask.

procedure ic_einit (in, nimages, key, default, maxval)

int	in[nimages]		#I Image pointers
int	nimages			#I Number of images
char	key[ARB]		#I Exposure time keyword
real	default			#I Default exposure time
int	maxval			#I Maximum mask value

int	i
real	exp, emin, emax, efrac, imgetr()

pointer	exps			# Exposure times
pointer	ev			# IMIO coordinate vector
real	ezero			# Integer to real zero
real	escale			# Integer to real scale
int	einit			# Initialization flag
common	/emask/ exps, ev, ezero, escale, einit

begin
	call malloc (ev, IM_MAXDIM, TY_LONG)
	call malloc (exps, nimages, TY_REAL)

	emax = 0.
	emin = MAX_REAL
	efrac = 0
	do i = 1, nimages {
	    iferr (exp = imgetr (in[i], key))
		exp = default
	    exp = max (0., exp)
	    emax = emax + exp
	    if (exp > 0.)
		emin = min (exp, emin)
	    efrac = max (abs(exp-nint(exp)), efrac)
	    Memr[exps+i-1] = exp
	}

	# Set scaling.
	ezero = 0.
	escale = 1.
	if (emin < 1.) {
	    escale = emin
	    emin = emin / escale
	    emax = emax / escale
	} else if (emin == MAX_REAL)
	    emin = 0.
	if (efrac > 0.001 && emax-emin < 1000.) {
	    escale = escale / 1000.
	    emin = emin * 1000.
	    emax = emax * 1000.
	}
	while (emax > maxval) {
	    escale = escale * 10.
	    emin = emin / 10.
	    emax = emax / 10.
	}
	einit = NO
end
