# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	<error.h>
include	<syserr.h>
include	<mach.h>
include	"../icombine.h"

# The following is for compiling under V2.11.
define	IM_BUFFRAC	IM_BUFSIZE
include	<imset.h>


# ICOMBINE -- Combine images
#
# The memory and open file descriptor limits are checked and an attempt
# to recover is made either by setting the image pixel files to be
# closed after I/O or by notifying the calling program that memory
# ran out and the IMIO buffer size should be reduced.  After the checks
# a procedure for the selected combine option is called.
# Because there may be several failure modes when reaching the file
# limits we first assume an error is due to the file limit, except for
# out of memory, and close some pixel files.  If the error then repeats
# on accessing the pixels the error is passed back.


procedure icombines (in, out, scales, zeros, wts, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	wts[nimages]		# Weights
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, k, npts, fd, stropen(), xt_imgnls()
pointer	sp, d, id, n, m, lflag, v, dbuf
pointer	im, buf, xt_opix(), impl1i()
errchk	stropen, xt_cpix, xt_opix, xt_imgnls, impl1i, ic_combines
pointer	impl1r()
errchk	impl1r

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (dbuf, nimages, TY_POINTER)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call amovki (D_ALL, Memi[lflag], nimages)
	call amovkl (1, Meml[v], IM_MAXDIM)

	# If not aligned or growing create data buffers of output length
	# otherwise use the IMIO buffers.

	if (!aligned || grow >= 1.) {
	    do i = 1, nimages {
		call salloc (Memi[dbuf+i-1], npts, TY_SHORT)
		call aclrs (Mems[Memi[dbuf+i-1]], npts)
	    }
	} else {
	    do i = 1, nimages {
		im = xt_opix (in[i], i, 1)
		if (im != in[i]) {
		    call salloc (Memi[dbuf+i-1], npts, TY_SHORT)
		    call aclrs (Mems[Memi[dbuf+i-1]], npts)
		}
	    }
	    call amovki (NULL, Memi[dbuf], nimages)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFFRAC, 0)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	    buf = impl1r (out[1])
	    call aclrr (Memr[buf], npts)
	    if (out[3] != NULL) {
		buf = impl1r (out[3])
		call aclrr (Memr[buf], npts)
	    }
	    if (out[2] != NULL) {
		buf = impl1i (out[2])
		call aclri (Memi[buf], npts)
	    }
	    if (out[4] != NULL) {
		buf = impl1i (out[4])
		call aclri (Memi[buf], npts)
	    }
	    if (out[5] != NULL) {
		buf = impl1i (out[5])
		call aclri (Memi[buf], npts)
	    }
	    if (out[6] != NULL) {
		buf = impl1i (out[6])
		call aclri (Memi[buf], npts)
	    }

	    # Do I/O for first input image line.
	    if (!project) {
		do i = 1, nimages {
		    call xt_imseti (i, "bufsize", bufsize)
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			call xt_cpix (i)
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			call xt_cpix (i)
		}

		do i = 1, nimages {
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			next
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			next
		    iferr {
			Meml[v+1] = j
			j = xt_imgnls (in[i], i, buf, Meml[v], 1)
		    } then {
			call imseti (im, IM_PIXFD, NULL)
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combines (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, scales, zeros, wts, nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combines (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output image
pointer	dbuf[nimages]		# Data buffers for nonaligned images
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# Image index ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Line flags
int	offsets[nimages,ARB]	# Input image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
real	wts[nimages]		# Combining weights
int	nimages			# Number of input images
int	npts			# Number of points per output line

int	i, ext, ctor(), errcode()
real	r, imgetr()
pointer	sp, fname, imname, v1, v2, v3, work
pointer	outdata, buf, nmod, nm, pms
pointer	immap(), impnli()
pointer	impnlr(), imgnlr()
errchk	immap, ic_scale, imgetr, ic_grow, ic_grows, ic_rmasks, ic_emask
errchk	ic_gdatas

include	"../icombine.com"
data	ext/0/

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

	# Set combine parameters
	switch (combine) {
	case AVERAGE, SUM, QUAD, NMODEL:
	    if (dowts)
		keepids = true
	    else
		keepids = false
	case MEDIAN:
	    dowts = false
	    keepids = false
	}
	docombine = true

	# Get noise model parameters.
	if (combine==NMODEL) {
	    call salloc (nmod, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nmod+3*(i-1)+2] = r
		}
	    }
	}

	# Set rejection algorithm specific parameters
	switch (reject) {
	case CCDCLIP, CRREJECT:
	    call salloc (nm, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nm+3*(i-1)+2] = r
		}
	    }
	    if (!keepids) {
		if (doscale1)
		    keepids = true
		else {
		    do i = 2, nimages {
			if (Memr[nm+3*(i-1)] != Memr[nm] ||
			    Memr[nm+3*(i-1)+1] != Memr[nm+1] ||
			    Memr[nm+3*(i-1)+2] != Memr[nm+2]) {
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
	case PCLIP:
	    mclip = true
	case AVSIGCLIP, SIGCLIP:
	    if (doscale1)
		keepids = true
	case NONE:
	    mclip = false
	}

	if (out[4] != NULL)
	    keepids = true

	if (out[6] != NULL) {
	    keepids = true
	    call ic_einit (in, nimages, Memc[expkeyword], 1., 2**27-1)
	}

	if (grow >= 1.) {
	    keepids = true
	    call salloc (work, npts * nimages, TY_INT)
	}
	pms = NULL

	if (keepids) {
	    do i = 1, nimages
		call salloc (id[i], npts, TY_INT)
	}

        # Reduce header memory use.
	do i = 1, nimages
	    call xt_minhdr (i)

	while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatas (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclips (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memr[outdata])
		else
		    call ic_accdclips (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memr[outdata])
	    case MINMAX:
		call ic_mms (d, id, n, npts)
	    case PCLIP:
		call ic_pclips (d, id, n, nimages, npts, Memr[outdata])
	    case SIGCLIP:
		if (mclip)
		    call ic_msigclips (d, id, n, scales, zeros, nimages, npts,
			Memr[outdata])
		else
		    call ic_asigclips (d, id, n, scales, zeros, nimages, npts,
			Memr[outdata])
	    case AVSIGCLIP:
		if (mclip)
		    call ic_mavsigclips (d, id, n, scales, zeros, nimages,
			npts, Memr[outdata])
		else
		    call ic_aavsigclips (d, id, n, scales, zeros, nimages,
			npts, Memr[outdata])
	    }

	    if (pms == NULL || nkeep > 0) {
		if (docombine) {
		    switch (combine) {
		    case AVERAGE:
			call ic_averages (d, id, n, wts, nimages, npts,
			    YES, YES, Memr[outdata])
		    case MEDIAN:
			call ic_medians (d, n, npts, YES, Memr[outdata])
		    case SUM:
			call ic_averages (d, id, n, wts, nimages, npts,
			    YES, NO, Memr[outdata])
		    case QUAD:
			call ic_quads (d, id, n, wts, nimages, npts,
			    YES, YES, Memr[outdata])
		    case NMODEL:
			call ic_nmodels (d, id, n, Memr[nmod], wts,
			    nimages, npts, YES, YES, Memr[outdata])
		    }
		}
	    }

	    if (grow >= 1.)
		call ic_grow (out, Meml[v2], id, n, Memi[work], nimages, npts,
		    pms)

	    if (pms == NULL) {
		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 1
		    }
		}

		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnlr (out[3], buf, Meml[v1])
		    call ic_sigmas (d, id, n, wts, npts, Memr[outdata],
			Memr[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)
	    }

	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	if (pms != NULL) {
	    if (nkeep > 0) {
		call imstats (out[1], IM_IMAGENAME, Memc[fname], SZ_FNAME)
		call imunmap (out[1])
		iferr (buf = immap (Memc[fname], READ_WRITE, 0)) {
		    switch (errcode()) {
		    case SYS_FXFOPNOEXTNV:
			call imgcluster (Memc[fname], Memc[fname], SZ_FNAME)
			ext = ext + 1
			call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
			    call pargstr (Memc[fname])
			    call pargi (ext)
			iferr (buf = immap (Memc[imname], READ_WRITE, 0)) {
			    buf = NULL
			    ext = 0
			}
			repeat {
			    call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
				call pargstr (Memc[fname])
				call pargi (ext+1)
			    iferr (outdata = immap (Memc[imname],READ_WRITE,0)) 
				break
			    if (buf != NULL)
				call imunmap (buf)
			    buf = outdata
			    ext = ext + 1
			}
		    default:
			call erract (EA_ERROR)
		    }
		}
		out[1] = buf
	    }

	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)
	    while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
		call ic_gdatas (in, out, dbuf, d, id, n, m, lflag, offsets,
		    scales, zeros, nimages, npts, Meml[v2], Meml[v3])

		call ic_grows (Meml[v2], d, id, n, Memi[work], nimages, npts,
		    pms)

		if (nkeep > 0) {
		    do i = 1, npts {
			if (n[i] < nkeep) {
			    Meml[v1+1] = Meml[v1+1] - 1
			    if (imgnlr (out[1], buf, Meml[v1]) == EOF)
				;
			    call amovr (Memr[buf], Memr[outdata], npts)
			    break
			}
		    }
		}

		switch (combine) {
		case AVERAGE:
		    call ic_averages (d, id, n, wts, nimages, npts,
		        NO, YES, Memr[outdata])
		case MEDIAN:
		    call ic_medians (d, n, npts, NO, Memr[outdata])
		case SUM:
		    call ic_averages (d, id, n, wts, nimages, npts,
		        NO, NO, Memr[outdata])
		case QUAD:
		    call ic_quads (d, id, n, wts, nimages, npts,
		        NO, YES, Memr[outdata])
		case NMODEL:
		    call ic_nmodels (d, id, n, Memr[nmod], wts,
		        nimages, npts, NO, YES, Memr[outdata])
		}

		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 2
			buf = buf + 1
		    }
		}
			
		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnlr (out[3], buf, Meml[v1])
		    call ic_sigmas (d, id, n, wts, npts, Memr[outdata],
			Memr[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)

		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }

	    do i = 1, nimages
		call pm_close (Memi[pms+i-1])
	    call mfree (pms, TY_POINTER)
	}

	call sfree (sp)
end

procedure icombinei (in, out, scales, zeros, wts, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	wts[nimages]		# Weights
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, k, npts, fd, stropen(), xt_imgnli()
pointer	sp, d, id, n, m, lflag, v, dbuf
pointer	im, buf, xt_opix(), impl1i()
errchk	stropen, xt_cpix, xt_opix, xt_imgnli, impl1i, ic_combinei
pointer	impl1r()
errchk	impl1r

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (dbuf, nimages, TY_POINTER)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call amovki (D_ALL, Memi[lflag], nimages)
	call amovkl (1, Meml[v], IM_MAXDIM)

	# If not aligned or growing create data buffers of output length
	# otherwise use the IMIO buffers.

	if (!aligned || grow >= 1.) {
	    do i = 1, nimages {
		call salloc (Memi[dbuf+i-1], npts, TY_INT)
		call aclri (Memi[Memi[dbuf+i-1]], npts)
	    }
	} else {
	    do i = 1, nimages {
		im = xt_opix (in[i], i, 1)
		if (im != in[i]) {
		    call salloc (Memi[dbuf+i-1], npts, TY_INT)
		    call aclri (Memi[Memi[dbuf+i-1]], npts)
		}
	    }
	    call amovki (NULL, Memi[dbuf], nimages)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFFRAC, 0)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	    buf = impl1r (out[1])
	    call aclrr (Memr[buf], npts)
	    if (out[3] != NULL) {
		buf = impl1r (out[3])
		call aclrr (Memr[buf], npts)
	    }
	    if (out[2] != NULL) {
		buf = impl1i (out[2])
		call aclri (Memi[buf], npts)
	    }
	    if (out[4] != NULL) {
		buf = impl1i (out[4])
		call aclri (Memi[buf], npts)
	    }
	    if (out[5] != NULL) {
		buf = impl1i (out[5])
		call aclri (Memi[buf], npts)
	    }
	    if (out[6] != NULL) {
		buf = impl1i (out[6])
		call aclri (Memi[buf], npts)
	    }

	    # Do I/O for first input image line.
	    if (!project) {
		do i = 1, nimages {
		    call xt_imseti (i, "bufsize", bufsize)
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			call xt_cpix (i)
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			call xt_cpix (i)
		}

		do i = 1, nimages {
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			next
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			next
		    iferr {
			Meml[v+1] = j
			j = xt_imgnli (in[i], i, buf, Meml[v], 1)
		    } then {
			call imseti (im, IM_PIXFD, NULL)
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combinei (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, scales, zeros, wts, nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combinei (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output image
pointer	dbuf[nimages]		# Data buffers for nonaligned images
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# Image index ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Line flags
int	offsets[nimages,ARB]	# Input image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
real	wts[nimages]		# Combining weights
int	nimages			# Number of input images
int	npts			# Number of points per output line

int	i, ext, ctor(), errcode()
real	r, imgetr()
pointer	sp, fname, imname, v1, v2, v3, work
pointer	outdata, buf, nmod, nm, pms
pointer	immap(), impnli()
pointer	impnlr(), imgnlr()
errchk	immap, ic_scale, imgetr, ic_grow, ic_growi, ic_rmasks, ic_emask
errchk	ic_gdatai

include	"../icombine.com"
data	ext/0/

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

	# Set combine parameters
	switch (combine) {
	case AVERAGE, SUM, QUAD, NMODEL:
	    if (dowts)
		keepids = true
	    else
		keepids = false
	case MEDIAN:
	    dowts = false
	    keepids = false
	}
	docombine = true

	# Get noise model parameters.
	if (combine==NMODEL) {
	    call salloc (nmod, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nmod+3*(i-1)+2] = r
		}
	    }
	}

	# Set rejection algorithm specific parameters
	switch (reject) {
	case CCDCLIP, CRREJECT:
	    call salloc (nm, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nm+3*(i-1)+2] = r
		}
	    }
	    if (!keepids) {
		if (doscale1)
		    keepids = true
		else {
		    do i = 2, nimages {
			if (Memr[nm+3*(i-1)] != Memr[nm] ||
			    Memr[nm+3*(i-1)+1] != Memr[nm+1] ||
			    Memr[nm+3*(i-1)+2] != Memr[nm+2]) {
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
	case PCLIP:
	    mclip = true
	case AVSIGCLIP, SIGCLIP:
	    if (doscale1)
		keepids = true
	case NONE:
	    mclip = false
	}

	if (out[4] != NULL)
	    keepids = true

	if (out[6] != NULL) {
	    keepids = true
	    call ic_einit (in, nimages, Memc[expkeyword], 1., 2**27-1)
	}

	if (grow >= 1.) {
	    keepids = true
	    call salloc (work, npts * nimages, TY_INT)
	}
	pms = NULL

	if (keepids) {
	    do i = 1, nimages
		call salloc (id[i], npts, TY_INT)
	}

        # Reduce header memory use.
	do i = 1, nimages
	    call xt_minhdr (i)

	while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatai (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclipi (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memr[outdata])
		else
		    call ic_accdclipi (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memr[outdata])
	    case MINMAX:
		call ic_mmi (d, id, n, npts)
	    case PCLIP:
		call ic_pclipi (d, id, n, nimages, npts, Memr[outdata])
	    case SIGCLIP:
		if (mclip)
		    call ic_msigclipi (d, id, n, scales, zeros, nimages, npts,
			Memr[outdata])
		else
		    call ic_asigclipi (d, id, n, scales, zeros, nimages, npts,
			Memr[outdata])
	    case AVSIGCLIP:
		if (mclip)
		    call ic_mavsigclipi (d, id, n, scales, zeros, nimages,
			npts, Memr[outdata])
		else
		    call ic_aavsigclipi (d, id, n, scales, zeros, nimages,
			npts, Memr[outdata])
	    }

	    if (pms == NULL || nkeep > 0) {
		if (docombine) {
		    switch (combine) {
		    case AVERAGE:
			call ic_averagei (d, id, n, wts, nimages, npts,
			    YES, YES, Memr[outdata])
		    case MEDIAN:
			call ic_mediani (d, n, npts, YES, Memr[outdata])
		    case SUM:
			call ic_averagei (d, id, n, wts, nimages, npts,
			    YES, NO, Memr[outdata])
		    case QUAD:
			call ic_quadi (d, id, n, wts, nimages, npts,
			    YES, YES, Memr[outdata])
		    case NMODEL:
			call ic_nmodeli (d, id, n, Memr[nmod], wts,
			    nimages, npts, YES, YES, Memr[outdata])
		    }
		}
	    }

	    if (grow >= 1.)
		call ic_grow (out, Meml[v2], id, n, Memi[work], nimages, npts,
		    pms)

	    if (pms == NULL) {
		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 1
		    }
		}

		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnlr (out[3], buf, Meml[v1])
		    call ic_sigmai (d, id, n, wts, npts, Memr[outdata],
			Memr[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)
	    }

	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	if (pms != NULL) {
	    if (nkeep > 0) {
		call imstats (out[1], IM_IMAGENAME, Memc[fname], SZ_FNAME)
		call imunmap (out[1])
		iferr (buf = immap (Memc[fname], READ_WRITE, 0)) {
		    switch (errcode()) {
		    case SYS_FXFOPNOEXTNV:
			call imgcluster (Memc[fname], Memc[fname], SZ_FNAME)
			ext = ext + 1
			call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
			    call pargstr (Memc[fname])
			    call pargi (ext)
			iferr (buf = immap (Memc[imname], READ_WRITE, 0)) {
			    buf = NULL
			    ext = 0
			}
			repeat {
			    call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
				call pargstr (Memc[fname])
				call pargi (ext+1)
			    iferr (outdata = immap (Memc[imname],READ_WRITE,0)) 
				break
			    if (buf != NULL)
				call imunmap (buf)
			    buf = outdata
			    ext = ext + 1
			}
		    default:
			call erract (EA_ERROR)
		    }
		}
		out[1] = buf
	    }

	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)
	    while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
		call ic_gdatai (in, out, dbuf, d, id, n, m, lflag, offsets,
		    scales, zeros, nimages, npts, Meml[v2], Meml[v3])

		call ic_growi (Meml[v2], d, id, n, Memi[work], nimages, npts,
		    pms)

		if (nkeep > 0) {
		    do i = 1, npts {
			if (n[i] < nkeep) {
			    Meml[v1+1] = Meml[v1+1] - 1
			    if (imgnlr (out[1], buf, Meml[v1]) == EOF)
				;
			    call amovr (Memr[buf], Memr[outdata], npts)
			    break
			}
		    }
		}

		switch (combine) {
		case AVERAGE:
		    call ic_averagei (d, id, n, wts, nimages, npts,
		        NO, YES, Memr[outdata])
		case MEDIAN:
		    call ic_mediani (d, n, npts, NO, Memr[outdata])
		case SUM:
		    call ic_averagei (d, id, n, wts, nimages, npts,
		        NO, NO, Memr[outdata])
		case QUAD:
		    call ic_quadi (d, id, n, wts, nimages, npts,
		        NO, YES, Memr[outdata])
		case NMODEL:
		    call ic_nmodeli (d, id, n, Memr[nmod], wts,
		        nimages, npts, NO, YES, Memr[outdata])
		}

		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 2
			buf = buf + 1
		    }
		}
			
		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnlr (out[3], buf, Meml[v1])
		    call ic_sigmai (d, id, n, wts, npts, Memr[outdata],
			Memr[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)

		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }

	    do i = 1, nimages
		call pm_close (Memi[pms+i-1])
	    call mfree (pms, TY_POINTER)
	}

	call sfree (sp)
end

procedure icombiner (in, out, scales, zeros, wts, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	wts[nimages]		# Weights
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, k, npts, fd, stropen(), xt_imgnlr()
pointer	sp, d, id, n, m, lflag, v, dbuf
pointer	im, buf, xt_opix(), impl1i()
errchk	stropen, xt_cpix, xt_opix, xt_imgnlr, impl1i, ic_combiner
pointer	impl1r()
errchk	impl1r

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (dbuf, nimages, TY_POINTER)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call amovki (D_ALL, Memi[lflag], nimages)
	call amovkl (1, Meml[v], IM_MAXDIM)

	# If not aligned or growing create data buffers of output length
	# otherwise use the IMIO buffers.

	if (!aligned || grow >= 1.) {
	    do i = 1, nimages {
		call salloc (Memi[dbuf+i-1], npts, TY_REAL)
		call aclrr (Memr[Memi[dbuf+i-1]], npts)
	    }
	} else {
	    do i = 1, nimages {
		im = xt_opix (in[i], i, 1)
		if (im != in[i]) {
		    call salloc (Memi[dbuf+i-1], npts, TY_REAL)
		    call aclrr (Memr[Memi[dbuf+i-1]], npts)
		}
	    }
	    call amovki (NULL, Memi[dbuf], nimages)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFFRAC, 0)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	    buf = impl1r (out[1])
	    call aclrr (Memr[buf], npts)
	    if (out[3] != NULL) {
		buf = impl1r (out[3])
		call aclrr (Memr[buf], npts)
	    }
	    if (out[2] != NULL) {
		buf = impl1i (out[2])
		call aclri (Memi[buf], npts)
	    }
	    if (out[4] != NULL) {
		buf = impl1i (out[4])
		call aclri (Memi[buf], npts)
	    }
	    if (out[5] != NULL) {
		buf = impl1i (out[5])
		call aclri (Memi[buf], npts)
	    }
	    if (out[6] != NULL) {
		buf = impl1i (out[6])
		call aclri (Memi[buf], npts)
	    }

	    # Do I/O for first input image line.
	    if (!project) {
		do i = 1, nimages {
		    call xt_imseti (i, "bufsize", bufsize)
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			call xt_cpix (i)
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			call xt_cpix (i)
		}

		do i = 1, nimages {
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			next
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			next
		    iferr {
			Meml[v+1] = j
			j = xt_imgnlr (in[i], i, buf, Meml[v], 1)
		    } then {
			call imseti (im, IM_PIXFD, NULL)
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combiner (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, scales, zeros, wts, nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combiner (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output image
pointer	dbuf[nimages]		# Data buffers for nonaligned images
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# Image index ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Line flags
int	offsets[nimages,ARB]	# Input image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
real	wts[nimages]		# Combining weights
int	nimages			# Number of input images
int	npts			# Number of points per output line

int	i, ext, ctor(), errcode()
real	r, imgetr()
pointer	sp, fname, imname, v1, v2, v3, work
pointer	outdata, buf, nmod, nm, pms
pointer	immap(), impnli()
pointer	impnlr(), imgnlr
errchk	immap, ic_scale, imgetr, ic_grow, ic_growr, ic_rmasks, ic_emask
errchk	ic_gdatar

include	"../icombine.com"
data	ext/0/

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

	# Set combine parameters
	switch (combine) {
	case AVERAGE, SUM, QUAD, NMODEL:
	    if (dowts)
		keepids = true
	    else
		keepids = false
	case MEDIAN:
	    dowts = false
	    keepids = false
	}
	docombine = true

	# Get noise model parameters.
	if (combine==NMODEL) {
	    call salloc (nmod, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nmod+3*(i-1)+2] = r
		}
	    }
	}

	# Set rejection algorithm specific parameters
	switch (reject) {
	case CCDCLIP, CRREJECT:
	    call salloc (nm, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nm+3*(i-1)+2] = r
		}
	    }
	    if (!keepids) {
		if (doscale1)
		    keepids = true
		else {
		    do i = 2, nimages {
			if (Memr[nm+3*(i-1)] != Memr[nm] ||
			    Memr[nm+3*(i-1)+1] != Memr[nm+1] ||
			    Memr[nm+3*(i-1)+2] != Memr[nm+2]) {
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
	case PCLIP:
	    mclip = true
	case AVSIGCLIP, SIGCLIP:
	    if (doscale1)
		keepids = true
	case NONE:
	    mclip = false
	}

	if (out[4] != NULL)
	    keepids = true

	if (out[6] != NULL) {
	    keepids = true
	    call ic_einit (in, nimages, Memc[expkeyword], 1., 2**27-1)
	}

	if (grow >= 1.) {
	    keepids = true
	    call salloc (work, npts * nimages, TY_INT)
	}
	pms = NULL

	if (keepids) {
	    do i = 1, nimages
		call salloc (id[i], npts, TY_INT)
	}

        # Reduce header memory use.
	do i = 1, nimages
	    call xt_minhdr (i)

	while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatar (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclipr (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memr[outdata])
		else
		    call ic_accdclipr (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memr[outdata])
	    case MINMAX:
		call ic_mmr (d, id, n, npts)
	    case PCLIP:
		call ic_pclipr (d, id, n, nimages, npts, Memr[outdata])
	    case SIGCLIP:
		if (mclip)
		    call ic_msigclipr (d, id, n, scales, zeros, nimages, npts,
			Memr[outdata])
		else
		    call ic_asigclipr (d, id, n, scales, zeros, nimages, npts,
			Memr[outdata])
	    case AVSIGCLIP:
		if (mclip)
		    call ic_mavsigclipr (d, id, n, scales, zeros, nimages,
			npts, Memr[outdata])
		else
		    call ic_aavsigclipr (d, id, n, scales, zeros, nimages,
			npts, Memr[outdata])
	    }

	    if (pms == NULL || nkeep > 0) {
		if (docombine) {
		    switch (combine) {
		    case AVERAGE:
			call ic_averager (d, id, n, wts, nimages, npts,
			    YES, YES, Memr[outdata])
		    case MEDIAN:
			call ic_medianr (d, n, npts, YES, Memr[outdata])
		    case SUM:
			call ic_averager (d, id, n, wts, nimages, npts,
			    YES, NO, Memr[outdata])
		    case QUAD:
			call ic_quadr (d, id, n, wts, nimages, npts,
			    YES, YES, Memr[outdata])
		    case NMODEL:
			call ic_nmodelr (d, id, n, Memr[nmod], wts,
			    nimages, npts, YES, YES, Memr[outdata])
		    }
		}
	    }

	    if (grow >= 1.)
		call ic_grow (out, Meml[v2], id, n, Memi[work], nimages, npts,
		    pms)

	    if (pms == NULL) {
		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 2
			buf = buf + 1
		    }
		}

		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnlr (out[3], buf, Meml[v1])
		    call ic_sigmar (d, id, n, wts, npts, Memr[outdata],
			Memr[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)
	    }

	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	if (pms != NULL) {
	    if (nkeep > 0) {
		call imstats (out[1], IM_IMAGENAME, Memc[fname], SZ_FNAME)
		call imunmap (out[1])
		iferr (buf = immap (Memc[fname], READ_WRITE, 0)) {
		    switch (errcode()) {
		    case SYS_FXFOPNOEXTNV:
			call imgcluster (Memc[fname], Memc[fname], SZ_FNAME)
			ext = ext + 1
			call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
			    call pargstr (Memc[fname])
			    call pargi (ext)
			iferr (buf = immap (Memc[imname], READ_WRITE, 0)) {
			    buf = NULL
			    ext = 0
			}
			repeat {
			    call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
				call pargstr (Memc[fname])
				call pargi (ext+1)
			    iferr (outdata = immap (Memc[imname],READ_WRITE,0)) 
				break
			    if (buf != NULL)
				call imunmap (buf)
			    buf = outdata
			    ext = ext + 1
			}
		    default:
			call erract (EA_ERROR)
		    }
		}
		out[1] = buf
	    }

	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)
	    while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
		call ic_gdatar (in, out, dbuf, d, id, n, m, lflag, offsets,
		    scales, zeros, nimages, npts, Meml[v2], Meml[v3])

		call ic_growr (Meml[v2], d, id, n, Memi[work], nimages, npts,
		    pms)

		if (nkeep > 0) {
		    do i = 1, npts {
			if (n[i] < nkeep) {
			    Meml[v1+1] = Meml[v1+1] - 1
			    if (imgnlr (out[1], buf, Meml[v1]) == EOF)
				;
			    call amovr (Memr[buf], Memr[outdata], npts)
			    break
			}
		    }
		}

		switch (combine) {
		case AVERAGE:
		    call ic_averager (d, id, n, wts, nimages, npts,
		        NO, YES, Memr[outdata])
		case MEDIAN:
		    call ic_medianr (d, n, npts, NO, Memr[outdata])
		case SUM:
		    call ic_averager (d, id, n, wts, nimages, npts,
		        NO, NO, Memr[outdata])
		case QUAD:
		    call ic_quadr (d, id, n, wts, nimages, npts,
		        NO, YES, Memr[outdata])
		case NMODEL:
		    call ic_nmodelr (d, id, n, Memr[nmod], wts,
		        nimages, npts, NO, YES, Memr[outdata])
		}

		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 2
			buf = buf + 1
		    }
		}
			
		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnlr (out[3], buf, Meml[v1])
		    call ic_sigmar (d, id, n, wts, npts, Memr[outdata],
			Memr[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)

		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }

	    do i = 1, nimages
		call pm_close (Memi[pms+i-1])
	    call mfree (pms, TY_POINTER)
	}

	call sfree (sp)
end

procedure icombined (in, out, scales, zeros, wts, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output images
real	scales[nimages]		# Scales
real	zeros[nimages]		# Zeros
real	wts[nimages]		# Weights
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, k, npts, fd, stropen(), xt_imgnld()
pointer	sp, d, id, n, m, lflag, v, dbuf
pointer	im, buf, xt_opix(), impl1i()
errchk	stropen, xt_cpix, xt_opix, xt_imgnld, impl1i, ic_combined
pointer	impl1d()
errchk	impl1d

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (dbuf, nimages, TY_POINTER)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (v, IM_MAXDIM, TY_LONG)
	call amovki (D_ALL, Memi[lflag], nimages)
	call amovkl (1, Meml[v], IM_MAXDIM)

	# If not aligned or growing create data buffers of output length
	# otherwise use the IMIO buffers.

	if (!aligned || grow >= 1.) {
	    do i = 1, nimages {
		call salloc (Memi[dbuf+i-1], npts, TY_DOUBLE)
		call aclrd (Memd[Memi[dbuf+i-1]], npts)
	    }
	} else {
	    do i = 1, nimages {
		im = xt_opix (in[i], i, 1)
		if (im != in[i]) {
		    call salloc (Memi[dbuf+i-1], npts, TY_DOUBLE)
		    call aclrd (Memd[Memi[dbuf+i-1]], npts)
		}
	    }
	    call amovki (NULL, Memi[dbuf], nimages)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFFRAC, 0)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 6 {
		if (out[i] != NULL) {
		    call imseti (out[i], IM_BUFFRAC, 0)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
		}
	    }
	    buf = impl1d (out[1])
	    call aclrd (Memd[buf], npts)
	    if (out[3] != NULL) {
		buf = impl1d (out[3])
		call aclrd (Memd[buf], npts)
	    }
	    if (out[2] != NULL) {
		buf = impl1i (out[2])
		call aclri (Memi[buf], npts)
	    }
	    if (out[4] != NULL) {
		buf = impl1i (out[4])
		call aclri (Memi[buf], npts)
	    }
	    if (out[5] != NULL) {
		buf = impl1i (out[5])
		call aclri (Memi[buf], npts)
	    }
	    if (out[6] != NULL) {
		buf = impl1i (out[6])
		call aclri (Memi[buf], npts)
	    }

	    # Do I/O for first input image line.
	    if (!project) {
		do i = 1, nimages {
		    call xt_imseti (i, "bufsize", bufsize)
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			call xt_cpix (i)
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			call xt_cpix (i)
		}

		do i = 1, nimages {
		    j = max (0, offsets[i,1])
		    k = min (npts, IM_LEN(in[i],1) + offsets[i,1])
		    if (k - j < 1)
			next
		    j = 1 - offsets[i,2]
		    if (j < 1 || j > IM_LEN(in[i],2))
			next
		    iferr {
			Meml[v+1] = j
			j = xt_imgnld (in[i], i, buf, Meml[v], 1)
		    } then {
			call imseti (im, IM_PIXFD, NULL)
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combined (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, scales, zeros, wts, nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combined (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[ARB]		# Output image
pointer	dbuf[nimages]		# Data buffers for nonaligned images
pointer	d[nimages]		# Data pointers
pointer	id[nimages]		# Image index ID pointers
int	n[npts]			# Number of good pixels
pointer	m[nimages]		# Mask pointers
int	lflag[nimages]		# Line flags
int	offsets[nimages,ARB]	# Input image offsets
real	scales[nimages]		# Scale factors
real	zeros[nimages]		# Zero offset factors
real	wts[nimages]		# Combining weights
int	nimages			# Number of input images
int	npts			# Number of points per output line

int	i, ext, ctor(), errcode()
real	r, imgetr()
pointer	sp, fname, imname, v1, v2, v3, work
pointer	outdata, buf, nmod, nm, pms
pointer	immap(), impnli()
pointer	impnld(), imgnld
errchk	immap, ic_scale, imgetr, ic_grow, ic_growd, ic_rmasks, ic_emask
errchk	ic_gdatad

include	"../icombine.com"
data	ext/0/

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

	# Set combine parameters
	switch (combine) {
	case AVERAGE, SUM, QUAD, NMODEL:
	    if (dowts)
		keepids = true
	    else
		keepids = false
	case MEDIAN:
	    dowts = false
	    keepids = false
	}
	docombine = true

	# Get noise model parameters.
	if (combine==NMODEL) {
	    call salloc (nmod, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nmod+3*(i-1)+1] = r * scales[i]
		    Memr[nmod+3*(i-1)] =
			max ((Memr[nmod+3*(i-1)] / Memr[nmod+3*(i-1)+1]) ** 2,
			1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nmod+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nmod+3*(i-1)+2] = r
		}
	    }
	}

	# Set rejection algorithm specific parameters
	switch (reject) {
	case CCDCLIP, CRREJECT:
	    call salloc (nm, 3*nimages, TY_REAL)
	    i = 1
	    if (ctor (Memc[rdnoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = r
	    } else {
		do i = 1, nimages
		    Memr[nm+3*(i-1)] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		do i = 1, nimages {
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
		    Memr[nm+3*(i-1)+1] = r
		    Memr[nm+3*(i-1)] =
			max ((Memr[nm+3*(i-1)] / r) ** 2, 1e4 / MAX_REAL)
		}
	    }
	    i = 1
	    if (ctor (Memc[snoise], i, r) > 0) {
		do i = 1, nimages
		    Memr[nm+3*(i-1)+2] = r
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[snoise])
		    Memr[nm+3*(i-1)+2] = r
		}
	    }
	    if (!keepids) {
		if (doscale1)
		    keepids = true
		else {
		    do i = 2, nimages {
			if (Memr[nm+3*(i-1)] != Memr[nm] ||
			    Memr[nm+3*(i-1)+1] != Memr[nm+1] ||
			    Memr[nm+3*(i-1)+2] != Memr[nm+2]) {
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
	case PCLIP:
	    mclip = true
	case AVSIGCLIP, SIGCLIP:
	    if (doscale1)
		keepids = true
	case NONE:
	    mclip = false
	}

	if (out[4] != NULL)
	    keepids = true

	if (out[6] != NULL) {
	    keepids = true
	    call ic_einit (in, nimages, Memc[expkeyword], 1., 2**27-1)
	}

	if (grow >= 1.) {
	    keepids = true
	    call salloc (work, npts * nimages, TY_INT)
	}
	pms = NULL

	if (keepids) {
	    do i = 1, nimages
		call salloc (id[i], npts, TY_INT)
	}

        # Reduce header memory use.
	do i = 1, nimages
	    call xt_minhdr (i)

	while (impnld (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatad (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclipd (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memd[outdata])
		else
		    call ic_accdclipd (d, id, n, scales, zeros, Memr[nm],
			nimages, npts, Memd[outdata])
	    case MINMAX:
		call ic_mmd (d, id, n, npts)
	    case PCLIP:
		call ic_pclipd (d, id, n, nimages, npts, Memd[outdata])
	    case SIGCLIP:
		if (mclip)
		    call ic_msigclipd (d, id, n, scales, zeros, nimages, npts,
			Memd[outdata])
		else
		    call ic_asigclipd (d, id, n, scales, zeros, nimages, npts,
			Memd[outdata])
	    case AVSIGCLIP:
		if (mclip)
		    call ic_mavsigclipd (d, id, n, scales, zeros, nimages,
			npts, Memd[outdata])
		else
		    call ic_aavsigclipd (d, id, n, scales, zeros, nimages,
			npts, Memd[outdata])
	    }

	    if (pms == NULL || nkeep > 0) {
		if (docombine) {
		    switch (combine) {
		    case AVERAGE:
			call ic_averaged (d, id, n, wts, nimages, npts,
			    YES, YES, Memd[outdata])
		    case MEDIAN:
			call ic_mediand (d, n, npts, YES, Memd[outdata])
		    case SUM:
			call ic_averaged (d, id, n, wts, nimages, npts,
			    YES, NO, Memd[outdata])
		    case QUAD:
			call ic_quadd (d, id, n, wts, nimages, npts,
			    YES, YES, Memd[outdata])
		    case NMODEL:
			call ic_nmodeld (d, id, n, Memr[nmod], wts,
			    nimages, npts, YES, YES, Memd[outdata])
		    }
		}
	    }

	    if (grow >= 1.)
		call ic_grow (out, Meml[v2], id, n, Memi[work], nimages, npts,
		    pms)

	    if (pms == NULL) {
		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 2
			buf = buf + 1
		    }
		}

		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnld (out[3], buf, Meml[v1])
		    call ic_sigmad (d, id, n, wts, npts, Memd[outdata],
			Memd[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)
	    }

	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	if (pms != NULL) {
	    if (nkeep > 0) {
		call imstats (out[1], IM_IMAGENAME, Memc[fname], SZ_FNAME)
		call imunmap (out[1])
		iferr (buf = immap (Memc[fname], READ_WRITE, 0)) {
		    switch (errcode()) {
		    case SYS_FXFOPNOEXTNV:
			call imgcluster (Memc[fname], Memc[fname], SZ_FNAME)
			ext = ext + 1
			call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
			    call pargstr (Memc[fname])
			    call pargi (ext)
			iferr (buf = immap (Memc[imname], READ_WRITE, 0)) {
			    buf = NULL
			    ext = 0
			}
			repeat {
			    call sprintf (Memc[imname], SZ_FNAME, "%s[%d]")
				call pargstr (Memc[fname])
				call pargi (ext+1)
			    iferr (outdata = immap (Memc[imname],READ_WRITE,0)) 
				break
			    if (buf != NULL)
				call imunmap (buf)
			    buf = outdata
			    ext = ext + 1
			}
		    default:
			call erract (EA_ERROR)
		    }
		}
		out[1] = buf
	    }

	    call amovkl (long(1), Meml[v1], IM_MAXDIM)
	    call amovkl (long(1), Meml[v2], IM_MAXDIM)
	    call amovkl (long(1), Meml[v3], IM_MAXDIM)
	    while (impnld (out[1], outdata, Meml[v1]) != EOF) {
		call ic_gdatad (in, out, dbuf, d, id, n, m, lflag, offsets,
		    scales, zeros, nimages, npts, Meml[v2], Meml[v3])

		call ic_growd (Meml[v2], d, id, n, Memi[work], nimages, npts,
		    pms)

		if (nkeep > 0) {
		    do i = 1, npts {
			if (n[i] < nkeep) {
			    Meml[v1+1] = Meml[v1+1] - 1
			    if (imgnld (out[1], buf, Meml[v1]) == EOF)
				;
			    call amovd (Memd[buf], Memd[outdata], npts)
			    break
			}
		    }
		}

		switch (combine) {
		case AVERAGE:
		    call ic_averaged (d, id, n, wts, nimages, npts,
		        NO, YES, Memd[outdata])
		case MEDIAN:
		    call ic_mediand (d, n, npts, NO, Memd[outdata])
		case SUM:
		    call ic_averaged (d, id, n, wts, nimages, npts,
		        NO, NO, Memd[outdata])
		case QUAD:
		    call ic_quadd (d, id, n, wts, nimages, npts,
		        NO, YES, Memd[outdata])
		case NMODEL:
		    call ic_nmodeld (d, id, n, Memr[nmod], wts,
		        nimages, npts, NO, YES, Memd[outdata])
		}

		if (out[2] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[2], buf, Meml[v1])
		    do i = 1, npts {
			if (n[i] > 0)
			    Memi[buf] = 0
			else if (n[i] == 0)
			    Memi[buf] = 1
			else
			    Memi[buf] = 2
			buf = buf + 1
		    }
		}
			
		if (out[3] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnld (out[3], buf, Meml[v1])
		    call ic_sigmad (d, id, n, wts, npts, Memd[outdata],
			Memd[buf])
		}

		if (out[4] != NULL)
		    call ic_rmasks (out[4], Meml[v2], id, nimages, n, npts)

		if (out[5] != NULL) {
		    call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		    i = impnli (out[5], buf, Meml[v1])
		    call amovki (nimages, Memi[buf], npts)
		    call asubi (Memi[buf], n, Memi[buf], npts)
		}

		if (out[6] != NULL)
		    call ic_emask (out[6], Meml[v2], id, nimages, n, wts, npts)

		call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	    }

	    do i = 1, nimages
		call pm_close (Memi[pms+i-1])
	    call mfree (pms, TY_POINTER)
	}

	call sfree (sp)
end

