# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<error.h>
include	<syserr.h>
include	<mach.h>
include	"../icombine.h"


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


procedure icombines (in, out, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, npts, fd, stropen(), errcode(), imstati()
pointer	sp, d, id, n, m, lflag, scales, zeros, wts, dbuf
pointer	buf, imgl1s(), impl1i()
pointer	impl1r()

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call amovki (D_ALL, Memi[lflag], nimages)

	# If aligned use the IMIO buffer otherwise we need vectors of
	# output length.

	if (!aligned) {
	    call salloc (dbuf, nimages, TY_POINTER)
	    do i = 1, nimages
		call salloc (Memi[dbuf+i-1], npts, TY_SHORT)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
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

	    do i = 1, nimages {
		call imseti (in[i], IM_BUFSIZE, bufsize)
		iferr (buf = imgl1s (in[i])) {
		    switch (errcode()) {
		    case SYS_MFULL:
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    default:
			if (imstati (in[i], IM_CLOSEFD) == YES) {
			    call strclose (fd)
			    call error (1,
				"icombine - Too many images to combine")
			}
			do j = i-2, nimages
			    call imseti (in[j], IM_CLOSEFD, YES)
			buf = imgl1s (in[i])
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combines (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, Memr[scales], Memr[zeros],
	    Memr[wts], nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combines (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output image
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

int	i, ctor()
real	r, imgetr()
pointer	sp, v1, v2, v3, outdata, buf, rn, g, impnli()
pointer	impnlr()
errchk	ic_scale, imgetr

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

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
		    Memr[rn+i-1] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		call amovkr (r, Memr[g], nimages)
		do i = 1, nimages
		    Memr[rn+i-1] = (Memr[rn+i-1] / r) ** 2
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
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

	while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatas (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclips (d, id, n, scales, zeros, Memr[rn],
			Memr[g], nimages, npts, Memr[outdata])
		else
		    call ic_accdclips (d, id, n, scales, zeros, Memr[rn],
			Memr[g], npts, Memr[outdata])
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

	    if (grow > 0)
		call ic_grows (d, id, n, nimages, npts, Memr[outdata])

	    if (docombine) {
		switch (combine) {
		case AVERAGE:
		    call ic_averages (d, id, n, wts, npts, Memr[outdata])
		case MEDIAN:
		    call ic_medians (d, n, npts, Memr[outdata])
		}
	    }

	    if (out[2] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnli (out[2], buf, Meml[v1])
		call amovki (nimages, Memi[buf], npts)
		call asubi (Memi[buf], n, Memi[buf], npts)
	    }
		    
	    if (out[3] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnlr (out[3], buf, Meml[v1])
		call ic_sigmas (d, id, n, wts, npts, Memr[outdata],
		    Memr[buf])
	    }
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end

procedure icombinei (in, out, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, npts, fd, stropen(), errcode(), imstati()
pointer	sp, d, id, n, m, lflag, scales, zeros, wts, dbuf
pointer	buf, imgl1i(), impl1i()
pointer	impl1r()

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call amovki (D_ALL, Memi[lflag], nimages)

	# If aligned use the IMIO buffer otherwise we need vectors of
	# output length.

	if (!aligned) {
	    call salloc (dbuf, nimages, TY_POINTER)
	    do i = 1, nimages
		call salloc (Memi[dbuf+i-1], npts, TY_INT)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
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

	    do i = 1, nimages {
		call imseti (in[i], IM_BUFSIZE, bufsize)
		iferr (buf = imgl1i (in[i])) {
		    switch (errcode()) {
		    case SYS_MFULL:
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    default:
			if (imstati (in[i], IM_CLOSEFD) == YES) {
			    call strclose (fd)
			    call error (1,
				"icombine - Too many images to combine")
			}
			do j = i-2, nimages
			    call imseti (in[j], IM_CLOSEFD, YES)
			buf = imgl1i (in[i])
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combinei (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, Memr[scales], Memr[zeros],
	    Memr[wts], nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combinei (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output image
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

int	i, ctor()
real	r, imgetr()
pointer	sp, v1, v2, v3, outdata, buf, rn, g, impnli()
pointer	impnlr()
errchk	ic_scale, imgetr

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

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
		    Memr[rn+i-1] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		call amovkr (r, Memr[g], nimages)
		do i = 1, nimages
		    Memr[rn+i-1] = (Memr[rn+i-1] / r) ** 2
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
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

	while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatai (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclipi (d, id, n, scales, zeros, Memr[rn],
			Memr[g], nimages, npts, Memr[outdata])
		else
		    call ic_accdclipi (d, id, n, scales, zeros, Memr[rn],
			Memr[g], npts, Memr[outdata])
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

	    if (grow > 0)
		call ic_growi (d, id, n, nimages, npts, Memr[outdata])

	    if (docombine) {
		switch (combine) {
		case AVERAGE:
		    call ic_averagei (d, id, n, wts, npts, Memr[outdata])
		case MEDIAN:
		    call ic_mediani (d, n, npts, Memr[outdata])
		}
	    }

	    if (out[2] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnli (out[2], buf, Meml[v1])
		call amovki (nimages, Memi[buf], npts)
		call asubi (Memi[buf], n, Memi[buf], npts)
	    }
		    
	    if (out[3] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnlr (out[3], buf, Meml[v1])
		call ic_sigmai (d, id, n, wts, npts, Memr[outdata],
		    Memr[buf])
	    }
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end

procedure icombiner (in, out, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, npts, fd, stropen(), errcode(), imstati()
pointer	sp, d, id, n, m, lflag, scales, zeros, wts, dbuf
pointer	buf, imgl1r(), impl1i()
pointer	impl1r()

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call amovki (D_ALL, Memi[lflag], nimages)

	# If aligned use the IMIO buffer otherwise we need vectors of
	# output length.

	if (!aligned) {
	    call salloc (dbuf, nimages, TY_POINTER)
	    do i = 1, nimages
		call salloc (Memi[dbuf+i-1], npts, TY_REAL)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
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

	    do i = 1, nimages {
		call imseti (in[i], IM_BUFSIZE, bufsize)
		iferr (buf = imgl1r (in[i])) {
		    switch (errcode()) {
		    case SYS_MFULL:
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    default:
			if (imstati (in[i], IM_CLOSEFD) == YES) {
			    call strclose (fd)
			    call error (1,
				"icombine - Too many images to combine")
			}
			do j = i-2, nimages
			    call imseti (in[j], IM_CLOSEFD, YES)
			buf = imgl1r (in[i])
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combiner (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, Memr[scales], Memr[zeros],
	    Memr[wts], nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combiner (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output image
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

int	i, ctor()
real	r, imgetr()
pointer	sp, v1, v2, v3, outdata, buf, rn, g, impnli()
pointer	impnlr()
errchk	ic_scale, imgetr

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

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
		    Memr[rn+i-1] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		call amovkr (r, Memr[g], nimages)
		do i = 1, nimages
		    Memr[rn+i-1] = (Memr[rn+i-1] / r) ** 2
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
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

	while (impnlr (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatar (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclipr (d, id, n, scales, zeros, Memr[rn],
			Memr[g], nimages, npts, Memr[outdata])
		else
		    call ic_accdclipr (d, id, n, scales, zeros, Memr[rn],
			Memr[g], npts, Memr[outdata])
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

	    if (grow > 0)
		call ic_growr (d, id, n, nimages, npts, Memr[outdata])

	    if (docombine) {
		switch (combine) {
		case AVERAGE:
		    call ic_averager (d, id, n, wts, npts, Memr[outdata])
		case MEDIAN:
		    call ic_medianr (d, n, npts, Memr[outdata])
		}
	    }

	    if (out[2] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnli (out[2], buf, Meml[v1])
		call amovki (nimages, Memi[buf], npts)
		call asubi (Memi[buf], n, Memi[buf], npts)
	    }
		    
	    if (out[3] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnlr (out[3], buf, Meml[v1])
		call ic_sigmar (d, id, n, wts, npts, Memr[outdata],
		    Memr[buf])
	    }
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end

procedure icombined (in, out, offsets, nimages, bufsize)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output images
int	offsets[nimages,ARB]	# Input image offsets
int	nimages			# Number of input images
int	bufsize			# IMIO buffer size

char	str[1]
int	i, j, npts, fd, stropen(), errcode(), imstati()
pointer	sp, d, id, n, m, lflag, scales, zeros, wts, dbuf
pointer	buf, imgl1d(), impl1i()
pointer	impl1d()

include	"../icombine.com"

begin
	npts = IM_LEN(out[1],1)

	# Allocate memory.
	call smark (sp)
	call salloc (d, nimages, TY_POINTER)
	call salloc (id, nimages, TY_POINTER)
	call salloc (n, npts, TY_INT)
	call salloc (m, nimages, TY_POINTER)
	call salloc (lflag, nimages, TY_INT)
	call salloc (scales, nimages, TY_REAL)
	call salloc (zeros, nimages, TY_REAL)
	call salloc (wts, nimages, TY_REAL)
	call amovki (D_ALL, Memi[lflag], nimages)

	# If aligned use the IMIO buffer otherwise we need vectors of
	# output length.

	if (!aligned) {
	    call salloc (dbuf, nimages, TY_POINTER)
	    do i = 1, nimages
		call salloc (Memi[dbuf+i-1], npts, TY_DOUBLE)
	}

	if (project) {
	    call imseti (in[1], IM_NBUFS, nimages)
	    call imseti (in[1], IM_BUFSIZE, bufsize)
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
	    }
	} else {
	    # Reserve FD for string operations.
	    fd = stropen (str, 1, NEW_FILE)

	    # Do I/O to the images.
	    do i = 1, 3 {
		if (out[i] != NULL)
		    call imseti (out[i], IM_BUFSIZE, bufsize)
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

	    do i = 1, nimages {
		call imseti (in[i], IM_BUFSIZE, bufsize)
		iferr (buf = imgl1d (in[i])) {
		    switch (errcode()) {
		    case SYS_MFULL:
			call sfree (sp)
			call strclose (fd)
			call erract (EA_ERROR)
		    default:
			if (imstati (in[i], IM_CLOSEFD) == YES) {
			    call strclose (fd)
			    call error (1,
				"icombine - Too many images to combine")
			}
			do j = i-2, nimages
			    call imseti (in[j], IM_CLOSEFD, YES)
			buf = imgl1d (in[i])
		    }
		}
	    }

	    call strclose (fd)
	}

	call ic_combined (in, out, Memi[dbuf], Memi[d], Memi[id], Memi[n],
	    Memi[m], Memi[lflag], offsets, Memr[scales], Memr[zeros],
	    Memr[wts], nimages, npts)
end


# IC_COMBINE -- Combine images.

procedure ic_combined (in, out, dbuf, d, id, n, m, lflag, offsets,
	scales, zeros, wts, nimages, npts)

pointer	in[nimages]		# Input images
pointer	out[3]			# Output image
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

int	i, ctor()
real	r, imgetr()
pointer	sp, v1, v2, v3, outdata, buf, rn, g, impnli()
pointer	impnld()
errchk	ic_scale, imgetr

include	"../icombine.com"

begin
	call smark (sp)
	call salloc (v1, IM_MAXDIM, TY_LONG)
	call salloc (v2, IM_MAXDIM, TY_LONG)
	call salloc (v3, IM_MAXDIM, TY_LONG)
	call amovkl (long(1), Meml[v1], IM_MAXDIM)
	call amovkl (long(1), Meml[v2], IM_MAXDIM)
	call amovkl (long(1), Meml[v3], IM_MAXDIM)

	call ic_scale (in, out, offsets, scales, zeros, wts, nimages)

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
		    Memr[rn+i-1] = imgetr (in[i], Memc[rdnoise])
	    }
	    i = 1
	    if (ctor (Memc[gain], i, r) > 0) {
		call amovkr (r, Memr[g], nimages)
		do i = 1, nimages
		    Memr[rn+i-1] = (Memr[rn+i-1] / r) ** 2
	    } else {
		do i = 1, nimages {
		    r = imgetr (in[i], Memc[gain])
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

	while (impnld (out[1], outdata, Meml[v1]) != EOF) {
	    call ic_gdatad (in, out, dbuf, d, id, n, m, lflag, offsets,
		scales, zeros, nimages, npts, Meml[v2], Meml[v3])

	    switch (reject) {
	    case CCDCLIP, CRREJECT:
		if (mclip)
		    call ic_mccdclipd (d, id, n, scales, zeros, Memr[rn],
			Memr[g], nimages, npts, Memd[outdata])
		else
		    call ic_accdclipd (d, id, n, scales, zeros, Memr[rn],
			Memr[g], npts, Memd[outdata])
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

	    if (grow > 0)
		call ic_growd (d, id, n, nimages, npts, Memd[outdata])

	    if (docombine) {
		switch (combine) {
		case AVERAGE:
		    call ic_averaged (d, id, n, wts, npts, Memd[outdata])
		case MEDIAN:
		    call ic_mediand (d, n, npts, Memd[outdata])
		}
	    }

	    if (out[2] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnli (out[2], buf, Meml[v1])
		call amovki (nimages, Memi[buf], npts)
		call asubi (Memi[buf], n, Memi[buf], npts)
	    }
		    
	    if (out[3] != NULL) {
		call amovl (Meml[v2], Meml[v1], IM_MAXDIM)
		i = impnld (out[3], buf, Meml[v1])
		call ic_sigmad (d, id, n, wts, npts, Memd[outdata],
		    Memd[buf])
	    }
	    call amovl (Meml[v1], Meml[v2], IM_MAXDIM)
	}

	call sfree (sp)
end

