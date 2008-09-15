include	<mach.h>
include	<pkg/rmsorted.h>

# RM_MED -- Running median library.
#
# This is a layer over the sorted running median routines.
# This layer provides:
#
#    1. Support for multiple datasets (e.g. pixels in running image)
#    2. Support for an interior average
#    3. Support for masks
#    4. Support for excluded index (e.g. image)


# Method object structure.
define	RM_LEN		25		# Structure size
define	RM_RMS		Memi[$1]	# Pointer to RMEDSRT method
define	RM_BOX		Memi[$1+1]	# Box size
define	RM_NDATA	Memi[$1+2]	# Number of datasets
define	RM_PIXTYPE	Memi[$1+3]	# Internal storage type
define	RM_GOOD		Memi[$1+4]	# Ptr to good array (box)
define	RM_MASK		Memi[$1+5]	# Ptr to mask array
define	RM_PWIN		Memi[$1+6]	# Ptr to packed window data (n*box)
define	RM_POUT		Memi[$1+7]	# Ptr to packed outlist data (n*box)
define	RM_PMASK	Memi[$1+8]	# Ptr to mask data (n*(box+15)/16)
define	RM_SETMASK	P2S($1+9)	# Ptr to set mask array (16)
define	RM_UNSETMASK	P2S($1+17)	# Ptr to unset mask array (16)

define	GOOD		Memr[RM_GOOD($1)+$2]
define	MASK		Mems[RM_MASK($1)+$2/16]
define	SETMASK		Mems[RM_SETMASK($1)+mod($2,16)]
define	UNSETMASK	Mems[RM_UNSETMASK($1)+mod($2,16)]

define	PWINR		Memr[RM_PWIN($1)+RM_BOX($1)*($2-1)]
define	PWINS		Mems[RM_PWIN($1)+RM_BOX($1)*($2-1)]
define	POUT		Mems[RM_POUT($1)+(RM_BOX($1)+1)/2*($2-1)]


# RM_MED -- Compute next running median value.

real procedure rm_med (rm, nclip, navg, blank, exclude, index, in, mask, nused)

pointer	rm			#I RM pointer
real	nclip			#I Clipping factor
int	navg			#I Number of central values to average
real	blank			#I Blank value
int	exclude			#I Index of excluded data (one indexed)
int	index			#I Index of new data (one indexed)
real	in			#I Input data value
short	mask			#I Input mask value	
short	nused			#O Number of values in calculated result
real	med			#R Return value

int	i, j, iexclude
short	s1, s2, ors(), ands()
pointer	rms
real	clip, rmsorted()

begin
	# Call running median sorted routine.
	rms = RM_RMS(rm)
	med = rmsorted (rms, nclip, index, in)

	# Set mask if needed.
	s2 = mod (index-1, RM_BOX(rm))
	s1 = MASK(rm,s2)
	if (mask != 0 || s1 != 0) {
	    if (mask != 0)
		MASK(rm,s2) = ors (s1, SETMASK(rm,s2))
	    else
		MASK(rm,s2) = ands (s1, UNSETMASK(rm,s2))
	    s1 = MASK(rm,s2)
	}

	# Recompute value if there are masks or an excluded value.
	iexclude = mod (exclude-1, RM_BOX(rm))
	if (s1 == 0 && iexclude < 0) {
	    do s2 = 0, RM_BOX(rm)-1, 16 {
		s1 = MASK(rm,s2)
		if (s1 != 0)
		    break
	    }
	}
	if (s1 != 0 || iexclude >= 0) {
	    nused = 0
	    do i = 0, RM_BOX(rm)-1 {
	        s2 = IN(rms,i)
		if (s2 != iexclude) {
		    s1 = MASK(rm,s2)
		    if (s1 == 0) {
			GOOD(rm,nused) = DATA(rms,i)
			nused = nused + 1
		    } else if (ands (s1, SETMASK(rm,s2)) == 0) {
			GOOD(rm,nused) = DATA(rms,i)
			nused = nused + 1
		    }
		}
	    }

	    if (nused > 2 && nclip > 0.) {
	        i = nused / 2
		if (mod (nused, 2) == 0)
		    med = (GOOD(rm,i) + GOOD(rm,i-1)) / 2
		else
		    med = GOOD(rm,i)
		clip = med + nclip * (med - GOOD(rm,0))
		do i = nused, 1, -1 {
		    if (GOOD(rm,i-1) < clip)
		        break
		}
		nused = i
	    }

	    if (nused > 2) {
		for (i = 0; nused-2*i>max(2,navg); i=i+1)
		    ;
		med = GOOD(rm,i)
		do j = i+1, nused-i-1 {
		    med = med + GOOD(rm,j)
		}
		nused = nused - 2 * i
		med = med / nused
	    } else if (nused == 0)
	        med = blank
	    else if (nused == 1)
	        med = GOOD(rm,0)
	    else
	        med = (GOOD(rm,0) + GOOD(rm,1)) / 2.
	} else
	    nused = min (navg, RM_BOX(rm))

	return (med)
end


# RM_GMED -- Running median value.

real procedure rm_gmed (rm, nclip, navg, blank, exclude, nused)

pointer	rm			#I RM pointer
real	nclip			#I Clipping factor
int	navg			#I Number of central values to average
real	blank			#I Blank value
int	exclude			#I Index of excluded data (one indexed)
short	nused			#O Number of values in calculated result
real	med			#R Return value

int	i, j, iexclude
short	mask, ands()
real	clip
pointer	rms

begin
	rms = RM_RMS(rm)
	iexclude = mod (exclude-1, RM_BOX(rm))

	# Extract good values to use.
	nused = 0
	do i = 0, RM_BOX(rm)-1 {
	    j = IN(rms,i)
	    mask = MASK(rm,j)
	    if (j != iexclude) {
		if (mask == 0) {
		    GOOD(rm,nused) = DATA(rms,i)
		    nused = nused + 1
		} else if (ands (mask, SETMASK(rm,j)) == 0) {
		    GOOD(rm,nused) = DATA(rms,i)
		    nused = nused + 1
		}
	    }
	}

	if (nused > 2 && nclip > 0.) {
	    i = nused / 2
	    if (mod (nused, 2) == 0)
		med = (GOOD(rm,i) + GOOD(rm,i-1)) / 2
	    else
		med = GOOD(rm,i)
	    clip = med + nclip * (med - GOOD(rm,0))
	    do i = nused, 1, -1 {
		if (GOOD(rm,i-1) < clip)
		    break
	    }
	    nused = i
	}

	# Compute the interior average or median.
	if (nused > 2) {
	    for (i = 0; nused-2*i>max(2,navg); i=i+1)
		;
	    med = GOOD(rm,i)
	    do j = i+1, nused-i-1
		med = med + GOOD(rm,j)
	    nused = nused - 2 * i
	    med = med / nused
	} else if (nused == 0)
	    med = blank
	else if (nused == 1)
	    med = GOOD(rm,0)
	else
	    med = (GOOD(rm,0) + GOOD(rm,1)) / 2.

	return (med)
end


# RM_GDATA -- Get data value for specified index

real procedure rm_gdata (rm, index)

pointer	rm			#I RM pointer
int	index			#I Index of new data (one indexed)

int	i, j
pointer	rms

begin
	rms = RM_RMS(rm)
	i = mod (index-1, RM_BOX(rm))
	do j = 0, RM_BOX(rm)-1 {
	    if (IN(rms,j) == i)
	        return (DATA(rms,j))
	}
end



# RM_OPEN -- Open running median package.
#
# This is called once to allocate memory and initialize the algorithms.

pointer procedure rm_open (box, ndatasets, pixtype)

int	box			#I Median box size (<= 128)
int	ndatasets		#I Number of datasets
int	pixtype			#I Internal storage type
pointer	rm			#O RM pointer

int	i
short	s, nots(), shifts()
pointer	rms, rms_open()

begin
	# Open sorted running median method.
	rms = rms_open (box, 0.)

	# Set higher level data structure.
	if (pixtype == TY_SHORT)
	    i = TY_SHORT
	else
	    i = TY_REAL

	call calloc (rm, RM_LEN, TY_STRUCT)
	call calloc (RM_GOOD(rm), box, TY_REAL)
	call calloc (RM_PWIN(rm), box*ndatasets, i)
	call calloc (RM_POUT(rm), ndatasets*(box+1)/2, TY_SHORT)
	call calloc (RM_PMASK(rm), ndatasets*(box+15)/16, TY_SHORT)
	
	RM_RMS(rm) = rms
	RM_BOX(rm) = box
	RM_NDATA(rm) = ndatasets
	RM_PIXTYPE(rm) = i
	RM_MASK(rm) = RM_PMASK(rm)

	# Set mask flags.
	s = 1
	do i = 0, 15 {
	    SETMASK(rm,i) = s
	    UNSETMASK(rm,i) = nots (s)
	    s = shifts (s, short(1))
	}

	do i = 1, ndatasets
	    call rm_pack (rm, i)

	return (rm)
end


# RM_CLOSE -- Close running median package.

procedure rm_close (rm)

pointer	rm			#I RM pointer

begin
	call rms_close (RM_RMS(rm))

	call mfree (RM_GOOD(rm), TY_REAL)
	call mfree (RM_PWIN(rm), RM_PIXTYPE(rm))
	call mfree (RM_POUT(rm), TY_SHORT)
	call mfree (RM_PMASK(rm), TY_SHORT)
	call mfree (rm, TY_STRUCT)
end


# RM_PACK -- Pack data.

procedure rm_pack (rm, dataset)

pointer	rm			#I RM pointer
int	dataset			#I Data set

pointer	rms

begin
	rms = RM_RMS(rm)
	if (RM_PIXTYPE(rm) == TY_SHORT)
	    call anirs (DATA(rms,0), PWINS(rm,dataset), RM_BOX(rm))
#	else
#	    call amovr (DATA(rms,0), PWINR(rm,dataset), RM_BOX(rm))
	call achtsb (OUT(rms,0), POUT(rm,dataset), RM_BOX(rm))
end


# RM_UNPACK -- Unpack data.

procedure rm_unpack (rm, dataset)

pointer	rm			#I RM pointer
int	dataset			#I Data set

int	i, j, box
pointer	rms

begin
	rms = RM_RMS(rm)
	box = RM_BOX(rm)

	if (RM_PIXTYPE(rm) == TY_SHORT)
	    call achtsr (PWINS(rm,dataset), DATA(rms,0), box)
	else
	    RMS_DATA(rms) = RM_PWIN(rm) + box * (dataset - 1)
#	    call amovr (PWINR(rm,dataset), DATA(rms,0), box)
	call achtbs (POUT(rm,dataset), OUT(rms,0), box)
	RM_MASK(rm) = RM_PMASK(rm) + (box + 15) / 16 * (dataset - 1)

	do i = 0, box-1 {
	    j = OUT(rms,i)
	    IN(rms,j) = i
	}
end


# ANIRS -- Convert real to short using nearest integer.

procedure anirs (a, b, n)

real	a[n]			#I Input real array
short	b[n]			#O Output short array
int	n			#I Number of array values

int	i

begin
	do i = 1, n
	    b[i] = a[i] + 0.5
end


# RM_DUMP -- Dump data.

procedure rm_dump (rm, unsorted, sorted, in, out)

pointer	rm			#I Method pointer
bool	unsorted		#I Dump data in unsorted order?
bool	sorted			#I Dump data in sorted order?
bool	in			#I Dump in list?
bool	out			#I Dump out list?

begin
	call rms_dump (RM_RMS(rm), unsorted, sorted, in, out)
end
