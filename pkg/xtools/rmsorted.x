include	<pkg/rmsorted.h>


# RMSORTED -- Compute running sorted value.

real procedure rmsorted (rm, nclip, index, data)

pointer	rm			#I Method pointer
real	nclip			#I Clipping factor
int	index			#I Index of new data
real	data			#I Input data  value
real	val			#R Return value

int	i, i1, box, outnext, out, nused
real	clip

begin
	# Extract from structure.
	box = RMS_BOX(rm)
	outnext = mod (index-1, box)
	out = OUT(rm,outnext)

	# Find value to replace.
	if (out == 0) {
	    do i = out, box-2 {
	        i1 = i + 1
	        if (data <= DATA(rm,i1))
		    break
		DATA(rm,i) = DATA(rm,i1)
		IN(rm,i) = IN(rm,i1)
		OUT(rm,IN(rm,i)) = i
	    }
	} else if (out == box-1) {
	    do i = out, 1, -1 {
	        i1 = i - 1
	        if (data >= DATA(rm,i1))
		    break
		DATA(rm,i) = DATA(rm,i1)
		IN(rm,i) = IN(rm,i1)
		OUT(rm,IN(rm,i)) = i
	    }
	} else if (data > DATA(rm,out+1)) {
	    do i = out, box-2 {
	        i1 = i + 1
	        if (data <= DATA(rm,i1))
		    break
		DATA(rm,i) = DATA(rm,i1)
		IN(rm,i) = IN(rm,i1)
		OUT(rm,IN(rm,i)) = i
	    }
	} else {
	    do i = out, 1, -1 {
	        i1 = i - 1
	        if (data >= DATA(rm,i1))
		    break
		DATA(rm,i) = DATA(rm,i1)
		IN(rm,i) = IN(rm,i1)
		OUT(rm,IN(rm,i)) = i
	    }
	}

	# Set new value.
	DATA(rm,i) = data
	IN(rm,i) = outnext
	OUT(rm,outnext) = i

	# Apply clipping if needed.
	nused = box
	if (nused > 2 && nclip > 0.) {
	    i = nused / 2
	    if (mod (nused, 2) == 0)
		val = (DATA(rm,i) + DATA(rm,i-1)) / 2
	    else
		val = DATA(rm,i)
	    clip = val + nclip * (val - DATA(rm,0))
	    do i = nused, 1, -1 {
		if (DATA(rm,i-1) < clip)
		    break
	    }
	    nused = i
	}

	# Compute output value.
	switch (RMS_TYPE(rm)) {
	case RMS_TYMED:
	    i = nused / 2
	    if (mod (nused, 2) == 0)
		val = (DATA(rm,i) + DATA(rm,i-1)) / 2
	    else
		val = DATA(rm,i)
	case RMS_TYMAX:
	    val = DATA(rm,nused-1)
	case RMS_TYMIN:
	    val = DATA(rm,0)
	}

	return (val)
end


# RMS_OPEN -- Open running sorted algorithm.

pointer procedure rms_open (box, type, data)

int	box			#I Running box
int	type			#I Output type
real	data			#I Initial data value
pointer	rm			#R Method pointer

int	i

begin
	call malloc (rm, RMS_LEN(box), TY_STRUCT)
	RMS_BOX(rm) = box
	RMS_TYPE(rm) = type
	RMS_DATA(rm) = rm + RMS_OFFSET
	RMS_IN(rm) = P2S(RMS_DATA(rm) + box)
	RMS_OUT(rm) = RMS_IN(rm) + box
	RMS_DATA(rm) = P2R(RMS_DATA(rm))

	do i = 0, box-1 {
	    DATA(rm,i) = data
	    IN(rm,i) = i
	    OUT(rm,i) = i
	}

	return (rm)
end


# RMS_CLOSE -- Close running sorted algorithm.

procedure rms_close (rm)

pointer	rm			#I Method pointer

begin
	call mfree (rm, TY_STRUCT)
end


# RMS_DUMP -- Dump data structure.

procedure rms_dump (rm, unsorted, sorted, in, out)

pointer	rm			#I RM pointer
bool	unsorted		#I Dump data in input order?
bool	sorted			#I Dump data in sorted order?
bool	in			#I Dump in list?
bool	out			#I Dump out list?

int	i

begin
	if (unsorted) {
	    do i = 0, RMS_BOX(rm)-1 {
		call printf (" %7.3f")
		    call pargr (DATA(rm,OUT(rm,i)))
	    }
	    call printf ("\n")
	}
	if (sorted) {
	    do i = 0, RMS_BOX(rm)-1 {
		call printf (" %7.3f")
		    call pargr (DATA(rm,i))
	    }
	    call printf ("\n")
	}
	if (in) {
	    do i = 0, RMS_BOX(rm)-1 {
		call printf (" %3d")
		    call pargs (IN(rm,i))
	    }
	    call printf ("\n")
	}
	if (out) {
	    do i = 0, RMS_BOX(rm)-1 {
		call printf (" %3d")
		    call pargs (OUT(rm,i))
	    }
	    call printf ("\n")
	}
end
