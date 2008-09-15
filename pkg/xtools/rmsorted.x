include	<pkg/rmsorted.h>


# RMSORTED -- Compute running median value.

real procedure rmsorted (rm, nclip, index, data)

pointer	rm			#I Method pointer
real	nclip			#I Clipping factor
int	index			#I Index of new data
real	data			#I Input data  value
real	med			#R Return value

int	i, i1, box, outnext, out
real	clip

begin
	box = RMS_BOX(rm)
	outnext = mod (index-1, box)
	out = OUT(rm,outnext)

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

	DATA(rm,i) = data
	IN(rm,i) = outnext
	OUT(rm,outnext) = i

	i1 = box / 2
	if (mod (box, 2) == 0)
	    med = (DATA(rm,i1) + DATA(rm,i1-1)) / 2
	else
	    med = DATA(rm,i1)

	if (nclip > 0.) {
	    clip = med + nclip * (med - DATA(rm,0))
	    do i = box, 1, -1 {
		if (DATA(rm,i-1) < clip)
		    break
	    }
	    if (i < box) {
		i1 = i / 2
		if (mod (i, 2) == 0)
		    med = (DATA(rm,i1) + DATA(rm,i1-1)) / 2
		else
		    med = DATA(rm,i1)
	    }
	}

	return (med)
end


# RMS_OPEN -- Open running median sorted algorithm.

pointer procedure rms_open (box, data)

int	box			#I Running median box
real	data			#I Initial data value
pointer	rm			#R Method pointer

int	i

begin
	call malloc (rm, RMS_LEN(box), TY_STRUCT)
	RMS_BOX(rm) = box
	RMS_DATA(rm) = rm + RMS_OFFSET
	RMS_IN(rm) = P2S(RMS_DATA(rm) + box)
	RMS_OUT(rm) = RMS_IN(rm) + box

	do i = 0, box-1 {
	    DATA(rm,i) = data
	    IN(rm,i) = i
	    OUT(rm,i) = i
	}

	return (rm)
end


# RMS_CLOSE -- Close running median sorted algorithm.

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
