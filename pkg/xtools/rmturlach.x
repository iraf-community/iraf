# Turlach -- Running median library.
#
# The algorithm is that described by Haerdle und Steiger (1995) and the
# implementation is after Turlach.  The starting point was the GNU General
# Pubic Licensed code from the R Foundation (see copyright heritage below).
# Besides the language recoding the structure has been significantly changed.
#
# Copyright (C) 1995   Berwin A. Turlach <berwin@alphasun.anu.edu.au>
# Copyright (C) 2000-2 Martin Maechler <maechler@stat.math.ethz.ch>
# Copyright (C) 2003   The R Foundation

include	<mach.h>

define	RMT_OFFSET	4			# Offset to data
define	RMT_LEN		(RMT_OFFSET+5*$1+3)	# Structure length
define	RMT_BOX		Memi[$1]		# Running box size
define	RMT_DATA	Memi[$1+1]		# Sorted data (ptr)
define	RMT_IN		Memi[$1+2]		# Mapping to input (ptr)
define	RMT_OUT		Memi[$1+3]		# Mapping to output (ptr)

define	DATA		Memr[RMT_DATA($1)+$2]
define	IN		Mems[RMT_IN($1)+$2]
define	OUT		Mems[RMT_OUT($1)+$2]


# RMTURLACH -- Compute running median value using the Turlach algorithm.

real procedure rmturlach (rm, index, data)

pointer	rm			#I Method pointer
int	index			#I Index of new data
real	data			#I Input data value

short	nrnew, box, outnext, out, leaf, one
data	one/1/

begin
	nrnew = index - 1
	box = RMT_BOX(rm)
	outnext = mod (nrnew, box)
	out = OUT(rm,outnext)
	DATA(rm,out) = data

	leaf = out - box
	if (out > box) {
	    if (data >= DATA(rm,box))
		call rm_uoui (leaf, box, DATA(rm,1), OUT(rm,1), IN(rm,1))
	    else
		call rm_uodi (leaf, box, nrnew, outnext, data,
		    DATA(rm,1), OUT(rm,1), IN(rm,1))
	} else if (out < box) {
	    if (data < DATA(rm,box))
		call rm_dodi (leaf, box, DATA(rm,1), OUT(rm,1), IN(rm,1))
	    else
		call rm_doui (leaf, box, nrnew, outnext, data,
		    DATA(rm,1), OUT(rm,1), IN(rm,1))
	} else if (DATA(rm,box) > DATA(rm,box+1)) {
	    call rm_swap (box, box+one, DATA(rm,1), OUT(rm,1), IN(rm,1));
	    call rm_uptoleaf (one, box, DATA(rm,1), OUT(rm,1), IN(rm,1));
	} else if (DATA(rm,box) < DATA(rm,box-1)) {
	    call rm_swap (box, box-one, DATA(rm,1), OUT(rm,1), IN(rm,1));
	    call rm_downtoleaf (-one, box, DATA(rm,1), OUT(rm,1), IN(rm,1));
	}

	return (DATA(rm,box))
end


# RMT_OPEN -- Open Turlach running median algorithm.

pointer procedure rmt_open (box, data)

int	box			#I Running median box
real	data			#I Initial data value
pointer	rm			#R Method pointer

short	i, halfbox
#short	i, j, k, halfbox, one
#data	one/1/

begin
	call malloc (rm, RMT_LEN(box), TY_STRUCT)

	RMT_BOX(rm) = box
	RMT_DATA(rm) = rm + RMT_OFFSET
	RMT_IN(rm) = P2S(RMT_DATA(rm) + 2 * box + 1)
	RMT_OUT(rm) = RMT_IN(rm) + 2 * box + 1

	halfbox = (box - 1) / 2

	do i = 1+halfbox, box+halfbox {
	    DATA(rm,i) = data
	    IN(rm,i) = i-halfbox-1
	    OUT(rm,i-halfbox-1) = i
	}

	do i = 0, halfbox {
	    DATA(rm,i) = -MAX_REAL
	    DATA(rm,i+box+halfbox+1) = MAX_REAL
	}

	return (rm)
end


# RMT_CLOSE -- Close Turlach running median algorithm.

procedure rmt_close (rm)

pointer	rm			#I Method pointer

begin
	call mfree (rm, TY_STRUCT)
end


# RMT_DUMP -- Dump data structure.

procedure rmt_dump (rm, unsorted, sorted, in, out)

pointer	rm			#I Method pointer
bool	unsorted		#I Dump data in unsorted order?
bool	sorted			#I Dump data in sorted order?
bool	in			#I Dump in list?
bool	out			#I Dump out list?

int	i, box, halfbox

begin
	box = RMT_BOX(rm)
	halfbox = box / 2
	if (unsorted) {
	    do i = 1+halfbox, halfbox+box {
		call eprintf (" %3.0f")
		    call pargr (DATA(rm,OUT(rm,i-halfbox-1)))
	    }
	    call eprintf ("\n")
	}
	if (sorted) {
	    #do i = 0, 2*box {
	    do i = 1+halfbox, halfbox+box {
		call eprintf (" %3.0f")
		    call pargr (DATA(rm,i))
	    }
	    call eprintf ("\n")
	}
	if (in) {
	    #do i = 0, 2*box {
	    do i = 1+halfbox, halfbox+box {
		call eprintf (" %3d")
		    call pargs (IN(rm,i))
	    }
	    call eprintf ("\n")
	}
	if (out) {
	    do i = 0, box-1 {
		call eprintf (" %3d")
		    call pargs (OUT(rm,i))
	    }
	    call eprintf ("\n")
	}
end


# RM_SWAP -- Swap positions `l' and `r'.

procedure rm_swap (l, r, window, outlist, nrlist)

short	l			#I Index to swap
short	r			#I Index to swap
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	nl, nr
real	w

begin
	w = window[l]; window[l] = window[r]; window[r] = w
	nl = nrlist[l]; nr = nrlist[r]; nrlist[l] = nr; nrlist[r] = nl
	outlist[nl] = r; outlist[nr] = l
end


# RM_SIFTUP -- Used only in the initial sorting.

procedure rm_siftup (l, r, window, outlist, nrlist)

short	l			#I Left index
short	r			#I Right index
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	i, j, nrold
real	w

begin
	i = l
	j = 2 * i
	w = window[i]
	nrold = nrlist[i]
	while (j <= r) {
	    if (j < r) {
	        if (window[j] < window[j+1])
		    j = j + 1
	    }
	    if (w >= window[j])
	        break

	    window[i] = window[j]
	    outlist[nrlist[j]] = i
	    nrlist[i] = nrlist[j]
	    i = j
	    j = 2 * i
	}

	window[i] = w
	outlist[nrold] = i
	nrlist[i] = nrold
end


# RM_UOUI - Upper Out Upper In

procedure rm_uoui (leaf, box, window, outlist, nrlist)

short	leaf			#I Leaf
short	box			#I Box size
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	i, j, k

begin
	call rm_uptoleaf (leaf, box, window, outlist, nrlist)

	i = leaf
	j = i + box
	k = i / 2 + box
	while (window[j] < window[k]) {
	    call rm_swap (j, k, window, outlist, nrlist)
	    i = (k - box)
	    j = i + box
	    k = i / 2 + box
	}
end


# RM_DODI - Down Out Down In

procedure rm_dodi (leaf, box, window, outlist, nrlist)

short	leaf			#I Leaf
short	box			#I Box size
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	i, j, k

begin
	call rm_downtoleaf (leaf, box, window, outlist, nrlist)

	i = leaf
	j = i + box
	k = i / 2 + box
	while (window[j] > window[k]) {
	    call rm_swap (j, k, window, outlist, nrlist)
	    i = (k - box)
	    j = i + box
	    k = i / 2 + box
	}
end


# RM_UODI -- Upper Out Down In

procedure rm_uodi (leaf, box, nrnew, outnext, in, window, outlist, nrlist)

short	leaf			#I Leaf
short	box			#I Box size
short	nrnew			#I nrnew
short	outnext			#I outnext
real	in			#I Input value
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	one
data	one/1/

begin
	call rm_toroot (leaf, box, nrnew, outnext, in, window, outlist,
	    nrlist)

	if (window[box] < window[box-1]) {
	    call rm_swap (box, box-one, window, outlist, nrlist)
	    call rm_downtoleaf (-one, box, window, outlist, nrlist)
	}
end


# RM_DOUI -- Down Out Upper In

procedure rm_doui (leaf, box, nrnew, outnext, in, window, outlist, nrlist)

short	leaf			#I Leaf
short	box			#I Box size
short	nrnew			#I nrnew
short	outnext			#I outnext
real	in			#I Input value
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	one
data	one/1/

begin
	call rm_toroot (leaf, box, nrnew, outnext, in, window, outlist,
	    nrlist)

	if (window[box] > window[box+1]) {
	    call rm_swap (box, box+one, window, outlist, nrlist)
	    call rm_uptoleaf (one, box, window, outlist, nrlist)
	}
end

# RM_TOROOT

procedure rm_toroot (leaf, box, nrnew, outnext, in, window, outlist, nrlist)

short	leaf			#I Leaf
short	box			#I Box size
short	nrnew			#I nrnew
short	outnext			#I outnext
real	in			#I Input value
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	i, j, k

begin
	i = leaf
	repeat {
	    j = i + box
	    k = i / 2 + box
	    window[j] = window[k]
	    outlist[nrlist[k]] = j
	    nrlist[j] = nrlist[k]
	    i = k - box
	} until (i == 0)

	window[box] = in
	outlist[outnext] = box
	nrlist[box] = outnext
end


# RM_DOWNTOLEAF

procedure rm_downtoleaf (leaf, box, window, outlist, nrlist)

short	leaf			#I Leaf
short	box			#I Box size
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	i, j, childl, childr

begin
	i = leaf
	repeat {
	    j = i + box
	    childl = 2 * i + box
	    childr = childl - 1
	    if (window[childl] < window[childr])
	        childl = childr
	    if (window[j] >= window[childl])
	        break
	    call rm_swap (j, childl, window, outlist, nrlist)
	    i = childl - box
	}
end


# RM_UPTOLEAF

procedure rm_uptoleaf (leaf, box, window, outlist, nrlist)

short	leaf			#I Leaf
short	box			#I Box size
real	window[ARB]		#U Work array
short	outlist[ARB]		#U Work array
short	nrlist[ARB]		#U Work array

short	i, j, childl, childr

begin
	i = leaf
	repeat {
	    j = i + box
	    childl = 2 * i + box
	    childr = childl + 1
	    if (window[childl] > window[childr])
	        childl = childr
	    if (window[j] <= window[childl])
	        break
	    call rm_swap (j, childl, window, outlist, nrlist)
	    i = childl - box
	}
end

