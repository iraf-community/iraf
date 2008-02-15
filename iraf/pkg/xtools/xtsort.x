# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# XT_SORT2 -- Sort 2 dimensional vectors by the first component.

procedure xt_sort2 (a1, a2, npts)

real	a1[npts], a2[npts]		# Arrays to be sorted
int	npts				# Number of points

int	i, j
pointer	sp, index, ptr

int	xts_compare()
extern	xts_compare

begin
	call smark (sp)
	call salloc (index, npts, TY_INT)
	call salloc (ptr, npts, TY_REAL)

	do i = 1, npts
	    Memi[index+i-1] = ptr + i - 1

	call amovr (a1, Memr[ptr], npts)

	call qsort (Memi[index], npts, xts_compare)

	do i = 1, npts {
	    j = Memi[index+i-1]
	    a1[i] = Memr[j]
	}

	call amovr (a2, Memr[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a2[i] = Memr[j]
	}

	call sfree (sp)
end


# XT_SORT3 -- Sort 3 dimensional vectors by the first component.

procedure xt_sort3 (a1, a2, a3, npts)

real	a1[npts], a2[npts], a3[npts]	# Arrays to be sorted
int	npts				# Number of points

int	i, j
pointer	sp, index, ptr

int	xts_compare()
extern	xts_compare

begin
	call smark (sp)
	call salloc (index, npts, TY_INT)
	call salloc (ptr, npts, TY_REAL)

	do i = 1, npts
	    Memi[index+i-1] = ptr + i - 1

	call amovr (a1, Memr[ptr], npts)

	call qsort (Memi[index], npts, xts_compare)

	do i = 1, npts {
	    j = Memi[index+i-1]
	    a1[i] = Memr[j]
	}

	call amovr (a2, Memr[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a2[i] = Memr[j]
	}

	call amovr (a3, Memr[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a3[i] = Memr[j]
	}

	call sfree (sp)
end


# XT_SORT4 -- Sort 4 dimensional vectors by the first component.

procedure xt_sort4 (a1, a2, a3, a4, npts)

real	a1[npts], a2[npts], a3[npts], a4[npts]	# Arrays to be sorted
int	npts					# Number of points

int	i, j
pointer	sp, index, ptr

int	xts_compare()
extern	xts_compare

begin
	call smark (sp)
	call salloc (index, npts, TY_INT)
	call salloc (ptr, npts, TY_REAL)

	do i = 1, npts
	    Memi[index+i-1] = ptr + i - 1

	call amovr (a1, Memr[ptr], npts)

	call qsort (Memi[index], npts, xts_compare)

	do i = 1, npts {
	    j = Memi[index+i-1]
	    a1[i] = Memr[j]
	}

	call amovr (a2, Memr[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a2[i] = Memr[j]
	}

	call amovr (a3, Memr[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a3[i] = Memr[j]
	}

	call amovr (a4, Memr[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a4[i] = Memr[j]
	}

	call sfree (sp)
end


# XTS_COMPARE -- Compare two real values in the Memr array.

int procedure xts_compare (i, j)

pointer	i, j		# Array indices to be compared.

begin
	if (Memr[i] < Memr[j])
	    return (-1)
	else if (Memr[i] > Memr[j])
	    return (1)
	else
	    return (0)
end


# XT_SORT3D -- Sort 3 double precision vectors by the first component.

procedure xt_sort3d (a1, a2, a3, npts)

double	a1[npts], a2[npts], a3[npts]	# Arrays to be sorted
int	npts				# Number of points

int	i, j
pointer	sp, index, ptr

int	xts_compared()
extern	xts_compared

begin
	call smark (sp)
	call salloc (index, npts, TY_INT)
	call salloc (ptr, npts, TY_DOUBLE)

	do i = 1, npts
	    Memi[index+i-1] = ptr + i - 1

	call amovd (a1, Memd[ptr], npts)

	call qsort (Memi[index], npts, xts_compared)

	do i = 1, npts {
	    j = Memi[index+i-1]
	    a1[i] = Memd[j]
	}

	call amovd (a2, Memd[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a2[i] = Memd[j]
	}

	call amovd (a3, Memd[ptr], npts)
	do i = 1, npts {
	    j = Memi[index+i-1]
	    a3[i] = Memd[j]
	}

	call sfree (sp)
end


# XTS_COMPARED -- Compare two double values in the Memd array.

int procedure xts_compared (i, j)

pointer	i, j		# Array indices to be compared.

begin
	if (Memd[i] < Memd[j])
	    return (-1)
	else if (Memd[i] > Memd[j])
	    return (1)
	else
	    return (0)
end
