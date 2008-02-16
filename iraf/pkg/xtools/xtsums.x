# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# XT_LSUM -- Sum lines
#
# A new sum vector is created when the data pointer is null or if the number
# of columns is changed.  If the previous sum overlaps the requested sum then
# additions and subtractions are performed on the previous sum to minimize
# the number of arithmetic operations.

procedure xt_lsum (im, col1, col2, line1, line2, data)

pointer	im		# IMIO pointer
int	col1, col2	# Column limits of the sum
int	line1, line2	# Line limits
pointer	data		# Data pointer

int	i
int	ncols, nlines, nc, nl, c1, c2, l1, l2
pointer	j

pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	if ((data == NULL) || (ncols != nc)) {
	    call mfree (data, TY_REAL)
	    call malloc (data, ncols, TY_REAL)
	    nc = ncols
	    l1 = 0
	    l2 = 0
	}

	if (nlines != nl) {
	    nl = nlines
	    l1 = 0
	    l2 = 0
	}

	# If only one line then don't bother with summing.

	if (nlines == 1) {
	    if ((line1 != l1) || (col1 != c1) || (col2 != c2)) {
	        c1 = col1
	        c2 = col2
	        l1 = line1
	        l2 = line2
		j = imgs2r (im, c1, c2, l1, l2)
	        call amovr (Memr[j], Memr[data], nc)
	    }
	    return
	}

	# If the sum limits are outside the last sum limits then form
	# the sums from scratch.

	if ((line1 > l2) || (line2 < l1) || (col1 != c1) || (col2 != c2)) {
	    c1 = col1
	    c2 = col2
	    l1 = line1
	    l2 = line2
	    call aclrr (Memr[data], nc)
	    do i = l1, l2 {
		j = imgs2r (im, c1, c2, i, i)
		call aaddr (Memr[data], Memr[j], Memr[data], nc)
	    }

	# If the sum limits overlap then add and subtract to compute the
	# new sums from the previous sums.  This minimizes the number of
	# arithmetic operations in common applications.

	} else if (line1 > l1) {
	    do i = l1, line1 - 1 {
		j = imgs2r (im, c1, c2, i, i)
		call asubr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    do i = l2 + 1, line2 {
		j = imgs2r (im, c1, c2, i, i)
		call aaddr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    l1 = line1
	    l2 = line2

	} else {
	    do i = line2 + 1, l2 {
		j = imgs2r (im, c1, c2, i, i)
		call asubr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    do i = line1, l1 - 1 {
		j = imgs2r (im, c1, c2, i, i)
		call aaddr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    l1 = line1
	    l2 = line2
	}
end


# XT_CSUM -- Sum columns
#
# A new sum vector is created when the data pointer is null or if the number
# of lines is changed.  If the previous sum overlaps the requested sum then
# additions and subtractions are performed on the previous sum to minimize
# the number of arithmetic operations.

procedure xt_csum (co, col1, col2, line1, line2, data)

pointer	co		# COIO pointer
int	col1, col2	# Column limits of the sum
int	line1, line2	# Line limits
pointer	data		# Data pointer

int	i
int	ncols, nlines, nc, nl, c1, c2, l1, l2
pointer	j

pointer	cogetr()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	if ((data == NULL) || (nlines != nl)) {
	    call mfree (data, TY_REAL)
	    call malloc (data, nlines, TY_REAL)
	    nl = nlines
	    c1 = 0
	    c2 = 0
	}

	if (ncols != nc) {
	    nc = ncols
	    c1 = 0
	    c2 = 0
	}

	# If only one column then don't bother with summing.

	if (ncols == 1) {
	    if ((col1 != c1) || (line1 != l1) || (line2 != l2)) {
	        c1 = col1
	        c2 = col2
	        l1 = line1
	        l2 = line2
		j = cogetr (co, c1, l1, l2)
	        call amovr (Memr[j], Memr[data], nl)
	    }
	    return
	}

	# If the sum limits are outside the last sum limits then form
	# the sums from scratch.

	if ((col1 > c2) || (col2 < c1) || (line1 != l1) || (line2 != l2)) {
	    c1 = col1
	    c2 = col2
	    l1 = line1
	    l2 = line2
	    call aclrr (Memr[data], nlines)
	    do i = c1, c2 {
		j = cogetr (co, i, l1, l2)
		call aaddr (Memr[data], Memr[j], Memr[data], nl)
	    }

	# If the sum limits overlap then add and subtract to compute the
	# new sums from the previous sums.  This minimizes the number of
	# arithmetic operations in common applications.

	} else if (col1 > c1) {
	    do i = c1, col1 - 1 {
		j = cogetr (co, i, l1, l2)
		call asubr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    do i = c2 + 1, col2 {
		j = cogetr (co, i, l1, l2)
		call aaddr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    c1 = col1
	    c2 = col2

	} else {
	    do i = col2 + 1, c2 {
		j = cogetr (co, i, l1, l2)
		call asubr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    do i = col1, c1 - 1 {
		j = cogetr (co, i, l1, l2)
		call aaddr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    c1 = col1
	    c2 = col2
	}
end


# XT_LSUMB -- Sum lines with buffering
#
# A new sum vector is created when the data pointer is null or if the number
# of columns is changed.  If the previous sum overlaps the requested sum then
# additions and subtractions are performed on the previous sum to minimize
# the number of arithmetic operations.  Buffering of previous lines is done.

procedure xt_lsumb (im, col1, col2, line1, line2, data)

pointer	im		# IMIO pointer
int	col1, col2	# Column limits of the sum
int	line1, line2	# Line limits
pointer	data		# Data pointer

int	i
int	ncols, nlines, nc, nl, c1, c2, l1, l2
pointer	j

pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	if ((data == NULL) || (ncols != nc)) {
	    call mfree (data, TY_REAL)
	    call malloc (data, (nlines + 1) * ncols, TY_REAL)
	    nc = ncols
	    l1 = 0
	    l2 = 0
	}

	if (nlines != nl) {
	    nl = nlines
	    l1 = 0
	    l2 = 0
	}

	# If only one line then don't bother with summing.

	if (nlines == 1) {
	    if ((line1 != l1) || (col1 != c1) || (col2 != c2)) {
	        c1 = col1
	        c2 = col2
	        l1 = line1
	        l2 = line2
		j = imgs2r (im, c1, c2, l1, l2)
	        call amovr (Memr[j], Memr[data], nc)
	    }
	    return
	}

	# If the sum limits are outside the last sum limits then form
	# the sums from scratch.

	if ((line1 > l2) || (line2 < l1) || (col1 != c1) || (col2 != c2)) {
	    c1 = col1
	    c2 = col2
	    l1 = line1
	    l2 = line2
	    call aclrr (Memr[data], nc)
	    do i = l1, l2 {
		j = data + (mod (i, nl) + 1) * nc
		call amovr (Memr[imgs2r (im, c1, c2, i, i)], Memr[j], nc)
		call aaddr (Memr[data], Memr[j], Memr[data], nc)
	    }

	# If the sum limits overlap then add and subtract to compute the
	# new sums from the previous sums.  This minimizes the number of
	# arithmetic operations in common applications.

	} else if (line1 > l1) {
	    do i = l1, line1 - 1 {
		j = data + (mod (i, nl) + 1) * nc
		call asubr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    do i = l2 + 1, line2 {
		j = data + (mod (i, nl) + 1) * nc
		call amovr (Memr[imgs2r (im, c1, c2, i, i)], Memr[j], nc)
		call aaddr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    l1 = line1
	    l2 = line2

	} else {
	    do i = line2 + 1, l2 {
		j = data + (mod (i, nl) + 1) * nc
		call asubr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    do i = line1, l1 - 1 {
		j = data + (mod (i, nl) + 1) * nc
		call amovr (Memr[imgs2r (im, c1, c2, i, i)], Memr[j], nc)
		call aaddr (Memr[data], Memr[j], Memr[data], nc)
	    }
	    l1 = line1
	    l2 = line2
	}
end


# XT_CSUMB -- Sum columns with buffering
#
# A new sum vector is created when the data pointer is null or if the number
# of lines is changed.  If the previous sum overlaps the requested sum then
# additions and subtractions are performed on the previous sum to minimize
# the number of arithmetic operations.  Buffering is done on the previous cols.

procedure xt_csumb (co, col1, col2, line1, line2, data)

pointer	co		# COIO pointer
int	col1, col2	# Column limits of the sum
int	line1, line2	# Line limits
pointer	data		# Data pointer

int	i
int	ncols, nlines, nc, nl, c1, c2, l1, l2
pointer	j

pointer	cogetr()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	if ((data == NULL) || (nlines != nl)) {
	    call mfree (data, TY_REAL)
	    call malloc (data, (ncols + 1) * nlines, TY_REAL)
	    nl = nlines
	    c1 = 0
	    c2 = 0
	}

	if (ncols != nc) {
	    nc = ncols
	    c1 = 0
	    c2 = 0
	}

	# If only one column then don't bother with summing.

	if (ncols == 1) {
	    if ((col1 != c1) || (line1 != l1) || (line2 != l2)) {
	        c1 = col1
	        c2 = col2
	        l1 = line1
	        l2 = line2
		j = cogetr (co, c1, l1, l2)
	        call amovr (Memr[j], Memr[data], nl)
	    }
	    return
	}

	# If the sum limits are outside the last sum limits then form
	# the sums from scratch.

	if ((col1 > c2) || (col2 < c1) || (line1 != l1) || (line2 != l2)) {
	    c1 = col1
	    c2 = col2
	    l1 = line1
	    l2 = line2
	    call aclrr (Memr[data], nlines)
	    do i = c1, c2 {
		j = data + (mod (i, nc) + 1) * nl
		call amovr (Memr[cogetr (co, i, l1, l2)], Memr[j], nl)
		call aaddr (Memr[data], Memr[j], Memr[data], nl)
	    }

	# If the sum limits overlap then add and subtract to compute the
	# new sums from the previous sums.  This minimizes the number of
	# arithmetic operations in common applications.

	} else if (col1 > c1) {
	    do i = c1, col1 - 1 {
		j = data + (mod (i, nc) + 1) * nl
		call asubr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    do i = c2 + 1, col2 {
		j = data + (mod (i, nc) + 1) * nl
		call amovr (Memr[cogetr (co, i, l1, l2)], Memr[j], nl)
		call aaddr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    c1 = col1
	    c2 = col2

	} else {
	    do i = col2 + 1, c2 {
		j = data + (mod (i, nc) + 1) * nl
		call asubr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    do i = col1, c1 - 1 {
		j = data + (mod (i, nc) + 1) * nl
		call amovr (Memr[cogetr (co, i, l1, l2)], Memr[j], nl)
		call aaddr (Memr[data], Memr[j], Memr[data], nl)
	    }
	    c1 = col1
	    c2 = col2
	}
end
