# SMOOTH -- Box smooth the array

procedure smooth (y, n)

real	y[ARB]
int	n

int	i, j, boxsize, halfbox, del
int	nsum
real	sum
pointer	sp, smy

int	clgeti()

begin
	call smark (sp)
	call salloc (smy, n, TY_REAL)

	# Get boxsize
	boxsize = clgeti ("boxsize")
	if (mod (boxsize, 2) == 0) {
	    boxsize = boxsize + 1
	    call eprintf ("WARNING: Using a box size of %d")
		call pargi (boxsize)
	}

	halfbox = boxsize/2

	# This is not efficiently coded, but easy to code
	# A running box mean would be faster
	do i = 1, n {
	    sum = 0.0
	    nsum = 0

	    if (i > halfbox && i < (n-halfbox))
		del = halfbox
	    else
	    if (i <= halfbox)
		del = i/2
	    else
		del = (n - i + 1)/2

	    do j = i-del, i+del {
		nsum = nsum + 1
		sum = sum + y[j]
	    }

	    Memr[smy+i-1] = sum / nsum
	}

	# Replace pixels back
	call amovr (Memr[smy], y, n)
	call sfree (sp)
end
