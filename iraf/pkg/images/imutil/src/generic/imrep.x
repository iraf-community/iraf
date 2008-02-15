include <imhdr.h>
include	<mach.h>



# IMREP -- Replace pixels in an image between lower and upper by value.

procedure imreps (im, lower, upper, value, img)

pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf1, buf2
int	npix, junk
real	ilower
short	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnls(), impnls()
	    
bool	fp_equalr()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im, 1)
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnls (im, buf2, v2) != EOF)
	        call amovks (newval, Mems[buf2], npix)

	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    ceil = int (upper)
	    while (imgnls (im, buf1, v1) != EOF) {
		junk = impnls (im, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
		call arles (Mems[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    while (imgnls (im, buf1, v1) != EOF) {
		junk = impnls (im, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
		call arges (Mems[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = int (upper)
	    while (imgnls (im, buf1, v1) != EOF) {
		junk = impnls (im, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
		call areps (Mems[buf2], npix, floor, ceil, newval)
	    }
	}
end


# IMRREP -- Replace pixels in an image between lower and upper by value
# and a radius around those pixels.

procedure imrreps (im, lower, upper, radius, value, img)


pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	radius				# Radius
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf, buf1, buf2, ptr
int	i, j, k, l, nc, nl, nradius, nbufs
real	ilower
short	floor, ceil, newval, val1, val2
real	radius2, y2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]	# IMIO vectors
int	imgnls(), impnls()
bool	fp_equalr()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	nc = IM_LEN(im, 1)
	if (IM_NDIM(im) > 1)
	    nl = IM_LEN(im,2)
	else
	    nl = 1
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnls (im, buf2, v2) != EOF)
	        call amovks (newval, Mems[buf2], nc)
	    return
	
	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    floor = -MAX_SHORT
	    ceil = int (upper)

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = MAX_SHORT

	# Replace pixels between lower and upper by value.
	} else {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = int (upper)
	}

	# Initialize buffering.
	radius2 = radius * radius
	nradius = int (radius)
	nbufs = min (1 + 2 * nradius, nl)
	call calloc (buf, nc*nbufs, TY_SHORT)

	while (imgnls (im, buf1, v1) != EOF) {
	    j = v1[2] - 1
	    buf2 = buf + mod (j, nbufs) * nc
	    do i = 1, nc {
		val1 = Mems[buf1]
		val2 = Mems[buf2]
		if ((val1 >= floor) && (val1 <= ceil)) {
		    do k = max(1,j-nradius), min (nl,j+nradius) {
			ptr = buf + mod (k, nbufs) * nc - 1
			y2 = (k - j) ** 2
			do l = max(1,i-nradius), min (nc,i+nradius) {
			    if ((l-i)**2 + y2 > radius2)
				next
			    Mems[ptr+l] = INDEFS
			}
		    }
		} else {
		    if (!IS_INDEFS(val2))
			Mems[buf2] = val1
		}
		buf1 = buf1 + 1
		buf2 = buf2 + 1
	    }

	    if (j > nradius) {
		while (impnls (im, buf2, v2) != EOF) {
		    k = v2[2] - 1
		    buf1 = buf + mod (k, nbufs) * nc
		    do i = 1, nc {
			val1 = Mems[buf1]
			if (IS_INDEFS(Mems[buf1]))
			    Mems[buf2] = newval
			else
			    Mems[buf2] = val1
			Mems[buf1] = 0.
			buf1 = buf1 + 1
			buf2 = buf2 + 1
		    }
		    if (j != nl)
			break
		}
	    }
	}

	call mfree (buf, TY_SHORT)
end


# AREP -- Replace array values which are between floor and ceil by value.

procedure areps (a, npts, floor, ceil, newval)

short	a[npts]				# Input arrays
int	npts				# Number of points
short	floor, ceil			# Replacement limits
short	newval				# Replacement value

int	i

begin

	do i = 1, npts {
	    if ((a[i] >= floor) && (a[i] <= ceil))
		a[i] = newval
	}
end


# ARLE -- If A[i] is less than or equal to FLOOR replace by NEWVAL.

procedure arles (a, npts, floor, newval)

short	a[npts]
int	npts
short	floor, newval

int	i

begin

	do i = 1, npts
	    if (a[i] <= floor)
		a[i] = newval
end


# ARGE -- If A[i] is greater than or equal to CEIL replace by NEWVAL.

procedure arges (a, npts, ceil, newval)

short	a[npts]
int	npts
short	ceil, newval

int	i

begin

	do i = 1, npts
	    if (a[i] >= ceil)
		a[i] = newval
end



# IMREP -- Replace pixels in an image between lower and upper by value.

procedure imrepi (im, lower, upper, value, img)

pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf1, buf2
int	npix, junk
real	ilower
int	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnli(), impnli()
	    
bool	fp_equalr()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im, 1)
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnli (im, buf2, v2) != EOF)
	        call amovki (newval, Memi[buf2], npix)

	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    ceil = int (upper)
	    while (imgnli (im, buf1, v1) != EOF) {
		junk = impnli (im, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
		call arlei (Memi[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    while (imgnli (im, buf1, v1) != EOF) {
		junk = impnli (im, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
		call argei (Memi[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = int (upper)
	    while (imgnli (im, buf1, v1) != EOF) {
		junk = impnli (im, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
		call arepi (Memi[buf2], npix, floor, ceil, newval)
	    }
	}
end


# IMRREP -- Replace pixels in an image between lower and upper by value
# and a radius around those pixels.

procedure imrrepi (im, lower, upper, radius, value, img)


pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	radius				# Radius
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf, buf1, buf2, ptr
int	i, j, k, l, nc, nl, nradius, nbufs
real	ilower
int	floor, ceil, newval, val1, val2
real	radius2, y2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]	# IMIO vectors
int	imgnli(), impnli()
bool	fp_equalr()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	nc = IM_LEN(im, 1)
	if (IM_NDIM(im) > 1)
	    nl = IM_LEN(im,2)
	else
	    nl = 1
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnli (im, buf2, v2) != EOF)
	        call amovki (newval, Memi[buf2], nc)
	    return
	
	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    floor = -MAX_INT
	    ceil = int (upper)

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = MAX_INT

	# Replace pixels between lower and upper by value.
	} else {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = int (upper)
	}

	# Initialize buffering.
	radius2 = radius * radius
	nradius = int (radius)
	nbufs = min (1 + 2 * nradius, nl)
	call calloc (buf, nc*nbufs, TY_INT)

	while (imgnli (im, buf1, v1) != EOF) {
	    j = v1[2] - 1
	    buf2 = buf + mod (j, nbufs) * nc
	    do i = 1, nc {
		val1 = Memi[buf1]
		val2 = Memi[buf2]
		if ((val1 >= floor) && (val1 <= ceil)) {
		    do k = max(1,j-nradius), min (nl,j+nradius) {
			ptr = buf + mod (k, nbufs) * nc - 1
			y2 = (k - j) ** 2
			do l = max(1,i-nradius), min (nc,i+nradius) {
			    if ((l-i)**2 + y2 > radius2)
				next
			    Memi[ptr+l] = INDEFI
			}
		    }
		} else {
		    if (!IS_INDEFI(val2))
			Memi[buf2] = val1
		}
		buf1 = buf1 + 1
		buf2 = buf2 + 1
	    }

	    if (j > nradius) {
		while (impnli (im, buf2, v2) != EOF) {
		    k = v2[2] - 1
		    buf1 = buf + mod (k, nbufs) * nc
		    do i = 1, nc {
			val1 = Memi[buf1]
			if (IS_INDEFI(Memi[buf1]))
			    Memi[buf2] = newval
			else
			    Memi[buf2] = val1
			Memi[buf1] = 0.
			buf1 = buf1 + 1
			buf2 = buf2 + 1
		    }
		    if (j != nl)
			break
		}
	    }
	}

	call mfree (buf, TY_INT)
end


# AREP -- Replace array values which are between floor and ceil by value.

procedure arepi (a, npts, floor, ceil, newval)

int	a[npts]				# Input arrays
int	npts				# Number of points
int	floor, ceil			# Replacement limits
int	newval				# Replacement value

int	i

begin

	do i = 1, npts {
	    if ((a[i] >= floor) && (a[i] <= ceil))
		a[i] = newval
	}
end


# ARLE -- If A[i] is less than or equal to FLOOR replace by NEWVAL.

procedure arlei (a, npts, floor, newval)

int	a[npts]
int	npts
int	floor, newval

int	i

begin

	do i = 1, npts
	    if (a[i] <= floor)
		a[i] = newval
end


# ARGE -- If A[i] is greater than or equal to CEIL replace by NEWVAL.

procedure argei (a, npts, ceil, newval)

int	a[npts]
int	npts
int	ceil, newval

int	i

begin

	do i = 1, npts
	    if (a[i] >= ceil)
		a[i] = newval
end



# IMREP -- Replace pixels in an image between lower and upper by value.

procedure imrepl (im, lower, upper, value, img)

pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf1, buf2
int	npix, junk
real	ilower
long	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnll(), impnll()
	    
bool	fp_equalr()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im, 1)
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnll (im, buf2, v2) != EOF)
	        call amovkl (newval, Meml[buf2], npix)

	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    ceil = int (upper)
	    while (imgnll (im, buf1, v1) != EOF) {
		junk = impnll (im, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
		call arlel (Meml[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    while (imgnll (im, buf1, v1) != EOF) {
		junk = impnll (im, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
		call argel (Meml[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = int (upper)
	    while (imgnll (im, buf1, v1) != EOF) {
		junk = impnll (im, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
		call arepl (Meml[buf2], npix, floor, ceil, newval)
	    }
	}
end


# IMRREP -- Replace pixels in an image between lower and upper by value
# and a radius around those pixels.

procedure imrrepl (im, lower, upper, radius, value, img)


pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	radius				# Radius
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf, buf1, buf2, ptr
int	i, j, k, l, nc, nl, nradius, nbufs
real	ilower
long	floor, ceil, newval, val1, val2
real	radius2, y2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]	# IMIO vectors
int	imgnll(), impnll()
bool	fp_equalr()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	nc = IM_LEN(im, 1)
	if (IM_NDIM(im) > 1)
	    nl = IM_LEN(im,2)
	else
	    nl = 1
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnll (im, buf2, v2) != EOF)
	        call amovkl (newval, Meml[buf2], nc)
	    return
	
	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    floor = -MAX_LONG
	    ceil = int (upper)

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = MAX_LONG

	# Replace pixels between lower and upper by value.
	} else {
	    ilower = int (lower)
	    if (fp_equalr(lower,ilower))
	        floor = int (lower)
	    else
	        floor = int (lower+1.0)
	    ceil = int (upper)
	}

	# Initialize buffering.
	radius2 = radius * radius
	nradius = int (radius)
	nbufs = min (1 + 2 * nradius, nl)
	call calloc (buf, nc*nbufs, TY_LONG)

	while (imgnll (im, buf1, v1) != EOF) {
	    j = v1[2] - 1
	    buf2 = buf + mod (j, nbufs) * nc
	    do i = 1, nc {
		val1 = Meml[buf1]
		val2 = Meml[buf2]
		if ((val1 >= floor) && (val1 <= ceil)) {
		    do k = max(1,j-nradius), min (nl,j+nradius) {
			ptr = buf + mod (k, nbufs) * nc - 1
			y2 = (k - j) ** 2
			do l = max(1,i-nradius), min (nc,i+nradius) {
			    if ((l-i)**2 + y2 > radius2)
				next
			    Meml[ptr+l] = INDEFL
			}
		    }
		} else {
		    if (!IS_INDEFL(val2))
			Meml[buf2] = val1
		}
		buf1 = buf1 + 1
		buf2 = buf2 + 1
	    }

	    if (j > nradius) {
		while (impnll (im, buf2, v2) != EOF) {
		    k = v2[2] - 1
		    buf1 = buf + mod (k, nbufs) * nc
		    do i = 1, nc {
			val1 = Meml[buf1]
			if (IS_INDEFL(Meml[buf1]))
			    Meml[buf2] = newval
			else
			    Meml[buf2] = val1
			Meml[buf1] = 0.
			buf1 = buf1 + 1
			buf2 = buf2 + 1
		    }
		    if (j != nl)
			break
		}
	    }
	}

	call mfree (buf, TY_LONG)
end


# AREP -- Replace array values which are between floor and ceil by value.

procedure arepl (a, npts, floor, ceil, newval)

long	a[npts]				# Input arrays
int	npts				# Number of points
long	floor, ceil			# Replacement limits
long	newval				# Replacement value

int	i

begin

	do i = 1, npts {
	    if ((a[i] >= floor) && (a[i] <= ceil))
		a[i] = newval
	}
end


# ARLE -- If A[i] is less than or equal to FLOOR replace by NEWVAL.

procedure arlel (a, npts, floor, newval)

long	a[npts]
int	npts
long	floor, newval

int	i

begin

	do i = 1, npts
	    if (a[i] <= floor)
		a[i] = newval
end


# ARGE -- If A[i] is greater than or equal to CEIL replace by NEWVAL.

procedure argel (a, npts, ceil, newval)

long	a[npts]
int	npts
long	ceil, newval

int	i

begin

	do i = 1, npts
	    if (a[i] >= ceil)
		a[i] = newval
end



# IMREP -- Replace pixels in an image between lower and upper by value.

procedure imrepr (im, lower, upper, value, img)

pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf1, buf2
int	npix, junk
real	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()
	    

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im, 1)
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnlr (im, buf2, v2) != EOF)
	        call amovkr (newval, Memr[buf2], npix)

	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    ceil = double (upper)
	    while (imgnlr (im, buf1, v1) != EOF) {
		junk = impnlr (im, buf2, v2)
		call amovr (Memr[buf1], Memr[buf2], npix)
		call arler (Memr[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    while (imgnlr (im, buf1, v1) != EOF) {
		junk = impnlr (im, buf2, v2)
		call amovr (Memr[buf1], Memr[buf2], npix)
		call arger (Memr[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	    while (imgnlr (im, buf1, v1) != EOF) {
		junk = impnlr (im, buf2, v2)
		call amovr (Memr[buf1], Memr[buf2], npix)
		call arepr (Memr[buf2], npix, floor, ceil, newval)
	    }
	}
end


# IMRREP -- Replace pixels in an image between lower and upper by value
# and a radius around those pixels.

procedure imrrepr (im, lower, upper, radius, value, img)


pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	radius				# Radius
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf, buf1, buf2, ptr
int	i, j, k, l, nc, nl, nradius, nbufs
real	floor, ceil, newval, val1, val2
real	radius2, y2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]	# IMIO vectors
int	imgnlr(), impnlr()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	nc = IM_LEN(im, 1)
	if (IM_NDIM(im) > 1)
	    nl = IM_LEN(im,2)
	else
	    nl = 1
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnlr (im, buf2, v2) != EOF)
	        call amovkr (newval, Memr[buf2], nc)
	    return
	
	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    floor = -MAX_REAL
	    ceil = double (upper)

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    ceil = MAX_REAL

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	}

	# Initialize buffering.
	radius2 = radius * radius
	nradius = int (radius)
	nbufs = min (1 + 2 * nradius, nl)
	call calloc (buf, nc*nbufs, TY_REAL)

	while (imgnlr (im, buf1, v1) != EOF) {
	    j = v1[2] - 1
	    buf2 = buf + mod (j, nbufs) * nc
	    do i = 1, nc {
		val1 = Memr[buf1]
		val2 = Memr[buf2]
		if ((val1 >= floor) && (val1 <= ceil)) {
		    do k = max(1,j-nradius), min (nl,j+nradius) {
			ptr = buf + mod (k, nbufs) * nc - 1
			y2 = (k - j) ** 2
			do l = max(1,i-nradius), min (nc,i+nradius) {
			    if ((l-i)**2 + y2 > radius2)
				next
			    Memr[ptr+l] = INDEFR
			}
		    }
		} else {
		    if (!IS_INDEFR(val2))
			Memr[buf2] = val1
		}
		buf1 = buf1 + 1
		buf2 = buf2 + 1
	    }

	    if (j > nradius) {
		while (impnlr (im, buf2, v2) != EOF) {
		    k = v2[2] - 1
		    buf1 = buf + mod (k, nbufs) * nc
		    do i = 1, nc {
			val1 = Memr[buf1]
			if (IS_INDEFR(Memr[buf1]))
			    Memr[buf2] = newval
			else
			    Memr[buf2] = val1
			Memr[buf1] = 0.
			buf1 = buf1 + 1
			buf2 = buf2 + 1
		    }
		    if (j != nl)
			break
		}
	    }
	}

	call mfree (buf, TY_REAL)
end


# AREP -- Replace array values which are between floor and ceil by value.

procedure arepr (a, npts, floor, ceil, newval)

real	a[npts]				# Input arrays
int	npts				# Number of points
real	floor, ceil			# Replacement limits
real	newval				# Replacement value

int	i

begin

	do i = 1, npts {
	    if ((a[i] >= floor) && (a[i] <= ceil))
		a[i] = newval
	}
end


# ARLE -- If A[i] is less than or equal to FLOOR replace by NEWVAL.

procedure arler (a, npts, floor, newval)

real	a[npts]
int	npts
real	floor, newval

int	i

begin

	do i = 1, npts
	    if (a[i] <= floor)
		a[i] = newval
end


# ARGE -- If A[i] is greater than or equal to CEIL replace by NEWVAL.

procedure arger (a, npts, ceil, newval)

real	a[npts]
int	npts
real	ceil, newval

int	i

begin

	do i = 1, npts
	    if (a[i] >= ceil)
		a[i] = newval
end



# IMREP -- Replace pixels in an image between lower and upper by value.

procedure imrepd (im, lower, upper, value, img)

pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf1, buf2
int	npix, junk
double	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnld(), impnld()
	    

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im, 1)
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnld (im, buf2, v2) != EOF)
	        call amovkd (newval, Memd[buf2], npix)

	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    ceil = double (upper)
	    while (imgnld (im, buf1, v1) != EOF) {
		junk = impnld (im, buf2, v2)
		call amovd (Memd[buf1], Memd[buf2], npix)
		call arled (Memd[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    while (imgnld (im, buf1, v1) != EOF) {
		junk = impnld (im, buf2, v2)
		call amovd (Memd[buf1], Memd[buf2], npix)
		call arged (Memd[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	    while (imgnld (im, buf1, v1) != EOF) {
		junk = impnld (im, buf2, v2)
		call amovd (Memd[buf1], Memd[buf2], npix)
		call arepd (Memd[buf2], npix, floor, ceil, newval)
	    }
	}
end


# IMRREP -- Replace pixels in an image between lower and upper by value
# and a radius around those pixels.

procedure imrrepd (im, lower, upper, radius, value, img)


pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	radius				# Radius
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf, buf1, buf2, ptr
int	i, j, k, l, nc, nl, nradius, nbufs
double	floor, ceil, newval, val1, val2
real	radius2, y2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]	# IMIO vectors
int	imgnld(), impnld()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	nc = IM_LEN(im, 1)
	if (IM_NDIM(im) > 1)
	    nl = IM_LEN(im,2)
	else
	    nl = 1
	newval = double (value)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnld (im, buf2, v2) != EOF)
	        call amovkd (newval, Memd[buf2], nc)
	    return
	
	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    floor = -MAX_DOUBLE
	    ceil = double (upper)

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    ceil = MAX_DOUBLE

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	}

	# Initialize buffering.
	radius2 = radius * radius
	nradius = int (radius)
	nbufs = min (1 + 2 * nradius, nl)
	call calloc (buf, nc*nbufs, TY_DOUBLE)

	while (imgnld (im, buf1, v1) != EOF) {
	    j = v1[2] - 1
	    buf2 = buf + mod (j, nbufs) * nc
	    do i = 1, nc {
		val1 = Memd[buf1]
		val2 = Memd[buf2]
		if ((val1 >= floor) && (val1 <= ceil)) {
		    do k = max(1,j-nradius), min (nl,j+nradius) {
			ptr = buf + mod (k, nbufs) * nc - 1
			y2 = (k - j) ** 2
			do l = max(1,i-nradius), min (nc,i+nradius) {
			    if ((l-i)**2 + y2 > radius2)
				next
			    Memd[ptr+l] = INDEFD
			}
		    }
		} else {
		    if (!IS_INDEFD(val2))
			Memd[buf2] = val1
		}
		buf1 = buf1 + 1
		buf2 = buf2 + 1
	    }

	    if (j > nradius) {
		while (impnld (im, buf2, v2) != EOF) {
		    k = v2[2] - 1
		    buf1 = buf + mod (k, nbufs) * nc
		    do i = 1, nc {
			val1 = Memd[buf1]
			if (IS_INDEFD(Memd[buf1]))
			    Memd[buf2] = newval
			else
			    Memd[buf2] = val1
			Memd[buf1] = 0.
			buf1 = buf1 + 1
			buf2 = buf2 + 1
		    }
		    if (j != nl)
			break
		}
	    }
	}

	call mfree (buf, TY_DOUBLE)
end


# AREP -- Replace array values which are between floor and ceil by value.

procedure arepd (a, npts, floor, ceil, newval)

double	a[npts]				# Input arrays
int	npts				# Number of points
double	floor, ceil			# Replacement limits
double	newval				# Replacement value

int	i

begin

	do i = 1, npts {
	    if ((a[i] >= floor) && (a[i] <= ceil))
		a[i] = newval
	}
end


# ARLE -- If A[i] is less than or equal to FLOOR replace by NEWVAL.

procedure arled (a, npts, floor, newval)

double	a[npts]
int	npts
double	floor, newval

int	i

begin

	do i = 1, npts
	    if (a[i] <= floor)
		a[i] = newval
end


# ARGE -- If A[i] is greater than or equal to CEIL replace by NEWVAL.

procedure arged (a, npts, ceil, newval)

double	a[npts]
int	npts
double	ceil, newval

int	i

begin

	do i = 1, npts
	    if (a[i] >= ceil)
		a[i] = newval
end



# IMREP -- Replace pixels in an image between lower and upper by value.

procedure imrepx (im, lower, upper, value, img)

pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf1, buf2
int	npix, junk
complex	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlx(), impnlx()
	    

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im, 1)
	newval = complex (value, img)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnlx (im, buf2, v2) != EOF)
	        call amovkx (newval, Memx[buf2], npix)

	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    ceil = double (upper)
	    while (imgnlx (im, buf1, v1) != EOF) {
		junk = impnlx (im, buf2, v2)
		call amovx (Memx[buf1], Memx[buf2], npix)
		call arlex (Memx[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    while (imgnlx (im, buf1, v1) != EOF) {
		junk = impnlx (im, buf2, v2)
		call amovx (Memx[buf1], Memx[buf2], npix)
		call argex (Memx[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	    while (imgnlx (im, buf1, v1) != EOF) {
		junk = impnlx (im, buf2, v2)
		call amovx (Memx[buf1], Memx[buf2], npix)
		call arepx (Memx[buf2], npix, floor, ceil, newval)
	    }
	}
end


# IMRREP -- Replace pixels in an image between lower and upper by value
# and a radius around those pixels.

procedure imrrepx (im, lower, upper, radius, value, img)


pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	radius				# Radius
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf, buf1, buf2, ptr
int	i, j, k, l, nc, nl, nradius, nbufs
complex	floor, ceil, newval, val1, val2
real	abs_floor, abs_ceil
real	radius2, y2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]	# IMIO vectors
int	imgnlx(), impnlx()

begin
	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	nc = IM_LEN(im, 1)
	if (IM_NDIM(im) > 1)
	    nl = IM_LEN(im,2)
	else
	    nl = 1
	newval = complex (value, img)

	# If both lower and upper are INDEF then replace all pixels by value.
	if (IS_INDEFR (lower) && IS_INDEFR (upper)) {
	    while (impnlx (im, buf2, v2) != EOF)
	        call amovkx (newval, Memx[buf2], nc)
	    return
	
	# If lower is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (lower)) {
	    floor = 0
	    ceil = real (upper)
	    abs_floor = abs (floor)
	    abs_ceil = abs (ceil)

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = real (lower)
	    ceil = MAX_REAL
	    abs_floor = abs (floor)
	    abs_ceil = abs (ceil)

	# Replace pixels between lower and upper by value.
	} else {
	    floor = real (lower)
	    ceil = real (upper)
	    abs_floor = abs (floor)
	    abs_ceil = abs (ceil)
	}

	# Initialize buffering.
	radius2 = radius * radius
	nradius = int (radius)
	nbufs = min (1 + 2 * nradius, nl)
	call calloc (buf, nc*nbufs, TY_COMPLEX)

	while (imgnlx (im, buf1, v1) != EOF) {
	    j = v1[2] - 1
	    buf2 = buf + mod (j, nbufs) * nc
	    do i = 1, nc {
		val1 = Memx[buf1]
		val2 = Memx[buf2]
		if ((abs (val1) >= abs_floor) && (abs (val1) <= abs_ceil)) {
		    do k = max(1,j-nradius), min (nl,j+nradius) {
			ptr = buf + mod (k, nbufs) * nc - 1
			y2 = (k - j) ** 2
			do l = max(1,i-nradius), min (nc,i+nradius) {
			    if ((l-i)**2 + y2 > radius2)
				next
			    Memx[ptr+l] = INDEFX
			}
		    }
		} else {
		    if (!IS_INDEFX(val2))
			Memx[buf2] = val1
		}
		buf1 = buf1 + 1
		buf2 = buf2 + 1
	    }

	    if (j > nradius) {
		while (impnlx (im, buf2, v2) != EOF) {
		    k = v2[2] - 1
		    buf1 = buf + mod (k, nbufs) * nc
		    do i = 1, nc {
			val1 = Memx[buf1]
			if (IS_INDEFX(Memx[buf1]))
			    Memx[buf2] = newval
			else
			    Memx[buf2] = val1
			Memx[buf1] = 0.
			buf1 = buf1 + 1
			buf2 = buf2 + 1
		    }
		    if (j != nl)
			break
		}
	    }
	}

	call mfree (buf, TY_COMPLEX)
end


# AREP -- Replace array values which are between floor and ceil by value.

procedure arepx (a, npts, floor, ceil, newval)

complex	a[npts]				# Input arrays
int	npts				# Number of points
complex	floor, ceil			# Replacement limits
complex	newval				# Replacement value

int	i
real	abs_floor
real	abs_ceil

begin
	abs_floor = abs (floor)
	abs_ceil = abs (ceil)

	do i = 1, npts {
	    if ((abs (a[i]) >= abs_floor) && (abs (a[i]) <= abs_ceil))
		a[i] = newval
	}
end


# ARLE -- If A[i] is less than or equal to FLOOR replace by NEWVAL.

procedure arlex (a, npts, floor, newval)

complex	a[npts]
int	npts
complex	floor, newval

int	i
real	abs_floor

begin
	abs_floor = abs (floor)

	do i = 1, npts
	    if (abs (a[i]) <= abs_floor)
		a[i] = newval
end


# ARGE -- If A[i] is greater than or equal to CEIL replace by NEWVAL.

procedure argex (a, npts, ceil, newval)

complex	a[npts]
int	npts
complex	ceil, newval

int	i
real	abs_ceil

begin
	abs_ceil = abs (ceil)

	do i = 1, npts
	    if (abs (a[i]) >= abs_ceil)
		a[i] = newval
end


