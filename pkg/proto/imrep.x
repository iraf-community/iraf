include <imhdr.h>



# IMREP -- Replace pixels in an image between lower and upper by value.

procedure imreps (im, lower, upper, value, img)

pointer	im				# Image descriptor
real	lower, upper			# Range to be replaced
real	value				# Replacement value
real	img				# Imaginary value for complex

pointer	buf1, buf2
int	npix, junk
short	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnls(), impnls()

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
	    ceil = double (upper)
	    while (imgnls (im, buf1, v1) != EOF) {
		junk = impnls (im, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
		call arles (Mems[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    while (imgnls (im, buf1, v1) != EOF) {
		junk = impnls (im, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
		call arges (Mems[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	    while (imgnls (im, buf1, v1) != EOF) {
		junk = impnls (im, buf2, v2)
		call amovs (Mems[buf1], Mems[buf2], npix)
		call areps (Mems[buf2], npix, floor, ceil, newval)
	    }
	}
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
int	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnli(), impnli()

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
	    ceil = double (upper)
	    while (imgnli (im, buf1, v1) != EOF) {
		junk = impnli (im, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
		call arlei (Memi[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    while (imgnli (im, buf1, v1) != EOF) {
		junk = impnli (im, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
		call argei (Memi[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	    while (imgnli (im, buf1, v1) != EOF) {
		junk = impnli (im, buf2, v2)
		call amovi (Memi[buf1], Memi[buf2], npix)
		call arepi (Memi[buf2], npix, floor, ceil, newval)
	    }
	}
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
long	floor, ceil, newval
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnll(), impnll()

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
	    ceil = double (upper)
	    while (imgnll (im, buf1, v1) != EOF) {
		junk = impnll (im, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
		call arlel (Meml[buf2], npix, ceil, newval)
	    }

	# If upper is INDEF then all pixels below upper are replaced by value.
	} else if (IS_INDEFR (upper)) {
	    floor = double (lower)
	    while (imgnll (im, buf1, v1) != EOF) {
		junk = impnll (im, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
		call argel (Meml[buf2], npix, floor, newval)
	    }

	# Replace pixels between lower and upper by value.
	} else {
	    floor = double (lower)
	    ceil = double (upper)
	    while (imgnll (im, buf1, v1) != EOF) {
		junk = impnll (im, buf2, v2)
		call amovl (Meml[buf1], Meml[buf2], npix)
		call arepl (Meml[buf2], npix, floor, ceil, newval)
	    }
	}
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
