include <imhdr.h>
include <mach.h>
include <math.h>



# IF_LOG10 -- Compute the base 10 logarithm of image1 and write the results to
# image2.

procedure if_log10r (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
real	if_elogr()
extern	if_elogr()
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call alogr (Memr[buf1], Memr[buf2], npix, if_elogr)
end


# IF_ELOG -- The error function for log10. Note that MAX_EXPONENT is
# currently an integer so it is converted to the appropriate data type
# before being returned.

real procedure if_elogr (x)

real	x				# the input pixel value

begin
	return (real(-MAX_EXPONENT))
end


# IF_ALOG10 -- Take the power of 10 of image1 and write the results to image2.

procedure if_alog10r (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_va10r (Memr[buf1], Memr[buf2], npix)
end


# IF_VA10 -- Take the antilog (base 10) of a vector.

procedure if_va10r (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of points

int	i
real	maxexp, maxval

begin
	maxexp = MAX_EXPONENT
	maxval = MAX_REAL

	do i = 1, n {
	   if (a[i] >= maxexp)
		b[i] = maxval
	   else if (a[i] <= (-maxexp))
		b[i] = 0.0
	   else
		b[i] = 10.0 ** a[i]
	}
end


# IF_LN -- Take the natural log of the pixels in image1 and write the results
# to image2. 

procedure if_lnr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2

real	if_elnr()
extern	if_elnr()
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call allnr (Memr[buf1], Memr[buf2], npix, if_elnr)
end


# IF_ELN -- The error function for the natural logarithm.

real	procedure if_elnr (x)

real	x				# input value

begin
	return (real (LN_10) * real(-MAX_EXPONENT))
end


# IF_ALN -- Take the natural antilog of the pixels in image1 and write the
# results to image2. 

procedure if_alnr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_valnr (Memr[buf1], Memr[buf2], npix)
end


# IF_VALN -- Take the natural antilog of a vector.

procedure if_valnr (a, b, n)

real	a[n]			# the input vector
real	b[n]			# the output vector
int	n			# the number of pixels

int	i
real	maxexp, maxval, eval

begin
	maxexp = log (10.0 ** real (MAX_EXPONENT))
	maxval = MAX_REAL
	eval = real (BASE_E)

	do i = 1, n {
	   if (a[i] >= maxexp)
		b[i] = maxval
	   else if (a[i] <= -maxexp)
		b[i] = 0.0
	   else
		b[i] = eval ** a[i]
	}
end


# IF_SQR -- Take the square root of pixels in image1 and write the results
# to image2.

procedure if_sqrr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
real	if_esqrr()
extern	if_esqrr()
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call asqrr (Memr[buf1], Memr[buf2], npix, if_esqrr)
end


# IF_ESQR -- Error function for the square root.

real procedure if_esqrr (x)

real	x			        # input value

begin
	return (0.0)
end


# IF_SQUARE -- Take the square of the pixels in image1 and write to image2. 
procedure if_squarer (im1, im2)

pointer	im1				# the input image pointer
pointer	im2				# the output image pointer

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call apowkr (Memr[buf1], 2, Memr[buf2], npix)
end


# IF_CBRT -- Take the cube root of the pixels in image1 and write the results
# to image2. 

procedure if_cbrtr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vcbrtr (Memr[buf1], Memr[buf2], npix)
end


# IF_VCBRT -- Compute the cube root of a vector.

procedure if_vcbrtr (a, b, n)

real	a[n]			# the input vector
real	b[n]			# the output vector
int	n			# the number of pixels

int	i
real	onethird

begin
	onethird = 1.0 / 3.0
	do i = 1, n {
	    if (a[i] >= 0.0) {
	        b[i] = a[i] ** onethird
	    } else {
		b[i] = -a[i]
	        b[i] = - (b[i] ** onethird)
	    }
	}
end


# IF_CUBE  -- Take the cube of the pixels in image1 and write the results to
# image2. 

procedure if_cuber (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call apowkr (Memr[buf1], 3, Memr[buf2], npix)
end


# IF_COS -- Take cosine of pixels in image1 and write the results to image2.

procedure if_cosr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vcosr (Memr[buf1], Memr[buf2], npix)
end


# IF_VCOS - Compute the cosine of a vector.

procedure if_vcosr (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n
	    b[i] = cos(a[i])
end


# IF_SIN -- Take sine of the pixels in image1 and write the results to image2.

procedure if_sinr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vsinr (Memr[buf1], Memr[buf2], npix)
end


# IF_VSIN - Take the sine of a vector.

procedure if_vsinr (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n
	    b[i] = sin(a[i])
end


# IF_TAN -- Take tangent of pixels in image1 and write the results to image2.

procedure if_tanr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vtanr (Memr[buf1], Memr[buf2], npix)
end


# IF_VTAN - Take the tangent of a vector.

procedure if_vtanr (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n 
	    b[i] = tan(a[i])
end


# IF_ACOS -- Take arccosine of pixels in image1 and write the results to image2.

procedure if_acosr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vacosr (Memr[buf1], Memr[buf2], npix)
end


# IF_VACOS - Take the arccosine of a vector.

procedure if_vacosr (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n {
	    if (a[i] > 1.0)
		b[i] = acos (1.0)
	    else if (a[i] < -1.0)
		b[i] = acos (-1.0)
	    else
	        b[i] = acos(a[i])
	}
end


# IF_ASIN -- Take arcsine of pixels in image1 and write the results to image2.

procedure if_asinr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vasinr (Memr[buf1], Memr[buf2], npix)
end


# IF_VASIN - Take arcsine of vector

procedure if_vasinr (a, b, n)

real	a[n]
real	b[n]
int	n

int	i

begin
	do i = 1, n {
	    if (a[i] > 1.0)
		b[i] = asin (1.0)
	    else if (a[i] < -1.0)
		b[i] = asin (-1.0)
	    else
	        b[i] = asin(a[i])
	}
end


# IF_ATAN -- Take arctangent of pixels in image1 and write the results to
# image2.

procedure if_atanr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vatanr (Memr[buf1], Memr[buf2], npix)
end


# IF_VATAN - Take the arctangent of a vector.

procedure if_vatanr (a, b, n)

real	a[n]
real	b[n]
int	n

int	i

begin
	do i = 1, n
	    b[i] = atan(a[i])
end


# IF_HCOS -- Take the hyperbolic cosine of pixels in image1 and write the
# results to image2.

procedure if_hcosr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vhcosr (Memr[buf1], Memr[buf2], npix)
end


# IF_VHCOS - Take the hyperbolic cosine of a vector.

procedure if_vhcosr (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of pixels

int	i
real	maxexp, maxval

begin
	maxexp = log (10.0 ** real(MAX_EXPONENT))
	maxval = MAX_REAL

	do i = 1, n {
	    if (abs (a[i]) >= maxexp)
		b[i] = maxval
	    else
	        b[i] = cosh (a[i])
	}
end


# IF_HSIN -- Take the hyperbolic sine of pixels in image1 and write the
# results to image2.

procedure if_hsinr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vhsinr (Memr[buf1], Memr[buf2], npix)
end


# IF_VHSIN - Take the hyperbolic sine of a vector.

procedure if_vhsinr (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of pixels

int	i
real	maxexp, maxval

begin
	maxexp = log (10.0 ** real(MAX_EXPONENT))
	maxval = MAX_REAL

	do i = 1, n {
	    if (a[i] >= maxexp)
		b[i] = maxval
	    else if (a[i] <= -maxexp)
		b[i] = -maxval
	    else
	        b[i] = sinh(a[i])
	}
end


# IF_HTAN -- Take the hyperbolic tangent of pixels in image1 and write the
# results to image2.

procedure if_htanr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call if_vhtanr (Memr[buf1], Memr[buf2], npix)
end


# IF_VHTAN - Take the hyperbolic tangent of a vector.

procedure if_vhtanr (a, b, n)

real	a[n]				# the input vector
real	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n
	    b[i] = tanh(a[i])
end


# IF_RECIP -- Take the reciprocal of the pixels in image1 and write the 
# results to image2. 

procedure if_recipr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
real	if_erecipr()
extern	if_erecipr()
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call arczr (1.0, Memr[buf1], Memr[buf2], npix, if_erecipr)
end


# IF_ERECIP -- Error function for the reciprocal computation.

real procedure if_erecipr (x)

real	x

begin
	return (0.0)
end



# IF_LOG10 -- Compute the base 10 logarithm of image1 and write the results to
# image2.

procedure if_log10d (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
double	if_elogd()
extern	if_elogd()
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call alogd (Memd[buf1], Memd[buf2], npix, if_elogd)
end


# IF_ELOG -- The error function for log10. Note that MAX_EXPONENT is
# currently an integer so it is converted to the appropriate data type
# before being returned.

double procedure if_elogd (x)

double	x				# the input pixel value

begin
	return (double(-MAX_EXPONENT))
end


# IF_ALOG10 -- Take the power of 10 of image1 and write the results to image2.

procedure if_alog10d (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_va10d (Memd[buf1], Memd[buf2], npix)
end


# IF_VA10 -- Take the antilog (base 10) of a vector.

procedure if_va10d (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of points

int	i
double	maxexp, maxval

begin
	maxexp = MAX_EXPONENT
	maxval = MAX_REAL

	do i = 1, n {
	   if (a[i] >= maxexp)
		b[i] = maxval
	   else if (a[i] <= (-maxexp))
		b[i] = 0.0D0
	   else
		b[i] = 10.0D0 ** a[i]
	}
end


# IF_LN -- Take the natural log of the pixels in image1 and write the results
# to image2. 

procedure if_lnd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2

double	if_elnd()
extern	if_elnd()
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call allnd (Memd[buf1], Memd[buf2], npix, if_elnd)
end


# IF_ELN -- The error function for the natural logarithm.

double	procedure if_elnd (x)

double	x				# input value

begin
	return (double (LN_10) * double(-MAX_EXPONENT))
end


# IF_ALN -- Take the natural antilog of the pixels in image1 and write the
# results to image2. 

procedure if_alnd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_valnd (Memd[buf1], Memd[buf2], npix)
end


# IF_VALN -- Take the natural antilog of a vector.

procedure if_valnd (a, b, n)

double	a[n]			# the input vector
double	b[n]			# the output vector
int	n			# the number of pixels

int	i
double	maxexp, maxval, eval

begin
	maxexp = log (10.0D0 ** double (MAX_EXPONENT))
	maxval = MAX_REAL
	eval = double (BASE_E)

	do i = 1, n {
	   if (a[i] >= maxexp)
		b[i] = maxval
	   else if (a[i] <= -maxexp)
		b[i] = 0.0D0
	   else
		b[i] = eval ** a[i]
	}
end


# IF_SQR -- Take the square root of pixels in image1 and write the results
# to image2.

procedure if_sqrd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
double	if_esqrd()
extern	if_esqrd()
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call asqrd (Memd[buf1], Memd[buf2], npix, if_esqrd)
end


# IF_ESQR -- Error function for the square root.

double procedure if_esqrd (x)

double	x			        # input value

begin
	return (0.0D0)
end


# IF_SQUARE -- Take the square of the pixels in image1 and write to image2. 
procedure if_squared (im1, im2)

pointer	im1				# the input image pointer
pointer	im2				# the output image pointer

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call apowkd (Memd[buf1], 2, Memd[buf2], npix)
end


# IF_CBRT -- Take the cube root of the pixels in image1 and write the results
# to image2. 

procedure if_cbrtd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vcbrtd (Memd[buf1], Memd[buf2], npix)
end


# IF_VCBRT -- Compute the cube root of a vector.

procedure if_vcbrtd (a, b, n)

double	a[n]			# the input vector
double	b[n]			# the output vector
int	n			# the number of pixels

int	i
double	onethird

begin
	onethird = 1.0D0 / 3.0D0
	do i = 1, n {
	    if (a[i] >= 0.0D0) {
	        b[i] = a[i] ** onethird
	    } else {
		b[i] = -a[i]
	        b[i] = - (b[i] ** onethird)
	    }
	}
end


# IF_CUBE  -- Take the cube of the pixels in image1 and write the results to
# image2. 

procedure if_cubed (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call apowkd (Memd[buf1], 3, Memd[buf2], npix)
end


# IF_COS -- Take cosine of pixels in image1 and write the results to image2.

procedure if_cosd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vcosd (Memd[buf1], Memd[buf2], npix)
end


# IF_VCOS - Compute the cosine of a vector.

procedure if_vcosd (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n
	    b[i] = cos(a[i])
end


# IF_SIN -- Take sine of the pixels in image1 and write the results to image2.

procedure if_sind (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vsind (Memd[buf1], Memd[buf2], npix)
end


# IF_VSIN - Take the sine of a vector.

procedure if_vsind (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n
	    b[i] = sin(a[i])
end


# IF_TAN -- Take tangent of pixels in image1 and write the results to image2.

procedure if_tand (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vtand (Memd[buf1], Memd[buf2], npix)
end


# IF_VTAN - Take the tangent of a vector.

procedure if_vtand (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n 
	    b[i] = tan(a[i])
end


# IF_ACOS -- Take arccosine of pixels in image1 and write the results to image2.

procedure if_acosd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vacosd (Memd[buf1], Memd[buf2], npix)
end


# IF_VACOS - Take the arccosine of a vector.

procedure if_vacosd (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n {
	    if (a[i] > 1.0D0)
		b[i] = acos (1.0D0)
	    else if (a[i] < -1.0D0)
		b[i] = acos (-1.0D0)
	    else
	        b[i] = acos(a[i])
	}
end


# IF_ASIN -- Take arcsine of pixels in image1 and write the results to image2.

procedure if_asind (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vasind (Memd[buf1], Memd[buf2], npix)
end


# IF_VASIN - Take arcsine of vector

procedure if_vasind (a, b, n)

double	a[n]
double	b[n]
int	n

int	i

begin
	do i = 1, n {
	    if (a[i] > 1.0D0)
		b[i] = asin (1.0D0)
	    else if (a[i] < -1.0D0)
		b[i] = asin (-1.0D0)
	    else
	        b[i] = asin(a[i])
	}
end


# IF_ATAN -- Take arctangent of pixels in image1 and write the results to
# image2.

procedure if_atand (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vatand (Memd[buf1], Memd[buf2], npix)
end


# IF_VATAN - Take the arctangent of a vector.

procedure if_vatand (a, b, n)

double	a[n]
double	b[n]
int	n

int	i

begin
	do i = 1, n
	    b[i] = atan(a[i])
end


# IF_HCOS -- Take the hyperbolic cosine of pixels in image1 and write the
# results to image2.

procedure if_hcosd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vhcosd (Memd[buf1], Memd[buf2], npix)
end


# IF_VHCOS - Take the hyperbolic cosine of a vector.

procedure if_vhcosd (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of pixels

int	i
double	maxexp, maxval

begin
	maxexp = log (10.0D0 ** double(MAX_EXPONENT))
	maxval = MAX_REAL

	do i = 1, n {
	    if (abs (a[i]) >= maxexp)
		b[i] = maxval
	    else
	        b[i] = cosh (a[i])
	}
end


# IF_HSIN -- Take the hyperbolic sine of pixels in image1 and write the
# results to image2.

procedure if_hsind (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vhsind (Memd[buf1], Memd[buf2], npix)
end


# IF_VHSIN - Take the hyperbolic sine of a vector.

procedure if_vhsind (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of pixels

int	i
double	maxexp, maxval

begin
	maxexp = log (10.0D0 ** double(MAX_EXPONENT))
	maxval = MAX_REAL

	do i = 1, n {
	    if (a[i] >= maxexp)
		b[i] = maxval
	    else if (a[i] <= -maxexp)
		b[i] = -maxval
	    else
	        b[i] = sinh(a[i])
	}
end


# IF_HTAN -- Take the hyperbolic tangent of pixels in image1 and write the
# results to image2.

procedure if_htand (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
pointer	buf1, buf2
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call if_vhtand (Memd[buf1], Memd[buf2], npix)
end


# IF_VHTAN - Take the hyperbolic tangent of a vector.

procedure if_vhtand (a, b, n)

double	a[n]				# the input vector
double	b[n]				# the output vector
int	n				# the number of pixels

int	i

begin
	do i = 1, n
	    b[i] = tanh(a[i])
end


# IF_RECIP -- Take the reciprocal of the pixels in image1 and write the 
# results to image2. 

procedure if_recipd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
double	if_erecipd()
extern	if_erecipd()
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call arczd (1.0, Memd[buf1], Memd[buf2], npix, if_erecipd)
end


# IF_ERECIP -- Error function for the reciprocal computation.

double procedure if_erecipd (x)

double	x

begin
	return (0.0D0)
end





# IF_ABS -- Take the absolute value of pixels in image1 and write the results
# to image2.

procedure if_absl (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnll(), impnll()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnll (im1, buf1, v1) != EOF) &&
	    (impnll (im2, buf2, v2) != EOF))
	    call aabsl (Meml[buf1], Meml[buf2], npix)
end


# IF_NEG -- Take negative of pixels in image1 and write the results to image2. 

procedure if_negl (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnll(), impnll()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnll (im1, buf1, v1) != EOF) &&
	    (impnll (im2, buf2, v2) != EOF))
	    call anegl (Meml[buf1], Meml[buf2], npix)
end



# IF_ABS -- Take the absolute value of pixels in image1 and write the results
# to image2.

procedure if_absr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call aabsr (Memr[buf1], Memr[buf2], npix)
end


# IF_NEG -- Take negative of pixels in image1 and write the results to image2. 

procedure if_negr (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnlr(), impnlr()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnlr (im1, buf1, v1) != EOF) &&
	    (impnlr (im2, buf2, v2) != EOF))
	    call anegr (Memr[buf1], Memr[buf2], npix)
end



# IF_ABS -- Take the absolute value of pixels in image1 and write the results
# to image2.

procedure if_absd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call aabsd (Memd[buf1], Memd[buf2], npix)
end


# IF_NEG -- Take negative of pixels in image1 and write the results to image2. 

procedure if_negd (im1, im2)

pointer	im1				# pointer to the input image
pointer	im2				# pointer to the output image

int	npix
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	buf1, buf2
int	imgnld(), impnld()

begin
	call amovkl (long(1), v1, IM_MAXDIM)
	call amovkl (long(1), v2, IM_MAXDIM)

	npix = IM_LEN(im1, 1)
	while ((imgnld (im1, buf1, v1) != EOF) &&
	    (impnld (im2, buf2, v2) != EOF))
	    call anegd (Memd[buf1], Memd[buf2], npix)
end


