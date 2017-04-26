include <math.h>
include <mach.h>

# COMPLEX.X  - File containing utility routines for complex arithmetic.

# CX_ADD - Addition of complex numbers.

procedure cx_add (ar, ai, br, bi, cr, ci)

real	ar, ai					#I First number
real	br, bi					#I Second number
real	cr, ci					#O Computed value

begin
	cr = ar + br
	ci = ai + bi
end


# CX_SUB - Subtraction of complex numbers.

procedure cx_sub (ar, ai, br, bi, cr, ci)

real	ar,ai					#I First number
real	br,bi					#I Second number
real	cr,ci					#O Computed value

begin
	cr = ar - br
	ci = ai - bi
end


# CX_MUL - Multiplication of complex numbers.

procedure cx_mul (ar, ai, br, bi, cr, ci)

real	ar,ai					#I First number
real	br,bi					#I Second number
real	cr,ci					#O Computed value

begin
	cr = ar*br - ai*bi
	ci = ai*br + ar*bi
end


# CX_DIV - Division of complex numbers.

procedure cx_div  (ar, ai, br, bi, cr, ci)

real	ar,ai					#I First number
real	br,bi					#I Second number
real	cr,ci					#O Computed value

real	r, den

begin
	if (br == 0.0 && bi == 0.0) {		# Trap divide by zero
	    cr = 0.0
	    ci = 0.0
	    return
	}

	if (abs(br) >= abs(bi)) {
	    r = bi / br
	    den = br + r*bi
	    cr = (ar + r*ai) / den
	    ci = (ai - r*ar) / den
	} else {
	    r = br / bi
	    den = bi + r*br
	    cr = (ar*r + ai) / den
	    ci = (ai*r - ar) / den
	}
end


# CX_ABS - Absolute value of complex numbers.

real procedure cx_abs (ar, ai)

real	ar, ai					#I First number

real	x, y, ans, temp

begin
	x = abs (ar)
	y = abs (ai)
	if (x == 0.0)
	    ans = y
	else if (y == 0.0)
	    ans = x
	else if (x > y) {
	    temp = y / x
	    ans = x * sqrt (1.0 + temp*temp)
	} else {
	    temp = x / y
	    ans = y * sqrt (1.0 + temp*temp)
	}

	return (ans)
end


# CX_CONJG - Complex conjugate.

procedure cx_conjg (ar, ai, br, bi)

real	ar,ai					#I First number
real	br,bi					#I Conjugate

begin
	br =   ar
	bi = - ai
end


# CX_SQRT - Square root of complex numbers.

procedure cx_sqrt (ar, ai, br, bi)

real	ar, ai					#I First number
real	br, bi					#I Square root

real	x, y, w, r

begin
	if (ar == 0.0 && ai == 0.0) {
	    br = 0.0
	    bi = 0.0
	} else {
	    x = abs (ar)
	    y = abs (ai)
	    if (x >= y) {
		r = y / x
		w = sqrt (x) * sqrt (0.5*(1.0+sqrt(1.0+r*r)))
	    } else {
		r = x / y
		w = sqrt (y) * sqrt (0.5*(1.0+sqrt(1.0+r*r)))
	    }
	    if (ar >= 0.0) {
		br = w
		bi = ai / (2.0*w)
	    } else {
		if (ai >= 0)
		    bi =  w
		else
		    bi = -w
		br = ai / (2.0*bi) 
	    }
	}
end


# CEXP1 - Complex exponentiation routine.

procedure cexp1 (a, b, dr, di) 

real	a				#I Real part of argument
real	b				#I Complex part of argument
real	dr, di				#O Resultant real/imaginary components

begin
	if (a > log(MAX_REAL)) {
	    dr = 0.0
	    di = 0.0
	} else
	    call cx_div (cos(b), sin(b), exp(a), 0.0, dr, di)
end 


# CX_PAK - Pack two real arrays of an FFT into one real FFT array.
# The array `fft' must be dimensioned to at least 2*fnpts elements.

procedure cx_pak (creal, cimg, fft, fnpts) 

real	creal[fnpts], cimg[fnpts]	#I Real/Img complex components
real	fft[ARB]			#O Output 'real' array
int	fnpts				#I Npts in array

int	i,j

begin
	j = 1
	do i = 1, fnpts {
	    fft[j] = creal[i]
	    j = j + 1
	    fft[j] = cimg[i]
	    j = j + 1
	}
end


# CX_UNPAK - Unpack one real FFT array into two component real arrays.
# The array `fft' must be dimensioned to at least 2*fnpts elements.

procedure cx_unpak (fft, creal, cimg, fnpts) 

real	fft[ARB]			#O Output 'real' array
real	creal[fnpts], cimg[fnpts]	#I Real/Img complex components
int	fnpts				#I Npts in array

int	i,j

begin
	j = 1
	do i = 1, fnpts {
	    creal[i] = fft[j]
	    j = j + 1
	    cimg[i] = fft[j]
	    j = j + 1
	}
end 
