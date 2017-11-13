# Test of Numerical Recipes functions (and their replacements)

Historically, IRAF contains a number of files that contain code from
the book "Numerical Recipes" (NR). The code from this book is not free
(neither as in Free Beer, nor as in Free Speech), and so it is
required to replace this code to make IRAF truly Open Source.

These tests written to ensure that code replacing Numerical Recipes
code will get the same results.

## Simple functions and random numbers

### GASDEV: normally distributed deviate of zero mean and unit var

We test the statistical momentums for the distribution.

File: `test-gasdev.x`
```
task test_gasdev = t_gasdev
procedure t_gasdev ()
real gasdev()
double x, y
double m[4]
int i,j,n
begin
	n=20000000
	do j=1, 4 {
		m[j] = 0
	}
	do i=1, n {
	    x = gasdev(0)
		y = 1
		do j=1, 4 {
		    y = y * x
			m[j] = m[j] + y
	    }
	}
	call printf("Mean     = %f\n")
	call pargd(m[1]/n)
	call printf("Variance = %f\n")
	call pargd(m[2]/n)
	call printf("Skew     = %f\n")
	call pargd(m[3]/n)
	call printf("Kurtosis = %f\n")
	call pargd(m[4]/n)
end
```

Compile it, declare and run as an IRAF task. The following numbers are
the result of the original NR code.

Test options: `decimals=2`
```
cl> copy noao$artdata/numrecipes.x .
cl> copy noao$artdata/gammln.c .
cl> softools
cl> xc -I$iraf/include test-gasdev.x numrecipes.x gammln.c
cl> task $test_gasdev = test-gasdev.e
cl> test_gasdev
Mean     = 0.0004794365668388
Variance = 1.0000408915199221
Skew     = 0.0013315610063655
Kurtosis = 3.0004513785393642
```

### POIDEV: Poisson deviates for a given mean

File: `test-poidev.x`
```
task test_poidev = t_poidev
procedure t_poidev ()
real poidev()
real x, y, xm
real clgetr()
double m[4]
int i,j,n, start
data start/0/
begin
    xm=clgetr("xm")
	n=1500000
	do j=1, 4 {
		m[j] = 0
	}
	do i=1, n {
	    x = poidev(xm,0) - xm
		y = 1
		do j=1, 4 {
		    y = y * x
			m[j] = m[j] + y
	    }
	}
	if (start == 0) {
		call printf("     xm |       mean |   variance |       skew |   kurtosis\n")
		call printf("--------|------------|------------|------------|------------\n")
		start = 1
	}
	# To avoid accuracy problems with different magnitudes of the value,
	# we normalize all expected values for the moments to 1.0.
	call printf("%7.1f | %10.4f | %10.4f | %10.4f | %10.4f\n")
	call pargr(xm)
	call pargd((xm + m[1]/n) / xm) # expected: xm
	call pargd((m[2]/n) / xm)      # expected: xm
	call pargd((m[3]/n) / xm)      # expected: xm
	call pargd((m[4]/n) / (xm * (1+3*xm))) # expected: xm*(1+3*xm)
end
```

Compile it, declare and run as an IRAF task. The following numbers are
the result of the original NR code.

Test options: `decimals=1`
```
cl> softools
cl> xc -x -I$iraf/include test-poidev.x numrecipes.x gammln.c
cl> task $test_poidev = test-poidev.e
cl> for ( x = 0.7; x < 60; x*=1.55)  test_poidev(x)
     xm |       mean |   variance |       skew |   kurtosis
--------|------------|------------|------------|------------
    0.7 |     0.9986 |     0.9998 |     1.0006 |     1.0014
    1.1 |     0.9991 |     1.0007 |     1.0007 |     1.0012
    1.7 |     1.0000 |     1.0009 |     1.0026 |     1.0021
    2.6 |     1.0006 |     1.0011 |     1.0055 |     1.0013
    4.0 |     1.0000 |     0.9988 |     0.9925 |     0.9951
    6.3 |     1.0004 |     1.0019 |     1.0155 |     1.0040
    9.7 |     1.0000 |     1.0004 |     1.0056 |     1.0011
   15.0 |     0.9999 |     1.0006 |     1.0010 |     0.9995
   23.3 |     1.0001 |     1.0013 |     1.0101 |     1.0027
   36.1 |     1.0001 |     0.9997 |     1.0120 |     1.0041
   56.0 |     0.9999 |     1.0001 |     0.9719 |     1.0023
```

### GAMMLN: natural log of gamma function

File: `test-gammln.x`
```
task test_gammln = t_gammln
procedure t_gammln ()
real gammln()
real x, y
real clgetr()
begin
    x = clgetr("x")
	y = gammln(x)
	call printf("gammln(%4.2f) = %.6f\n")
	call pargr(x)
	call pargr(y)
end
```

Compile it, declare and run as an IRAF task. The following numbers are
the result of the original NR code.

Test options: `decimals=4`
```
cl> softools
cl> xc -x -I$iraf/include test-gammln.x gammln.c
cl> task $test_gammln = test-gammln.e
cl> for ( x = 0.23; x < 10; x += 0.667)  test_gammln(x)
gammln(0.23) = 1.376194
gammln(0.90) = 0.068650
gammln(1.56) = -0.116568
gammln(2.23) = 0.114094
gammln(2.90) = 0.601105
gammln(3.57) = 1.273372
gammln(4.23) = 2.090656
gammln(4.90) = 3.027074
gammln(5.57) = 4.064579
gammln(6.23) = 5.189871
gammln(6.90) = 6.392745
gammln(7.57) = 7.665116
gammln(8.23) = 9.000430
gammln(8.90) = 10.393257
gammln(9.57) = 11.839035
cl> test_gammln(20)
gammln(20.0) = 39.339886
cl> test_gammln(80)
gammln(80.0) = 269.291107
cl> test_gammln(200)
gammln(200.0) = 857.933655
```

### DAORAN: random number generator RAN2

The DAORAN random number generator is going to be replaced by the
URAND random number generator by D.Knuth.

File: `test-ran2.x`
```
task test_ran2 = t_ran2
procedure t_ran2 ()
real urand()
double x, y
double m[4]
int i,j,n
begin
	n=20000000
	do j=1, 4 {
		m[j] = 0
	}
	do i=1, n {
	    x = urand(0)
		if (x < 0 | x >= 1) {
			call printf("Outlyer: %g\n")
			call pargd(x)
	    }
		x = x - 0.5
		y = 1
		do j=1, 4 {
		    y = y * x
			m[j] = m[j] + y
	    }
	}
	call printf("Mean     = %f\n")
	call pargd(m[1]/n)
	call printf("Variance = %f\n")
	call pargd(m[2]/n)
	call printf("Skew     = %f\n")
	call pargd(m[3]/n)
	call printf("Kurtosis = %f\n")
	call pargd(m[4]/n)
end
```

Compile it, declare and run as an IRAF task. The following numbers are
the result of the original NR code.

Test options: `decimals=4`
```
cl> softools
cl> xc test-ran2.x
cl> task $test_ran2 = test-ran2.e
cl> test_ran2
Mean     = 0.0000018238614310
Variance = 0.0833325982276019
Skew     = 0.0000003118007337
Kurtosis = 0.0124998479272606
```

### AST_JULDAY_TO_DATE: Convert Julian date to calendar date

File: `test-jd.x`
```
task test_jd = t_jd
procedure t_jd ()
double jd
int year, month, day
double t
double clgetd()
begin
    jd = clgetd("jd")
	call ast_julday_to_date(jd, year, month, day, t)
	call printf("JD (%.2f) = %d/%02d/%02d + %5.2f\n")
	call pargd(jd)
	call pargi(year)
	call pargi(month)
	call pargi(day)
	call pargd(t)
end
```

Compile it, declare and run as an IRAF task. The following numbers are
the result of the original NR code.

```
cl> copy noao$astutil/asttools/asttimes.x .
cl> softools
cl> xc -/Wno-shift-op-parentheses test-jd.x asttimes.x
cl> task $test_jd = test-jd.e
cl> for (x = 2450123.7; x < 2450123.7 + 35; x += 2.13)  test_jd(jd=x)
JD (2450123.70) = 1996/02/10 +  4.80
JD (2450125.83) = 1996/02/12 +  7.92
JD (2450127.96) = 1996/02/14 + 11.04
JD (2450130.09) = 1996/02/16 + 14.16
JD (2450132.22) = 1996/02/18 + 17.28
JD (2450134.35) = 1996/02/20 + 20.40
JD (2450136.48) = 1996/02/22 + 23.52
JD (2450138.61) = 1996/02/25 +  2.64
JD (2450140.74) = 1996/02/27 +  5.76
JD (2450142.87) = 1996/02/29 +  8.88
JD (2450145.00) = 1996/03/02 + 12.00
JD (2450147.13) = 1996/03/04 + 15.12
JD (2450149.26) = 1996/03/06 + 18.24
JD (2450151.39) = 1996/03/08 + 21.36
JD (2450153.52) = 1996/03/11 +  0.48
JD (2450155.65) = 1996/03/13 +  3.60
JD (2450157.78) = 1996/03/15 +  6.72
```

## Matrix operations

### LU decomposition of a square matrix

`MW_LUDECOMPOSE`, `LUDCMD`, `LUDCMP`: Replace an NxN matrix A by the LU
decomposition of a rowwise permutation of the matrix.  The LU decomposed
matrix A and the permutation index IX are output.  The decomposition is
performed in place.

`MW_LUBACKSUB`, `LUBKSD`, `LUBKSB`: Solves the set of N linear equations
`A*X=B`.  Here A is input, not as the matrix A but rather as its LU
decomposition, determined by the routine mw_ludecompose.  IX is input as the
permutation vector as returned by `MW_LUDECOMPOSE` & Co.  B is input as the
right hand side vector B, and returns with the solution vector X.

These subroutines are going to be replaced by LAPACK routines

File: `test_ludecompose.x`
```
task test_ludecompose = t_ludecompose

procedure eval_ludecompose (x, y, ndim)
double x[ndim, ndim]
double y[ndim]
int ndim, i, j, istat
double d
pointer sp, ix, m_in, m_out, y_in
begin
    call smark(sp)
    call salloc(ix, ndim, TY_INT)
    call salloc(m_out, ndim*ndim, TY_DOUBLE)
    call salloc(m_in, ndim*ndim, TY_DOUBLE)
    call salloc(y_in, ndim, TY_DOUBLE)
    call achtdd(x, Memd[m_in], ndim*ndim)
    call achtdd(y, Memd[y_in], ndim)

# Test MW_LUDECOMPOSE and MW_INVERTD

    call printf("Original matrix:\n")
    do i = 1, ndim {
        call printf("%d  ")
	call pargi(i)
	do j = 1, ndim {
	    call printf(" %6.3f")
	    call pargd(x[j, i])
	}
	call printf("\n")
    }
    call mw_invertd(Memd[m_in], Memd[m_out], ndim)
    call printf("Inverted matrix:\n")
    do i = 1, ndim {
        call printf("%d  ")
	call pargi(i)
	do j = 1, ndim {
	    call printf(" %6.3f")
	    call pargd(Memd[m_out + (j-1) + (i-1)*ndim])
	}
	call printf("\n")
    }
    call mw_ludecompose(Memd[m_in], Memi[ix], ndim)
    call printf("Decomposed matrix (MW_LUDECOMPOSE):\n")
    do i = 1, ndim {
        call printf("%d %d")
	call pargi(i)
	call pargi(Memi[ix+i-1])
	do j = 1, ndim {
	    call printf(" %6.3f")
	    call pargd(Memd[m_in + (j-1) + (i-1)*ndim])
	}
	call printf("\n")
    }

    call printf("solve [")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargd(y[j])
    }
    call mw_lubacksub(Memd[m_in], Memi[ix], Memd[y_in], ndim)
    call printf("] ==> [")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargd(Memd[y_in + (j-1)])
    }
    call printf("]\n")

    call achtdd(x, Memd[m_in], ndim*ndim)
    call ludcmd(Memd[m_in], ndim, ndim, Memi[ix], d, istat)
    call printf("Decomposed matrix (LUDCMD):\n")
    do i = 1, ndim {
        call printf("%d %d")
	call pargi(i)
	call pargi(Memi[ix+i-1])
	do j = 1, ndim {
	    call printf(" %6.3f")
	    call pargd(Memd[m_in + (j-1) + (i-1)*ndim])
	}
	call printf("\n")
    }

    call achtdd(y, Memd[y_in], ndim)
    call printf("solve [")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargd(y[j])
    }
    call lubksd(Memd[m_in], ndim, ndim, Memi[ix], Memd[y_in])
    call printf("] ==> [")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargd(Memd[y_in + (j-1)])
    }
    call printf("]\n")

    call salloc(m_in, ndim*ndim, TY_REAL)
    call achtdr(x, Memr[m_in], ndim*ndim)
    call ludcmp(Memr[m_in], ndim, ndim, Memi[ix], d)
    call printf("Decomposed matrix (LUDCMB):\n")
    do i = 1, ndim {
        call printf("%d %d")
	call pargi(i)
	call pargi(Memi[ix+i-1])
	do j = 1, ndim {
	    call printf(" %6.3f")
	    call pargr(Memr[m_in + (j-1) + (i-1)*ndim])
	}
	call printf("\n")
    }
    call salloc(y_in, ndim, TY_REAL)
    call achtdr(y, Memr[y_in], ndim)
    call printf("solve [")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargd(y[j])
    }
    call lubksb(Memr[m_in], ndim, ndim, Memi[ix], Memr[y_in])
    call printf("] ==> [")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargr(Memr[y_in + (j-1)])
    }
    call printf("]\n")

    call sfree(sp)
end

procedure t_ludecompose ()
double x1[2,2], y1[2]
data x1 /2., 8., 1., 7./
data y1 /4., 1./
double x2[3,3], y2[3]
data x2 /4., 5., -2., 7., -1., 2., 3., 1., 4./
data y2 /1., 3., 2./
begin
    call eval_ludecompose(x1, y1, 2)
    call eval_ludecompose(x2, y2, 3)
end
```

```
cl> copy pkg$utilities/nttools/stxtools/lu* .
cl> softools
cl> xc -x test_ludecompose.x lubksb.f  lubksd.f  ludcmd.x  ludcmp.x
cl> task $test_ludecompose = test_ludecompose.e
cl> test_ludecompose
Original matrix:
1    2.000  8.000
2    1.000  7.000
Inverted matrix:
1    1.167 -1.333
2   -0.167  0.333
Decomposed matrix (MW_LUDECOMPOSE):
1 2  8.000  0.250
2 2  7.000 -0.750
solve [  4.000  1.000] ==> [  4.500 -5.000]
Decomposed matrix (LUDCMD):
1 2  8.000  0.250
2 2  7.000 -0.750
solve [  4.000  1.000] ==> [  4.500 -5.000]
Decomposed matrix (LUDCMB):
1 2  8.000  0.250
2 2  7.000 -0.750
solve [  4.000  1.000] ==> [  4.500 -5.000]
Original matrix:
1    4.000  5.000 -2.000
2    7.000 -1.000  2.000
3    3.000  1.000  4.000
Inverted matrix:
1    0.039  0.143 -0.052
2    0.143 -0.143  0.143
3   -0.065 -0.071  0.253
Decomposed matrix (MW_LUDECOMPOSE):
1 2  5.000  0.800 -0.400
2 2 -1.000  7.800  0.205
3 3  1.000  2.200  3.949
solve [  1.000  3.000  2.000] ==> [  0.338 -0.429  0.883]
Decomposed matrix (LUDCMD):
1 2  5.000  0.800 -0.400
2 2 -1.000  7.800  0.205
3 3  1.000  2.200  3.949
solve [  1.000  3.000  2.000] ==> [  0.338 -0.429  0.883]
Decomposed matrix (LUDCMB):
1 2  5.000  0.800 -0.400
2 2 -1.000  7.800  0.205
3 3  1.000  2.200  3.949
solve [  1.000  3.000  2.000] ==> [  0.338 -0.429  0.883]
```

## Fast Fourier transform

### REALFT: FFT of a set of 2N real valued data points

`realfft` Calculates the Fourier Transform of a set of 2N real valued data
points.  Replaces this data by the positive frequency half of it's complex
Fourier Transform.  The real valued first and last components of the complex
transform are returned as elements DATA(1) and DATA(2) respectively.  N must
be an integer power of 2.  This routine also calculates the inverse transform
of a complex array if it is the transform of real data.  (Result in this case
must be multiplied by 1/N). A forward transform is perform for `isign == 1`,
otherwise the inverse transform is computed.

File: `test_realfft.x`
```
task test_realfft = t_realfft

procedure eval_realfft(x, ndim)
int ndim
real x[ndim]
int j
begin
    call printf("FFT input  ")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargr(x[j])
    }
    call realfft(x, ndim/2, 1)
    call printf("\nFFT output ")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargr(x[j])
    }
    call realfft(x, ndim/2, -1)
    call printf("\nInverse    ")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargr(x[j]/(ndim/2))
    }
    call printf("\n")
end

procedure t_realfft ()
real x[8]
data x /1.0, 2.0, 1.0, -1.0, 1.5, 1.0, 0.5, 1.0/
begin
    call eval_realfft(x, 8)
end
```

```
cl> copy noao$rv/numrep.x .
cl> softools
cl> xc -x test_realfft.x numrep.x
cl> task $test_realfft = test_realfft.e
cl> test_realfft
FFT input    1.000  2.000  1.000 -1.000  1.500  1.000  0.500  1.000
FFT output   7.000  1.000  1.621 -0.207  1.000  3.000 -2.621 -1.207
Inverse      1.000  2.000  1.000 -1.000  1.500  1.000  0.500  1.000
```

### TWOFFT: complex FFTs of two input real arrays

Given two real input arrays, each of length N, this routine calls `cc_four1()`
and returns two complex output arrays, FFT1 and FFT2, each of complex length N
(i.e. real length 2*N), which contain the discrete Fourier transforms of the
respective `DATA`s.  As always, N must be an integer power of 2.

File: `test_twofft.x`
```
task test_twofft = t_twofft

procedure eval_twofft(x1, x2, ndim)
int ndim
real x1[ndim], x2[ndim]
int j
pointer sp, y1, y2

begin
    call smark(sp)
    call salloc(y1, 2*ndim, TY_REAL)
    call salloc(y2, 2*ndim, TY_REAL)

    call printf("in 1 ")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargr(x1[j])
    }
    call printf("\nin 2 ")
    do j = 1, ndim {
        call printf(" %6.3f")
        call pargr(x2[j])
    }
    call twofft(x1, x2, Memr[y1], Memr[y2], ndim)
    call printf("\nout 1")
    do j = 1, ndim {
        call printf(" %6.3f + %6.3fi,")
        call pargr(Memr[y1 + 2*j-2])
        call pargr(Memr[y1 + 2*j-1])
    }
    call printf("\nout 2")
    do j = 1, ndim {
        call printf(" %6.3f + %6.3fi,")
        call pargr(Memr[y2 + 2*j-2])
        call pargr(Memr[y2 + 2*j-1])
    }
    call printf("\n")

    call sfree(sp)
end

procedure t_twofft ()
real x1[8]
real x2[8]
data x1 /1.0, 2.0, 1.0, -1.0, 1.5, 1.0, 0.5, 1.0/
data x2 /0.9, 7.5, 6.5, 5.0, 7.5, 5.2, 5.1, 7.7/
begin
    call eval_twofft(x1, x2, 8)
end
```

Test options: `decimals=3`
```
cl> softools
cl> xc -x test_twofft.x numrep.x
cl> task $test_twofft = test_twofft.e
cl> test_twofft
in 1   1.000  2.000  1.000 -1.000  1.500  1.000  0.500  1.000
in 2   0.900  7.500  6.500  5.000  7.500  5.200  5.100  7.700
out 1  7.000 +  0.000i,  1.621 + -0.207i,  1.000 +  3.000i, -2.621 + -1.207i,  1.000 +  0.000i, -2.621 +  1.207i,  1.000 + -3.000i,  1.621 +  0.207i,
out 2 45.400 +  0.000i, -3.064 +  1.117i, -3.200 +  0.000i, -10.14 + -1.683i, -5.400 +  0.000i, -10.14 +  1.683i, -3.200 + 0.000i, -3.064 + -1.117i,
```

## Interpolation routines for `trebin`

This covers `tucspl`, `tuispl`, `tuhunt`, and `tuispl`.

These are just the [`trebin` test in `nttools`](nttools.md), with an
extended resolution:

File: `rebin.tbl`
```
#c lambda d %6.2f nm
#c Y d %6.3f pixels
453.02   5.873
464.60  17.939
603.04  39.843
625.08  68.326
647.27  44.617
723.45  68.226
730.31  36.557
764.82  42.797
784.33   2.650
862.67  38.502
```

### TUCSPL/TUISPL: Spline interpolation

```
cl> trebin rebin.tbl rebspl.tbl lambda 450. 550. 10. function=spline
rebin.tbl --> rebspl.tbl
cl> type rebspl.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
450.00  INDEF
460.00 13.514
470.00 22.065
480.00 26.780
490.00 28.293
500.00 27.306
510.00 24.524
520.00 20.653
530.00 16.395
540.00 12.456
550.00  9.540
cl> trebin rebin.tbl rebsplu.tbl lambda 860. 870. 10. function=spline
rebin.tbl --> rebsplu.tbl
cl> type rebsplu.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
860.00 33.545
870.00  INDEF
```

### TUIEP3: Poly3 interpolation


```
cl> trebin rebin.tbl rebpol.tbl lambda 450. 550. 10. function=poly3
rebin.tbl --> rebpol.tbl
cl> type rebpol.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
450.00  INDEF
460.00 13.681
470.00 22.107
480.00 27.688
490.00 30.876
500.00 32.122
510.00 31.879
520.00 30.599
530.00 28.733
540.00 26.734
550.00 25.052
cl> trebin rebin.tbl rebpolu.tbl lambda 860. 870. 10. function=poly3
rebin.tbl --> rebpolu.tbl
cl> type rebpolu.tbl
#c lambda d %6.2f nm
#c Y d %6.3f pixels
860.00 22.331
870.00  INDEF
```
