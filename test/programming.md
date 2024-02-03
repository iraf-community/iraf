# Writing own procedures

## SPP tasks

This example is taken from a the [slides of a talk by Rob
Seaman](https://iraf-community.github.io/doc/spp_intro.pdf).

Take a simple program from the test directory:

File: `hello.x`
```
# HELLO -- Sample program introducing SPP.
task hello = t_hello_world
procedure t_hello_world ()
begin
    call printf ("Hello,world!\n")
end
```

Compile it, declare and run as an IRAF task

```
cl> softools
cl> xc hello.x
cl> task $hello = hello.e
cl> hello
Hello, world!
```

## SPP debugging info

This is a test for [#98](https://iraf-community.github.io/iraf-v216/issues/98).

XC is able to keep the line number of the main file, for
debugging. This is done with the `-x` flag:

```
cl> softools
cl> xc -x -f hello.x
cl> system
cl> tail hello.f nlines=22 | head nlines=11
      subroutine thelld ()
      integer*2 st0001(14)
      save
      integer iyy
      data (st0001(iyy),iyy= 1, 8) / 72,101,108,108,111, 44,119,111/
      data (st0001(iyy),iyy= 9,14) /114,108,100, 33, 10, 0/
#line 5 "hello.x"
         call xprinf(st0001)
100      call zzepro
         return
      end
```

## Machine dependent limits

This is a test for [#106](https://iraf-community.github.io/iraf-v216/issues/106).

Some limits are defined by the `R1MACH` and `D1MACH` functions. We
just test some very basic properties of these numbers:

File: `machtest.x`
```
task machtest = t_machtest
procedure t_machtest ()
real r1mach()
double d1mach()
int i
int failures
begin

# Check that all values are positive numbers

    failures = 0
    do i=1, 5 {
        if (! (r1mach(i) == r1mach(i))) {
            call printf("R1MACH(%d) is not a number\n")
	         call pargi(i)
            failures = failures + 1
        } else if (r1mach(i) <= 0) {
            call printf("R1MACH(%d) is not positive\n")
	         call pargi(i)
            failures = failures + 1
	}
    }
    call printf("\n")
    do i=1, 5 {
        if (! (d1mach(i) == d1mach(i))) {
            call printf("D1MACH(%d) is not a number\n")
	         call pargi(i)
            failures = failures + 1
        } else if (d1mach(i) <= 0) {
            call printf("D1MACH(%d) is not positive\n")
	         call pargi(i)
            failures = failures + 1
	}
    }
    if (failures == 0) {
        call printf("Simple consistency check passed.\n")
    } else {
        call printf("Simple consistency check has %d failures\n")
        call pargi(failures)
    }
end
```
Running this should give:

```
cl> softools
cl> xc machtest.x
cl> task $machtest = machtest.e
cl> machtest

Simple consistency check passed.
```

## ILP64 memory model

This is a test for [#107](https://iraf-community.github.io/iraf-v216/issues/107).

On 64-bit machines, IRAF uses the ILP64 model that makes normal SPP
`integer` 8 byte wide. `real` values however remain at 4 byte. This
should be recognized by the Fortran compiler when doing
`equivalence`.

File: `test_equiv.x`
```
task test_equiv = t_equiv
procedure t_equiv ()
real	fval, gval
int	ival
%	equivalence (fval, ival)
begin
    ival = 0
    gval = 1.2345e06
    call printf("Should be zero: %d\n")
    call pargi(ival)

    gval = 0.
    ival = 998765123423
    call printf("Should be zero: %g\n")
    call pargr(gval)
end
```

In that example, setting `ival` should not affect the independently
defined variable `gval` and vice versa:

```
cl> softools
cl> xc test_equiv.x
cl> task $test_equiv = test_equiv.e
cl> test_equiv
Should be zero: 0
Should be zero: 0.
```

## Plain FORTRAN files

The xc compiler should be able to compile plain FORTRAN files (with
FORTRAN I/O) as well. The f2c compiler used in 2.17, 2.17.1, 2.18
cannot compile this. See [discussion
#369](https://github.com/orgs/iraf-community/discussions/369).

File: `test_io.f`
```
      PROGRAM TESTIO
      OPEN (11, FILE='testio.dat')
      WRITE (11, *) 'Hello world'
      CLOSE (11)
      END
```

```
cl> softools
cl> xc -h test_io.f
cl> !./test_io.e
cl> type testio.dat
Hello world
```

## Loop optimization

This is a test for [#60](https://iraf-community.github.io/iraf-v216/issues/60).

With the original 2.16.1 release, there is a problem with loop
optimization on "newer" platforms. A simple example task is here:

File: `otest.x`
```
task otest = t_otest
procedure t_otest ()
int i
pointer p, sp
begin
    call smark(sp)
    call salloc(p, 4, TY_DOUBLE)
    do i = 1, 4
        memd[p+i-1] = i

    do i = 1, 4 {
        call printf("%d == %g\n")
	    call pargi(i)
        call pargd(memd[p+i-1])
    }

    call sfree(sp)
end
```

All this code does is to allocate a temporary array with four
integers, fill each position with its index, and then print out the
integers. Compile it, declare the task in (e)cl and run it:

```
cl> softools
cl> xc otest.x
cl> task $otest = otest.e
cl> otest
1 == 1.
2 == 2.
3 == 3.
4 == 4.
```

See issue #73 for the bug report.

## The `generic` preprocessor

The `generic` preprocessor is used to translate generic source code (code
written to work for any datatype) into type dependent source code,
suitable for compilation and insertion into a library.  The generic source
is translated for each datatype, producing a type dependent copy of the
source code for each datatype.

One way to operate `generic` is to embed `$for` and `$endfor`
directives into the source file. The example is taken from the
`sys/vops` subdir:

File: `aabs.gx`
```
$for (dr)
procedure aabs$t (a, b, npix)
PIXEL	a[ARB], b[ARB]
int	npix, i
begin
	do i = 1, npix
	    b[i] = abs(a[i])
end
$endfor
```

which produces the following single output file:

```
cl> softools
cl> generic aabs.gx -o aabs.x
cl> dir aabs*
aabs.gx aabs.x 
cl> type aabs.x

procedure aabsd (a, b, npix)
double	a[ARB], b[ARB]
int	npix, i
begin
	do i = 1, npix
	    b[i] = abs(a[i])
end

procedure aabsr (a, b, npix)
real	a[ARB], b[ARB]
int	npix, i
begin
	do i = 1, npix
	    b[i] = abs(a[i])
end

```

One may also specify the types on the command line of `generic`, like
for this input file:

File: `alim.gx`
```
procedure alim$t (a, npix, minval, maxval)
PIXEL   a[ARB], minval, maxval, value
int     npix, i
begin
        minval = a[1]
        maxval = a[1]
        do i = 1, npix {
            value = a[i]
            $if (datatype == x)
                if (abs(value) < abs(minval))
                    minval = value
                else if (abs(value) > abs(maxval))
                    maxval = value
            $else
                if (value < minval)
                    minval = value
                else if (value > maxval)
                    maxval = value
            $endif
        }
end
```

It produces one output file per type:

```
cl> softools
cl> generic -t  silrdx alim.gx
cl> dir alim*
alim.gx   alimd.x   alimi.x   aliml.x   alimr.x   alims.x   alimx.x   
cl> type alimi.x
procedure alimi (a, npix, minval, maxval)
int   a[ARB], minval, maxval, value
int     npix, i
begin
        minval = a[1]
        maxval = a[1]
        do i = 1, npix {
            value = a[i]
            if (value < minval)
                minval = value
            else if (value > maxval)
                maxval = value
        }
end
cl> type alimr.x
procedure alimr (a, npix, minval, maxval)
real   a[ARB], minval, maxval, value
int     npix, i
begin
        minval = a[1]
        maxval = a[1]
        do i = 1, npix {
            value = a[i]
                if (value < minval)
            minval = value
                else if (value > maxval)
            maxval = value
        }
end
```

The `generic` has a specific handling for `INDEF`: `INDEF` and
`IS_INDEF` are replaced with the ones specific for the processed data
type, while `INDEFR` etc. are kept as they are. Also, `$if` can limit
the preprocessing.

For example,

File: `geofxy.gx`
```
$for (rd)
procedure geo_fxy$t(fit, sf1)
pointer fit, sf1
begin
$if (datatype == r)
        if (IS_INDEFD(GM_XO(fit)))
            call gsset (sf1, GSXREF, INDEF)
$else
        call gsset (sf1, GSXREF, INDEF)
$endif
end
$endfor
```

which produces:

```
cl> softools
cl> generic geofxy.gx -o geofxy.x
cl> type geofxy.x

procedure geo_fxyr(fit, sf1)
pointer fit, sf1
begin
        if (IS_INDEFD(GM_XO(fit)))
            call gsset (sf1, GSXREF, INDEFR)
end

procedure geo_fxyd(fit, sf1)
pointer fit, sf1
begin
        call gsset (sf1, GSXREF, INDEFD)
end

```
