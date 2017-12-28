# Writing own procedures

## CL scripts

This is based on [An Introductory User’s Guide to IRAF
Scripts](http://iraf.noao.edu/iraf/ftp/iraf/docs/script.pdf) by Ed Anderson.

You will recall that several tasks may be called in sequence on a
single command line, using a semicolon `;` to separate each command.
For example:

```
cl> mkdir database; dir; cd database; dir
database
no files found
```

If the command sequence is too long to fit on a single line, one could
construct a compound statement by using the curly braces, `{}`:

```
cl> {
>>> dir
database
>>> cd database
>>> dir
no files found
>>> }
```

The `>>>` prompt indicates that the CL requires more input (in this
case, the CL is waiting for a `}`) before executing the task or
sequence of tasks.  A Terminal Script is essentially a compound
statement, but uses some of the simple programming tools provided by
the CL.

Now we check some control structures. First create a list file with a
`for` loop

File: `loop.cl`
```
for (i = 1; i < 5; i+=1) {
    j = i*i
    print (i, j, >> "sqr.lis")
}
```

Execute this script and check the content of the list:


```
cl> cl < loop.cl
cl> type "sqr.lis"
1 1 
2 4 
3 9 
4 16 
```

Read the file just created with a `while` loop with this script:

File: `while.cl`
```
list = "sqr.lis"
while (fscan(list, i, j) != EOF) print(i, j)
```



```
cl> cl < while.cl
1 1 
2 4 
3 9 
4 16 
```

# SPP tasks

This example is taken from a the [slides of a talk by Rob
Seaman](http://iraf.noao.edu/ftp/docs/spp_intro.pdf).

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

This is a test for [#98](https://github.com/iraf/iraf-v216/pull/98).

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

This is a test for [#106](https://github.com/iraf/iraf-v216/pull/106).

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

This is a test for [#107](https://github.com/iraf/iraf-v216/pull/107).

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

## Loop optimization

This is a test for [#60](https://github.com/iraf/iraf-v216/pull/60).

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

## Non-local goto

IRAF uses its own version of a "long jump" (non-local goto), which needs a
small piece of code to be written in assembler (`zsvjmp.s`). This is highly
CPU and OS specific. Here is some test code:

File: `jmptest.x`
```
include <config.h>
task jmptest = t_jmptest
procedure t_jmptest ()
int jmp_buf[LEN_JUMPBUF]
int status, step
begin
    status = 0
    step = 0

    call zsvjmp(jmp_buf, status)
    call printf("status = %d, step = %d\n")
    call pargi(status)
    call pargi(step)
    if (status == 0) {
        if (step == 1) {
	    call printf("Error: Called zsvjmp a second time\n")
            return
	 }
         step = 1
         call printf("Calling zdojmp\n")
         call zdojmp(jmp_buf, status)
         call printf("Error: return from ZDOJMP\n")
         return
      }
      if (step == 0) {
         call printf("Error: ZSVJMP was not called successfully\n")
         return
      }
      call printf("All OK\n")
end
```

`ZSVJMP` saves the processor registers in a buffer, while a following `ZDOJMP`
restores them (and therefore goes back to the place where `ZSVJMP was called).

```
cl> softools
cl> xc -w jmptest.x
cl> task $jmptest = jmptest.e
cl> jmptest
status = 0, step = 0
Calling zdojmp
status = 1, step = 1
All OK
```

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
