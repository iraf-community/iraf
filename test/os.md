# OS specific function is host$os

## ZGTIME

ZGTIME returns a local clock  time in seconds and the
used cpu time in milliseconds.
	
File: `timetest.x`
```
task timetest = t_timetest
procedure t_timetest ()
long clktime0, cputime0
long clktime1, cputime1
int errs
begin
    call zgtime(clktime0, cputime0)
    if (clktime0 < 1280000000) {
        call printf("Unreasonable clktime %d (cputime is %d)\n")
            call pargl(clktime0)
            call pargl(cputime0)
	return
    }
    clktime1 = clktime0
    cputime1 = cputime0
    errs = 0
    # Busy loop until wall clock increases by one second
    while (clktime1 == clktime0) {
        call zgtime(clktime1, cputime1)
    }
    if (clktime1 < clktime0) {
        call printf("ERROR: Backward clktime %d --> %d\n")
            call pargl(clktime0)
            call pargl(clktime1)
	errs = errs + 1
    }
    if (clktime1 > clktime0 + 2) {
        call printf("ERROR: Clktime is too fast %d --> %d\n")
            call pargl(clktime0)
            call pargl(clktime1)
	errs = errs + 1
    }
    if (clktime1 > clktime0 + 2) {
        call printf("ERROR: Clktime is too fast %d --> %d\n")
            call pargl(clktime0)
            call pargl(clktime1)
	errs = errs + 1
    }
    if (cputime1 < cputime0) {
        call printf("ERROR: Backward cputime %d --> %d\n")
            call pargl(cputime0)
            call pargl(cputime1)
	errs = errs + 1
    }
    if (cputime1 == cputime0) {
        call printf("ERROR: Constant cputime %d --> %d\n")
            call pargl(cputime0)
            call pargl(cputime1)
	errs = errs + 1
    }
    if (cputime1 > cputime0 + 1000 * (clktime1 - clktime0)) {
        call printf("ERROR: Cputime faster that clktime: %d --> %d\n")
            call pargl(cputime0)
            call pargl(cputime1)
	errs = errs + 1
    }
    call printf("%d errors\n")
        call pargi(errs)
end
```

The test program should just return zero errors:

```
cl> softools
cl> xc -w timetest.x
cl> task $timetest = timetest.e
cl> timetest
0 errors
```

## ZGMTCO

ZGMTCO returns the correction, in seconds, from local standard
time. This may have some issues with daylight saving time.

File: `gmtcotest.x`
```
task gmtcotest = t_gmtcotest
procedure t_gmtcotest ()
int corr
begin
    call zgmtco(corr)
	if ((corr < 315532800 - 86400) || (corr < 315532800 - 86400)) {
	    call printf("Unreasonable correction %d\n")
		    call pargi(corr)
	} else {
	    call printf("OK")
	}
end
```

The test program should just return OK:

```
cl> softools
cl> xc -w gmtcotest.x
cl> task $gmtcotest = gmtcotest.e
cl> gmtcotest
OK
```

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

