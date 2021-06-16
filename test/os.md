# OS specific function is host$os

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
	if ((corr < - 86400) || (corr > 86400)) {
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

