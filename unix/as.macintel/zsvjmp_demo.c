#include <stdio.h>
#include <setjmp.h>

/*  Compile with gcc -S to get demo assembler code.  In the actual ZSVJMP we
 *  need to execute basically these three lines of code but in the context of
 *  the routine calling the zsvjmp, so the stack needs to be adjusted 
 *  accordingly (i.e. the assembler from this code WILL NOT work since the
 *  ZDOJMP will return here, and not the parent routine).  The adjustment
 *  then is to change the return stack to point to jmpbuf[1] and zero the
 *  second argument of the jmp_buf env.
 */

void
zsvjmp_ (int *buf, int *status)
{
 	*status = 0;            // zero the return status value
	buf[0] = status;        // store address of status in jmp_buf[0]
	setjmp (&buf[1]);       // use setjmp to return to jmp_buf[1]
}
