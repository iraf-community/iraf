#include <stdio.h>

/*  Compile with gcc -S to get demo assembler code.  In the actual ZSVJMP we
 *  need to execute basically these three lines of code but in the context of
 *  the routine calling the zsvjmp, so the stack needs to be adjusted 
 *  accordingly (i.e. the assembler from this code WILL NOT work since the
 *  ZDOJMP will return here, and not the parent routine).
 */

zsvjmp_(buf,status)
int *buf;
int *status;
{
 	*status = 0;
	buf[0] = *status;
	setjmp (&buf[1]);
}
